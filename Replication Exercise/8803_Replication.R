## 8803 Replication Exercise
## Rohit Borah & Ping Yang
## April 2026


# Load requisite libraries ------------------------------------------------

library(tidyverse)
library(modelsummary)
library(haven)
library(gt)
library(ggthemes)
library(sf)
library(tigris)
library(zoo)
library(did)
library(fixest)
library(patchwork)
options(tigris_use_cache = TRUE)


# Load data ---------------------------------------------------------------

df_s17 <- read_dta("~/Desktop/PhD/Spring 2026/8803 - Economic Data Analysis/Replication Exercise/Data/CBC_Shale_Combined_Characteristics_17_5_Panel.dta")
df_w17 <- read_dta("~/Desktop/PhD/Spring 2026/8803 - Economic Data Analysis/Replication Exercise/Data/CBC_Wind_Combined_Characteristics_17_5_Panel.dta")


# 1.3 Descriptive Statistics ----------------------------------------------

# Reshape to long format with a column identifying variable group and type
bird_long <- df_s17 |>
  select(
    # Winsorized counts
    num_tot_w, num_grassland_w, num_woodland_w, num_wetland_w,
    num_otherhabitat_w, num_urban_w, num_nonurban_w,
    num_resident_w, num_shortmigration_w, num_longermigration_w,
    # Non-winsorized counts
    num_tot, num_grassland, num_woodland, num_wetland,
    num_otherhabitat, num_urban, num_nonurban,
    num_resident, num_shortmigration, num_longermigration,
    # Species counts (non-winsorized)
    spec_tot, spec_grassland, spec_woodland, spec_wetland,
    spec_otherhabitat, spec_urban, spec_nonurban,
    spec_resident, spec_shortmigration, spec_longermigration
  ) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(
    Panel = case_when(
      str_ends(variable, "_w")      ~ "wins",
      str_starts(variable, "num_")  ~ "raw",
      str_starts(variable, "spec_") ~ "spec"
    ),
    Label = case_when(
      str_detect(variable, "tot")            ~ "Total",
      str_detect(variable, "grassland")      ~ "Grassland/Shrubland",
      str_detect(variable, "woodland")       ~ "Woodland",
      str_detect(variable, "wetland")        ~ "Wetland",
      str_detect(variable, "otherhabitat")   ~ "Other Habitat",
      str_detect(variable, "nonurban")       ~ "Non-Urban",
      str_detect(variable, "urban")          ~ "Urban",
      str_detect(variable, "resident")       ~ "Resident",
      str_detect(variable, "shortmigration") ~ "Short/Irruptive Migrants",
      str_detect(variable, "longermigration")~ "Moderate/Long Migrants"
    ),
    Label = factor(Label, levels = c(
      "Grassland/Shrubland", "Woodland", "Wetland",
      "Other Habitat", "Urban", "Non-Urban",
      "Resident", "Short/Irruptive Migrants", "Moderate/Long Migrants",
      "Total"
    )),
    Panel = factor(Panel, levels = c("wins", "raw", "spec"))
  )

bird_long |>
  group_by(Panel, Label) |>
  summarise(
    Mean = mean(value, na.rm = TRUE),
    SD   = sd(value,   na.rm = TRUE),
    Max  = max(value,  na.rm = TRUE), # intentionally excluding min
    .groups = "drop"
  ) |>
  pivot_wider(
    id_cols     = Label,
    names_from  = Panel,
    values_from = c(Mean, SD, Max),
    names_vary  = "slowest",
    names_glue  = "{Panel}_{.value}"
  ) |>
  arrange(Label) |>
  gt(rowname_col = "Label") |>
  tab_spanner(label = "Bird Count (Winsorized)",        columns = starts_with("wins_")) |>
  tab_spanner(label = "Bird Count (Non-Winsorized)",    columns = starts_with("raw_")) |>
  tab_spanner(label = "Species Count (Non-Winsorized)", columns = starts_with("spec_")) |>
  cols_label(
    wins_Mean = "Mean", wins_SD = "Std. Dev.", wins_Max = "Max",
    raw_Mean  = "Mean", raw_SD  = "Std. Dev.", raw_Max  = "Max",
    spec_Mean = "Mean", spec_SD = "Std. Dev.", spec_Max = "Max"
  ) |>
  fmt_number(columns = c(wins_Mean, wins_SD, raw_Mean, raw_SD, spec_Mean, spec_SD), decimals = 1) |>
  fmt_number(columns = c(wins_Max, raw_Max, spec_Max), decimals = 0) |>
  tab_style(
    style     = cell_borders(sides = "left", color = "gray50", weight = px(1.5)),
    locations = cells_body(columns = c(raw_Mean, spec_Mean))
  ) |>
  tab_style(
    style     = cell_borders(sides = "left", color = "gray50", weight = px(1.5)),
    locations = cells_column_labels(columns = c(raw_Mean, spec_Mean))
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_stub()
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_spanners()
  ) |>
  tab_header(title = "Descriptive Statistics: Bird Population and Species Counts")


# 1.4 County-Level Maps ---------------------------------------------------

# Approach: point-in-polygon spatial join. Each CBC circle centroid is assigned
# to whichever county contains it. Data are then aggregated to county-year level
# and averaged across years for mapping.

lower48_fips <- tigris::fips_codes |>
  filter(!state %in% c("AK", "HI", "PR", "GU", "AS", "VI", "MP")) |>
  pull(state_code) |>
  unique()

counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2020) |>
  filter(STATEFP %in% lower48_fips) |>
  st_transform(crs = 4326)

# Assign shale circles to counties
circles_shale_sf <- df_s17 |>
  select(circle_id, latitude, longitude) |>
  distinct() |>
  drop_na(latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

circle_county_s <- st_join(circles_shale_sf,
                            counties_sf |> select(GEOID),
                            join = st_within) |>
  st_drop_geometry()

# Assign wind circles to counties
circles_wind_sf <- df_w17 |>
  select(circle_id, latitude, longitude) |>
  distinct() |>
  drop_na(latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

circle_county_w <- st_join(circles_wind_sf,
                            counties_sf |> select(GEOID),
                            join = st_within) |>
  st_drop_geometry()

# Aggregate shale data to county mean across years
county_shale <- df_s17 |>
  left_join(circle_county_s, by = "circle_id") |>
  filter(!is.na(GEOID)) |>
  group_by(GEOID, year) |>
  summarise(
    avg_birds = mean(num_tot_w,       na.rm = TRUE),
    avg_spec  = mean(spec_tot,        na.rm = TRUE),
    avg_wells = mean(shale_wells_num, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(GEOID) |>
  summarise(
    avg_birds = mean(avg_birds, na.rm = TRUE),
    avg_spec  = mean(avg_spec,  na.rm = TRUE),
    avg_wells = mean(avg_wells, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate wind data to county mean across years
county_wind <- df_w17 |>
  left_join(circle_county_w, by = "circle_id") |>
  filter(!is.na(GEOID)) |>
  group_by(GEOID, year) |>
  summarise(
    avg_wind_cap = mean(cumulative_capacity_mw, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(GEOID) |>
  summarise(
    avg_wind_cap = mean(avg_wind_cap, na.rm = TRUE),
    .groups = "drop"
  )

map_data <- counties_sf |>
  left_join(county_shale, by = "GEOID") |>
  left_join(county_wind,  by = "GEOID")

map_theme <- theme_fivethirtyeight() +
  theme(
    legend.position  = "right",
    legend.direction = "vertical",
    panel.grid.major = element_blank(),
    axis.text        = element_blank()
  )

# Figure 2a: County average bird population (winsorized)
p_birds <- ggplot(map_data) +
  geom_sf(aes(fill = avg_birds), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Count",
                       na.value = "grey85", labels = scales::comma) +
  labs(title = "Number of Birds Counted (2000-2020 Average, Winsorized)") +
  map_theme

p_birds

# Figure 2b: County average species count
p_species <- ggplot(map_data) +
  geom_sf(aes(fill = avg_spec), color = NA) +
  scale_fill_viridis_c(option = "magma", name = "Species", na.value = "grey85") +
  labs(title = "Number of Species Reported (2000-2020 Average)") +
  map_theme

p_species

# Figure 3a: County average shale wells
p_wells <- ggplot(map_data) +
  geom_sf(aes(fill = avg_wells), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Wells",
                       na.value = "grey85", labels = scales::comma) +
  labs(title = "Shale Oil and Gas Wells (2000-2020 Average)") +
  map_theme

p_wells

# Figure 3b: County average wind turbine capacity
p_wind <- ggplot(map_data) +
  geom_sf(aes(fill = avg_wind_cap), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Capacity (MW)",
                       na.value = "grey85", labels = scales::comma) +
  labs(title = "Wind Turbine Capacity (2000-2020 Average)") +
  map_theme

p_wind



# 2.4 Event Study Specification --------------------------------------------------

# Covariates for event study (Figures A5/A6): counters + weather only.
# Land-use shares are omitted here — they require interpolation across NLCD
# survey years and introduce ~20% missingness. The A5/A6 captions confirm
# land-use is not in the event study spec (unlike Figure 4).
covs_formula <- ~ total_effort_counters + Min_temp + Max_temp + Max_snow + Max_wind

# IHS-transform outcomes (both bird counts and species counts per paper)
df_s17 <- df_s17 |>
  mutate(
    y_birds_ihs = asinh(num_tot_w),
    y_spec_ihs  = asinh(spec_tot)
  ) #|>
  #drop_na(all_of(cov_vars), y_birds_ihs, y_spec_ihs) |>                       
  #group_by(circle_id) |>
  #filter(n()==21) |>
  #ungroup()
  

df_w17 <- df_w17 |>
  mutate(
    y_birds_ihs = asinh(num_tot_w),
    y_spec_ihs  = asinh(spec_tot)
  )

write_clip(df_w17)

# gname: first year circle is treated (0 = never treated), required by att_gt()
first_shale <- df_s17 |>
  filter(cum_shalewells > 0) |>
  group_by(circle_id) |>
  summarise(g_shale = min(year), .groups = "drop")

df_s17 <- df_s17 |>
  select(-any_of("g_shale")) |>
  left_join(first_shale, by = "circle_id") |>
  mutate(g_shale = replace_na(g_shale, 0L))

first_wind <- df_w17 |>
  filter(cumulative_capacity_mw > 0) |>
  group_by(circle_id) |>
  summarise(g_wind = min(year), .groups = "drop")

df_w17 <- df_w17 |>
  select(-any_of("g_wind")) |>
  left_join(first_wind, by = "circle_id") |>
  mutate(g_wind = replace_na(g_wind, 0L))

# --- Estimate att_gt (Callaway & Sant'Anna) -----------------------------------
# Note: these take a few minutes each

# Pre-filter: drop rows with NA in any covariate or outcome to avoid internal
# balancing issues inside att_gt
cov_vars <- c("total_effort_counters", "Min_temp", "Max_temp", "Max_snow", "Max_wind")

df_s17_cc <- df_s17 |> drop_na(all_of(cov_vars), y_birds_ihs, y_spec_ihs)
df_w17_cc <- df_w17 |> drop_na(all_of(cov_vars), y_birds_ihs, y_spec_ihs)

cs_shale_birds <- att_gt(
  yname                  = "y_birds_ihs",
  tname                  = "year",
  idname                 = "circle_id",
  gname                  = "g_shale",
  xformla                = covs_formula,
  data                   = df_s17_cc,
  control_group          = "nevertreated",
  clustervars            = "circle_id",
  allow_unbalanced_panel = TRUE,
  faster_mode            = FALSE,
  est_method             = "reg",
  bstrap                 = FALSE
)

cs_shale_spec <- att_gt(
  yname                  = "y_spec_ihs",
  tname                  = "year",
  idname                 = "circle_id",
  gname                  = "g_shale",
  xformla                = covs_formula,
  data                   = df_s17_cc,
  control_group          = "nevertreated",
  clustervars            = "circle_id",
  allow_unbalanced_panel = TRUE,
  faster_mode            = FALSE,
  est_method             = "reg",
  bstrap                 = FALSE
)

cs_wind_birds <- att_gt(
  yname                  = "y_birds_ihs",
  tname                  = "year",
  idname                 = "circle_id",
  gname                  = "g_wind",
  xformla                = covs_formula,
  data                   = df_w17_cc,
  control_group          = "nevertreated",
  clustervars            = "circle_id",
  allow_unbalanced_panel = TRUE,
  faster_mode            = FALSE,
  est_method             = "reg",
  bstrap                 = FALSE
)

cs_wind_spec <- att_gt(
  yname                  = "y_spec_ihs",
  tname                  = "year",
  idname                 = "circle_id",
  gname                  = "g_wind",
  xformla                = covs_formula,
  data                   = df_w17_cc,
  control_group          = "nevertreated",
  clustervars            = "circle_id",
  allow_unbalanced_panel = TRUE,
  faster_mode            = FALSE,
  est_method             = "reg",
  bstrap                 = FALSE
)

# Dynamic aggregation: event study from -5 to +12 relative periods
es_shale_birds <- aggte(cs_shale_birds, type = "dynamic", min_e = -5, max_e = 12, na.rm = TRUE)
es_shale_spec  <- aggte(cs_shale_spec,  type = "dynamic", min_e = -5, max_e = 12, na.rm = TRUE)
es_wind_birds  <- aggte(cs_wind_birds,  type = "dynamic", min_e = -5, max_e = 12, na.rm = TRUE)
es_wind_spec   <- aggte(cs_wind_spec,   type = "dynamic", min_e = -5, max_e = 12, na.rm = TRUE)

aggte(cs_shale_birds, type = "simple", na.rm = TRUE)
aggte(cs_shale_spec,  type = "simple", na.rm = TRUE)
aggte(cs_wind_birds,  type = "simple", na.rm = TRUE)                          
aggte(cs_wind_spec,   type = "simple", na.rm = TRUE)


# Extract results into a tidy data frame with 90% and 95% CIs
es_to_df <- function(es) {
  tibble(
    e       = es$egt,
    att     = es$att.egt,
    se      = es$se.egt,
    ci90_lo = att - qnorm(0.95)  * se,
    ci90_hi = att + qnorm(0.95)  * se,
    ci95_lo = att - qnorm(0.975) * se,
    ci95_hi = att + qnorm(0.975) * se
  )
}

df_es_shale_birds <- es_to_df(es_shale_birds)
df_es_shale_spec  <- es_to_df(es_shale_spec)
df_es_wind_birds  <- es_to_df(es_wind_birds)
df_es_wind_spec   <- es_to_df(es_wind_spec)

# Bar-style event study plot matching Appendix Figures A5/A6
plot_es <- function(df, title, color_fill) {
  ggplot(df, aes(x = e)) +
    geom_col(aes(y = att), fill = color_fill, alpha = 0.5, width = 0.6) +
    geom_linerange(aes(ymin = ci95_lo, ymax = ci95_hi),
                   linewidth = 0.5, color = color_fill, alpha = 0.5) +
    geom_linerange(aes(ymin = ci90_lo, ymax = ci90_hi),
                   linewidth = 1.2, color = color_fill) +
    geom_point(aes(y = att), shape = 20, size = 2,
               color = color_fill, fill = "white", stroke = 1) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(breaks = seq(-5, 12, 1)) +
    scale_y_continuous(
      breaks = scales::breaks_width(0.1),
      labels = scales::label_number(accuracy = 0.1)
    ) + 
    labs(x = "Years from First Treatment", y = "ATT Estimate", title = title) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank())
}

# Figure A5: Shale wells
p_a5_birds <- plot_es(df_es_shale_birds, "Number of Birds Reported",   "tomato3")
p_a5_spec  <- plot_es(df_es_shale_spec,  "Number of Species Reported", "tomato3")
fig_a5 <- p_a5_birds + p_a5_spec +
  plot_annotation(title = "Figure A5: Effects of Shale Wells on Bird and Species Counts (Dynamic)")
fig_a5

# Figure A6: Wind turbines
p_a6_birds <- plot_es(df_es_wind_birds, "Number of Birds Reported",   "deepskyblue3")
p_a6_spec  <- plot_es(df_es_wind_spec,  "Number of Species Reported", "deepskyblue3")
fig_a6 <- p_a6_birds + p_a6_spec +
  plot_annotation(title = "Figure A6: Effects of Wind Turbines on Bird and Species Counts (Dynamic)")
fig_a6


