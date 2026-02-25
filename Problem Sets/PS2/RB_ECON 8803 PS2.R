## ECON 8803 PS2
## Rohit Borah
## February 2026

# Load requisite packages. -----------------------------------------------

library(tidyverse)
library(readr)
library(gt)
library(ggthemes)
library(stargazer)

# Import and manipulate data. --------------------------------------------

df <- read.csv(
  "/Users/bwom/Documents/PhD/Spring 2026/8803 EDA/Problem Sets/PS2/pset2.csv"
)
# View(df)

# 1a) Fix missing values. ------------------------------------------------

missing_codes <- list(
  cardiac = 9,
  lung = 9,
  diabetes = 9,
  herpes = c(8, 9),
  chyper = 9,
  phyper = 9,
  pre4000 = 9,
  preterm = 9,
  tobacco = 9,
  cigar6 = 6,
  alcohol = 9,
  drink5 = 5,
  wgain = 99
)

df[names(missing_codes)] <- lapply(names(missing_codes), function(col) {
  ifelse(df[[col]] %in% missing_codes[[col]], NA, df[[col]])
})

# table(df$cardiac) # data validation test

# 1b) Create analysis dataset. --------------------------------------------------

## Remove observations with missing values.
df_a <- df[complete.cases(df), ]

## Remove instructed columns.
df_a <- df_a[, !(names(df_a) %in% c("stresfip", "birmon", "weekday"))]


# 1c) Produce a summary statistics table with mean, standard deviation, minimum, and maximum. ----

## Summary Table 1: Demographic Variables
# Variables for table 1
t1c_vars <- c(
  "dmage",
  "dmeduc",
  "dfage",
  "dfeduc",
  "csex",
  "dgestat",
  "wgain",
  "monpre",
  "nprevist"
)

# Create summary statistics with variables as rows
df_1c <- data.frame(
  Variable = c(
    "Mother's age",
    "Mother's education",
    "Father's age",
    "Father's education",
    "Child sex",
    "Gestation (Weeks)",
    "Weight Gain",
    "First Month of Prenatal Care",
    "Prenatal Visits"
  ),
  Mean = sapply(t1c_vars, function(v) round(mean(df_a[[v]], na.rm = TRUE), 2)),
  Median = sapply(t1c_vars, function(v) {
    round(median(df_a[[v]], na.rm = TRUE), 2)
  }),
  SD = sapply(t1c_vars, function(v) round(sd(df_a[[v]], na.rm = TRUE), 2)),
  Min = sapply(t1c_vars, function(v) min(df_a[[v]], na.rm = TRUE)),
  Max = sapply(t1c_vars, function(v) max(df_a[[v]], na.rm = TRUE))
)

# Format with gt
t_1c <-
  df_1c %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics"
  ) %>%
  cols_label(
    Variable = "",
    Mean = "Mean",
    Median = "Median",
    SD = "SD",
    Min = "Min",
    Max = "Max"
  ) %>%
  fmt_number(
    columns = c(Mean, Median, SD),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = 11,
    heading.title.font.size = 13,
    column_labels.font.weight = "bold",
    table.width = pct(100)
  )

# t_1c

## Summary Table 2: Maternal Child Health Outcome Variables

## Summary Table 3: Behavioral Variables

# 1d) Histogram of birthweight. ------------------------------------------

p_1d <- ggplot(df_a, aes(x = dbrwt)) +
  geom_histogram(bins = 30, fill = "#003057", color = "white") +
  geom_density() +
  labs(
    title = "1d. Histogram of Birth Weight",
    x = "Birth Weight (grams)"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    axis.title.y = element_blank()
  )

# p_1d

# 2a) Difference in birthweight by tobacco users vs non-users. -----------

df_2a <- df_a %>%
  group_by(tobacco) %>%
  summarize(n = n(), mean_brwt = mean(dbrwt), sd_brwt = sd(dbrwt)) %>%
  mutate(category = ifelse(tobacco == 1, "Smoker", "Non-Smoker"), .before = n)

t_2a <- df_2a %>%
  gt() %>%
  tab_header(
    title = "Birthweight by Tobacco Use"
  ) %>%
  cols_label(
    tobacco = "Group",
    category = "Category",
    n = "n",
    mean_brwt = "Mean",
    sd_brwt = "SD"
  ) %>%
  fmt_number(
    columns = c(mean_brwt, sd_brwt),
    decimals = 2
  ) %>%
  tab_options(data_row.padding.horizontal = px(10)) %>%
  cols_align(
    align = "center",
    columns = everything()
  )

# t_2a

# 2b Balance table for causality. ----------------------------------------

df_2b <- df_a %>%
  group_by(tobacco) %>%
  summarise(
    n = n(),
    mean_brwt = mean(dbrwt),
    mean_age = mean(dmage, na.rm = TRUE),
    mean_education = mean(dmeduc, na.rm = TRUE),
    pct_married = mean(dmar == 1, na.rm = TRUE) * 100,
    pct_black = mean(mrace3 == 2, na.rm = TRUE) * 100, # adjust category as needed
    mean_prenatal_visits = mean(nprevist, na.rm = TRUE),
    pct_hypertension = mean(phyper == 1, na.rm = TRUE) * 100,
    pct_diabetes = mean(diabetes == 1, na.rm = TRUE) * 100
  ) %>%
  mutate(
    tobacco_status = ifelse(tobacco == 1, "Smoker", "Non-smoker"),
    .before = n
  )

df_2b_melt <- df_2b %>%
  select(-tobacco) %>%
  pivot_longer(
    cols = -tobacco_status,
    names_to = "Characteristic",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = tobacco_status, values_from = Value)

t_2b <- df_2b_melt %>%
  gt() %>%
  tab_header(
    title = "Balance Table by Smoking Status"
  ) %>%
  cols_label(
    Characteristic = "Characteristic",
    Smoker = "Smokers",
    `Non-smoker` = "Non-Smokers"
  ) %>%
  fmt_number(
    columns = c(Smoker, `Non-smoker`),
    decimals = 2
  ) %>%
  cols_align(
    align = "center",
    columns = c(Smoker, `Non-smoker`)
  )
t_2b

# 3a Basic Linear Regression. --------------------------------------------

# Define outcomes
outcomes <- tribble(
  ~outcome_var , ~outcome_label                          , ~scale ,
  "dbrwt"      , "Birth weight (grams)"                  ,      1 ,
  "lbw"        , "Birth weight < 2500g (per 1000)"       ,   1000 ,
  "vlbw"       , "Birth weight < 1500g (per 1000)"       ,   1000 ,
  "elbw"       , "Birth weight < 1000g (per 1000)"       ,   1000 ,
  "dgestat"    , "Gestation length (weeks)"              ,      1 ,
  "preterm"    , "Premature birth < 32 weeks (per 1000)" ,   1000 ,
  "fmaps"      , "5-minute APGAR score"                  ,      1
)

# Create binary outcome variables
df_a <- df_a %>%
  mutate(
    lbw = as.numeric(dbrwt < 2500),
    vlbw = as.numeric(dbrwt < 1500),
    elbw = as.numeric(dbrwt < 1000),
    preterm = as.numeric(dgestat < 32)
  )

# Control variables
controls <- c(
  "dmage",
  "dmeduc",
  "mrace3",
  "ormoth",
  "dmar",
  "dfage",
  "dfeduc",
  "orfath",
  "nlbnl",
  "dlivord",
  "totord9",
  "isllb10",
  "monpre",
  "nprevist",
  "adequacy",
  "alcohol",
  "drink5",
  "csex"
)

# Variables to exclude
excluded <- c(
  "dgestat",
  "phyper",
  "preterm",
  "pre4000",
  "anemia",
  "cardiac",
  "lung",
  "diabetes",
  "herpes",
  "chyper"
)


# 3a. Basic linear regression. -------------------------------------------

m_3a <- lm(
  dbrwt ~ tobacco +
    dmage +
    dmeduc +
    mrace3 +
    ormoth +
    dmar +
    dfage +
    dfeduc +
    orfath +
    nlbnl +
    dlivord +
    totord9 +
    isllb10 +
    monpre +
    nprevist +
    adequacy +
    alcohol +
    drink5 +
    csex,
  data = df_a
)

summary(m_3a)

stargazer(m_3a)

# 3b. Adding in bad controls. --------------------------------------------

m_3b <- lm(
  dbrwt ~ tobacco +
    dmage +
    dmeduc +
    mrace3 +
    ormoth +
    dmar +
    dfage +
    dfeduc +
    orfath +
    nlbnl +
    dlivord +
    totord9 +
    isllb10 +
    monpre +
    nprevist +
    adequacy +
    alcohol +
    drink5 +
    csex +
    dgestat +
    phyper +
    preterm +
    pre4000,
  data = df_a
)

summary(m_3b)


# 3c. Add in cigar6 to m_3a. ---------------------------------------------
m_3c <- lm(
  dbrwt ~ tobacco +
    dmage +
    dmeduc +
    mrace3 +
    ormoth +
    dmar +
    dfage +
    dfeduc +
    orfath +
    nlbnl +
    dlivord +
    totord9 +
    isllb10 +
    monpre +
    nprevist +
    adequacy +
    alcohol +
    drink5 +
    csex +
    cigar6,
  data = df_a
)

summary(m_3c)


# 3d. Interact smoking status with diabetes. -----------------------------

m_3d <- lm(
  dbrwt ~ tobacco *
    diabetes +
    dmage +
    dmeduc +
    mrace3 +
    ormoth +
    dmar +
    dfage +
    dfeduc +
    orfath +
    nlbnl +
    dlivord +
    totord9 +
    isllb10 +
    monpre +
    nprevist +
    adequacy +
    alcohol +
    drink5 +
    csex,
  data = df_a
)

summary(m_3d)

# 4a. LPM for PS ---------------------------------------------------------

m_4a_ps <- lm(
  tobacco ~ dmage +
    dmeduc +
    mrace3 +
    ormoth +
    dmar +
    dfage +
    dfeduc +
    orfath +
    nlbnl +
    dlivord +
    totord9 +
    isllb10 +
    monpre +
    nprevist +
    adequacy +
    alcohol +
    drink5 +
    csex,
  data = df_a
)

df_a$p_score <- predict(m_4a_ps)

# hist(df_a$p_score) # to see distribution of p-scores

m_4a_ols <- lm(dbrwt ~ tobacco + p_score, data = df_a)

summary(m_4a_ols)
