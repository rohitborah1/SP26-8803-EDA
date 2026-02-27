## ECON 8803 PS2
## Rohit Borah
## February 2026

# Load requisite packages. -----------------------------------------------

library(tidyverse)
library(readr)
library(gt)
library(ggthemes)
library(MatchIt)
library(glmnet)
library(hdm)
library(stargazer)

# Import and manipulate data. --------------------------------------------

df_raw <- read.csv(
  "/Users/bwom/Documents/PhD/Spring 2026/8803 EDA/Problem Sets/PS2/pset2.csv"
)

# 1a) Fix missing values. ------------------------------------------------

df_raw <- df_raw %>%
  mutate(
    across(
      c(
        cardiac,
        lung,
        diabetes,
        herpes,
        chyper,
        phyper,
        pre4000,
        preterm,
        tobacco,
        alcohol
      ),
      ~ na_if(., 9)
    ),
    herpes = na_if(herpes, 8),
    cigar6 = na_if(cigar6, 6),
    drink5 = na_if(drink5, 5),
    wgain = na_if(wgain, 99)
  )

binary_vars <- c(
  "anemia",
  "cardiac",
  "lung",
  "diabetes",
  "herpes",
  "chyper",
  "phyper",
  "pre4000",
  "preterm",
  "tobacco",
  "alcohol"
)

df_raw <- df_raw %>%
  mutate(across(all_of(binary_vars), ~ ifelse(. == 1, 1, 0)))

# str(df) # data validation test

# 1b) Create analysis dataset. --------------------------------------------------

df <- df_raw %>%
  select(-stresfip, -birmon, -weekday) %>%
  drop_na()

# 1c_cnt.) Continuous Variables Summary Table-----------------------------

## Summary Table 1: Continuous Variables
# Variables for table 1
t1c_cnt_vars <- c(
  "dmage",
  "dmeduc",
  "nlbnl",
  "dlivord",
  "totord9",
  "monpre",
  "nprevist",
  "isllb10",
  "dfage",
  "dfeduc",
  "dgestat",
  "dbrwt",
  "clingest",
  "cigar6",
  "drink5",
  "wgain"
)

# Create summary statistics with variables as rows
df_1c_cnt <- data.frame(
  Variable = c(
    "Mother's Age",
    "Mother's Education",
    "Number of Live Births (Now Living)",
    "Number of Live Births (Now Dead)",
    "Total Birth Order",
    "Month Prenatal Care Began",
    "Total Number of Prenatal Visits",
    "Interval Since Last Live Birth",
    "Father's Age",
    "Father's Education",
    "Gestation - Detail in Weeks",
    "Birthweight (g)",
    "Clinical Estimate of Gestation",
    "Avg Number of Cigarettes Per Day",
    "Avg Number of Drinks",
    "Weight Gain"
  ),
  Mean = sapply(t1c_cnt_vars, function(v) {
    round(mean(df[[v]], na.rm = TRUE), 2)
  }),
  Median = sapply(t1c_cnt_vars, function(v) {
    round(median(df[[v]], na.rm = TRUE), 2)
  }),
  SD = sapply(t1c_cnt_vars, function(v) round(sd(df[[v]], na.rm = TRUE), 2)),
  Min = sapply(t1c_cnt_vars, function(v) min(df[[v]], na.rm = TRUE)),
  Max = sapply(t1c_cnt_vars, function(v) max(df[[v]], na.rm = TRUE))
)

# Format with gt
t_1c_cnt <-
  df_1c_cnt %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics: Continuous Variables"
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

# t_1c_cnt

# 1c_cat Categorical Variables Summary Table------------------------------

t1c_cat_vars <- c(
  "csex1",
  "dmar1",
  "ormoth1",
  "orfath1",
  "dplural1",
  "anemia",
  "cardiac",
  "lung",
  "diabetes",
  "herpes",
  "chyper",
  "phyper",
  "pre4000",
  "preterm",
  "tobacco",
  "alcohol"
)

df <- df %>%
  mutate(dplural1 = ifelse(dplural > 1, 1, 0))
df <- df %>%
  mutate(csex1 = ifelse(csex > 1, 1, 0))
df <- df %>%
  mutate(dmar1 = ifelse(dmar > 1, 1, 0))
df <- df %>%
  mutate(ormoth1 = ifelse(ormoth %in% c(1, 2, 3, 4, 5), 1, 0))
df <- df %>%
  mutate(orfath1 = ifelse(orfath %in% c(1, 2, 3, 4, 5), 1, 0))

df_1c_cat <- data.frame(
  Variable = c(
    "Female Children",
    "Married Mothers",
    "Hispanic Mothers",
    "Hispanic Fathers",
    "Multiple Births",
    "Anemic Mothers",
    "Mothers with Cardiac Disease",
    "Mothers with Lung Disease",
    "Diabetic Mothers",
    "Mothers with Genital Herpes",
    "Mothers with Chronic Hypertension",
    "Mothers with Pregnancy-Related Hypertension",
    "Previous Infants Weighing > 4000g",
    "Previous Preterm Infants",
    "Tobacco Use During Pregnancy",
    "Alcohol Use During Pregnancy"
  ),
  N = sapply(t1c_cat_vars, function(v) sum(!is.na(df[[v]]))),
  Proportion = sapply(t1c_cat_vars, function(v) {
    round(mean(df[[v]], na.rm = TRUE), 3)
  })
)

# df_1c_cat

# Format with gt
t_1c_cat <-
  df_1c_cat %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics: Categorical Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    N = "N",
    Proportion = "Proportion"
  ) %>%
  tab_options(
    table.font.size = 11,
    heading.title.font.size = 13,
    column_labels.font.weight = "bold",
    table.width = pct(100)
  )

t_1c_cat

# 1d) Histogram of birthweight. ------------------------------------------

p_1d <- ggplot(df, aes(x = dbrwt)) +
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

df_2a <- df %>%
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

t_2a

# 2b Balance table for causality. ----------------------------------------

df_2b <- df %>%
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

# Control variables as reference
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

# Variables to exclude as reference
excluded <- c(
  "dgestat",
  "phyper",
  "preterm",
  "pre4000"
)

# Linear model
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
  data = df
)

summary(m_3a)

# stargazer(m_3a)

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
  data = df
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
  data = df
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
  data = df
)

summary(m_3d)


# 3e. Oaxaca-Blinder -----------------------------------------------------

m_3e <- lm(
  dbrwt ~ tobacco *
    (dmage +
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
      drink5),
  data = df
)

df_3e_smoking <- df %>%
  filter(tobacco == 1)

df_3e_nonsmoking <- df %>%
  filter(tobacco == 0)

pred_smoking <- predict(m_3e, newdata = df_3e_smoking)
pred_nonsmoking <- predict(m_3e, newdata = df_3e_nonsmoking)

ate <- mean(pred_smoking) - mean(pred_nonsmoking)

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
  data = df
)

df$p_score <- predict(m_4a_ps)

hist(df$p_score) # to see distribution of p-scores

m_4a_ols <- lm(dbrwt ~ tobacco + p_score, data = df)

summary(m_4a_ols)


# 4b. Logit P-Score ------------------------------------------------------

# Use glm to run a logit specification for p-score
m_4b_logit <- glm(
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
  data = df,
  family = binomial(link = "logit")
)

summary(m_4b_logit)

# Add logit propensity score to working dataset
df$pscore_logit <- predict(m_4b_logit, newdata = df, type = "response")

# 4c. P-Score Overlap Plot -----------------------------------------------

# Visualize p-score overlaps using density plots.
p_4c <-
  ggplot(df, aes(x = pscore_logit, fill = factor(tobacco))) +
  geom_density(alpha = 0.5) +
  labs(title = "4c. Propensity Score Overlap Plot", fill = "Smoking Status") +
  theme_fivethirtyeight() +
  theme(legend.position = "top", legend.title = element_text(face = "bold"))

# p_4c

# 4d. ATE of Maternal Smoking on Birthweight Using OLS Controlling for P-Score ----

m_4d <- lm(dbrwt ~ tobacco + pscore_logit, data = df)

summary(m_4d)


# 4e. Estimate ATE binned by propensity score. ---------------------------

df_4e <- df %>%
  mutate(bin_pscore = ntile(pscore_logit, 10))

table(df_4e$bin_pscore, df_4e$tobacco)

df_4e_block <- df_4e %>%
  group_by(bin_pscore) %>%
  summarize(
    n = n(),
    mean_diff = mean(dbrwt[tobacco == 1]) - mean(dbrwt[tobacco == 0])
  )

ate_block <- sum(df_4e_block$mean_diff * df_4e_block$n / sum(df_4e_block$n))

# 4f. Hajek P-Score Weighting --------------------------------------------

# Calculate Hajek IPW estimator manually
ate_treated <- sum(df$tobacco * df$dbrwt / df$pscore_logit) /
  sum(df$tobacco / df$pscore_logit)

ate_control <- sum((1 - df$tobacco) * df$dbrwt / (1 - df$pscore_logit)) /
  sum((1 - df$tobacco) / (1 - df$pscore_logit))

ate_ipw <- ate_treated - ate_control

ate_ipw # -223.348g


# 5a. P-Score Matching + Reg Adj -----------------------------------------

# Perform propensity score matching
matched <- matchit(
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
  data = df,
  method = "nearest",
  distance = df$pscore_logit
)

# summary(matched)

# Establish matched dataset
matched_data <- match.data(matched)

# Regress on matched data
matched_reg <- lm(
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
  data = matched_data,
  weights = weights
)

summary(matched_reg)


# 5b. Double-Selection LASSO -----------------------------------------------------------------

## Create flexible spec using polynomials and interaction terms.

# Create double-selection lasso.
m_5b_ds_lasso <- rlassoEffect(
  x = model.matrix(
    ~ dmage +
      I(dmage^2) +
      dmeduc +
      I(dmeduc^2) +
      mrace3 +
      ormoth +
      dmar +
      I(dmar^2) +
      dfage +
      I(dfage^2) +
      dfeduc +
      I(dfeduc^2) +
      nprevist +
      I(nprevist^2) +
      orfath +
      nlbnl +
      dlivord +
      totord9 +
      isllb10 +
      monpre +
      adequacy +
      alcohol +
      drink5 +
      csex +
      dmage * dmeduc +
      dmage * dmar +
      dmage * mrace3,
    data = df
  ),
  y = df$dbrwt,
  d = df$tobacco
)

summary(m_5b_ds_lasso)
