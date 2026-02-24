## ECON 8803 PS1
## Rohit Borah & Ping Yang
## January 31, 2026

# Load requisite packages. ------------------------------------------------
library(tidyverse)
library(stargazer)
library(ggthemes)

options(scipen = 10)
options(digits = 3)

# Import and manipulate data. ---------------------------------------------
setwd("~/Documents/PhD/Spring 2026/8803 EDA")

df_jtrain2 <- as.data.frame(read_csv("PS1_Data/JTRAIN2.csv",
                                     show_col_types = FALSE))
df_jtrain2 <- df_jtrain2 %>%
  mutate(id = row_number()) %>%
  select(id, everything())

# Question 1 --------------------------------------------------------------

## (a) Run a regression of the outcome (earnings in 1978 in $1000s) on the
## training dummy, linearly controlling for age, marriage status, and earnings
## in 1974. Report the coefficients and standard errors with 3 significant
## digits. Provide an interpretation of the coefficient of the treatment dummy.

lm_1a <- lm(re78 ~ train + age + married + re74,
            data = df_jtrain2)

summary(lm_1a)

# stargazer(lm_1a, title = "Regression 1a", align = TRUE) # Latex table generator

## (b) Verify that you get the same result for \hat\beta if you first run Y_i
## and the treatment dummy D_i on other covariates, take residuals, and then run
## the residuals from Y_i regression on the residuals from the D_i regression.

### Run regressions
lm_1by <- lm(re78 ~ age + married + re74,
             data = df_jtrain2)
lm_1bd <- lm(train ~ age + married + re74,
             data = df_jtrain2)

### Store residuals
lm_1by_residuals <- residuals(lm_1by)
lm_1bd_residuals <- residuals(lm_1bd)


### Regress y_residuals on d_residuals
lm_residuals <- lm(lm_1by_residuals ~ lm_1bd_residuals)

summary(lm_residuals)

# stargazer(lm_residuals)

## (d.i) Estimate the CEF h(.) = E[re78|age] non-parametrically.

df_di <- df_jtrain2 %>%
  group_by(age) %>%
  summarize(count = n(),
            mean_re78 = mean(re78))

### Note to GD: I used AI to help write the nonparametric estimation code as
### I am unfamiliar with this.
cef <- loess(re78 ~ age,
             data = df_jtrain2)

df_jtrain2$np_cef <- fitted(cef)

## (d.ii) Approximate the CEF using a linear regression and a quadratic
## regression of earnings on age.

### Linear regression
lm_dii_linear <- lm(re78 ~ age,
                    data = df_jtrain2)

summary(lm_dii_linear)

df_jtrain2$lm_linear <- fitted(lm_dii_linear)

### Quadratic regression
lm_dii_quad <- lm(re78 ~ age + I(age ^ 2),
                  data = df_jtrain2)

summary(lm_dii_quad)

df_jtrain2$lm_quad <- fitted(lm_dii_quad)

## (d.iii) Plot the CEF, linear, and quadratic fits on the scatter plot between
## earnings and age. Clearly label which curve corresponds to each
## specification. What do you observe?


p_1d <- ggplot(data = df_jtrain2,
             aes(x = age,
                 y = re78)) +
  geom_jitter() +
  geom_line(data = df_di,
              aes(x = age,
                  y = mean_re78,
                  color = "CEF"),
              se = FALSE) +
  geom_smooth(
    aes(color = "Linear"),
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  geom_smooth(
    aes(color = "Quadratic"),
    method = "lm",
    formula = y ~ x + I(x ^ 2),
    se = FALSE
  ) +
  theme_fivethirtyeight() +
  labs(
    title = "8803 PS1 d.iii: Scatter Plot Between Earnings and Age",
    subtitle = "Overlaid with non-parametric CEF, linear regression, and quadratic regression",
    caption = "Rohit Borah & Ping Yang",
    x = "Age",
    y = "1978 Earnings (in $1000s)",
    color = "Specification"
  ) +
  theme(legend.position = "bottom",
        axis.title = element_text())

p_1d

## (d.iv) Using non-parametric CEF as the proxy of true CEF, calculate the
## MSE of linear and quadratic fits. Which specification provides a better
## approximation, and why do you think that is?

mse_linear <- mean((df_jtrain2$np_cef - df_jtrain2$lm_linear) ^ 2)

mse_quad <- mean((df_jtrain2$np_cef - df_jtrain2$lm_quad) ^ 2)

## (e) Now focus on the estimate of the training dummy \hat\beta.

weights_fwl <- lm_1bd_residuals / sum(lm_1bd_residuals^2)

p_1e_fwl <- hist(weights_fwl, 
     main = "Histogram of Implicit Weights (FWL)",
     xlab = "Weights",
     col = "#003057",
     breaks = 40)

p_1e_fwl

matrix_1e1 <- model.matrix(~ train + age + married + re74,
                               data = df_jtrain2)

matrix_1e2 <- solve(t(matrix_1e) %*% matrix_1e) %*% t(matrix_1e)

weights_matrix <-matrix_1e2[2, ]

hist(weights_matrix,
     main = "Histogram of Implicit Weights (Matrix)",
     xlab = "Weights",
     col = "#B3A369",
     breaks = 40)

# Question 2 --------------------------------------------------------------

df_n <- read_csv("Nicaragua_RCT.csv")

variables <-
  c(
    "complier",
    "income_r1",
    "income_r2",
    "rubro",
    "age",
    "education",
    "mobcap",
    "fixcap",
    "land"
  )

## (a) E[complier | assign = 1] - E[complier | assign = 0]. Use a difference-
## of-means test to check if the difference is statistically significant.

df_n %>%
  group_by(assign) %>%
  summarize(x_complier = mean(complier))

t_2a <- t.test(complier ~ assign, data = df_n)
t_2a

## (b) Sample means for all variables.

### Create df for E[var|assign = 1]
df_assign_1 <- df_n %>%
  filter(assign == 1) %>%
  summarize(
    complier = mean(complier, na.rm = TRUE),
    income_r1 = mean(income_r1, na.rm = TRUE),
    income_r2 = mean(income_r2, na.rm = TRUE),
    rubro = mean(rubro, na.rm = TRUE),
    age = mean(age, na.rm = TRUE),
    education = mean(education, na.rm = TRUE),
    mobcap = mean(mobcap, na.rm = TRUE),
    fixcap = mean(fixcap, na.rm = TRUE),
    land = mean(land, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "mean_assign_1")

### Create df for E[var|assign = 0]
df_assign_0 <- df_n %>%
  filter(assign == 0) %>%
  summarize(
    complier = mean(complier, na.rm = TRUE),
    income_r1 = mean(income_r1, na.rm = TRUE),
    income_r2 = mean(income_r2, na.rm = TRUE),
    rubro = mean(rubro, na.rm = TRUE),
    age = mean(age, na.rm = TRUE),
    education = mean(education, na.rm = TRUE),
    mobcap = mean(mobcap, na.rm = TRUE),
    fixcap = mean(fixcap, na.rm = TRUE),
    land = mean(land, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "mean_assign_0")

### Run t-tests for each variable.
### Note for GD- this was assisted by AI for speed.

p_values <- sapply(variables, function(v) {
  t.test(df_n[[v]][df_n$assign == 1], df_n[[v]][df_n$assign == 0])$p.value
})

### Create joined df
df_2b <- left_join(df_assign_1, df_assign_0) %>%
  mutate(
    delta = mean_assign_1 - mean_assign_0,
    p_value = p_values,
    sig = ifelse(p_value < 0.05, "Yes", "No")
  )

## (c) ATT effect of treat on income among treated households, using
## income_i = \alpha + \beta_ATT \times treat_i + e_i.
## If we estimate this model on the full set of households using OLS,
## will \hat\beta be an unbiased estimate of \beta_ATT?

### Comparing baseline income_r1 for compliers vs non-compliers
mean(df_n$income_r1[df_n$complier == 1], na.rm = TRUE)
mean(df_n$income_r1[df_n$complier == 0], na.rm = TRUE)

### Running correlation between baseline income and complier.
t_2c <- cor.test(df_n$complier, df_n$income_r1)
t_2c

## (d) Estimate two sets of specs for Equation 2, one without any
## covariates and one with the baseline variables.

### Without covariates
lm_2di <- lm(income_r2 ~ treat,
             data = df_n)

summary(lm_2di)

stargazer(lm_2di)

### With baseline variables
lm_2dii <-
  lm(income_r2 ~ treat + rubro + age + education + mobcap + fixcap + land,
     data = df_n)

summary(lm_2dii)

stargazer(lm_2dii)
