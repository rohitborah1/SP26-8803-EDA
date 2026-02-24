## ECON 8803 PS2
## Rohit Borah
## February 16, 2026


# Load requisite packages. ------------------------------------------------

library(tidyverse)


# Import and manipulate data. ---------------------------------------------

df <- read_csv("pset2.csv")
## view(df)


# 1a) Fix missing values. -------------------------------------------------

missing_codes <- list(
  cardiac = 9,
  lung = 9,
  diabetes = 9,
  herpes = 9,
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
  ifelse(df[[col]] == missing_codes[[col]], NA, df[[col]])
})


# 1b) Create analysis dataset. --------------------------------------------

## Remove observations with missing values.
df_a <- df[complete.cases(df), ]

## Remove instructed columns.
df_a <- df_a[, !(names(df_a) %in% c("stresfip", "birmon", "weekday"))]
