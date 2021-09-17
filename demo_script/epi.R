# https://epirhandbook.com/suggested-packages-1.html

# EpiModel: An R Package for Mathematical Modeling of Infectious Disease over Networks
# PHEindicators: Common Public Health Statistics and their Confidence Intervals
# SimInf: An R Package for Data-Driven Stochastic Disease Simulations
# SPARSEMODr: Construct spatial, stochastic disease models that show how parameter values fluctuate in response to public health interventions

pacman::p_load(
  rio,
  gtsummary
)

linelist <- rio::import("~/Downloads/linelist_cleaned.rds") %>%
  as_tibble()
all_names <- dput(names(linelist)) # cant assign a dput out to a vairble

skimr::skim(linelist)

# how to use gtsummary
linelist %>%
  as_tibble() %>%
  select(gender, outcome) %>%
  tbl_summary(by = outcome) %>%
  add_p()

linelist %>%
  as_tibble() %>%
  select(age_years, outcome) %>%
  tbl_summary(by = outcome) %>%
  add_p()
