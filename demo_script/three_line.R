# 统计模型 三线表

# library(pubh)
library(table1)
# library(tableone)
library(modelsummary)
library(gtsummary)


data(pbc, package = "survival")


# use table1 package ------------------------------------------------------

pbc2 <- pbc
pbc2$sex <- factor(pbc2$sex,
  levels = c("m", "f"),
  labels = c("Male", "Female")
)
pbc2$status <- factor(
  pbc2$status,
  levels = c(0, 1, 2),
  labels = c("Censored", "Transplant", "Dead")
)

label(pbc2$sex) <- "Sex"
label(pbc2$age) <- "Age"

units(pbc2$age) <- "years"

table1::table1(
  ~ age + sex + protime | status,
  data = pbc2,
  overall = "Total"
)

# another theme
my.render.cont <- function(x) {
  with(
    stats.apply.rounding(stats.default(x), digits = 2),
    c("", "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD))
  )
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
}
labels <- list(
  variables = list(
    sex = "Sex",
    age = "Age (years)",
    protime = "Protime"
  ),
  groups = list("", "", "Progress")
)

strata <- c(list(Total = pbc2), split(pbc2, pbc2$status))

table1::table1(
  strata,
  labels,
  groupspan = c(1, 1, 2),
  render.continuous = my.render.cont,
  render.categorical = my.render.cat
)

# use modelsummary package ------------------------------------------------
library(survival)

pbc3 <- as_tibble(pbc) %>%
  mutate(
    status = as_factor(status),
    trt = as_factor(trt)
  )

mm <- survival::coxph(
  Surv(time, status) ~ age + sex + protime,
  data = pbc
)

modelsummary(mm)

# crosstab, table1
datasummary_skim(pbc)
datasummary_skim(pbc, type = "categorical") # 对于因子?
datasummary_df(pbc, output = "default") # 可以输出多种格式

datasummary_balance(age + sex + chol ~ status, # 似乎不能筛选变量
  data = pbc3,
  title = "Summary Study",
  notes = "data from survival"
)
datasummary_crosstab(age + sex + chol ~ status, # 太简单的功能
  data = pbc3
)

# datasummary for more complex cross tabulation
# 其的用法为一侧变量，一侧计算函数（任何输入vector输出single value的都可以）
datasummary(
  formula = age + chol ~ Mean + SD,
  data = pbc3,
)

datasummary(
  formula = age + chol ~ status * (Mean + SD),
  data = pbc3,
)
datasummary(
  formula = age + chol ~ (Mean + SD) * status,
  data = pbc3,
)
# By default, datasummary omits column headers with a single value/label across all columns
datasummary(
  formula = age + chol ~ Mean * status,
  data = pbc3,
)
# finally
datasummary(
  formula = (Age <- age) + (Chol <- chol) ~ Factor(status) * (Mean + SD),
  data = pbc3,
  fmt = 3,
  title = "Summary Study",
  notes = "data from survival",
  add_columns = data.frame("Total" = c(1, 2))
  # output = 'table.jpg'
)




# use gtsummary -----------------------------------------------------------

# by crosstable -----------------------------------------------------------

library(crosstable)
library(flextable)
