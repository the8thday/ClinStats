library(easystats)
library(rstatix)

df <- read.csv("~/Downloads/crop.data.csv",
  header = TRUE,
  colClasses = c("factor", "factor", "factor", "numeric")
)

# anova -------------------------------------------------------------------
# one-way anova
table(df$fertilizer, df$block)
one.way <- aov(yield ~ fertilizer,
  data = df
)

summary(one.way)
report::report(one.way) # perfect!
effectsize::effectsize(one.way) # effect size

# two-way anova
two.way <- aov(yield ~ fertilizer + block,
  data = df
)
report(two.way)

two.way2 <- aov(yield ~ fertilizer + block + density,
  data = df
)
check_model(two.way) # 模型检查

tukey.two.way <- TukeyHSD(two.way2)
plot(tukey.two.way)
summary(tukey.two.way)
rstatix::get_summary_stats(df)


# kruskal wallis ----------------------------------------------------------

kt <- kruskal.test(yield ~ fertilizer,
  data = df
)

# effect size

# post-hoc test
pairwise.wilcox.test(
  x = df$yield,
  g = df$fertilizer,
  p.adjust.method = "BH" # 在用wilcox进行多重比较时需要矫正P值
)

### masybe rstatix is a good choice
library(rstatix)
kt2 <- kruskal_test(yield ~ fertilizer,
  data = df
)
# effect size
effec_df <- kruskal_effsize(
  yield ~ fertilizer,
  data = df
)
# Dunn's test/Wilcoxon’s test
dunn_test(
  yield ~ fertilizer,
  data = df
)

wilcox_test(
  yield ~ fertilizer,
  data = df
)






# proportion --------------------------------------------------------------
prop_test(mymatrix)


# RR OR Association ----------------------------------------------------------

mymatrix <- matrix(c(156, 9421, 1531, 14797),
  nrow = 2, byrow = TRUE
)
colnames(mymatrix) <- c("Disease", "Control")
rownames(mymatrix) <- c("Exposed", "Unexposed")

mymatrix2 <- matrix(c(30, 24, 76, 241, 82, 509), nrow = 3, byrow = TRUE)
colnames(mymatrix2) <- c("Disease", "Control")
rownames(mymatrix2) <- c("Exposure1", "Exposure2", "Unexposed")

# RR:The relative risk can be estimated for a cohort study but not for a case-control study
source('./R/RROR.R')
calcRelativeRisk(mymatrix)

epitools::epitab(mymatrix,
                 method = 'riskratio',
                 rev = 'both'
                 )


# OR:The odds ratio can be estimated for either a cohort study or a case-control study
ft <- fisher.test(mymatrix)
ft$p.value

calcOddsRatio(mymatrix = mymatrix)


# Mantel-Haenszel
m_male <- matrix(
  data = c(4, 5, 5, 103),
  nrow = 2,
  ncol = 2, byrow = T,
  dimnames = list(
    c("Exposure", "Unexposed"),
    c("Disease", "Control")
  )
)
m_female <- matrix(
  data = c(10, 3, 5, 43),
  nrow = 2,
  ncol = 2, byrow = T,
  dimnames = list(
    c("Exposure", "Unexposed"),
    c("Disease", "Control")
  )
)

myarray <- array(c(m_male, m_female), dim = c(2, 2, 2))

stats::mantelhaen.test(myarray)
# lawstat::cmh.test(myarray) #还可进行Tarones test 验证是否为混杂因素


# Dose-response analysis --------------------------------------------------
# dose-response need case-control?
# 随着dose变化，OR变化的情况; 或者利用线性回归

m_dose <- matrix(
  data = c(35, 82, 250, 293, 196, 190, 136, 71, 32, 13),
  nrow = 5,
  byrow = T,
  dimnames = list(
    c("2", "9.5", "19.5", "37", "50"),
    c("Disease", "Control")
  )
)

source("./R/doseOddsDiseaseRegression.R")
doseOddsDiseaseRegression(m_dose)




# Calculating the Sample Size Required for a Randomised Control Trial --------
source(file = "./R/calcSampleSizeForRCT.R")
calcSampleSizeForRCT(alpha = 0.05, gamma = 0.90, piT = 0.15, piC = 0.2)

# This tells us that the sample size required in each group is 1214 people,
# so overall we need 1214*2=2428 people in the randomised control trial.


# Calculating the Power of a Randomised Control Trial ---------------------

calcPowerForRCT(alpha = 0.05, piT = 0.2, piC = 0.3, n = 250)





# some usefull R package --------------------------------------------------

pacman::p_load(
  pwr
  # rpact,
  # TrialSize
)

# sample size for RCT，即two-sample t-test, p1/p2为proportions
# 由前期实验数据得到
p.out <- pwr.t.test(
  d = ES.h(p1 = 0.2, p2 = 0.15),
  sig.level = 0.05,
  type = "two.sample",
  power = 0.9
)
# RCT 患病率更适合
pwr.2p.test(
  h = ES.h(p1 = 0.2, p2 = 0.15),
  sig.level = 0.05,
  alternative = "two.sided",
  power = 0.9
)

stats::power.prop.test(
  p1 = 0.2,
  p2 = 0.15,
  sig.level = 0.05,
  power = 0.9
)
#
# TrialSize::TwoSampleProportion.Equality(
#   alpha = 0.05,
#   beta = 0.1,
#   p1 = 0.2,
#   p2 = 0.15,
#   k = 1
# )

# modified poissson for RR ------------------------------------------------


