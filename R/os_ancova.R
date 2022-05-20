
#' Title
#'
#' @param data data frame contains needed variable
#' @param outcome outcome
#' @param group group
#' @param covariate covariate
#' @param type Anova type,default='III'
#' @param plot whether show plots,TRUE of FALSE
#' @param check_model whether check the model
#'
#' @return a list contains all you need
#' @export
#'
#' @examples
#' load(ancova_data)
#' os_ancova(data = ancova_data, outcome = 'diffW24', group = 'group', covariate = 'W1D1')
os_ancova <- function(data,
                      outcome, group, covariate,
                      type = 'III',
                      plot = TRUE,
                      check_model = TRUE
                      ){
  if (!inherits(data, 'data.frame')){
    stop('data should be a data frame')
  }
  if (!is.numeric(data[[covariate]])){
    stop('covariate should be numberic')
  }

  res <- list()

  formula <- as.formula(paste0(outcome, ' ~ ', covariate, ' + ', group))
  # print(formula)
  aov_res <- aov(formula = formula, data = data)

  anova_res <- car::Anova(aov_res, type = type)

  emm_res <- emmeans::emmeans(aov_res, specs = group)

  emm_contrast <- graphics::pairs(emm_res, adjust = 'bonferroni',
                                  infer = c(TRUE, TRUE))

  p_emm <- plot(emm_res, comparisons = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Estimated Marginal Mean")

  res[['formula']] = formula
  res[['anova_res']] = data.frame(anova_res)
  res[['emmeans']] = emm_res
  res[['contrasts']] = emm_contrast
  if(plot) res[['emm_plot']] = p_emm
  if(check_model){
    p <- performance::check_model(aov_res)
    res[['check_model']][['plot']] = p
    res[['check_model']][['outlier']] = performance::check_outliers(aov_res)
  }

  return(res)
}


