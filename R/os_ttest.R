
os_ttest <- function (x, formula, response = NULL, explanatory = NULL, order = NULL,
                      alternative = "two-sided", mu = 0, conf_int = TRUE, conf_level = 0.95,
                      ...)
{
  check_conf_level(conf_level)
  x <- tibble::as_tibble(x) %>% mutate_if(is.character, as.factor) %>%
    mutate_if(is.logical, as.factor)
  response <- enquo(response)
  explanatory <- enquo(explanatory)
  x <- parse_variables(x = x, formula = formula, response = response,
                       explanatory = explanatory)
  if (alternative %in% c("two-sided", "two_sided", "two sided",
                         "two.sided")) {
    alternative <- "two.sided"
  }
  if (has_explanatory(x)) {
    order <- check_order(x, order, in_calculate = FALSE,
                         stat = NULL)
    x <- reorder_explanatory(x, order)
    prelim <- stats::t.test(formula = as.formula(paste0(response_name(x),
                                                        " ~ ", explanatory_name(x))), data = x,
                            alternative = alternative,
                            mu = mu, conf.level = conf_level, ...) %>% broom::glance()
  }
  else {
    prelim <- stats::t.test(response_variable(x), alternative = alternative,
                            mu = mu, conf.level = conf_level) %>% broom::glance()
  }
  if (conf_int) {
    results <- prelim %>% dplyr::select(statistic, t_df = parameter,
                                        p_value = p.value, alternative, estimate, lower_ci = conf.low,
                                        upper_ci = conf.high)
  }
  else {
    results <- prelim %>% dplyr::select(statistic, t_df = parameter,
                                        p_value = p.value, alternative, estimate)
  }
  results
}




