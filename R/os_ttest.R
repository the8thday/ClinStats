# 借鉴infer包里的t_test函数

#' one step ttest modified from infer package
#'
#' @description
#'
#' A tidier version of [t.test()][stats::t.test()] for two sample tests.
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param formula A formula with the response variable on the left and the
#'   explanatory on the right.
#' @param response The variable name in `x` that will serve as the response.
#'   This is alternative to using the `formula` argument.
#' @param explanatory The variable name in `x` that will serve as the
#'   explanatory variable.
#' @param order A string vector of specifying the order in which the levels of
#'   the explanatory variable should be ordered for subtraction, where `order =
#'   c("first", "second")` means `("first" - "second")`.
#' @param alternative Character string giving the direction of the alternative
#'   hypothesis. Options are `"two-sided"` (default), `"greater"`, or `"less"`.
#' @param mu A numeric value giving the hypothesized null mean value for a one
#'   sample test and the hypothesized difference for a two sample test.
#' @param conf_int A logical value for whether to include the confidence
#'   interval or not. `TRUE` by default.
#' @param conf_level A numeric value between 0 and 1. Default value is 0.95.
#' @param ... For passing in other arguments to [t.test()][stats::t.test()].
#'
#' @examples
#' library(tidyr)
#' data(gss, package = "infer")
#'
#' # t test for number of hours worked per week
#' # by college degree status
#' gss %>%
#'    tidyr::drop_na(college) %>%
#'    os_ttest(formula = hours ~ college,
#'       order = c("degree", "no degree"),
#'       alternative = "two-sided")
#'
#' # see vignette("infer") for more explanation of the
#' # intuition behind the infer package, and vignette("os_ttest")
#' # for more examples of t-tests using infer
#'
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom stats as.formula
#' @importFrom rlang enquo
#' @importFrom rlang get_expr
#' @family wrapper functions
#' @export
os_ttest <- function(x, formula, response = NULL, explanatory = NULL, order = NULL,
                     alternative = "two-sided", mu = 0, conf_int = TRUE, conf_level = 0.95,
                     ...)
{
  check_conf_level(conf_level)
  x <- tibble::as_tibble(x) %>%
    # character and ordered to factor
    dplyr::mutate(
      dplyr::across(
        where(~ is.character(.x) || is.ordered(.x)),
        ~ factor(.x, ordered = FALSE)
      )
    ) %>%
    # logical to factor, with TRUE as the first level
    dplyr::mutate(
      dplyr::across(
        where(~ is.logical(.x)),
        ~ factor(.x, levels = c("TRUE", "FALSE")),
      )
    )  %>%
    # integer to numeric
    dplyr::mutate(
      dplyr::across(
        where(is.integer),
        as.numeric
      )
    )

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



check_conf_level <- function(conf_level) {
  if (
    (class(conf_level) != "numeric") | (conf_level < 0) | (conf_level > 1)
  ) {
    stop_glue("The `conf_level` argument must be a number between 0 and 1.")
  }
}


has_attr <- function(x, at) {
  !is.null(attr(x, at, exact = TRUE))
}

has_explanatory <- function(x) {
  has_attr(x, "explanatory")
}

has_response <- function(x) {
  has_attr(x, "response")
}

explanatory_expr <- function(x) {
  attr(x, "explanatory")
}

explanatory_name <- function(x) {
  all.vars(explanatory_expr(x))
}

response_name <- function(x) {
  as.character(response_expr(x))
}

response_expr <- function(x) {
  attr(x, "response")
}

response_variable <- function(x) {
  x[[response_name(x)]]
}

explanatory_variable <- function(x) {
  if (!is.null(explanatory_expr(x))) {
    if (length(explanatory_name(x)) > 1) {
      x[explanatory_name(x)]
    } else {
      x[[explanatory_name(x)]]
    }
  } else {
    NULL
  }
}

determine_variable_type <- function(x, variable) {
  var <- switch(
    variable,
    response = response_variable(x),
    explanatory = explanatory_variable(x)
  )

  if (is.null(var)) {
    ""
  } else if (inherits(var, "numeric")) {
    "num"
  } else if (length(unique(var)) == 2) {
    "bin"
  } else {
    "mult"
  }
}


check_order <- function(x, order, in_calculate = TRUE, stat) {
  # If there doesn't need to be an order argument, warn if there is one,
  # and otherwise, skip checks
  if (!(infer:::theory_type(x) %in% c("Two sample props z", "Two sample t") ||
        is.null(stat) ||
        stat %in% c("diff in means", "diff in medians",
                    "diff in props", "ratio of props", "odds ratio"))) {
    if (!is.null(order)) {
      infer:::warning_glue(
        "Statistic is not based on a difference or ratio; the `order` argument ",
        "will be ignored. Check `?calculate` for details."
      )
    } else {
      return(order)
    }
  }

  explanatory_variable <- explanatory_variable(x)
  unique_ex <- sort(unique(explanatory_variable))

  if (is.null(order) & in_calculate) {
    # Default to subtracting/dividing the first (alphabetically) level by the
    # second, unless the explanatory variable is a factor (in which case order
    # is preserved); raise a warning if this was done implicitly.
    order <- as.character(unique_ex)
    infer:::warning_glue(
      "The statistic is based on a difference or ratio; by default, for ",
      "difference-based statistics, the explanatory variable is subtracted ",
      "in the order \"{unique_ex[1]}\" - \"{unique_ex[2]}\", or divided in ",
      "the order \"{unique_ex[1]}\" / \"{unique_ex[2]}\" for ratio-based ",
      "statistics. To specify this order yourself, supply `order = ",
      "c(\"{unique_ex[1]}\", \"{unique_ex[2]}\")` to the calculate() function."
    )
  } else if (is.null(order)) {
    order <- as.character(unique_ex)
    infer:::warning_glue(
      "The statistic is based on a difference or ratio; by default, for ",
      "difference-based statistics, the explanatory variable is subtracted ",
      "in the order \"{unique_ex[1]}\" - \"{unique_ex[2]}\", or divided in ",
      "the order \"{unique_ex[1]}\" / \"{unique_ex[2]}\" for ratio-based ",
      "statistics. To specify this order yourself, supply `order = ",
      "c(\"{unique_ex[1]}\", \"{unique_ex[2]}\")`."
    )
  } else {
    if (xor(is.na(order[1]), is.na(order[2]))) {
      stop_glue(
        "Only one level specified in `order`. Both levels need to be specified."
      )
    }
    if (length(order) > 2) {
      stop_glue("`order` is expecting only two entries.")
    }
    if (order[1] %in% unique_ex == FALSE) {
      stop_glue("{order[1]} is not a level of the explanatory variable.")
    }
    if (order[2] %in% unique_ex == FALSE) {
      stop_glue("{order[2]} is not a level of the explanatory variable.")
    }
  }
  # return the order as given (unless the argument was invalid or NULL)
  order
}

reorder_explanatory <- function(x, order) {
  x[[explanatory_name(x)]] <- factor(
    explanatory_variable(x),
    levels = c(order[1], order[2])
  )
  x
}



parse_variables <- function(x, formula, response, explanatory) {
  if (methods::hasArg(formula)) {
    tryCatch(
      rlang::is_formula(formula),
      error = function(e) {
        stop_glue("The argument you passed in for the formula does not exist.
                  * Were you trying to pass in an unquoted column name?
                  * Did you forget to name one or more arguments?")
      }
    )
    if (!rlang::is_formula(formula)) {
      stop_glue("The first unnamed argument must be a formula.
                * You passed in '{get_type(formula)}'.
                * Did you forget to name one or more arguments?")
    }
  }

  attr(x, "response")    <- get_expr(response)
  attr(x, "explanatory") <- get_expr(explanatory)
  attr(x, "formula") <- NULL

  if (methods::hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
    attr(x, "formula") <- formula
  }

  # Check response and explanatory variables to be appropriate for later use
  if (!has_response(x)) {
    stop_glue("Please supply a response variable that is not `NULL`.")
  }

  check_var_correct(x, "response")
  check_var_correct(x, "explanatory")

  # If there's an explanatory var
  check_vars_different(x)

  if (!has_attr(x, "response")) {
    attr(x, "response_type") <- NULL
  } else {
    attr(x, "response_type") <- class(response_variable(x))
  }

  if (!has_attr(x, "explanatory")) {
    attr(x, "explanatory_type") <- NULL
  } else {
    attr(x, "explanatory_type") <-
      purrr::map_chr(as.data.frame(explanatory_variable(x)), class)
  }

  attr(x, "type_desc_response") <- determine_variable_type(x, "response")
  attr(x, "type_desc_explanatory") <- determine_variable_type(x, "explanatory")

  # Determine params for theoretical fit
  x <- infer:::set_params(x)

  x
}

check_success_arg <- function(x, success) {
  response_col <- response_variable(x)

  if (!is.null(success)) {
    if (!is.character(success)) {
      stop_glue("`success` must be a string.")
    }
    if (!is.factor(response_col)) {
      stop_glue(
        "`success` should only be specified if the response is a categorical ",
        "variable."
      )
    }
    if (!(success %in% levels(response_col))) {
      stop_glue('{success} is not a valid level of {response_name(x)}.')
    }
    if (sum(table(response_col) > 0) > 2) {
      stop_glue(
        "`success` can only be used if the response has two levels. ",
        "`filter()` can reduce a variable to two levels."
      )
    }
  }

  if ((attr(x, "response_type") == "factor" &&
       is.null(success) &&
       length(levels(response_variable(x))) == 2) &&
      ((!has_attr(x, "explanatory_type") ||
        length(levels(explanatory_variable(x))) == 2))) {
    stop_glue(
      'A level of the response variable `{response_name(x)}` needs to be ',
      'specified for the `success` argument in `specify()`.'
    )
  }

}

check_var_correct <- function(x, var_name) {
  var <- attr(x, var_name)

  # Variable (if present) should be a symbolic column name
  if (!is.null(var)) {
    if (!rlang::is_symbolic(var)) {
      stop_glue(
        "The {var_name} should be a bare variable name (not a string in ",
        "quotation marks)."
      )
    }

    if (any(!(all.vars(var) %in% names(x)))) {
      stop_glue(
        'The {var_name} variable `{var}` cannot be found in this dataframe.'
      )
    }
  }

  TRUE
}

check_vars_different <- function(x) {
  if (has_response(x) && has_explanatory(x)) {
    if (identical(response_name(x), explanatory_name(x))) {
      stop_glue(
        "The response and explanatory variables must be different from one ",
        "another."
      )
    }
  }

  TRUE
}
