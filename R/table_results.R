#' Outputs a table with model results
#'
#' @description
#' This function takes a model run using \code{lme4} and outputs a table of results, including model coefficients and likelihood-ratio test of each predictor.
#'
#' @param label_list List of predictor names to appear in the table of results.
#' @param model_test Name of the model to produce a result table from.
#' @param test either 'F' or 'Chisq'
#' @return A table with model results. It can be easily saved in html to include in your reports / manuscript.
#' @examples
#' # to be included
#'
#' @export
table_results <- function(label_list, model_test, test) {
  
  table_00 <- gtsummary::tbl_regression(
    x = model_test,
    intercept = TRUE,
    label = label_list,
    estimate_fun = ~ gtsummary::style_number(.x, digits = 2)
  )
  
  table_01 <- gtsummary::add_global_p(
    x = table_00,
    anova_fun = drop1_output,
    test = test
  )
  table_01 <- gtsummary::bold_p(table_01, t = 0.05)
  table_01 <- gtsummary::bold_labels(table_01)
  table_01 <- gtsummary::italicize_levels(table_01)
  table_01 <- gtsummary::modify_table_body(
    x = table_01,
    fun = function(x) {
      output <- dplyr::left_join(
        x = x,
        y = dplyr::select(
          drop1_output(model_test = model_test, test = test),
          variable = term,
          Chisq = statistic,
          df
        ),
        by = "variable"
      )
      output$df <- base::ifelse(output$row_type == "label", output$df, NA)
      output$Chisq <- base::ifelse(output$row_type == "label", output$Chisq, NA)
      output
    }
  )
  
  table_02 <- gtsummary::modify_fmt_fun(
    x = table_01,
    c(Chisq) ~ function(x) gtsummary::style_number(x, digits = 2)
  )
  table_02 <- gtsummary::modify_fmt_fun(
    x = table_02,
    c(std.error) ~ function(x) gtsummary::style_number(x, digits = 2)
  )
  table_02 <- gtsummary::modify_fmt_fun(
    x = table_02,
    c(p.value) ~ function(x) gtsummary::style_number(x, digits = 3)
  )
  table_02 <- gtsummary::modify_table_body(
    x = table_02,
    fun = function(x) dplyr::relocate(x, p.value, .after = df)
  )
  table_02 <- gtsummary::modify_header(table_02, label ~ "**Fixed effect**")
  table_02 <- gtsummary::modify_header(table_02, std.error ~ "**SE**")
  table_02 <- gtsummary::modify_header(table_02, estimate ~ "**Estimate**")
  
  if (test == "Chisq") {
    table_02 <- gtsummary::modify_header(table_02, df ~ "**df**")
    table_02 <- gtsummary::modify_header(table_02, Chisq ~ gt::html("<b>&chi;<sup>2</sup></b>"))
  } else {
    table_02 <- gtsummary::modify_header(table_02, Chisq ~ "**F**")
  }
  
  table_02 <- gtsummary::as_gt(table_02)
  table_02 <- gt::opt_footnote_marks(data = table_02, marks = "LETTERS")
  
  table_02
}
