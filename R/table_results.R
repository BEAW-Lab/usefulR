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
  
  #source('./scripts/00_FUNCTIONS/FUNCTION_drop1_output.R')
  
  table_00 <- model_test %>%
    gtsummary::tbl_regression(intercept = T,
                              label = label_list,
                              estimate_fun = ~ style_number(.x, digits = 2))
  
  ## add features
  table_01 <- table_00 %>% 
    gtsummary::add_global_p(anova_fun = drop1_output, test = test) %>% 
    gtsummary::bold_p(t = 0.05) %>% 
    gtsummary::bold_labels() %>%
    gtsummary::italicize_levels() %>% 
    gtsummary::modify_table_body(fun = function(.){
      output <- dplyr::left_join(x = .,
                                 y = drop1_output(model_test = model_test, test = test) %>% 
                                   dplyr::select(variable = term, Chisq=statistic, df),
                                 by = "variable")
      output$df <- base::ifelse(output$row_type == "label",  output$df, NA)
      output$Chisq <- base::ifelse(output$row_type == "label",  output$Chisq, NA)
      return(output)
    })
  
  if(test == 'Chisq'){
    table_02 <- table_01 %>% 
      gtsummary::modify_fmt_fun(c(Chisq) ~ function(x) style_number(x, digits = 2)) %>%
      gtsummary::modify_fmt_fun(c(std.error) ~ function(x) style_number(x, digits = 2)) %>%
      gtsummary::modify_fmt_fun(c(p.value) ~ function(x) style_number(x, digits = 3)) %>%
      gtsummary::modify_table_body(~.x %>% dplyr::relocate(p.value, .after = df)) %>% 
      gtsummary::modify_header(label ~ "**Fixed effect**") %>% 
      gtsummary::modify_header(std.error ~ "**SE**") %>%
      gtsummary::modify_header(estimate ~ "**Estimate**") %>%
      gtsummary::modify_header(df ~ "**df**") %>% 
      gtsummary::modify_header(Chisq ~ html("<b>&chi;<sup>2</sup></b>")) %>% 
      gtsummary::as_gt() %>% 
      gt::opt_footnote_marks(marks = "LETTERS")
    
  } else {
    table_02 <- table_01 %>% 
      gtsummary::modify_fmt_fun(c(Chisq) ~ function(x) style_number(x, digits = 2)) %>%
      gtsummary::modify_fmt_fun(c(std.error) ~ function(x) style_number(x, digits = 2)) %>%
      gtsummary::modify_fmt_fun(c(p.value) ~ function(x) style_number(x, digits = 3)) %>%
      gtsummary::modify_table_body(~.x %>% dplyr::relocate(p.value, .after = df)) %>% 
      gtsummary::modify_header(label ~ "**Fixed effect**") %>% 
      gtsummary::modify_header(std.error ~ "**SE**") %>%
      gtsummary::modify_header(estimate ~ "**Estimate**") %>%
      gtsummary::modify_header(Chisq ~ "**F**") %>% 
      gtsummary::as_gt() %>% 
      gt::opt_footnote_marks(marks = "LETTERS")
    
  }
  return(table_02)
}

