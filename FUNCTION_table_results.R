#' Outputs a table with model results
#'
#' @description
#' This function takes a model run using \code{lme4} and outputs a table of results, including model coefficients and likelihood-ratio test of each predictor.
#'
#' @param label_list List of predictor names to appear in the table of results.
#' @param model_test Name of the model to produce a result table from.
#' @return A table with model results. It can be easily saved in html to include in your reports / manuscript.
#' @examples
#' # to be included
#'
#' @export
table_results <- function(label_list, model_test) {
  
  #source('./scripts/00_FUNCTIONS/FUNCTION_drop1_output.R')
  
  table_00 <- model_test %>%
    tbl_regression(intercept = T,
                   label = label_list,
                   estimate_fun = ~ style_number(.x, digits = 2))
  
  ## add features
  table_01 <- table_00 %>% 
    add_global_p(anova_fun = drop1_output) %>% 
    bold_p(t = 0.05) %>% 
    bold_labels() %>%
    italicize_levels() %>% 
    modify_table_body(fun = function(.){
      output <- left_join(x = .,
                          y = drop1_output(x = model_test) %>% 
                            dplyr::select(variable = term, Chisq=statistic, df),
                          by = "variable")
      output$df <- ifelse(output$row_type == "label",  output$df, NA)
      output$Chisq <- ifelse(output$row_type == "label",  output$Chisq, NA)
      return(output)
    }) %>% 
    modify_fmt_fun(c(Chisq) ~ function(x) style_number(x, digits = 2)) %>%
    modify_fmt_fun(c(std.error) ~ function(x) style_number(x, digits = 2)) %>%
    modify_fmt_fun(c(p.value) ~ function(x) style_number(x, digits = 3)) %>%
    modify_table_body(~.x %>% dplyr::relocate(p.value, .after = df)) %>% 
    modify_header(label ~ "**Fixed effect**") %>% 
    modify_header(std.error ~ "**SE**") %>%
    modify_header(estimate ~ "**Estimate**") %>%
    modify_header(df ~ "**df**") %>% 
    modify_header(Chisq ~ html("<b>&chi;<sup>2</sup></b>")) %>% 
    as_gt() %>% 
    opt_footnote_marks(marks = "LETTERS")
  
  return(table_01)
}


