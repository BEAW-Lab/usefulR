#' Computes likelihood-ratio test results for a given model.
#'
#' @description
#' This function is a wrapper of stats::drop1() inside gtsummary::tbl_regression. 
#'
#' @param model_test Name of the model to be tested.
#' @return Tidy table containing likelihood-ratio test results.
#' @examples
#' # to be included
#'
#' @export
drop1_output <- function(model_test, test = c("Chisq", "F"), ...){
    x <- stats::drop1(model_test, test = test)
    
    if(test == "Chisq") {
      x$AIC <- NULL
      names(x) <- base::c("Df", "Chisq", "Pr(>Chisq)")
      output <- broom::tidy(x)
    } else {
      x$AIC <- NULL
      names(x) <- base::c("Df", "Sum of Sq", "RSS", "F value", "Pr(>F)")
      output <- broom::tidy(x)
    }
    return(output)  
    
}
