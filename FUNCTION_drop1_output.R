#' Computes likelihood-ratio test results for a given mdoel.
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
drop1_output <- function(model_test, ...){
    x <- stats::drop1(model_test, test = "Chisq")
    x$AIC <- NULL
    names(x) <- c("Df", "Chisq", "Pr(>Chisq)")
    output <- broom::tidy(x)
    return(output)  
}
