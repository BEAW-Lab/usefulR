## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  )

library(usefulR)


## ----include = T, eval = F----------------------------------------------------
# # install.packages("pak") # install pak if not previously installed
# pak::pkg_install("BEAW-Lab/usefulR")

## ----data simulation, include = T---------------------------------------------
set.seed(7)
coor <- data.frame(lon = runif(n = 50, min = -6, max = -3),
                   lat = runif(n = 50, min = 39, max = 42),
                   x_var1 = rnorm(n = 50, mean = 0, sd = 5))
head(coor)

