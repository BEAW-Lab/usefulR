# Collection of useful R functions

Bundle of R functions in the form of an R package containing useful resources for data wrangling, visualisation, analysis and creation of publication-ready tables of results. 
If you find a problem or would like to add or improve a function, create a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests).

## **IMPORTANT**: Use our critical assessment when using these functions. Always double check that the output they produce aligns with the results of your analysis.

# Installation
```{r eval = F}
# install.packages("devtools")
devtools::install_github("BEAW-Lab/usefulR", build_vignettes = TRUE)
```

# Usage
After installation, you can use the functions in this package as any other R package. If you like, you can load the package using:
```{r eval = F}
library(usefulR)
```

There is a vignette that you can inspect and follow as a tutorial, to access the vignette, run:
```{r eval = F}
browseVignettes("usefulR")
```

Get in touch if anything is unclear and we
will try to polish both the functions and the tutorial in the vignette.