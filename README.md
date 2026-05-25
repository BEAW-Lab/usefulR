# Collection of useful R functions

Bundle of R functions in the form of an R package containing useful resources for data wrangling, visualisation, analysis and creation of publication-ready tables of results. 
If you find a problem or would like to add or improve a function, create a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests).

The package currently includes helpers to calculate urban land cover around coordinates from ESA WorldCover 2021 and CORINE Land Cover rasters, as well as artificial light at night from VIIRS data. It also includes download helpers for ESA WorldCover, VIIRS, and CORINE Land Cover 2018.

## **IMPORTANT**: Use our critical assessment when using these functions. Always double check that the output they produce aligns with the results of your analysis.

# Installation
The package metadata is set so that R dependencies listed in `Imports` are installed automatically when you install `usefulR` with a dependency-aware installer.

Recommended:
```r
# install.packages("pak")
pak::pkg_install("BEAW-Lab/usefulR")
```

Alternative:
```r
# install.packages("remotes")
remotes::install_github(
  "BEAW-Lab/usefulR",
  dependencies = TRUE,
  build_vignettes = TRUE
)
```

If you download the repository ZIP manually from GitHub and install it as a local source package, R may not install dependencies for you automatically. In that case, use `pak::pkg_install()` or `remotes::install_github()` instead.

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
