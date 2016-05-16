# Screenmill R package

Tools for working with [ScreenMill](http://www.rothsteinlab.com/tools/screen_mill/cm_engine) data.

## Installation

This package is not currently available on [CRAN](https://cran.r-project.org), but can be installed from GitHub by following the instructions below.

**Step 1**: Install [R](https://cloud.r-project.org) (>= 3.3.0 recommended).

**Step 2**: Install R package developer tools. Why? This package contains some Rcpp code that must be compiled, so you will need a C++ compiler (e.g. [GCC](https://gcc.gnu.org), or [clang](http://clang.llvm.org)). For more help, checkout this guide for R's [package development prerequisites](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites).

- **Mac OS X**: Install command-line developter tools in terminal with: `xcode-select --install`. Or install [Xcode](https://developer.apple.com/xcode/) from the app store.
- **Windows**: Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
- **Debian/Ubuntu**: Install the build-essential package in shell with: `sudo apt-get install build-essential`

**Step 3 (*optional*)**: Install [RStudio](https://www.rstudio.com) (>= v0.99.878). Why? RStudio makes programming in R fun! Also, version 0.99.878 introduces "Addins" which makes it easy to launch interactive tools from a drop-down menu (e.g. to launch `screenmill::annotate()`).

**Step 4**: Install the package by running the following code in R

```r
# Install the latest version of Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite()

# Install the latest version of devtools (>= 1.9.1)
install.packages('devtools', dependencies = T)

# Install the latest version of screenmill
devtools::install_github('EricEdwardBryant/screenmill')
```
