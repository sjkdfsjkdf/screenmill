# ScreenMill R package

Tools for working with [ScreenMill](http://www.rothsteinlab.com/tools/screen_mill/cm_engine) data.

## Installation

The `screenmill` *R* package can be installed by running the following *R* 
commands. You must have [*R*](https://www.r-project.org) (>= 3.2.2) already 
installed, and [Rstudio](https://www.rstudio.com) (>= v0.99.878) is recommended 
for using the "Shiny gadgets" provided in this package 
(e.g. `screenmill::annotate_plates()`).

```r
# devtools (>= 1.9.1) supports installation of GitHub "Remotes" in DESCRIPTION
if (!require('devtools', quietly = T) || packageVersion('devtools') < '1.9.1') {
  install.packages('devtools')
}

# Install/update screenmill to the latest development version
devtools::install_github('EricEdwardBryant/screenmill')
```
