# ScreenMill R package

Tools for working with [ScreenMill](http://www.rothsteinlab.com/tools/screen_mill/cm_engine) data.

## Installation

The `screenmill` *R* package depends on the `EBImage` package from [Bioconductor](http://bioconductor.org/install/). If your installation of 
the `BiocInstaller` package is less than version `1.18.0` you should upgrade 
your installation of Bioconductor. Installing packages from GitHub through
Bioconductor's `BiocInstaller::biocLite` function also requires `devtools`. Run 
the code below to install all necessary dependencies. If there is an issue installing
Bioconductor, try using `http` instead of `https`. ‚

```r
# BiocInstaller (>= 1.18.0) supports GitHub packages with Bioconductor dependencies
if (!require('BiocInstaller') || packageVersion('BiocInstaller') < '1.18.0') {
  source('https://bioconductor.org/biocLite.R')  # use 'http' if 404 on 'https'
}

# Install devtools (>= 1.9.1)
if (!require('devtools') || packageVersion('devtools') < '1.9.1') {
  install.packages('devtools')
}

biocLite('EricEdwardBryant/screenmill')
```
