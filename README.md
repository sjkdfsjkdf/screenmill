# ScreenMill R package

Tools for working with [ScreenMill](http://www.rothsteinlab.com/tools/screen_mill/cm_engine) data.

## Installation

The `screenmill` *R* package depends on the 
[EBImage](http://bioconductor.org/packages/release/bioc/html/EBImage.html) package from 
[Bioconductor](http://bioconductor.org/install/). If
[BiocInstaller](http://bioconductor.org/packages/release/bioc/html/BiocInstaller.html) 
is less than version `1.18.0` you will need to upgrade. Installing packages from 
GitHub via `BiocInstaller::biocLite` also requires 
[devtools](https://github.com/hadley/devtools). Run the code below to install 
all necessary dependencies described above. If there is an issue updating/installing
Bioconductor, try using `'http'` instead of `'https'`.

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
