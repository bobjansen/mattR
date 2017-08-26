# mattR, bare bones web framework

`mattR` is a bare bones web framework that aims to give you complete control 
over your app.

At the moment `mattR` doesn't concern itself with models so you could say it is
a View-Tempate framework.

## Installation

Install `mattR` from GitHub using 
[devtools](https://cran.r-project.org/web/packages/devtools/index.html), load
the package and create a skeleton for your app.

```R
devtools::install_github("bobjansen/mattR")
library(mattR)
skeleton()
```

## Usage

After creating the app using `skeleton()` you need to create routes and views
for your app.

## Examples

Example application can be found in 
[mattR_examples](https://github.com/bobjansen/mattR_examples).
