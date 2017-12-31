# mattR, bare bones web framework

`mattR` is a bare bones web framework that aims to give you complete control
over your app.

At the moment `mattR` doesn't concern itself with models so you could say it is
a View-Tempate (MV) framework.

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

Application routes must be defined in `routes.R` as a list of vectors named
`routes` as follows

```R
routes <- list(
  c("^/todo$", genericView(todo)),
  c("^/admin$", templateView(admin)),
  c("^/login$", genericView(login)),
  c("^/.*$", staticView(file.path(getwd(), "static/"), "/"))
)
```
where the `*`View functions are defined by `mattR`.

The first element of the list is used to match the path of the url and the
second element is (after evaluation) a function that takes returns the
response.

Optionally, a file `init.R` can be defined which runs when `mattR` is started
to setup the application. By convention, the result of `source()`'-ing this
file is put in a list `appState` which will be `attach()`ed to the search path.

## Examples

Example application can be found in
[mattR_examples](https://github.com/bobjansen/mattR_examples).

