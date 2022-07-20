
# biofeedback-vis

<!-- badges: start -->
[![deploy-to-connect](https://github.com/cct-datascience/biofeedback-vis/actions/workflows/deploy-to-connect.yaml/badge.svg)](https://github.com/cct-datascience/biofeedback-vis/actions/workflows/deploy-to-connect.yaml)
<!-- badges: end -->

The goal of biofeedback-vis is to ...

[Shiny App](https://viz.datascience.arizona.edu/biofeedback_sankey/)

[Notes](notes/notes.md)

## Collaboration Guidelines

This project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package managment.  When opening this repo as an RStudio Project for the first time, `renv` should automatically install itself and prompt you to run `renv::restore()` to install all package dependencies.

To contribute to this project, please create a new branch for your changes and make a pull request.  One easy way to do this from within R is with the `usethis` package and the `pr_*` functions.  `pr_init("branch-name")` begins a new branch locally, `pr_push()` helps you create a new pull request, and after it is merged you can use `pr_finish()` to clean things up.  More about this workflow [here](https://usethis.r-lib.org/articles/pr-functions.html).

## Repo structure

`.github/` houses the GitHub action YAML file that controls how the Shiny app is automatically deployed to RStudio Connect.  This shouldn't need to be edited other than maybe to change the name/URL of the app.

`.Rprofile`, `renv.lock`, and `renv/` are all infrastructure added by the `renv` package and should not be edited manually.

`app/` contains the files for the Shiny App, including the cleaned and wrangled data.

`data_raw/` contains the raw exported CSV file.

`notes/` contains notes and early drafts of code.

`R/` contains R scripts to be run to wrangle the raw data.


