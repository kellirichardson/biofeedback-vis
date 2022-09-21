
# Health Behavior Change Interventions Using Biological Feedback


<!-- badges: start -->
[![deploy-to-connect](https://github.com/cct-datascience/biofeedback-vis/actions/workflows/deploy-to-connect.yaml/badge.svg)](https://github.com/cct-datascience/biofeedback-vis/actions/workflows/deploy-to-connect.yaml)
<!-- badges: end -->

An interactive visualization was developed using data from a scoping review on biological feedback as a behavior change technique for adults in randomized clinical trials. The visualization is designed to allow users to isolate and extract studies most relevant to their field of interest.

[Link to Interactive Visualization](https://viz.datascience.arizona.edu/biofeedback_sankey/)

The visualization was made through the University of Arizona Communications and Cyber Technologies (CCT) Data Science Incubator Program. Please contact Dr. Susan Schembre, PhD, RD at ss4731@georgetown.edu if you have questions or comments. 

Protocol: https://www.researchprotocols.org/2022/1/e32579

## Collaboration Guidelines

This project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package managment.  When opening this repo as an RStudio Project for the first time, `renv` should automatically install itself and prompt you to run `renv::restore()` to install all package dependencies.

To contribute to this project, please create a new branch for your changes and make a pull request.  One easy way to do this from within R is with the `usethis` package and the `pr_*` functions.  `pr_init("branch-name")` begins a new branch locally, `pr_push()` helps you create a new pull request, and after it is merged you can use `pr_finish()` to clean things up.  More about this workflow [here](https://usethis.r-lib.org/articles/pr-functions.html).

## How it works 

- The csv file output by DistillerSR is in the `data_raw` folder.
- An R script in the `R` folder has code to wrangle those data and save the result as `articles_clean.csv` in the `app` folder. If the raw data changes, this script will need to be re-run manually!
- The Shiny app code is in `app/app.R` and is made of two parts---a UI, which defines the look of the app and what inputs and outputs are shown, and a server that handles the data and plotting.
- The packages needed to wrangle the data and run the Shiny app are kept track of by the `renv` package.  If you add new packages or update packages, you can run `renv::snapshot()` to record this. `.Rprofile`, `renv.lock`, and the `renv` folder are all needed for `renv` to work and should not be edited manually.
- When changes are made to the app through a GitHub pull request, it triggers a GitHub action to run automatically to deploy the Shiny app to viz.datascience.arizona.edu.  This action is defined in `.github/workflows/deploy-to-connect.yaml` and probably never needs to be edited.
