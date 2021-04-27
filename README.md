
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reasyeda

<!-- badges: start -->

<!-- badges: end -->

This is a package developed to quickly create interactive visualizations
for your data and standardize how they look. Other functionality has
also been built in order to check correlation, predictive power score
(PPS), and how various transformations might influence your data.

## Installation

You can install reasyeda like this:

``` r
# install.packages("devtools")
devtools::install_github("amanda-park/reasyeda")
```

This package was built recently so it’s highly recommended to be on
version 4 of R.

## How to Use

``` r
require(reasyeda)
#> Loading required package: reasyeda

#Load penguins dataset
data(penguins, package = "modeldata")

shinyEDA(penguins)
#> PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

Upon opening the shiny app there will be 5 tabs: \* Visualization \*
Transformations \* Table \* Predictive Power Score \* Pearson
correlations

Each tab will make that step of the exploratory data analysis process
hopefully simpler than before while creating plots that executive
leaders will find pleasing to the eyes. Most plots are plotly-wrapped
ggplots for a more Tableau-like feel, but can be downloaded separately
to remove the interactivity component.

Transformations will let you see how normally distributed your data
looks visually and according to Shapiro-Wilk after applying some common
transformations to your numeric variables.

Table will let you see your raw data in an interactive datatable for
easier exploration at a very granular level.

Predictive Power Score is an alternative measurement to correlation that
can detect relationships between numeric and categorical variables. This
runs on all variables in your dataset to come up with a solution. A
heatmap plot is also provided to more easily visualise which
correlations are most important.

Pearson correlations are your regular way of measuring strength between
numeric variables. Much like Predictive Power Score, it will return a
heatmap which visually shows the strengths and weaknesses of
correlations in your dataset.
