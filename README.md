
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

#shinyEDA(penguins)
```

Upon opening the shiny app there will be 5 tabs:

  - Visualization
  - Transformations
  - Table
  - Predictive Power Score
  - Pearson correlations

**Visualization** creates plots of your raw data that executive leaders
will find pleasing to the eyes. Most plots are plotly-wrapped ggplots
for a more Tableau-like feel, but can be downloaded separately to remove
the interactivity component. A variety of color choices are available to
make sure the plot fits your organization’s preferred color schemes.

**Transformations** will let you see how normally distributed your data
looks visually and according to Shapiro-Wilk after applying some common
transformations to your numeric variables.

**Table** will let you see your raw data in an interactive datatable for
easier exploration at a very granular level.

**Predictive Power Score** is an alternative measurement to correlation
that can detect relationships between numeric and categorical variables.
This runs on all variables in your dataset to come up with a solution. A
heatmap plot is also provided to more easily visualise which
correlations are most important.

**Pearson Correlations** are your regular way of measuring strength
between numeric variables. Much like Predictive Power Score, it will
return a heatmap which visually shows the strengths and weaknesses of
correlations in your dataset.
