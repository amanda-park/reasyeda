
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reasyeda

<!-- badges: start -->
<!-- badges: end -->

This is a package developed to quickly create interactive visualizations
for your data and standardize how they look. Other functionality has
also been built in order to check correlation, predictive power score
(PPS), and how various transformations might influence your data.

Note: This package was built on R 4.1.2.

## Installation

You can install reasyeda like this:

``` r
# install.packages("devtools")
devtools::install_github("amanda-park/reasyeda")
```

## How to Use

``` r
require(reasyeda)

#Load penguins dataset
data(penguins, package = "modeldata")

#Opens Shiny App to Interactively Explore Data
#explore_df(penguins)
```

Upon opening the shiny app there will be 5 tabs:

-   Visualization
-   Transformations
-   Table
-   Predictive Power Score
-   Pearson correlations

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

**Correlations** measures the relationship between numeric variables via
Pearson’s Correlation and then detecting overall relationships between
variables using Predictive Power Score. Plots and Table outputs are
provided.
