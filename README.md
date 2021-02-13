# R-Easy-EDA

The goal of this Shiny app is to:
* Make visualizing data in R more Tableau-like (my organizaiton loves Tableau)
* Assist with the data preprocessing that all data scientists need to do in their daily workflows

My main inspiration for this was the [explore](https://github.com/rolkra/explore) package. I also used a small function from the author's code to help get this Shiny app functional. I recommend checking the package out if you're interested in enhancing your R EDA process! 

## Components Currently Developed

* **Visualization tab** that dynamically handles categorical or numeric data and plots relationships of said variables
* **Transformations tab** that checks the normality of a numeric variable based on the following:
  * Box-Cox
  * Yeo-Johnson
  * Log
  * Square Root
* **Table tab** which allows dynamically filtering data
* **Predictive Power Score tab** which uses a recently developed measure to detect associations between categorical variables and also nonlinear associations
* **Pearson Correlation tab** which measures the Pearson correlation across all numeric variables

### Notes

Code is currently functional, though it is not perfect. Enhancements may be developed for this in the future, as well as code cleanup (right now there's a fair amount of code duplication). At some point I might put this into an R package like easytidymodels.
