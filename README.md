## Basic Linear Regression 
<!-- badges: start -->
  [![R-CMD-check](https://github.com/cyclopenta/mylinear/workflows/R-CMD-check/badge.svg)](https://github.com/cyclopenta/mylinear/actions)
  <!-- badges: end -->
<!-- badges: start -->
  [![codecov](https://codecov.io/gh/cyclopenta/mylinear/branch/main/graph/badge.svg?token=9HR9PMXG0M)](https://codecov.io/gh/cyclopenta/mylinear)
  <!-- badges: end -->

# Introdction  
This package integrates the basic elements in the linear regression based on BIOSTAT650. The main function **mylm** will return a list of 5 items, They are:  
  
**results**: dataframe, with similar format with summary(lm)  
  
**residuals**: The residuals of the model  
  
**R.square**: The R^2 which demonstrate the percentage of variance in Y explained by the model  
  
**R.square.adj.**: The adjusted of R^2  
  
**SSE**: The sum of square errors in the model  
  
Also, the function will automatically plot the **partial regression plots** for MLR and residual plot for SLR to test **"linearity"** assumption. Residuals v.s. Y_fit is also generated to test **"constant variance" assumption**  
  
Help pages are available for "mylm" amd "categorize"  
  
The example dataset named with "mydata" was generated from the reference below:  
  
Morgenstern, Lewis B., et al. "Fatalism, optimism, spirituality, depressive symptoms, and stroke outcome: a population-based analysis." Stroke 42.12 (2011): 3518-3523.  

The further tutorial is in the /vignettes.
