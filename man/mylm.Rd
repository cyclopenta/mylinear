% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mylinear.R
\name{mylm}
\alias{mylm}
\title{mylm}
\usage{
mylm(
  dat,
  response,
  covariates,
  inter = c(),
  category = c(),
  cat_method = "reference",
  ref = c(),
  model.diag = T,
  intercept = T,
  cutoff = 0.05
)
}
\arguments{
\item{dat}{input data frame with no NAs, should include all the data that will be used}

\item{response}{name string for response}

\item{covariates}{string vector of the chosen variables in the input data frame}

\item{inter}{specify the pairwise interaction in regression , e.g.:c('Age*Sex', 'Age*R_E')}

\item{category}{specify the categorical variables by input the names vector}

\item{cat_method}{choose the coding method for categorical variables, should be 'reference' or 'cellmeans'}

\item{ref}{specify the reference level for each categorical variable}

\item{model.diag}{default  is True, activate the model diagnose function}

\item{intercept}{default is True, make the model with an intercept}

\item{cutoff}{specify the level of significance for the test}
}
\value{
similar output table as summary(lm), residuals,R.square, R.square.adj, SSE
}
\description{
fit a linear model with basic model diagnose
}
\examples{
data(mydata)
covar2 = c('Fatalism', 'Sex', 'R_E', 'Age_4Cat', 'NIHSS_4Cat')
covar3 = c('Fatalism', 'Sex', 'R_E', 'Age_4Cat')
t4 = mylm(mydata, 'Depression', covar2, category = c('Age_4Cat', 'NIHSS_4Cat'), ref = c(1,1))
t5 = mylm(mydata, 'Depression', covar3, category = c('Age_4Cat'),
cat_method = 'cellmeans', intercept = FALSE)

}
