#'categorize
#'
#'categorize a set of variables automatically into dummy variables. Return data frame with categorized
#'values. The method could only be reference coding or cell means coding.Score coding is not integrated
#'in this function. The main usage for this function is usesd internally in mylm function. But user could
#'also use this to create categorical variables as well.
#'
#'@param data input data frame with no NAs, should include all the covariates that will be used later
#'
#'@param vars name vector for the category variables
#'
#'@param method specify the method for categorization
#'@param refs specify the reference level for each categorical variable
#'@return data frame with all the category variables transformed to numerous dummy variables.
#'
#'@examples
#'
#'categorize(mydata, c('R_E', 'NIHSS_4Cat'), ref = c(0,1))
#'categorize(mydata, c('R_E', 'NIHSS_4Cat'), method = 'cellmeans') = F)
#'
#'@export
#'
categorize = function(data, vars, method = c('reference', 'cellmeans'), ref = c()){
  method = match.arg(method)
  l = nrow(data)
  CAT_dat = select(data, -c(vars))
  if (method == 'reference'){
    if( length(ref) < 1){
      stop('specified the reference')
    }
    i = 1
    while (i <= length(vars)){
      cats = unique(data[[vars[i]]])
      cats = cats[cats != ref[i]]
      w = length(cats)
      # initialize the reference coding matrix
      tmp = matrix(rep(data[[vars[i]]], w), nrow =l , ncol = w)
      colnames(tmp) = paste(vars[i], cats,sep = '.')
      j = 1
      while (j <= length(cats)){
        tmp[,j] = ifelse(tmp[,j] == cats[j], 1, 0)
        j = j + 1
      }
      CAT_dat = cbind(CAT_dat, tmp)
      i = i + 1
    }
  }
  # cell means coding
  else{
    i = 1
    while (i <= length(vars)){
      cats = unique(data[[vars[i]]])
      w = length(cats)
      # initialize the reference coding matrix
      tmp = matrix(rep(data[[vars[i]]], w), nrow =l , ncol = w)
      colnames(tmp) = paste(vars[i], cats,sep = '.')
      j = 1
      while (j <= length(cats)){
        tmp[,j] = ifelse(tmp[,j] == cats[j], 1, 0)
        j = j + 1
      }
      CAT_dat = cbind(CAT_dat, tmp)
      i = i + 1
    }
  }
  return(CAT_dat)
}
