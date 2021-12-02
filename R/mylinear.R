#'mylm
#'
#'fit a linear model with basic model diagnose
#'
#'@param dat input data frame with no NAs, should include all the data that will be used
#'
#'@param response name string for response
#'
#'@param covariates string vector of the chosen variables in the input data frame
#'
#'@param inter specify the pairwise interaction in regression , e.g.:c('Age*Sex', 'Age*R_E')
#'
#'@param category specify the categorical variables by input the names vector
#'
#'@param cat_method choose the coding method for categorical variables, should be 'reference' or 'cellmeans'
#'
#'@param ref specify the reference level for each categorical variable
#'
#'@param model.diag default  is True, activate the model diagnose function
#'
#'@param intercept default is True, make the model with an intercept
#'
#'@param cutoff specify the level of significance for the test
#'
#'@return similar output table as summary(lm), residuals,R.square, R.square.adj, SSE
#'
#'@examples
#'data(mydata)
#'t1 = mylm(mydata, 'Depression', covar1)
#'t2 = mylm(mydata, 'Depression', covar1, intercept = F)
#'t3 = mylm(mydata, 'Depression', covar2)
#'t4 = mylm(mydata, 'Depression', covar2, category = c('Age_4Cat', 'NIHSS_4Cat'), ref = c(1,1))
#'t5 = mylm(mydata, 'Depression', covar3, category = c('Age_4Cat'), cat_method = 'cellmeans', intercept = F)
#'
#'@export
#'
mylm = function(dat, response, covariates,inter = c(),
                category = c(), cat_method = 'reference', ref = c(),
                model.diag = T, intercept = T, cutoff = 0.05){
  model_dat = subset(dat, select = c(response, covariates))
  # checking the data
  if (any(is.na(model_dat))){
    stop("Nas exist among the selected variables")
  }
  # describe the data
  obs = nrow(model_dat)

  if(obs <= 1){
    stop('do not input empty data or Need more observations ')
  }
  tmp_dat = subset(model_dat, select = covariates)
  # categorize variables
  if(length(category) >= 1 ){
    model_dat = categorize(data = tmp_dat, vars = category, method = cat_method, ref = ref)
    tmp_dat = model_dat
  }
  # process the interaction term
  inter_n = length(inter)
  if(inter_n >= 1){
    inter_matrix = matrix(rep(0,inter_n*obs), nrow = obs, ncol = inter_n)
    colnames(inter_matrix) = inter
    i = 1
    while( i <= inter_n){
      # parse the parameter
      new_inter_vec = str_split(inter[i], ":")[[1]]
      tmp_group = subset(tmp_dat, select = new_inter_vec)
      inter_matrix[, i] =rowProds(as.matrix(tmp_group))
      i = i + 1
    }
    tmp_dat = cbind(tmp_dat, inter_matrix)
  }
  beta_n = ncol(tmp_dat)
  X_matrix = as.matrix(tmp_dat)
  covariates = colnames(tmp_dat)
  if (intercept){
    X_matrix = cbind(rep(1, nrow(model_dat)), X_matrix)
    beta_n = beta_n + 1
    colnames(X_matrix) = c('intercept', covariates)
  }
  Y_matrix = as.matrix(subset(dat, select = response))
  X_T = t(X_matrix)
  # store the value of  the (t(X)X)-1 to save time
  pre_useful_matrix = X_T %*% X_matrix
  flag = min(nrow(pre_useful_matrix), ncol(pre_useful_matrix))
  if (rankMatrix(pre_useful_matrix) != flag ){
    warning('t(X) %*% X is singular, stop the process')
    return(c())
  }
  useful_matrix = solve(pre_useful_matrix)
  # calculate betas
  betas = useful_matrix %*% X_T %*% Y_matrix
  # fitting the model
  Hat = X_matrix %*% useful_matrix %*% X_T
  Y_fitted = Hat %*% Y_matrix
  res = Y_matrix - Y_fitted
  res_T = t(res)
  # estimating variance
  MSE = ((res_T %*% res)/(obs - beta_n))[1,1]
  SSE = MSE * (obs - beta_n)
  SSY =  sum((Y_matrix-mean(Y_matrix))^2)
  R_square = 1 - SSE/SSY
  R_adj_square = 1 - MSE/(SSY/(obs-1))
  betas_var = MSE * useful_matrix
  betas_sd = sqrt(diag(betas_var))
  # t-statistic
  t_stats = c(betas/betas_sd)
  p_val =  c(2 * (1 - pt(q = abs(t_stats), df = obs-beta_n)))
  ifsign = ifelse(p_val <= cutoff, ifelse(p_val <= 0.01, ifelse(p_val<=0.001, '***', '**'),'*'), '-')
  results = data.frame(coef = c(betas), std. = betas_sd, t.val = t_stats,
                       p.val = p_val, significant = ifsign)
  out_list =  list(results = results, residuals = res, R.square = R_square,
                   R.square.adj = R_adj_square, SSE = SSE)
  if (model.diag == T){
    # Linearity dignose
    # consider the SLR scenario
    if(beta_n <= 2){
      AXIS_Y = res
      if (intercept){
        AXIS_X = X_matrix[,2]
      }
      else{
        AXIS_X = X_matrix[,1]
      }
      xlab = 'X'
      ylab = 'residuals'
      title = 'Residuals Plot'
      df_pic = data.frame(x = AXIS_X, y = AXIS_Y)
      colnames(df_pic) = c('x', 'y')
      tmp_p = ggplot(df_pic, aes(x = x, y = y)) +
        geom_point(shape = 5) + geom_smooth(method = lm)+
        labs(x=xlab,y=ylab,title=title)+
        theme_bw()+
        theme(
          plot.title = element_text(size = 18,face = "bold", vjust = 0.5, hjust = 0.5),
          axis.title.x = element_text(size = 10,face = "bold"),
          axis.title.y = element_text(size = 10,face = "bold"),
          axis.text.x=element_text(size = 10,face = "bold"),
          axis.text.y=element_text(size = 10,face = "bold"),
        )
      print(tmp_p)

    }
    else{
      model_dignose(Y_matrix, covariates, tmp_dat)
    }
    # equal variance test
    df_pic = data.frame(x = Y_fitted, y = res)
    colnames(df_pic) = c('x', 'y')
    xlab = 'Fitted values'
    ylab = 'residuals'
    title = "Plot for Constant Variance Test"
    tmp_p2 = ggplot(df_pic, aes(x = x, y = y)) +
      geom_point(shape = 5) +
      labs(x=xlab,y=ylab,title=title)+
      theme_bw()+
      theme(
        plot.title = element_text(size = 18,face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(size = 10,face = "bold"),
        axis.title.y = element_text(size = 10,face = "bold"),
        axis.text.x=element_text(size = 10,face = "bold"),
        axis.text.y=element_text(size = 10,face = "bold"),
      )
    print(tmp_p2)
  }
  return(out_list)
}



# model diagnose using partial plots
model_dignose = function(Y, covariates, modeldat){
  n = length(covariates)
  i = 1
  ylab = paste(colnames(Y), 'others', sep = '|')
  title = 'Partial Regression Plot'
  while (i <= n){
    tmp_X = select(modeldat, -c(covariates[i]))
    new_vars  = colnames(tmp_X)
    newdata = cbind(Y, tmp_X)
    AXIS_Y = mylm(newdata, colnames(Y), new_vars, model.diag = F)
    if (length(AXIS_Y) !=0 ){
      AXIS_Y = AXIS_Y[['residuals']]
    }
    else{
      print('matrix is singular in diagnose process')
      break()
    }
    choosen = select(modeldat, c(covariates[i]))
    newdata = cbind(choosen, tmp_X)
    AXIS_X = mylm(newdata, colnames(choosen), new_vars, model.diag = F)
    if (length(AXIS_X) !=0){
      AXIS_X = AXIS_X[['residuals']]
    }
    else{
      print('matrix is singular in diagnose process')
      break()
    }
    df_pic = data.frame(x = AXIS_X, y = AXIS_Y)
    colnames(df_pic) = c('x', 'y')
    xlab = paste(covariates[i], 'others', sep = '|')
    tmp_p = ggplot(df_pic, aes(x = x, y = y)) +
      geom_point(shape = 5) + geom_smooth(method = lm)+
      labs(x=xlab,y=ylab,title=title)+
      theme_bw()+
      theme(
        plot.title = element_text(size = 18,face = "bold", vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(size = 10,face = "bold"),
        axis.title.y = element_text(size = 10,face = "bold"),
        axis.text.x=element_text(size = 10,face = "bold"),
        axis.text.y=element_text(size = 10,face = "bold"),
      )
    print(tmp_p)
    i = i + 1
  }
}



