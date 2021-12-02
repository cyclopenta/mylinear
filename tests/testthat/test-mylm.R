test_that("mylm works", {
  data(mydata)
  # we include MLR, SLR, no intercept, interaction term, category cases
  m1 = lm(Depression ~  Fatalism + Sex + R_E + Age , data = mydata)
  m2 = lm(Depression ~  -1 + Fatalism + Sex + R_E + Age , data = mydata)
  m3 = lm(Depression ~  Fatalism + Sex + R_E + Age_4Cat + NIHSS_4Cat, data = mydata)
  m4 = lm(Depression ~  Fatalism + Sex + R_E + factor(Age_4Cat) + factor(NIHSS_4Cat), data = mydata)
  m5 = lm(Depression ~  -1 + Fatalism + Sex + R_E + factor(Age_4Cat) , data = mydata)
  m6 = lm(Depression ~ Fatalism, data = mydata)
  m7 = lm(Depression ~ -1 + Fatalism, data = mydata)
  m8 = lm(Depression ~  Fatalism + Sex + R_E + Age + Age*Sex + Age*Fatalism, data = mydata)
  m9 = lm(Depression ~  Fatalism + Sex + R_E + Age + factor(NIHSS_4Cat) + factor(Comorbidity1) +
            + Optimism + HiChol + Age*Sex + Age*Fatalism + HiChol * Fatalism, data = mydata)
  # lm will only cell mean coding the first factor
  m10 = lm(Depression ~  -1 + Fatalism + Sex + R_E + Age + factor(NIHSS_4Cat) + factor(Comorbidity1) +
            + Optimism + HiChol + Age*Sex + Age*Fatalism + HiChol * Fatalism, data = mydata)
  covar1 = c('Fatalism', 'Sex', 'R_E', 'Age')
  covar2 = c('Fatalism', 'Sex', 'R_E', 'Age_4Cat', 'NIHSS_4Cat')
  covar3 = c('Fatalism', 'Sex', 'R_E', 'Age_4Cat')
  covar4 = c('Fatalism')
  # Comorbidity1 could use 0 as ref, HiChol us 0
  covar5 = c('Fatalism', 'Sex', 'R_E', 'Age', 'Optimism','NIHSS_4Cat', 'Comorbidity1','HiChol')
  t1 = mylm(mydata, 'Depression', covar1)
  t2 = mylm(mydata, 'Depression', covar1, intercept = F)
  t3 = mylm(mydata, 'Depression', covar2)
  t4 = mylm(mydata, 'Depression', covar2, category = c('Age_4Cat', 'NIHSS_4Cat'), ref = c(1,1))
  t5 = mylm(mydata, 'Depression', covar3, category = c('Age_4Cat'), cat_method = 'cellmeans', intercept = F)
  t6 = mylm(mydata, 'Depression', covar4)
  t7 = mylm(mydata, 'Depression', covar4, intercept = F)
  t8 = mylm(mydata, 'Depression', covar1, inter = c('Age:Sex','Age:Fatalism'))
  # for catefory with interaction, user need to categorize the variables first
  dat9 = categorize(mydata, c('NIHSS_4Cat', 'Comorbidity1'), ref = c(1,0))
  covar5 = c('NIHSS_4Cat.2', 'NIHSS_4Cat.4', 'NIHSS_4Cat.3', 'Comorbidity1.4', 'Comorbidity1.3', 'Comorbidity1.6', 'Comorbidity1.5',
              'Comorbidity1.2', 'Comorbidity1.1', 'Comorbidity1.7',
             'Fatalism', 'Sex', 'R_E', 'Age', 'Optimism','HiChol')
  covar6 = c('NIHSS_4Cat.1','NIHSS_4Cat.2', 'NIHSS_4Cat.4', 'NIHSS_4Cat.3', 'Comorbidity1.4', 'Comorbidity1.3', 'Comorbidity1.6',
             'Comorbidity1.5','Comorbidity1.2', 'Comorbidity1.1', 'Comorbidity1.7',
             'Fatalism', 'Sex', 'R_E', 'Age', 'Optimism','HiChol')
  dat10 = categorize(mydata, c('NIHSS_4Cat', 'Comorbidity1'), method = 'cellmeans')
  t9 = mylm(dat9, 'Depression', covar5, inter = c('Age:Sex','Age:Fatalism','HiChol:Fatalism'))
  t10 = mylm(dat10, 'Depression', covar6, inter = c('Age:Sex','Age:Fatalism','HiChol:Fatalism'), intercept = F)
  mylm_test = function(my, R_out){
    my_p = nrow(my)
    r_summy = summary(R_out)
    r_SSE = anova(R_out)[['Sum Sq']]
    r_sse_index = length(r_SSE)
    r_SSE = r_SSE[r_sse_index]
    # use sum to aviod different oders after categorizaion
    if (sum(abs(my$results[['coef']]))-sum(abs(R_out$coefficients)) > 1e-6){
      return(0)
    }
    # then check std.
    else if (sum(my$results[['std.']])-sum(r_summy$coefficients[,2]) > 1e-6){
      return(0)
    }
    # then check t-stats
    else if (sum(abs(my$results[['t.val']]))-sum(abs(r_summy$coefficients[,3])) > 1e-6){
      return(0)
    }
    # then check p-val
    else if (sum(my$results[['p.val']])-sum(r_summy$coefficients[,4]) > 1e-6){
      return(0)
    }
    # then check R square
    else if ((my$R.square - r_summy$r.squared > 1e-6) || (my$R.square.adj - r_summy$adj.r.squared > 1e-6)){
      return(0)
    }
    # then check residuals
    else if (any(abs(my[['residuals']]-r_summy$residuals) > 1e-6)){
      return(0)
    }
    # then check SSE
    else if (abs(my[['SSE']]-r_SSE <= 1e-6)){
      return(1)
    }
  }
  expect_equal(mylm_test(t1,m1),1)
  expect_equal(mylm_test(t2,m2),1)
  expect_equal(mylm_test(t3,m3),1)
  expect_equal(mylm_test(t4,m4),1)
  expect_equal(mylm_test(t5,m5),1)
  expect_equal(mylm_test(t6,m6),1)
  expect_equal(mylm_test(t7,m7),1)
  expect_equal(mylm_test(t8,m8),1)
  expect_equal(mylm_test(t9,m9),1)
  expect_equal(mylm_test(t10,m10),1)
})
