source("ps4_q2_funcs.R")
install.packages("doParallel",lib='~/newlib')
library(doParallel)
install.packages("data.table",lib='~/newlib')
library(data.table)

library()
# Parameters: -----------------------------------------------------------------
n = 1e3; p = 1e2; r = .1; 
rho_s = seq(-3,3)*.25  #rho is chose from rho_s
sigma_s = c(0.25,0.5,1)

beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)  # let beta become px1 matrix

# X ~ N(0, Sigma): -----------------------------------------------------------
get_result_b = function(rho_input,sigma_input){ #sigma=1,mc_rep=1e4
  Sig = diag(p)
  for (i in 1:p){
    for (j in 1:p){
      if (i != j) Sig[i,j] = rho_input*beta[i]*beta[j]
      if (i == j) Sig[i,j] = 1
    }
  }
  R = chol(Sig)
  X = matrix( rnorm(n*p), n, p) %*%  R
  P = sim_beta(X, beta, sigma=sigma_input, mc_rep = 1e4)
  all0 =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })
  all = rbindlist(all0)
  dt = all[ , method := c('holm', 'bonferroni', 'BH', 'BY') ]  %>% 
    .[,c("rho","sigma") := list(rep(rho_input,4),rep(1,4))]  #sigma=1
  
  dt_est = dt[,.(rho,sigma,fwer,fdr,sens,spec,method)] %>% 
    melt(.,id.vars=c('rho','sigma','method'), measure.vars = c('fwer', 'fdr', 'sens', 'spec'),
         variable.name = "metric",value.name = "est" )
  
  dt_se = dt[,.(rho,sigma,fwer_se,fdr_se,sens_se,spec_se,method)] %>% 
    melt(.,id.vars=c('rho','sigma','method'), measure.vars = c('fwer_se', 'fdr_se', 'sens_se', 'spec_se'),
         variable.name = "metric",value.name = "se" ) %>% .[,.(se)]
  
  result = cbind(dt_est,dt_se)
  result = result[,.(rho,sigma,metric,method,est,se)]
  return(result)
}

#doparallel simulations
ncores = 4
cl = makeCluster(ncores)
# register the cluster
registerDoParallel(cl)

## do parallel simutation with foreach
# nested foreach loops 
result_b = foreach(i=1:length(rho_s), .combine='rbind') %:% 
  foreach(j=1:length(sigma_s), .combine='rbind') %do% {
    get_result_b(rho_s[i],sigma_s[j])
  }
result_b

## Always shut the cluster down when done
stopCluster(cl)


