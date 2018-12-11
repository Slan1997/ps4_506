source("ps4_q2_funcs.R")

# Parameters: -----------------------------------------------------------------
n = 1e3; p = 1e2; r = .1; rho_s = seq(-3,3)*.25  #rho is chose from rho_s
beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)  # let beta become px1 matrix

# X ~ N(0, Sigma): -----------------------------------------------------------
get_result_a = function(rho_input){ #sigma=1,mc_rep=1e4
  Sig = diag(p)
  for (i in 1:p){
    for (j in 1:p){
      if (i != j) Sig[i,j] = rho_input*beta[i]*beta[j]
      if (i == j) Sig[i,j] = 1
    }
  }
  R = chol(Sig)
  X = matrix( rnorm(n*p), n, p) %*%  R
  P = sim_beta(X, beta, sigma = 1, mc_rep = 1e4)
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

#parallel simulations
library(parallel)
RNGkind("L'Ecuyer-CMRG")
set.seed(3)
para = mclapply(X=rho_s,get_result_a,mc.cores = 6)

result_a = rbindlist(para)




