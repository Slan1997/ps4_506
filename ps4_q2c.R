source("ps4_q2_funcs.R")
.libPaths('/afs/umich.edu/user/l/a/lanshi/R/x86_64-redhat-linux-gnu-library/3.5')
library(doParallel)
library(data.table)
library(magrittr)
library(future)

args = commandArgs(trailingOnly=TRUE)
sigma_arg = as.numeric(args[2])
mc_rep_arg = as.numeric(args[3])
n_cores_arg = as.numeric(args[4])
print(paste("sigma=",sigma_arg))
print(paste("mc_rep=",mc_rep_arg))
print(paste("n_cores=",n_cores_arg))

# Parameters: -----------------------------------------------------------------
n = 1e3; p = 1e2; r = .1; 
rho_s = seq(-3,3)*.25  #rho is chose from rho_s

beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)  # let beta become px1 matrix

# X ~ N(0, Sigma): -----------------------------------------------------------
get_result_c = function(rho_input=rho_s,sigma_input,mc_rep_input){ 
  Sig = diag(p)
  for (i in 1:p){
    for (j in 1:p){
      if (i != j) Sig[i,j] = rho_input*beta[i]*beta[j]
      if (i == j) Sig[i,j] = 1
    }
  }
  R = chol(Sig)
  X = matrix( rnorm(n*p), n, p) %*%  R
  P = sim_beta(X, beta, sigma=sigma_input, mc_rep = mc_rep_input)
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


## do parallel simutation with future
plan(cluster,workers=n_cores_arg)
f <- list()
for (i in 1:length(rho_s)){
    f[[i]] <- future({
      get_result_c(rho_s[i],sigma_arg,mc_rep_arg)
  })
}
result_c <- rbindlist(lapply(f, FUN = value))

result_c
