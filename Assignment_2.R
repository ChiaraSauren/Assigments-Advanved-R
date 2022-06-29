library(stats)
## 1

neg_likelihood_NB <- function(p) -prod(dnbinom(x,size=10,prob = p))
neg_loglikelihood_NB <- function(p) -sum(log(dnbinom(x,size=10,prob = p)))


lprob_nbinomial <- function(p){
  likelihood_NB_optim = optim(par = 0.5,
                              fn = neg_likelihood_NB,
                              lower = 1e-8,
                              upper = 1-1e-8,
                              method = 'L-BFGS-B');
  loglikelihood_NB_optim = optim(par = 0.5,
                                 fn=neg_loglikelihood_NB,
                                 lower = 1e-8,
                                 upper = 1-1e-8,
                                 method = 'L-BFGS-B');
 
 
}

lprob_nbinomial(0.2)

likelihood_NB_optim$par


