library(stats)
library(profvis)
## 1
x <- (5)

lprob_nbinomial <- function(p){
  #negative likelihood
  neg_likelihood_NB <- list(-prod(dnbinom(x,size=10,prob = p)),
  #negative log-likelihood
  neg_loglikelihood_NB <- (-sum(log(dnbinom(x,size=10,prob = p))))
                            )
  return(neg_likelihood_NB)
  
 

}


lprob_nbinomial(0.2)


# generating the dnbinom call should be the most computationally demanding

#b

# ephermal environments are meant to last for  only a short amount of time. Thus they
# just are just temporarily generated to serve a function in e.g only a single call or R function.

