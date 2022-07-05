library(stats)
library(profvis)
library(purrr)
## 1


# function to calculate log-likelihood function with k = 10

lprob_nbinomial <- function(pi, x){
  n <- length(x)
  (-sum((log(choose(x+10-1, x)))+ 10 * log(pi)+x * log(1-pi)))
}
lprob_nbinomial(0.5, x)                          


#generate some data to test the function:
x <- rnbinom( n = 1e5, size = 5, prob = .2)
x <- rnbinom( n = 1e6, size = 5, prob = .2)
length(x)

lprob_nbinomial(0.5)  # test function

# profile collecting of example data and parts of the function in comparison
profvis({

    n <- length(x)
  test <-  (-sum((log(choose(x+10-1, x)))+ 10 * log(0.5)+x * log(1-0.5)))
  
})

# with increasing length of the example data x, the computation time increases
#accordingly. With n = 100000 (1e5) observations the lprob_nbinomial function takes up
# to 10 ms, using up 2.3 mbit of memory space. With n = 1000000 (1e6) the computation 
# time already takes 110 ms and takes up to 22.9 mbits of memory space.



#b )

# ephemeral environments are meant to serve as a testing, temporary or dynamic environment
# that show a replica of the actual work-environment. They usually use replicated data and
# test whether a test or step is applicable before risking on damaging the real work environmen.
# They help in accelerating the programming or software development cycle, limiting the need to
# rework broad parts of the written code.
# In comparison to a ephemeral environment, the function factory 
# encloses the environment of the manufactured function. This serves as the execution
# environment of the function factory.




#c)
ll_nbinomial<-function(p){
  function(x){
    neg_loglikelihood_NB <- (-sum(log(dnbinom(x,size=10,prob = p))))
  return(neg_loglikelihood_NB)
  }
}


a<-ll_nbinomial(0.2)
a(5)
a(10)
# Kann beliebiger Wert für x eingesetzt werden

#d) 
set.seed(123)
x1<-rnbinom(n=1e3, size = 10, prob = .3)
a(x1)


#e)
nbin_mle<-function(x){
  mle<-list(optim(par=0, fn=a))
  return(mle)
}

nbin_mle(x1)


# mit optimize (komische Ergebnisse)
b<-function(x){
  mle<-optimize(a, interval = c(0,1000))
  return(mle)
}

b(x1)

## alternativer Versuch
x<-5
neg_loglikelihood_NB <- function(p){
  (-sum(log(dnbinom(x,size=10, prob = p))))
}

nbin_mle<-function(x){
mle <-  list(optim(par = 0.5,fn=neg_loglikelihood_NB ,lower = 1e-8,
        upper = 1-1e-8,method = 'L-BFGS-B'))
return(mle)
}


nbin_mle(x1)

## Das Problem am MLE optimieren ist, dass in der funktion lprob_nbinomial
#2 Funktionen optimiert werden sollen, also likelihood und loglikelihood, 
# in der Aufgabe steht nur man soll log-likelihood mit MLE schätzen. Wenn ich es
# nur mit der loglikelihood funktion mache, scheint optim() zu funktionieren
#$par müsste dann der MLE Schätzer sein