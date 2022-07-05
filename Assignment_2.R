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


#generate some data to test and profile the function:
x <- rnbinom( n = 1e5, size = 5, prob = .2)
x <- rnbinom( n = 1e6, size = 5, prob = .2)
length(x)


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
ll_nbinomial<-function(x){
  length(x)
  c1 <- log(choose(x+10-1, x))
  c2 <- x

function(pi){
(-sum((c1) + 10 * log(pi)+c2 * log(1-pi)))
  
}
  }



ll<-ll_nbinomial(x)
ll


#d) 
set.seed(123)
x1<-rnbinom(n=1e3, size = 10, prob = .3)
x <- x1

ll(0.1) # pi = 0.1
ll(0.5)  # pi = 0.5





#e)
# mit optimize (komische Ergebnisse)
b<-function(x){
  mle<-optimize(ll, interval = c(0,10), maximum=T)
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


# f)
nbin_mle<-function(x){
  mle <-  list(optim(par = 0.5,fn=neg_loglikelihood_NB ,lower = 1e-8,
                     upper = 1-1e-8,method = 'L-BFGS-B'))
  class(mle)<-"my_mle"
  return(mle)
}


nbin_mle(x1)

summary.my_mle <- function(x) {
  if(class(x) == "my_mle") {
    x <- unlist(x)
    out <- list(
      name = quote(x),
      n = length(x),
      min = min(x),
      max = max(x),
      tibble(
        prob=seq(0,1,0.1)
        
      )
    )
    class(out) <- "summary.my_mle"
    return(out)
  } else {
    message("Object not of class my_mle!")
  }
}

my_nbin_mle<-nbin_mle(x1)
summary(my_nbin_mle)

