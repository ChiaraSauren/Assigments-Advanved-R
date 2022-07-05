library(stats)
library(profvis)
library(purrr)
## 1
x <- (5)

lprob_nbinomial <- function(p){
  #negative likelihood
  neg_likelihood_NB <- list(-prod(dnbinom(x,size=10,prob = p)),
                            #negative log-likelihood
                            neg_loglikelihood_NB <<- (-sum(log(dnbinom(x,size=10,prob = p))))
  )
  return(neg_likelihood_NB)
  
}


lprob_nbinomial(0.2)


# generating the dnbinom call should be the most computationally demanding

#b )

# ephermal environments are meant to last for  only a short amount of time. Thus they
# just are just temporarily generated to serve a function in e.g only a single call or R function.


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

# f)
# f)
x<-5
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

my_nbin_mle<-nbin_mle(x)
summary(my_nbin_mle)


## g)
print.summary.my_mle <- function(x) {
  if(class(x) == "summary.my_mle") {
    cat(
      "Schöne Zusammenfassung für", x$name, ":\n\n",
      x$prob, "Wahrscheinlichkeiten für MLE", x$min, "to", x$max
    )
  } else {
    message("Object not of class summary.my_mle!")
  }
  
}

summary(my_nbin_mle)


###ss