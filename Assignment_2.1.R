library(profvis)
library(purrr)
## 1

lprob_nbinomial<-function(p, x){
  (-sum(log(choose(x+10-1, x))+10*log(p)+x*log(1-p)))
}

lprob_nbinomial(0.2, 2)


#generate some data to test and profile the function:
x <- rnbinom( n = 1e5, size = 5, prob = .2)
x <- rnbinom( n = 1e6, size = 5, prob = .2)
length(x)


# profile collecting of example data and parts of the function in comparison
profvis({
  
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
  function(p){
    (-sum(log(choose(x+10-1, x))+10*log(p)+x*log(1-p)))
  }
}


ll<-ll_nbinomial(2)
ll(0.2)


#d) 
set.seed(123)
x1<-rnbinom(n=1e3, size = 10, prob = .3)
ll<-ll_nbinomial(x1)
ll(0.2)


#e)
nbin_mle<-function(x){
mle <-  list(optim(par = 0,fn=ll ,lower = 1e-8,
        upper = 1-1e-8))
return(mle)
}

nbin_mle(x1)

a<-optimize(ll, interval = c(0,1))
a
## selben werte


# f)
nbin_mle<-function(x){
  mle <-  list(optim(par = 0,fn=ll ,lower = 1e-8,
                     upper = 1-1e-8, method = "L-BFGS-B"))
  class(mle)<-"my_mle"
  return(mle)
}


nbin_mle(x1)

nbin_mle(x1)[[1]]$par

summary.my_mle <- function(x) {
  if(class(x) == "my_mle") {
    x <- unlist(x)
    out <<- list(
      name = quote(x),
      min = min(x),
      tibble(
        prob=seq(0,1,0.01)),
      ll=my_nbin_mle[[1]]$par,
      MLE=my_nbin_mle[[1]]$value,
      counts=my_nbin_mle[[1]]$counts
    )
    class(out) <- "summary.my_mle"
    return(out)
  } else {
    message("Object not of class my_mle!")
  }
}

my_nbin_mle<-nbin_mle(x1)
summary(my_nbin_mle)


## g)
print.summary.my_mle <- function(x) {
  if(class(x) == "summary.my_mle") {
    cat(
      "Schöne Zusammenfassung für", x$name, ":\n\n",
      "Wahrscheinlichkeiten für MLE", my_nbin_mle[[1]]$value, "ist", my_nbin_mle[[1]]$par
    )
  } else {
    message("Object not of class summary.my_mle!")
  }
  
}

summary(my_nbin_mle)

nbin_mle(x1)[[1]]$par

##### plot
plot.summary.my_mle <- function(x) {
 pi<-(1:99)/100
 loglikelihood<-sapply(pi, ll)
 plot(pi, loglikelihood, type="l",
      xlab="Pi", ylab = ".log L(pi; x, k=10)")
 points(y=my_nbin_mle[[1]]$value,x=my_nbin_mle[[1]]$par,col="red", type = "p", lwd=2)
}

plot.summary.my_mle(x1)
##


##
##h)
my_nbin_mle<-nbin_mle(x1)
summary(my_nbin_mle)
plot.summary.my_mle(x1)

