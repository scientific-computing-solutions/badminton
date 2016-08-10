##' Define an accrual model for simple power-law accrual
##' @param duration the accrual period
##' @param weight the power law to use
##' @return an object of class \code{recruitmentModel}
##' @export
simpleAccrual <- function(duration, weight) {
    if(!is.numeric(duration) || !is.numeric(weight)){
      stop(error="Arguments must be numeric")
    }
  
    if(duration < 0){
      stop(error="duration must be non negative")
    }
    if(weight <= 0){
      stop(error="weight must be positive")
    }
    res <- list(duration=duration,
                weight=weight,
                sampler=function(n) {
                    if(!is.numeric(n) || n <= 0){
                      stop(error="invalid number of samples")
                    }
                    duration * exp(-rexp(n) / weight)
                },
                display=function(...) {
                    cat("Simple accrual model\n\n")
                    cat("Accrual period: ", duration, "\n",
                        "Weight        : ", weight, "\n",
                        sep='')
                })
    class(res) <- "recruitmentModel"
    res
}

##' @export
print.recruitmentModel <- function(x, ...) {
    x$display(...)
    invisible(x)
}

##' Simulate recruitment times from an accrual model
##' 
##' @param object the \code{recruitmentModel} from which to simulate
##' @param nsim the number of simulations to do
##' @param seed an object specifying if and how the random number generator
##' should be initialized ('seeded').
##' @param ... Argument required for S3 method
##' @return A vector of simulated accrual times, of length \code{nsim}.
##' @export
simulate.recruitmentModel <- function(object, nsim=1, seed=NULL, ...) {
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        runif(1)
    }

    if (is.null(seed)) {
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    } else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }

    times <- object$sampler(nsim)
    attr(times, "seed") <- RNGstate
    class(times) <- c("accrualTime", "numeric")
    times
}

##' @export
print.accrualTime <- function(x, ...) {
    attr(x, "seed") <- NULL
    x <- unclass(x)
    NextMethod()
}
