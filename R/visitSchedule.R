##' @export
NewVisitSchedule <- function(frequencies=Inf, numberVisits=Inf, noise=NULL){
    if(any(is.infinite(head(numberVisits,-1))) || !is.infinite(tail(numberVisits,1))) {
        stop(error="Only the final numberVisits value can (and must) be Inf")
    }
      
    if(any(is.infinite(head(frequencies,-1)))){
        stop(error="Only the final frequencies value can be Inf")
    }
    
    if(length(frequencies)!= length(numberVisits)){
        stop(error="Input vectors of incorrect length")
    }
    
    if(any(frequencies <= 0) || any(numberVisits <= 0)){
        stop(error="All frequencies and numberVisit values must be positive")
    }
    
    retVal <- Map(function(x,y)list(every=x,number=y),frequencies,numberVisits)
    
    class(retVal) = "VisitSchedule"
    return(retVal)
}

