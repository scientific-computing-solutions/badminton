
##' Check the times (both patient and calendar) for hazard function switch are valid
##' 
##' A vector is valid if all elements are positive and unique. If the times are valid
##' the time "0" is appended to the vector and the vector is sorted in ascending order  
##' 
##' @param times a numeric vector of times at which hazard functions are to change
##' @return a sorted vector of times 
ValidateSwitchTimes <- function(times){
    if(any(!is.numeric(times)) ||  any(times <= 0) || any(duplicated(times)) || any(is.infinite(times))){
        stop(error="Switching Times must be numeric, unique, positive and finite")
    }  
    return(sort(c(0,times)))
}


##' Creates an object of type \code{Switches}, which contains the times
##' at which the hazard functions change. 
##' 
##' The \code{Switches} object contains the patient times at which the hazard functions change
##' and the calendar times at which the set of "patient time" hazard functions change.
##' 
##' @param times a vector of positive calendar times at which "patient time" hazard functions change
##' If \code{NULL} then the same patient time hazard functions will be used throughout the whole study 
##' @return An object of type \code{Switches} which specifies a single "patient time" hazard function for 
##' each calendar time period.   
##' 
##' Printing the object \code{print(InitializeStudySwitches(times=c(5,10)))} gives further details
##' @examples
##'  switches <- InitializeStudySwitches(c(5,10,20))
##'  print(switches)
##'  switches <- SetSubjectSwitchTimes.Switches(switches,10,c(5,50))
##'  print(switches)
##' @seealso \code{\link{SetSubjectSwitchTimes.Switches}}
##' 
##' @export
InitializeStudySwitches <- function(times=vector("numeric")){
    
    calendarTimes <- ValidateSwitchTimes(times)
    patientTimes <- lapply(calendarTimes,function(x){return(c(0))})
    
    res = list(calendarTimes=calendarTimes,
               patientTimes=patientTimes)
        
    class(res) = c("Switches")
    return(res)
}


##' Attempts to find element in a container
##' 
##' If cannot find element, an exception is thrown and 
##' the valid inputs are displayed
##' 
##' @param toFind The element to find
##' @param findIn The container in which \code{toFind} is to be compared to
##' @param errMsg The error message which should be displayed if the match fails
##' @return The position of \code{toFind} in \code{findIn}  
MatchWithException <- function(toFind,findIn,errMsg){
    index = match(toFind, findIn)
    if(is.na(index)){
      stop(error=paste(errMsg,"Shoud be one of:",paste(findIn,collapse=" ")))
    }
    return(index)
}


##' Set the patient times at which the hazard functions changes 
##'
##' For a given study-time period there is a set of subject times at which
##' the hazard functions changes. This function allows these time to be entered 
##' into a \code{Switches} object. If there is already a set of patient times for 
##' \code{calendarTime} then they will be overwritten
##' 
##' @param object The \code{switches} object to which the new times are added
##' @param calendarTime The calendar time at which these new patient times are used. \code{calendarTime} should
##' be in \code{object$calendarTimes} (which includes calendar time 0 -- the start of the trial).  
##' @param times a vector of positive patient times at which the hazard function changes, the default option
##' \code{times = vector("numeric")} is used when the same hazard functions are used for all patient-time 
##' for this calendar time   
##' @return \code{switches} object with the new patient times added.
##' @examples
##'  switches <- InitializeStudySwitches(c(5,10,20))
##'  print(switches)
##'  switches <- SetSubjectSwitchTimes.Switches(switches,0,c(5,50))
##'  print(switches)
##'  switches <- SetSubjectSwitchTimes.Switches(switches,10,c(5,50))
##'  print(switches)
##' 
##' @export
SetSubjectSwitchTimes.Switches <- function(object,calendarTime,times=vector("numeric")){

    calendarIndex <- MatchWithException(calendarTime,object$calendarTimes,"Calendar Time not found") 
    patientTimes <- ValidateSwitchTimes(times)
    object$patientTimes[[calendarIndex]] = patientTimes
    
    return(object)
    
}


##' This function returns the number of different hazard matrices
##' which are required for this \code{Switch} object
##'    
##' @param object the \code{Switch} object
##' @return the number of hazard matrices required
NumberOfRateMatrices.Switches <- function(object){
    tail(GetCalendarSwitchIndexes.Switches(object),1)
}


##' Calculate cumulative sum of length of vectors in
##' \code{object$patientTimes} list
##' 
##' @param object A \code{switch} object
##' @return A cumulative sum of the length of the vectors in
##' \code{object$patientTimes}
GetCalendarSwitchIndexes.Switches <- function(object){
  cumsum(sapply(object$patientTimes,function(x){length(x)}))  
}


##' @export
print.Switches <- function(x, ...) {
    
   #TODO refactor this
   for(st in 1:length(x$calendarTimes)){
      cat("From calendar time ",x$calendarTimes[st], "until ")  
      endTime <- if(st == length(x$calendarTimes)) Inf else x$calendarTimes[st+1] 
      cat(endTime,":\n")
      
      for(pt in 1:length(x$patientTimes[[st]])){
        cat("    Unchanged hazard functions from patient time ",
             x$patientTimes[[st]][pt]," until time ")
        endTime <- if(pt == length(x$patientTimes[[st]])) Inf else x$patientTimes[[st]][pt+1]
        cat(endTime,"\n")
      }
                
        
    }
  
    invisible(x)
}

##' Returns the calendar times for rate matrices required for Gillespe algorithm
##' 
##' For object \code{Switches}, returns a vector containing the calendar times
##' at which the rate matrices switch. Each calendar time is replicated in the array
##' the numbers of patient time switch occuring for this calendar time.
##' 
##' In the example below, the rate matrices switch at time 50 and Inf. Until time
##' 50 there are two separate patient time rate matrices (one before time 20 one after) and
##' After time 50 there are also two separate patient time rate matrices (one before
##' time 40, one after). Therefore the return value will be c(50,50,Inf,Inf)  
##' 
##' \code{
##' switches <- InitializeStudySwitches(50)
##' switches <- SetSubjectSwitchTimes.Switches(switches,0,20)
##' switches <- SetSubjectSwitchTimes.Switches(switches,50,c(40))
##' GetCalendarEndTime.Switches(switches)}
##' 
##' @param object A \code{Switches} object
##' @return The vector of calendar times
GetCalendarEndTimes.Switches <- function(object){
  endTimes <- c(tail(object$calendarTimes,-1),Inf)
  unlist(Map(function(x,y) rep(x,length(y)),endTimes,object$patientTimes))
}


##' Returns patient times for tate matrices requred for Gillespe algorithm
##' 
##' For object \code{Switches}, returns a vector containing the patient times
##' at which the rate matrices switch. The patient times for different calendar times
##' are appended together to return a single vector
##' 
##' In the example below, the rate matrices switch at time 50 and Inf. Until time
##' 50 there are two separate patient time rate matrices (one before time 20 one after) and
##' After time 50 there are also two separate patient time rate matrices (one before
##' time 40, one after). Therefore the return value will be c(20,Inf,40,Inf)
##' 
##' \code{
##' switches <- InitializeStudySwitches(50)
##' switches <- SetSubjectSwitchTimes.Switches(switches,0,20)
##' switches <- SetSubjectSwitchTimes.Switches(switches,50,c(40))
##' GetCalendarEndTimes.Switches(switches)}
##' 
##' @param object A \code{Switches} object
##' @return The vector of patient times
##'  
GetPatientEndTimes.Switches <- function(object){
  
  unlist(lapply(object$patientTimes,function(x){
      c(tail(x,-1),Inf)
  }
  ))
  
}


##' An indexing function for \code{switch} objects
##' 
##' A switch object consists of a set of calendar times and for each calendar
##' time a set of patient times. The function orders the pairs (calendar time, patient time)
##' in lexicographic order and returns the index for the given pair (\code{calendarStartTime},
##' \code{calendarEndTime}). An exception is thrown if no suich pair exists
##' @param object A \code{Switches} object
##' @param calendarStartTime The calendar switch time for which the index is required 
##' @param patientStartTime  The patient switch time for which the index is required
##' @return The index of the (\code{calendarStartTime}, \code{calendarEndTime}) pair
GetIndex.Switches <- function(object,calendarStartTime,patientStartTime){
    calendarIndex <- MatchWithException(calendarStartTime,object$calendarTimes,"Calendar Time not found") 
    patientIndex <-  MatchWithException(patientStartTime,object$patientTimes[[calendarIndex]],"Patient Time not found") 
  
    index <-c(0,GetCalendarSwitchIndexes.Switches(object))
    return(patientIndex+index[calendarIndex])
}
