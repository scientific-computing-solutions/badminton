##' Generate an object of type \code{EventSim}
##' 
##' An object of \code{EventSim} contains all the data required to run a
##' time to event simulation
##' 
##' @param progressionGraph An object of type \code{ProgressionGraph} describing the underlying transition DAG
##' @param switches An object of type \code{Switches} describing when transition rates change
##' @param recruitmentModel An object of type \code{recruitmentModel} used to sample subject recruitment
##' @return An object of type \code{EventSim}   
##' @export
InitializeEventSim <- function(progressionGraph,switches,recruitmentModel){
    
    if(IsEmpty.ProgressionGraph(progressionGraph)){
      stop(error="There must be at least one state!")
    }
        
    
    numberRateMatrices <- NumberOfRateMatrices.Switches(switches)
   
    res <- list(progressionGraph=rep(progressionGraph,numberRateMatrices),
                numberRateMatrices=numberRateMatrices,
                switches=switches,
                recruitmentModel=recruitmentModel,
                params=list())
    class(res) <- "EventSim"
    return(res)
}

##' Insert a transition rate into \code{EventSim} object
##' 
##' Add a numeric value or function as the transition rate for an edge of the DAG for a specific
##' patient and calendar time. If a rate already exists for this edge, patient and
##' calendar time then the previous value is overwritten. See the Vignette for further details.
##' 
##' @param object The \code{EventSim} object this rate is to be added into.
##' @param fromNode The rate being added is for subjects leaving this node (i.e. state) entering \code{toNode}. 
##' The node should be a string matching a node name in \code{object$progressionGraph} 
##' @param toNode The rate being added is for subjects arriving into this node from \code{toNode}. 
##' The node should be a string matching a node name in \code{object$progressionGraph} 
##' @param calendarStartTime The calendar start time for this rate to be used. The rate will only be used from this
##' time until the next calendar time switch. If NULL this rate is used for
##' all calendar start times. Note calendar start time must match a calendar time in \code{object$Switches}
##' @param patientStartTime The patient start time for this rate to be used 
##' @param rate Transition rate. Either a (non-negative) value or a function. See Vignette for further details 
##' @return \code{object} with the appropriate rates added   
##' @export
InsertRate.EventSim <- function(object,fromNode,toNode,calendarStartTime=NULL,patientStartTime=NULL,rate){
    
    switchIndex <- vector()
    
    if(is.null(calendarStartTime) && is.null(patientStartTime)){
        switchIndex <- 1:object$numberRateMatrices
    }
    else if(is.null(calendarStartTime) && !is.null(patientStartTime)){
        stop(error="Both calendar and patient StartTime must be null if either is")    
    }
    else if(!is.null(calendarStartTime) && is.null(patientStartTime)){
        stop(error="Both calendar and patient StartTime must be null if either is")   
    }
    else{
        switchIndex <- GetIndex.Switches(object$switches,calendarStartTime,patientStartTime)  
    }
    
    for(x in switchIndex)
      object$progressionGraph[x] <- InsertRate.ProgressionGraph(object$progressionGraph[x],fromNode,toNode,rate)  
          
    return(object)
}

##' Insert more parameter names into an \code{EventSim} object
##' 
##' \code{object$params} contains a list of parameters which can be used
##' in functions describing rates for this object. For example if
##' \code{object$params = c("p","x")} then the function \code{myfunc1 <- function(pars){pars["p"]*pars["x"]}}
##' can be inserted into \code{object} using the function \code{\link{InsertRate.EventSim}} 
##' 
##' This function appends a new vector of parameter names to the existing  \code{object$params} function
##' returning a new \code{EventSim} with this addition
##' 
##' @param object An \code{EventSim} object
##' @param name  A vector of parameter names to be added into \code{object}  
##' @return \code{object} with the addition parameters found in  \code{name}  
##' @export
InsertParameter.EventSim <- function(object,name){
    
    t <-  c(object$params,name)
    
    if(any(duplicated(t))){
        stop(error="Note all parameter names must be different")
    }
    
    object$params <- t
    return(object)
}


##' Returns a vector with names \code{object$params}.
##' 
##' Given a vector of values return the vector with names matching those
##' found in \code{object$params}. An exception is thrown if the length of the vector of values
##' and vector of names are not identical
##' 
##' @param object Object of type \code{EventSim} from which the parameter names will be taken
##' @param values The vector to be named
##' @return A vector containing values with names from \code{object$params} 
##' 
##' @export
GetParameters.EventSim <- function(object,values){
    if(length(values) != length(object$params)){
        stop(error="values should be of size",length(object$params))
    }
    
  
    names(values) <- object$params
    return(values)
}

##' Evaluate transition rates parameterized by functions
##' 
##' The rates for transitions can be entered as functions (\code{\link{InsertRate.EventSim}}) 
##' \code{EvaluateFormula.EventSim} takes a set of numeric values to be used to evaluate these functions and returns an
##' \code{EventSim} object with the functions replaced by their evaluated values  
##' 
##' @param object An object of type \code{EventSim}
##' @param params A named vector of parameter values with names which match those found in
##' \code{object$params}. The function \code{\link{GetParameters.EventSim}} will generate such a vector
##' 
##' @return \code{object} with the rates which were parameterized by functions replaced with their evaluated values 
##'    
##' @export
EvaluateFormula.EventSim <- function(object, params){
    #validation on parameters occurs inside EvaluateFormula.ProgressionGraph
  
    object$progressionGraph <- unlist(lapply(1:object$numberRateMatrices, function(x){
        EvaluateFormula.ProgressionGraph(object$progressionGraph[x],params)}              
    ) )
  
    object$paramValues <- params
      
    return(object)
   
}


##' Generate a list of patient start states
##' 
##' For example:
##' \code{nodeNames <- c("control.progressing","control.progressed","death")
##' startCounts <- c("control.progressing",5,"death",2)
##' GenerateStartStateVector(nodeNames,startCounts)}  
##'       
##' @param nodeNames A list of nodeNames 
##' @param startCounts A list of startCounts of the form c("nodeName",numberStartingatNode,...)
##' @return A numeric vector containing the starting states of subjects. The example returns
##' \code{c(1,1,1,1,1,3,3)}
GenerateStartStateVector <- function(nodeNames, startCounts){
    if(length(startCounts)%% 2 == 1){
      stop(error="Error initializing subject start nodes")
    }  
  
  
    retVal <- lapply(seq(2,length(startCounts),2), function(x){
        nodeIndex <- match(startCounts[x-1],nodeNames)
            
        count <- as.numeric(startCounts[x])
    
        if(round(count) != count){
          stop(error="Error initializing subject start nodes")
        }
        
        if(count < 0 || nodeIndex < 0){
            stop(error="Error initializing subject start nodes")
        }
        
        return(rep(nodeIndex,count))
        
      }
    )
          
    return(unlist(retVal))
    
}

##' Generate a list of hazard rates for use in Simulation
##' 
##' For each \code{progressionGraph} a rate Matrix is calculated 
##' using \code{\link{GeneratRateMatrix.ProgressionGraph}} and is of the form
##' The matrix that is produced is such that M[i,j] = the rate
##' from node i to node j. 
##' 
##' Each matrix is transposed, unlisted and appended together
##' so that in the example there are N states, the first N rates in the list
##' are the transition rates out of state 1 at calendarTime = 0, patientTime = 0. 
##' The next N are the transition rates out of state 2 at calendarTime = 0, patientTime = 0
##' etc.          
##' 
##' @param object An \code{EventSim} object from which to calculate the rates
##' @return A list of rates. The rates are ordered as described
##'  
GenerateRates.EventSim <- function(object){
  
    retVal <- lapply(1:object$numberRateMatrices, function(x){
      GeneratRateMatrix.ProgressionGraph(object$progressionGraph[x])}              
    )
    
    return(unlist(lapply(retVal,t),use.names=FALSE))
        
}

##'  Perform a time to event simulation 
##'  
##'  A time to event simulation is run for subjects specified by \code{startCounts}
##'  Each subject is simulated until enough time has passed so that the rate matrices 
##'  will no longer switch and the subject cannot transition into another state
##'  
##'  All rates which have been defined as formulae must be evaluated before calling this function
##'  See \code{\link{EvaluateFormula.EventSim}}. If this is not the case then an error message
##'  will be displayed
##'  
##'  @param object An \code{EventSim} object
##'  @param startCounts A vector containing nodeNames and number of subjects starting in this node
##'  For example \code{c("control.progressing",5,"active.progressing",5)}. If a node is not included
##'  in this list then it is assumed that no subjects start at this location   
##'  @param seed Random seed (for recruitment)
##'  @param duration The patient time at which the simulation should end. 
##'  @return A list containing a data frame ("data") containing  columns id, state,	patient_transition_time which 
##'  describe which patient transitions into which state at which patient_time and a vector or recruitment times
##'  (recruitmentTimes)  
##'  @export
Simulate.EventSim <- function(object, startCounts,seed=NULL,duration=Inf){
    
    nodeNames <- nodes(object$progressionGraph[1]$graph)
    startNodes <- GenerateStartStateVector(nodeNames,startCounts)  
    totalSubjects <- length(startNodes)
     
    if(totalSubjects == 0){
      stop(error="No Subjects!")
    }
    
    if(duration <= 0 || !is.numeric(duration) ){
      stop(error="duration must be a positive number or Inf")
    }
    
    if(!is.DAG(object$progressionGraph[1]$graph) && is.infinite(duration)){
      stop(error="A ProgressionGraph with cycles must have a finite duration")
    }
    
    subjectRecruitmentTimes <- simulate.recruitmentModel(object$recruitmentModel,nsim=totalSubjects,seed)
  
    rateMatrices <- GenerateRates.EventSim(object)
    
    if(all(rateMatrices == 0)){
      stop(error="All Transitions are zero!")
    }
    
    patientEndTimes <- GetPatientEndTimes.Switches(object$switches) 
    calendarEndTimes <- GetCalendarEndTimes.Switches(object$switches) 
    
    shape <- GetShapes.ProgressionGraph(object$progressionGraph[1])
        
    #print(shape)
        
    resetEdges <- GetResetEdges.ProgressionGraph(object$progressionGraph[1])
     
    retVal <- gillespie(startNodes,subjectRecruitmentTimes,length(nodeNames),rateMatrices,
                        patientEndTimes,calendarEndTimes,shape,resetEdges,duration)
    
    #convert states from integers to their names 
    retVal$state <-  mapvalues(as.factor(retVal$state), from = 1:length(nodeNames), to = nodeNames,warn_missing = FALSE )
    
    return(list(data=retVal,
                 recruitmentTimes=subjectRecruitmentTimes))
}


##' Create a function for parameterizing cross over probabilities
##' 
##' See Vignette tutorials 4 and 6 for further details 
##' @param parameterName Parameter name for the cross over probability
##' @param rate rate parameter for hazard function
##' @param shape Weibull shape parameter for hazard function
##' @return A function to be used as the rate argument to the InsertRates.EventSim fnction
##' @export
crossOver_functional <- function(parameterName,rate,shape=1){
  #When calculating the hazard functions the package takes the rate and applies the following
  #formula rate=r -> hazard function = shape*(r^shape)*[time^(shape-1)]. If you want the hazard function to be
  #p*shape*(rate^shape)*[time^(shape-1)] then rate must be replaced by rate*p^(1/shape)
  #so that the desired hazard function is used 
  f <- function(pars){
    ans <- pars[parameterName]^(1/shape)*rate
    names(ans) <- NULL
    return(ans) 
  }
  return(f)
}

##' Create a function for parameterizing cross over probabilities
##' 
##' See Vignette tutorials 4 and 6 for further details 
##' @param parameterName Parameter name for the cross over probability
##' @param rate rate parameter for hazard function
##' @param shape Weibull shape parameter for hazard function
##' @return A function to be used as the rate argument to the InsertRates.EventSim fnction
##' @export
non_crossOver_functional <- function(parameterName,rate,shape=1){
  f <- function(pars){
    ans <-(1-pars[parameterName])^(1/shape)*rate 
    names(ans) <- NULL
    return(ans)
  }
  return(f)
}

