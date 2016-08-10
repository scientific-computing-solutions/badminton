##' Creates an \code{EventSet} object
##' 
##' \code{EventSet} objects are used to store the set of events
##' of interest for post-processing the simulator output
##' @return \code{EventSet} object
##' @export
InitializeEventSet <- function(){
    retVal <- list(events=list())
    class(retVal) = "EventSet"
    return(retVal)
}

##' Insert \code{Event} into \code{EventSet} object
##' 
##' Add an event of interest into \code{EventSet}.
##' For example \code{myEvents <- InsertEvent.EventSet(myEvents,NewEvent(name="initial_state",type="startState"))}
##' See \code{\link{NewEvent}} and \code{\link{ProcessEventSimOutput}} for further details
##' 
##' @param object \code{EventSim} object
##' @param event \code{Event} object
##' @return \code{object} with \code{event} added
##' 
##' @export
InsertEvent.EventSet <- function(object,event){
    
    if(class(event) != "Event"){
        stop(error="event must be of class Event, see NewEvent function")
    }
    
    eventNames <- sapply(object$event,function(x){x$name})
    
    if(event$name %in% eventNames){
        stop(error="Event with same name already exists")
    }
    
    eventTypes <- sapply(object$event,function(x){x$type})
    if(event$type %in% c("startState","endState","recruitTime") && event$type %in% eventTypes){
        stop(error=paste("Only one event of type",event$type,"per event set"))
    }
      
    object$events[[length(object$events)+1]] <- event 
    
    return(object)  
}

##' Sanity Check an \code{EventSet} with respect to a \code{ProgressionGraph}
##' 
##' @param object An \code{EventSet} object
##' @param progressionGraph A \code{progressionGraph} object
##' @return True if all nodes and edges described by events in \code{object}
##' are found in the \code{ProgressionGraph} DAG. If this function returns True then
##' \code{ProcessEventSimOutput} can be called with an output from a simulation
##' which \code{ProgressionGraph} is the underlying DAG and the EventSet is \code{object}.
##' 
##' There may be occasions where it still makes sense for \code{ProcessEventSimOutput} to
##' be called even if this function returns FALSE 
##' @export
ValidateAgainstProgressionGraph.EventSet <- function(object,progressionGraph){
    if(class(progressionGraph)!= "ProgressionGraph" || class(object) != "EventSet"){
        stop(error="Expect EventSet and ProgressionGraph objects")
    }
    
    retVal <- sapply(object$events,function(x){
      #if any nodes in event$nodeNames are not in graph then return false
      if(!is.null(x$nodeNames) && any(!(x$nodeNames %in% nodes(progressionGraph$graph)) )) 
          return(FALSE)
      #if any edges in event$edges are not in graph then return false
      if(!is.null(x$edges) && !(IsEdge.ProgressionGraph(progressionGraph,x$edges[c(TRUE,FALSE)],x$edges[c(FALSE,TRUE)]))) 
          return(FALSE)
      return(TRUE)
    })
  
    return(all(retVal))
}


##' The main function for producing processed output for the package
##' 
##' This function takes output from \code{Simulate.EventSim} e.g. raw transition
##' times from the event simulator and a set of events e.g. "first time transitioning into 
##' a state from a given set" and produces a data frame summarizing the events for each 
##' subject. 
##' 
##' See \code{\link{NewEvent}} for description of the different
##' events and the output that they produce and for further details
##' see the examples in the vignette.
##' 
##' @param eventSimOutput The output from \code{Simulate.EventSim}
##' @param eventSet An \code{EventSet} object which contains the events to be
##' processed for each patient 
##' @return A data frame, one row per patient with an initial column "id" and
##' each subsequent column the output for an \code{Event} found in \code{EventSet}
##' @export
ProcessEventSimOutput <- function(eventSimOutput,eventSet){
    if(length(eventSet) == 0){
        stop(error="No events!")
    }
  
    ids <- unique(eventSimOutput$data$id)
    
    retVal <- lapply(seq_along(ids),
                     function(x){
                           rec <- eventSimOutput$recruitmentTimes[x]
                           data <- subset(eventSimOutput$data,eventSimOutput$data$id==ids[x])
                           ProcessOneSubject(data,rec,eventSet)
    })
        
    
    retVal <- as.data.frame(rbindlist(retVal,use.names=FALSE))
    retVal$id <- ids
    
    colnames(retVal) <- c(sapply(eventSet$events,function(x){x$name}),"id")
    return(retVal[c("id",sapply(eventSet$events,function(x){x$name} ))])
      
  
}

##' Process the events for a single patient
##' 
##' This function performs \code{\link{ProcessEventSimOutput}} for a
##' single patient
##' 
##' @param data The output from a event simulation for a single patient Id
##' @param recruitTime The time of recruitment for this patient Id
##' @param eventSet \code{EventSet} object describing the events to be processed 
##' @return A list containing the data for a single row
##' of the data frame returned by \code{\link{ProcessEventSimOutput}} 
ProcessOneSubject <- function(data,recruitTime,eventSet){
  retVal <- lapply(eventSet$events,function(x){
        ProcessOneEvent(data,recruitTime,x)
  }) 
  return(retVal)
}


##' Process a sngle event for a single patient
##' 
##' This function performs \code{\link{ProcessEventSimOutput}} for a
##' single patient and single event
##' 
##' @param data The output from a event simulation for a single patient Id
##' @param recruitTime The time of recruitment for this patient Id
##' @param event \code{Event} object describing the event to be processed 
##' @return A value to be inserted into the appropriate cell
##' of the data frame returned by \code{\link{ProcessEventSimOutput}} 
ProcessOneEvent <- function(data,recruitTime,event){
  if(event$type=="recruitTime"){
    return(recruitTime)
  }
  
  ans <- NULL
  
  if(event$type=="startState"){
    return( StartState(data)$state)
  }
  if(event$type=="endState"){
    return( EndState(data)$state)
  }
  
  
  if(event$type=="timeToHit" || event$type=="hitsBefore"){
    ans <- TimeToHit(data,states=event$nodeNames)
  }
  if(event$type=="timeToHit_Edge" || event$type=="hitsBefore_Edge"){
    ans <- TimeToHitEdge(data,edges=event$edges)
  }    
    
  
  if(event$calendarTime==TRUE){
    ans <- ans + recruitTime 
  }
  
  if(event$type == "hitsBefore" || event$type == "hitsBefore_Edge"){
    #strict inequality so that if time=Inf get TRUE iff event occurs
    ans <- ifelse(ans < event$time,TRUE,FALSE) 
  }
  return(ans)  
  
  
}

##' Create an event which is used for processing the raw transition times
##' 
##' @param name parameter will be used as column name for this event
##' when calling \code{\link{ProcessEventSimOutput}} 
##' 
##' @param type There are a set of possible event types
##' There are a set of different events, each described with example code
##' \code{"startState"}: \code{NewEvent(name="colName",type="startState")}
##' will output the starting state for each patient 
##' \code{"endState"}: \code{NewEvent(name="colName",type="endState")}
##' will output the final state each patient transitioned into 
##' \code{"recruitTime"}: \code{NewEvent(name="colName",type="recruitTime")}
##' will output the (calendar) time at which the patient was recruited onto the trial
##' \code{"timeToHit"}: \code{NewEvent(name="colName",type="timeToHit",nodeNames=c("n1","n2"))}
##' will output the time at which patients first transition into any node in nodeNames argument.
##' If the none of the nodes are transitioned into then Inf is output
##' \code{"hitsBefore"}: \code{NewEvent(name="colName",type="hitsBefore",nodeNames=c("n1","n2"),time=5)}
##' will output whether the patients first transition into any node in nodeNames argument before the given time
##' \code{"timeToHit_Edge"}: \code{NewEvent(name="colName",type="timeToHit_Edge",edges=c("from_1","to_1","from_2","to_2"))}
##' will output the time at which patients first transition across any of the given edges. In this example 
##' the edges {from_1,to_1} and {from_2,to_2}. If the none of the edges are traversed then Inf is output 
##' \code{"hitsBefore_Edge"}: \code{NewEvent(name="colName",type="hitsBefore_Edge",edges=c("from_1","to_1","from_2","to_2"),time=Inf)}
##' will output whether any of the edges have been traversed before the given Time
##' @param nodeNames The node list for \code{timeToHit} and \code{hitsBefore} event types
##' @param time The time before which \code{hitsBefore} or \code{hitsBefore_Edge} events must occur
##' for "TRUE" to be output. This is a strict inequality so using the time Inf will display TRUE iff 
##' the event occurs at any time. If \code{calendarTime} is TRUE then this time represents calendar time
##' otherwise it represents patient time. 
##' @param edges The edge list for \code{timeToHit_Edge} and \code{hitsBefore_Event} event types. It is of the form
##' \code{c("from_1","to_1","from_2","to_2")}
##' @param calendarTime If true then \code{time} and the associated output for this event are
##' calendar times, if false they are patient times  
##'  
##' @return An \code{Event} object 
##' @export
NewEvent <- function(name,type,nodeNames=NULL,time=NULL,edges=NULL,calendarTime=FALSE){
    eventTypes = c("startState","endState","recruitTime")
    eventTypes = c(eventTypes,"timeToHit","hitsBefore")
    eventTypes = c(eventTypes,"timeToHit_Edge","hitsBefore_Edge")
    
    if(!(type %in% eventTypes) ){
        stop(error="event type unknown")
    }
    
    if(name==""){
        stop(error="invalid name")
    }
  
    ValidateNullArgs(type,nodeNames,time,edges)
    
    if(!is.null(edges) && length(edges) %% 2 == 1){
        stop(error="edges must be vector of even length")
    }
    if(!is.null(time) && (!is.numeric(time) || length(time)!= 1 || time < 0)){
        stop(error="time must be numeric and non-negative")
    }
    if(!is.logical(calendarTime)){
        stop(error="calendarTime should be logical")
    }
     
    retVal <- list(name=name,
                 type=type,
                 nodeNames=nodeNames,
                 time=time,
                 edges=edges,
                 calendarTime=calendarTime)
    class(retVal) = "Event"
    return(retVal)
    
}

##' Validate which arguments are null for different \code{NewEvent} event types
##' 
##' See \code{\link{NewEvent}} for further details  
##' @param type If type \code{timeToHit}, only \code{nodeNames} is not null. If
##' type \code{hitsBefore} then both \code{nodeNames} and \code{time} are not null.
##' If type \code{timeToHit_Edge} then only \code{edges} is not null. If type \code{hitsBefore_Edge}
##' then both \code{edges} and \code{time} are not null. For all other types all arguments should be NULL
##' @param nodeNames nodeNames argument
##' @param time time argument
##' @param edges edges argument
##' @return Whether the expected arguments are null  
ValidateNullArgs <- function(type,nodeNames,time,edges){
    nullvals <- c(TRUE,TRUE,TRUE)
    if(type == "timeToHit" || type == "hitsBefore"){
        nullvals[1] <- FALSE
    }  
    if(type == "hitsBefore" || type == "hitsBefore_Edge"){
        nullvals[2] <- FALSE
    }
    if(type == "timeToHit_Edge" || type == "hitsBefore_Edge"){
        nullvals[3] <- FALSE
    }
    if(is.null(nodeNames) != nullvals[1] || is.null(time) != nullvals[2] || is.null(edges) != nullvals[3]){
        stop(error="invalid input for this event type")
    }
}


##' Auxiliary function to calculate first transition time for a set of edges
##' 
##' @param data A subset of EventSimOutput data with a specific patient Id
##' @param edges A set of edges of the form \code{c("fromNode_1","toNode_1","fromNode_2","toNode_2",...)}
##' @return The first transition into \code{toNode_i} from \code{fromNode_i} for some i
##' If no edge in \code{edges} is traversed then Inf is returned 
TimeToHitEdge <- function(data,edges){
  #Expect sorted data
  from <- edges[seq(1,length(edges),2)]
  to <- edges[seq(2,length(edges),2)]
   
  #for each row of data
  for(x in 1:(nrow(data)-1)){
              
      for(m in 1:length(from)){  
        if(from[m] == data[x,"state"] && data[x+1,"state"] == to[m] ){
          return(data[x+1,]$patient_transition_time) 
          
        }
      }  
      
  }
  #No matches, return Inf
  return(Inf)
    
}

##' Auxiliary function to calculate first hitting time for a set of states
##' 
##' @param data A subset of EventSimOutput data with a specific patient Id
##' @param states A set of states
##' @return The first transition into any node in the
##' vector states
##' If no node in states is hit then Inf is returned 
TimeToHit <- function(data,states){
  a <- subset(data, data$state %in% states )  
  if(nrow(a) == 0) return(Inf)
  m <- min(a$patient_transition_time)
  a[a$patient_transition_time == m,]$patient_transition_time 
}

##' Auxiliary function to calculate start state for subjects
##' 
##' @param data A subset of EventSimOutput data with a specific patient Id
##' @return The row of \code{data} with the lowest patient_transition_time
##' If more than one, the first in the data frame is output 
StartState <- function(data){
  m <- min(data$patient_transition_time)
  x <- data[data$patient_transition_time == m,]
  x[1,]
  
}

##' Auxiliary function to calculate end state for subjects
##' 
##' @param data A subset of EventSimOutput data with a specific patient Id
##' @return The row of \code{data} with the highest patient_transition_time
##' If more than one, the last in the data frame is output 
EndState <- function(data){
  m <- max(data$patient_transition_time)
  x <- data[data$patient_transition_time == m,]
  x[nrow(x),]
}

