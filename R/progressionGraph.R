##' Creates an object of type \code{ProgressionGraph}
##' 
##' Creates an object of type \code{ProgressionGraph} which is used to 
##' described the possible state transitions of the event simulator 
##' @return An object of type \code{ProgressionGraph} with no nodes or edges
##' @export
InitializeProgressionGraph <- function(){
    graph <- graphNEL(edgemode="directed")
    edgeDataDefaults(graph,"hazard") <- 0 #all rates default to zero
    edgeDataDefaults(graph,"isVisitEdge") <- FALSE #should this be (false, first, every)?
    nodeDataDefaults(graph,"visitSchedule") <- NewVisitSchedule() #defaults to no visits
    nodeDataDefaults(graph,"isVisitNode") <- FALSE
    edgeDataDefaults(graph,"isResetEdge") <- FALSE 
    edgeDataDefaults(graph,"shape") <- 1 #default to exponential
    res <- list(graph=graph)
    class(res) <- c("ProgressionGraph")
    return(res)
}


##' Checks whether a graphNEL object is a DAG
##' 
##' Returns true if an graphNEL object is a directed acyclic graph. Specifically if
##' edgemode(graph) == "directed" and the graph contains no cycles.
##' @param graph An object of type \code{graphNEL}
##' @return True if DAG, false otherwise
is.DAG <- function(graph){
 
    if(edgemode(graph) != "directed") return(FALSE)
  
    #see http://stackoverflow.com/questions/4168/graph-serialization/4577#4577
    #for algorithm
    
    #sorted elements
    L <- vector()
    #All nodes with no incoming edges
    Q <- setdiff(1:length(nodes(graph)),edgeMatrix(graph)[2,])
    
    while(length(Q) > 0){
         
        n <- tail(Q,1)
        Q <- head(Q,-1)
    
        L <- c(L,n)
        
        eM <- edgeMatrix(graph)
                  
        #All nodes with edge coming from n
        from_n  <- eM[2,eM[1,]==n]
                
        for(m in from_n){
            graph <- removeEdge(from=nodes(graph)[n],to=nodes(graph)[m],graph)
            if(! m %in% edgeMatrix(graph)[2,] ) Q <- c(Q,m)
        }    
          
    } 
        
    if(length(edgeMatrix(graph)) == 0) return(TRUE)
    return(FALSE)                        
    
}


##' Plot the \code{ProgressionGraph} DAG
##' 
##' @param x \code{ProgressionGraph} object
##' @param ... Ignored
##' @method plot ProgressionGraph 
##' @export
plot.ProgressionGraph <- function(x,...){
    y <- layoutGraph(x$graph)
    renderGraph(y)
}

##' Adds nodes to an object of type \code{ProgressionGraph}
##' 
##' Adds nodes to an object of type \code{ProgressionGraph}, will
##' throw an exception if attempting to add a new node with the same
##' name as an existing node
##' @param object the \code{ProgressionGraph} to add new nodes  
##' @param nodeNames a vector of strings to be added as nodes in the graph
##' @param visitSchedule Either a \code{VisitSchedule} object which is the visit schedule to
##' be used for the new nodes to be added or \code{NULL} if no visit schedule is being used (or visit Schedule is
##' to be added later)
##' @param isVisitNode A logical value. Is the new node a `visit' node, i.e. a node for which the transition into it is unknown
##' until the next visit
##' @return a new \code{ProgressionGraph} with the new nodes added
##' @export 
AddNode.ProgressionGraph <- function(object,nodeNames,visitSchedule=NULL,isVisitNode=FALSE){
    if(any(duplicated(nodeNames)) || any(nodeNames=="all")){
        stop(error="Node names must be unique and cannot be 'all'")
    }
    if(!is.null(visitSchedule) && class(visitSchedule) != "VisitSchedule"){
        stop(error="visit schedule incorrect type")
    }
    
    if(!is.logical(isVisitNode) || length(isVisitNode) != 1){
        stop(error="isVisitNode must be a single logical value")
    }
    
    object$graph <- addNode(nodeNames,object$graph)
    
    if(!is.null(visitSchedule)){
       object <- AddVisitSchedule.ProgressionGraph(object,visitSchedule,nodeNames)
    }
    
    object <- SetIsVisitNode.ProgressionGraph(object,isVisitNode,nodeNames)
          
    return(object)
}

##' Set the isVisitNode variable for nodes in \code{ProgressionGraph}
##' 
##' @param object A \code{ProgressionGraph} object
##' @param isVisitNode A logical value as to value to set the isVisitNode property 
##' @param nodeNames A list of node names whose isVisitNode property is to be set
##' or \code{"all"} if all nodes currently in the Progression Graph are to have this property set
##' @return \code{object} with the isVisitNode properties updated. Previous isVisitNode values are overwritten 
##' @export
SetIsVisitNode.ProgressionGraph <- function(object,isVisitNode,nodeNames="all"){
  #TODO refactor with function AddVisitSchedule.ProgressionGraph?
  if(length(nodeNames)==1 && nodeNames =="all"){
    nodeNames <- nodes(object$graph) 
  }
  
  if(!is.logical(isVisitNode) || length(isVisitNode) != 1){
    stop(error="Invalid isVisitNode value")
  }
  
  for(x in nodeNames)
    nodeData(object$graph,x,"isVisitNode")[[1]] <- isVisitNode
  
  return(object) 
}

##' Attach a visit schedule to nodes in \code{ProgressionGraph}
##' 
##' @param object A \code{ProgressionGraph} object
##' @param visitSchedule A \code{VisitSchedule} object describing the visit schedule for nodes on
##' the progression graph
##' @param nodeNames A list of node names for which the \code{visitSchedule} is applicable
##' or \code{"all"} if the visit schedule is to be used for all nodes currently in the Progression Graph
##' @return \code{object} with the visit schedules inserted. Previously inserted schedules are overwritten 
##' @export
AddVisitSchedule.ProgressionGraph <- function(object,visitSchedule,nodeNames="all"){
    if(class(visitSchedule) != "VisitSchedule"){
        stop(error="invalid visitSchedule")
    }
    
    if(length(nodeNames)==1 && nodeNames =="all"){
        nodeNames <- nodes(object$graph) 
    }
   
    for(x in nodeNames)
      nodeData(object$graph,x,"visitSchedule")[[1]] <- visitSchedule

    return(object)
}


##' Set the Weibull shape parameter for the hazard functions for
##' an edge of the progression graph
##' 
##' @param object \code{ProgressionGraph} object
##' @param from A list of nodes e.g. if \code{from = c("a","a","b")} and \code{to=c("c","d","a")}
##' then the edges \code{a} -> \code{c}, \code{a}-> \code{d} and \code{b}->\code{a} will have their 
##' isResetEdge properties set.
##' @param to See \code{from}
##' @param shape The shape parameter to be set
##' @return object with the appropriately changed Weibull shape parameters
##' @export
SetShape.ProgressionGraph <- function(object,from,to,shape){
    if(!is.numeric(shape) || length(shape) != 1 || shape <= 0){
        stop(error="Invalid shape value")
    }
    edgeData(object$graph,from=from,to=to,"shape") <- shape
    return(object)
}


##' Set the \code{isResetEdge} property of edges in the \code{ProgressionGraph}
##' 
##' If there is a patient switch time at time = 3 and the subject transitions from control.progressing 
##' to active.progressed at patient time = 2, then although patient time is unchanged, we would now like the 
##' patient switch time to be reset so it now occurs at 2 (time of transition) + 3 (original switch time) = 5
##' If this behaviour is required then set isResetEdge to be TRUE for the control.progressing -> active.progressed
##' edge. In  all other cases (as is the default) isResetEdge should be set to FALSE  
##' 
##' @param object A \code{ProgressionGraph} object
##' @param from A list of nodes e.g. if \code{from = c("a","a","b")} and \code{to=c("c","d","a")}
##' then the edges \code{a} -> \code{c}, \code{a}-> \code{d} and \code{b}->\code{a} will have their 
##' isResetEdge properties set.
##' @param to See \code{from}
##' @param isResetEdge Logical value of isResetEdge for 
##' @return \code{object} with the appropriate isResetEdge properties set
##' @export
SetIsResetEdge.ProgressionGraph <- function(object,from,to,isResetEdge){
  if(!is.logical(isResetEdge) || length(isResetEdge) != 1){
    stop(error="Invalid isResetEdge value")
  }
  edgeData(object$graph,from=from,to=to,"isResetEdge") <- isResetEdge
  return(object)
}


##' Return a list of edges with isResetEdge True
##' 
##' @param object A \code{ProgressionGraph} object
##' @return A vector of the form \code{c(from_1,to_1,from_2,to_2,...)}
##' where the edges from node with index from_i to node with index to_i
##' has isResetEdge TRUE.
GetResetEdges.ProgressionGraph <- function(object){
  
  retVal <- vector()
  
  theNodes <- nodes(object$graph) 
  numNodes <- length(theNodes)
  em <- edgeMatrix(object$graph)
  
  for(y in 1:length(em[1,])){
    
    from <- theNodes[em[1,y]]
    to <- theNodes[em[2,y]]
  
    if(edgeData(object$graph,from=from,to=to,attr="isResetEdge")[[1]]){
      retVal <- c(retVal,em[1,y],em[2,y])
    }
    
  }
  names(retVal) <- NULL
  return(retVal)
}


##' Adds edges to an object of type \code{ProgressionGraph} 
##' 
##' Adds edges to an object of type \code{ProgressionGraph} ensuring
##' that the graph remains a DAG. An exception is thrown if a cycle is
##' introduced or an edge which already exists is attempted to be added
##' @param object the \code{ProgressionGraph} to add new edges
##' @param fromNodeNames a vector of starting nodes of edges
##' @param toNodeNames a vector of ending nodes of edges. For example An edge
##' is added between fromNodeNames[i] to toNodeNames[i] for all i
##' @param isResetEdge logical value for whether these edges are edges for
##' which patient time switches are reset (this wouldbe true, in case of crossing over onto
##' a different arm and then requiring a lag before the new treatment kicks in)
##' @return a new \code{ProgressionGraph} with the new edges added
##' 
##' If an edge is attempted to be added twice, e.g. 
##' AddEdge.ProgressionGraph <- (g,from=c("a","a"),to=c("b","b")) it will
##' only be added once. 
##' 
##' If an edge already exists then a warning message will be displayed
##'  
##' @export 
AddEdge.ProgressionGraph <- function(object,fromNodeNames,toNodeNames,isResetEdge=FALSE){
    if(length(fromNodeNames) != length(toNodeNames)){
        stop(error="Length mismatch of vectors fromNodeNames and toNodeName")
    }
    
    wasDag <- is.DAG(object$graph)
    
    object$graph <- addEdge(fromNodeNames,toNodeNames,object$graph)
    if(wasDag && !is.DAG(object$graph)){ 
        warning("Progression graph is no longer a DAG. Are you sure this is what you want",immediate. = FALSE)
    }
    
    object <- SetIsResetEdge.ProgressionGraph(object,fromNodeNames,toNodeNames,isResetEdge)
    return(object)
}

##' Allows an entire arm to be added to an object of type \code{ProgressionGraph}
##' 
##' Allows an entire arm to be added to an object of type \code{ProgressionGraph}
##' The Weibull shape parameter for the rates for each node will be 1, 
##' to change use \code{\link{SetShape.ProgressionGraph}} 
##' 
##' @param object the \code{ProgressionGraph} to add the new arm
##' @param armName the name of the arm, for example "control" 
##' @param armProgression a vector of states subjects experience
##' for example "progressing", "progressed". This vector should not include the "death" state  
##' @param deathNode the name of the death node in the graph, which should already exist 
##' @param edges determines which edges are to be added for arm
##' must be one of \code{"none","immediate","death","all"}. \code{none}: no edges added
##' \code{immediate}: must progress sequentially through all states \code{death}: must progress
##' sequentially through all states or go straight to death \code{all}: can jump straight to any
##' state (always progressing towards death)
##' @param visitSchedule Either a \code{VisitSchedule} object which is the visit schedule to
##' be used for the nodes of this arm or \code{NULL} if no visit schedule is being used (or visit Schedule is
##' to be added later)
##' @param isVisitNode A logical value: Are the new node `visit' nodes, i.e. nodes for which the transition into it is unknown
##' until the next visit
##' @return a new \code{ProgressionGraph} with the arm added. The new nodes will be of
##' the form armName.armProgression[i]
##' @export 
AddArm.ProgressionGraph <- function(object,armName,armProgression,deathNode="death",edges="immediate",
                                    visitSchedule=NULL,isVisitNode=FALSE){
    if(!(deathNode %in% nodes(object$graph))){
        stop(error="Please ensure deathNode exists in the graph")
    }
    
    if(length(armProgression)== 0){
        stop(error="armProgression cannot be empty")
    }
    
    if(armName == "" || !is.character(armName)){
        stop(error="Invalid arm name")
    }
    
    edgeOptions = c("none","immediate","death","all")
    if(!(edges %in% edgeOptions)){
        stop(error=paste("Invalid edges option. It must be one of",paste(edgeOptions,collapse=", ")))
    }
    
    nodeNames <- sapply(armProgression,function(x){paste(armName,x,sep=".")})
    names(nodeNames) <- NULL 
      
    object <- AddNode.ProgressionGraph(object,nodeNames,visitSchedule=visitSchedule,isVisitNode=isVisitNode)
  
    fromNodes <- NULL
    toNodes <- NULL
    
    if(edges == "immediate"){
        fromNodes <- nodeNames
        toNodes <- c(tail(nodeNames,-1),deathNode)
    }
    if(edges == "death"){
        fromNodes <- c(nodeNames,head(nodeNames,-1))
        toNodes <- c(tail(nodeNames,-1),rep(deathNode,length(nodeNames)))
    }
    if(edges == "all"){
       for(x in 1:length(nodeNames)){
           fromNodes <- c(fromNodes,rep(nodeNames[x],1+length(nodeNames)-x))
           toNodes <- c(toNodes,tail(nodeNames,-x),deathNode)
       } 
    }
    
    if(edges != "none"){
        object <- AddEdge.ProgressionGraph(object,fromNodes,toNodes)
    }
            
    return(object)
}


##' Create a \code{ProgressionGraph} describing a simple disease progression clinical trial 
##' 
##' This function creates the transition state graph for clinical trials with arbitrary number of 
##' arms, disease progression states and crossovers, returning an object of type \code{ProgressionGraph}.
##' The disease progression and possible transitions must be the same for each arm and no drop out state is included.
##' 
##' If more complicated disease progression transitions are required then this function can be used in conjunction 
##' with \code{\link{AddArm.ProgressionGraph}}, \code{\link{AddEdge.ProgressionGraph}}, \code{\link{AddNode.ProgressionGraph}}  
##' 
##' The Weibull shape parameter for the rates for each node will be 1, 
##' to change use \code{\link{SetShape.ProgressionGraph}}
##' 
##' The edges added by the crossOver argument default to having isResetEdge set to true (i.e. the 
##' patient switch times are reset when transitioning over this edge) This can be changed by calling
##' \code{\link{SetIsResetEdge.ProgressionGraph}}
##' 
##' @inheritParams AddArm.ProgressionGraph
##' @param arms a vector of arm names
##' @param crossOvers a vector containing additional transitions of the form \code{c("fromnode1","tonode1","fromnode2","tonode2")}
##' @param visitSchedule Either a \code{VisitSchedule} object which is the visit schedule to
##' be used for the nodes of this Progression Graph or \code{NULL} if no visit schedule is being used (or visit Schedule is
##' to be added later). Note the node 'death' will have the same visit schedule, which should not cause problems.
##' @param isVisitNode A logical value. Are the nodes `visit' nodes, i.e. node for which the transition into it is unknown
##' until the next visit. If TRUE then all nodes in the graph are visit nodes, if false then none are. The function
##' \code{\link{SetIsVisitNode.ProgressionGraph}} can be used to change the isVisitNode status of individual nodes
##' @return An object of type \code{ProgressionGraph} which details the transition
##' states of study
##' @examples 
##' crossOver <- c("control.progressed","active.progressed")
##' 
##' SimpleStudyProgressionGraph(arms=c("control","active"),
##'                             armProgression=c("progressing","progressed"),
##'                             edge="immediate",
##'                             crossOver=crossOver)
##' @export
SimpleStudyProgressionGraph <- function(arms,armProgression,edges="immediate",crossOvers=NULL,
                                        visitSchedule=NULL,isVisitNode=FALSE){
    if(length(crossOvers) %% 2 == 1){
        stop(error="length of crossOvers must be even")
    }
  
    res <- InitializeProgressionGraph()
    res <- AddNode.ProgressionGraph(res,"death",visitSchedule=visitSchedule,isVisitNode=isVisitNode)
    for(armName in arms){
        res <- AddArm.ProgressionGraph(res,armName,armProgression,deathNode="death",edges, 
                                       visitSchedule=visitSchedule,isVisitNode=isVisitNode)
    }
    
    if(!is.null(crossOvers)){
        res <- AddEdge.ProgressionGraph(res,crossOvers[c(TRUE,FALSE)],crossOvers[c(FALSE,TRUE)],isResetEdge=TRUE)
    }
    return(res)
}


##'Insert a rate value/function into a \code{ProgressionGraph}
##'
##'See \code{\link{InsertRate.EventSim}} for further details. If a rate
##'already exists for this edge then it is overwritten
##'
##'@param object An object of type \code{ProgressionGraph}
##'@param fromNode The node out of which this rate is describing 
##'@param toNode The node into which this rate describes the transition
##'@param rate The rate (a numeric value or a function) which is to be added to the
##' object
##'@param mustBeNumeric True if insisting \code{rate} is numeric, false if it can 
##'be numeric or function
##'@return \code{object} with the additional rate included
InsertRate.ProgressionGraph <- function(object,fromNode,toNode,rate,mustBeNumeric=FALSE){
    if(mustBeNumeric && !is.numeric(rate)){
        stop(error="rate must be numeric if mustbeNumeric is True")
    }
    if(!mustBeNumeric && (class(rate) != "numeric" && class(rate) != "function")){
        stop(error="rate must be numeric or a function")
    }
  
  
    if(class(rate)=="numeric" && rate <0){
      stop(error="rates must be non-negative")
    }
    edgeData(object$graph,from=fromNode,to=toNode,attr="hazard") <- rate
    return(object)
}

##' True iff \code{ProgressionGraph} has no nodes
##' 
##' @param object A \code{ProgressionGraph} object
##' @return True iff \code{ProgressionGraph} has no nodes 
IsEmpty.ProgressionGraph <- function(object){
  if(length(nodes(object$graph))==0){
    return(TRUE)
  }
  return(FALSE)
}


##' Returns True if edges are on DAG
##' 
##' @param object \code{ProgressionGraph} object
##' @param from A vector of 'from' nodes \code{c("from_1","from_2",...)}
##' @param to A 'to' nodes \code{c("to_1","to_2",...)}
##' @return True if all nodes are in graph and from_i -> to_i is an edge in graph
##' for all i 
IsEdge.ProgressionGraph <- function(object,from,to){
    if(length(from)!= length(to)){
        stop(error="length mismatch in IsEdge.ProgressionGraph")
    }
  
    if (any(!(from %in% nodes(object$graph))) || any(!(to %in% nodes(object$graph))) ){
      return(FALSE)
    }
    
    retVal <- sapply(seq_along(from),function(x){
       return(to[x] %in% (edges(object$graph,which=from[x])[[1]]))
    })
    
    return(all(retVal))
    
}



##' Outputs a matrix of rates from the edge Data of a \code{ProgressionGraph}
##' object
##' 
##' The matrix that is produced is such that M[i,j] = the rate
##' from node i to node j, where the nodes are numbered in creation 
##' order (\code{nodes(object$graph))}. If the transition rates are 
##' formula then these should be evaluated before calling this function
##' 
##' @param object A \code{ProgressionGraph} object
##' @return A Matrix of transition rates
##'
GeneratRateMatrix.ProgressionGraph <- function(object){
    GetEdgeProperty.ProgressionGraph(object,"hazard")        
}

##' Returns a matrix of values from edge properties
##' @param object A \code{ProgressionGraph} object
##' @param propertyName Either \code{"hazard"} or \code{"shape"}
##' @return A matrix. See \code{GeneratRateMatrix.ProgressionGraph} and
##' \code{GetShapes.ProgressionGraph} for further details
##' 
GetEdgeProperty.ProgressionGraph <- function(object,propertyName)  
{  
    theNodes <- nodes(object$graph) 
    numNodes <- length(theNodes)
    ans <- matrix(rep(0,numNodes*numNodes),nrow=numNodes)
    em <- edgeMatrix(object$graph)

    for(y in 1:length(em[1,])){
        
        from <- theNodes[em[1,y]]
        to <- theNodes[em[2,y]]
  
        if(class(edgeData(object$graph,from=from,to=to,attr=propertyName)[[1]])=="function"){
            stop(error="Cannot create rate matrix, please call EvaluateFormula first")
        }
  
        #put rate into matrix
        ans[em[1,y],em[2,y]] <- edgeData(object$graph,from=from,to=to,attr=propertyName)[[1]] 
  
    } 

    if(!MaxOneInfPerRow(ans)){
      stop(error="Only one rate transitioning out of a node can be Inf at any time")  
    }
        
    return(ans)
}

##' Return TRUE if the given matrix contains at most one Inf per row
##' 
##' @param mat the matrix
##' @return TRUE iff the given matrix contains at most one Inf per row
MaxOneInfPerRow <- function(mat){
    temp <- sapply(1:nrow(mat),function(y){if(!is.infinite(max(mat[y,]))) 0 else table(mat[y,])[["Inf"]]})
    if(max(temp) > 1){
      return(FALSE)
    }
    return(TRUE)
}

##' Evaluate the formulae of a \code{ProgressionGraph} object
##' using the parameter values \code{params}
##' 
##' See \code{\link{EvaluateFormula.EventSim}}for further details
##' 
##' @param object A \code{ProgressionGraph} whose rate formuale are to
##' be evaluated
##' @param params A named vector of parameter values to be used when 
##' evaluating the formulae
##' @return \code{object} with the rate formulae evaluated
EvaluateFormula.ProgressionGraph <- function(object,params){

    theNodes <- nodes(object$graph)
    eM <- (edgeMatrix(object$graph))

    newObject <- object
    
    for(y in 1:length(eM[1,])){
  
        from <- theNodes[eM[1,y]]
        to <- theNodes[eM[2,y]]
  
        if (class(edgeData(object$graph,from=from,to=to,attr="hazard")[[1]])=="function"){
            f <- edgeData(object$graph,from=from,to=to,attr="hazard")[[1]] 
            
            rate <-tryCatch({
              f(params)[[1]]  
            },error = function(err) {
            stop(error="Error evaluating formula, are the correctly named parameters used?")})
            
            newObject <- InsertRate.ProgressionGraph(newObject,from,to,rate,mustBeNumeric=TRUE)
                
        
        }
  
    }
    return(newObject)
}

##' Outputs a matrix of shapes from the edge Data of a \code{ProgressionGraph}
##' object
##' 
##' The matrix that is produced is such that M[i,j] = the shape
##' from node i to node j, where the nodes are numbered in creation 
##' order (\code{nodes(object$graph))}.
##' 
##' @param object A \code{ProgressionGraph} object
##' @return A Matrix of Weibull shape parameters, note that if no edge exists 
##' between two nodes then M[i,j] = 0
GetShapes.ProgressionGraph <- function(object){
    GetEdgeProperty.ProgressionGraph(object,"shape")   
}
