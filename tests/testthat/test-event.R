context("event test")

ReadInSimulatorData <- function(path="."){
  ans <- list()
  ans$data <- read.csv(file=paste(path,"/eventtest.csv",sep=""))
  ans$data$X <- NULL
  rT <- read.csv(file=paste(path,"/eventtestRecruitTime.csv",sep=""))
  ans$recruitmentTimes = as.vector(rT$x)
  return(ans)
} 

test_that("InitializeEventSet",{
  ES <- InitializeEventSet()
  expect_equal(list(),ES$events)
  expect_equal("EventSet",class(ES))
})

test_that("NewEvent",{
  expect_error(NewEvent(name="",type="startState"))
  expect_warning(NewEvent(name=c("a","b"),type="startState"))
  
  expect_error(NewEvent(name=NULL,type="startState"))
  expect_error(NewEvent(name="event",type="s"))
  
  expect_error(NewEvent(name="n",type="startState",time=5))
  expect_error(NewEvent(name="n",type="endState",edges=c("4","5")))
  expect_error(NewEvent(name="n",type="recruitTime",nodeNames=c("b","d")))
  
  expect_error(NewEvent(name="n",type="timeToHit",nodeNames=c("a"),time=5))
  expect_error(NewEvent(name="n",type="timeToHit"))
  expect_error(NewEvent(name="n",type="timeToHit_Edge",nodeNames=c("a")))
  expect_error(NewEvent(name="n",type="timeToHit_Edge",nodeNames=c("a")))
  
  expect_error(NewEvent(name="n",type="timeToHit_Edge",edges=c(1,2,3)))
  expect_error(NewEvent(name="n",type="hitsBefore_Edge",edges=c(1,2,3,4)))
  expect_error(NewEvent(name="n",type="hitsBefore_Edge",edges=c(1,2,3,4),time="a"))
  
  expect_error(NewEvent(name="n",type="hitsBefore",nodeNames=c(1,2,3,4),time=c(1,5)))
  expect_error(NewEvent(name="n",type="hitsBefore",nodeNames=c(1,2,3,4),time=-5.4))
  
  a <- NewEvent(name="hello",type="hitsBefore_Edge",edges=c("1","2","3","4"),time=11)
  
  expect_equal("hello",a$name)
  expect_equal("hitsBefore_Edge",a$type)
  expect_equal(c("1","2","3","4"),a$edges)
  expect_null(a$nodeNames)
  expect_false(a$calendarTime)
  expect_equal(11,a$time)
  expect_equal("Event",class(a))
  
  a <- NewEvent(name="hello",type="timeToHit",nodeNames=c("1","2","3","4"),calendarTime=TRUE)
  expect_equal(c("1","2","3","4"),a$nodeNames)
  expect_true(a$calendarTime)
  
  
})


test_that("InsertEvent",{
    ES <- InitializeEventSet()
    expect_error(InsertEvent(ES,"a"))
    
    a <- NewEvent(name="n",type="startState")
    
    ES <- InsertEvent.EventSet(ES,a)
    
    expect_equal(1,length(ES$events))
    expect_equal(a,ES$events[[1]])
    
    b <- NewEvent(name="n",type="endState") 
    
    expect_error(InsertEvent.EventSet(ES,a)) #same name
    
    d <- NewEvent(name="n2",type="startState")
    expect_error(InsertEvent.EventSet(ES,d)) #only 1 of startState, endState, recruitTime allowed
    
    e <- NewEvent(name="n2",type="timeToHit",nodeNames=c("1","2","3","4"),calendarTime=TRUE)
    e2 <- NewEvent(name="n3",type="timeToHit",nodeNames=c("1","2","3","4"),calendarTime=FALSE)
    ES <- InsertEvent.EventSet(ES,e)
    ES <- InsertEvent.EventSet(ES,e2)
    
    expect_equal(3,length(ES$events))
    expect_equal(a,ES$events[[1]])
    expect_equal(e,ES$events[[2]])
    expect_equal(e2,ES$events[[3]])
    
})


test_that("ValidateAgainstProgressionGraph",{
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"),
                                   armProgression=c("1","2","3"),edge="death")
  
  h <- SimpleStudyProgressionGraph(arms=c("a","b"),
                                   armProgression=c("1","2","3"),edge="death",crossOver=c("a.1","b.2"))
  
  
  ES <- InitializeEventSet()
  
  ES <- InsertEvent.EventSet(ES,NewEvent(name="n",type="endState"))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES,g))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES,h))
  
  ES <- InsertEvent.EventSet(ES,NewEvent(name="t",type="timeToHit",nodeNames=c("a.1","b.2")))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES,g))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES,h))
  
  ES2 <- InsertEvent.EventSet(ES,NewEvent(name="t2",type="timeToHit",nodeNames=c("a.1","b")))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,g))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,h))
  
  ES2 <- InsertEvent.EventSet(ES,NewEvent(name="t2",type="hitsBefore",nodeNames=c("c.1","b.2"),time=Inf))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES2,g))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,h))
  
  ES2 <- InsertEvent.EventSet(ES,NewEvent(name="t2",type="hitsBefore_Edge",edges=c("a.1","b.2"),time=Inf))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,g))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES2,h))
  
  ES2 <- InsertEvent.EventSet(ES,NewEvent(name="t2",type="hitsBefore_Edge",edges=c("b.2","a.1"),time=Inf))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,g))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,h))
  
  ES2 <- InsertEvent.EventSet(ES,NewEvent(name="t2",type="hitsBefore_Edge",edges=c("b","a.1"),time=Inf))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,g))
  expect_false(ValidateAgainstProgressionGraph.EventSet(ES2,h))
  
  ES2 <- InsertEvent.EventSet(ES,NewEvent(name="t2",type="hitsBefore_Edge",edges=c("a.1","a.2","b.2","death"),time=Inf))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES2,g))
  expect_true(ValidateAgainstProgressionGraph.EventSet(ES2,h))
  
})


test_that("recruitTime",{
  ans <- ReadInSimulatorData()
  
  ES <- InitializeEventSet()
  ES <- InsertEvent.EventSet(ES,NewEvent(name="rec",type="recruitTime"))
  
  
  output <- ProcessEventSimOutput(ans,ES)
  
  expect_equal(c("id","rec"),colnames(output))
  expect_equal(unique(ans$data$id),output$id)
  expect_equal(ans$recruitmentTimes,output$rec)
  
})

test_that("startStateandEndState",{
  ans <- ReadInSimulatorData()
  
  ES <- InitializeEventSet()
  ES <- InsertEvent.EventSet(ES,NewEvent(name="start",type="startState"))
  ES <- InsertEvent.EventSet(ES,NewEvent(name="end",type="endState"))
  
  output <- ProcessEventSimOutput(ans,ES)
  
  expect_equal(c("id","start","end"),colnames(output))
  
  a <- subset(ans$data,patient_transition_time==0)
  expect_equal(a$state,output$start)
  
  expect_equal(rep("death",max(a$id)),as.character(output$end))
  
  
})


test_that("timeToHit + hitsBefore",{
    ans <- ReadInSimulatorData()
    ES <- InitializeEventSet()
    
    n1 <- c("active.progressed","control.progressed")
    n2 <- c(n1,"death")
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n1_pt",type="timeToHit",nodeNames=n1))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n1_ct",type="timeToHit",nodeNames=n1,calendarTime=TRUE))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n2_pt",type="timeToHit",nodeNames=n2))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n2_ct",type="timeToHit",nodeNames=n2,calendarTime=TRUE))
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n1_pt_0",type="hitsBefore",nodeNames=n1,time=0))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n1_pt_2",type="hitsBefore",nodeNames=n1,time=2))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n1_pt_inf",type="hitsBefore",nodeNames=n1,time=Inf))
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n2_ct_0",type="hitsBefore",nodeNames=n2,time=0,calendarTime=TRUE))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n2_ct_2",type="hitsBefore",nodeNames=n2,time=2,calendarTime=TRUE))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="n2_ct_inf",type="hitsBefore",nodeNames=n2,time=Inf,calendarTime=TRUE))
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="allinf",type="timeToHit",nodeNames="Hello"))
    
    
    output <- ProcessEventSimOutput(ans,ES)
    
    cs <- c("id","n1_pt","n1_ct","n2_pt","n2_ct","n1_pt_0","n1_pt_2")
    cs <- c(cs,"n1_pt_inf","n2_ct_0","n2_ct_2","n2_ct_inf","allinf")
    
    expect_equal(cs,colnames(output))
    
    lapply(output$id,function(x){
        a <- subset(ans$data,id==x & patient_transition_time == output$n1_pt[x] )
        b <- subset(ans$data,id==x & patient_transition_time < output$n1_pt[x] )
        if(nrow(a) != 0){
          expect_true(all(as.character(a$state) %in% n1))
        }
        if(nrow(b) != 0){
          expect_false(any(as.character(b$state) %in% n1))
        }
        
        
        a <- subset(ans$data,id==x & patient_transition_time == output$n2_pt[x] )
        b <- subset(ans$data,id==x & patient_transition_time < output$n2_pt[x] )
        if(nrow(a) != 0){
          expect_true(all(as.character(a$state) %in% n2))
        }
        if(nrow(b) != 0){
          expect_false(any(as.character(b$state) %in% n2))
        }
        
    })
    
    expect_equal(output$n1_ct,output$n1_pt+ans$recruitmentTimes)
    expect_equal(output$n2_ct,output$n2_pt+ans$recruitmentTimes)
    
    expect_equal( !is.infinite(output$n2_ct) ,output$n2_ct_inf)
    expect_equal( output$n2_ct < 0 ,output$n2_ct_0)
    expect_equal( output$n2_ct < 2 ,output$n2_ct_2)
    
    expect_equal( !is.infinite(output$n1_pt) ,output$n1_pt_inf)
    expect_equal( output$n1_pt < 0 ,output$n1_pt_0)
    expect_equal( output$n1_pt < 2 ,output$n1_pt_2)
    expect_equal(rep(Inf,nrow(output)),output$allinf)
    
})


test_that("timeToHit_Edge + hitsBefore_Edge",{
    ans <- ReadInSimulatorData()
    ES <- InitializeEventSet()
    
    e1 <- c("control.progressing","control.progressed")
    e2 <- c("control.progressed","death","active.progressed","death")
      
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e1_pt",type="timeToHit_Edge",edges=e1))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e2_pt",type="timeToHit_Edge",edges=e2))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e1_ct",type="timeToHit_Edge",edges=e1,calendarTime=TRUE))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e2_ct",type="timeToHit_Edge",edges=e2,calendarTime=TRUE))
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e1_pt_0",type="hitsBefore_Edge",edges=e1,time=0))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e1_pt_2",type="hitsBefore_Edge",edges=e1,time=2))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e1_pt_inf",type="hitsBefore_Edge",edges=e1,time=Inf))
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e2_ct_0",type="hitsBefore_Edge",edges=e2,time=0,calendarTime=TRUE))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e2_ct_2",type="hitsBefore_Edge",edges=e2,time=2,calendarTime=TRUE))
    ES <- InsertEvent.EventSet(ES,NewEvent(name="e2_ct_inf",type="hitsBefore_Edge",edges=e2,time=Inf,calendarTime=TRUE))
    
    ES <- InsertEvent.EventSet(ES,NewEvent(name="allinf",type="timeToHit_Edge",edges=c("death","active_progressed")))
    
    output <- ProcessEventSimOutput(ans,ES)
    
    cs <- c("id","e1_pt","e2_pt","e1_ct","e2_ct","e1_pt_0","e1_pt_2")
    cs <- c(cs,"e1_pt_inf","e2_ct_0","e2_ct_2","e2_ct_inf","allinf")
    
    expect_equal(cs,colnames(output))
    
    lapply(output$id,function(x){
        a <- subset(ans$data,id==x & state=="control.progressed")
        if(nrow(a)==0){
          expect_true(is.infinite(output$e1_pt[x]))
                      
        }
        else{
          expect_equal(output$e1_pt[x],a$patient_transition_time)
        }
        a <- subset(ans$data,id==x & state=="death")
        
        b <- subset(ans$data,id==x & (state=="control.progressed" | state=="active.progressed"))
        if(nrow(b)==0){
            expect_true(is.infinite(output$e2_pt[x]))
        }
        else{
              expect_equal(output$e2_pt[x],a$patient_transition_time) 
        }
    })
    
    expect_equal(output$e1_ct,output$e1_pt+ans$recruitmentTimes)
    expect_equal(output$e2_ct,output$e2_pt+ans$recruitmentTimes)
    
    expect_equal( !is.infinite(output$e2_ct) ,output$e2_ct_inf)
    expect_equal( output$e2_ct < 0 ,output$e2_ct_0)
    expect_equal( output$e2_ct < 2 ,output$e2_ct_2)
    
    expect_equal( !is.infinite(output$e1_pt) ,output$e1_pt_inf)
    expect_equal( output$e1_pt < 0 ,output$e1_pt_0)
    expect_equal( output$e1_pt < 2 ,output$e1_pt_2)
    expect_equal(rep(Inf,nrow(output)),output$allinf)
    
    
})
