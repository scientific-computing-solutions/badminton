context("test ProgressionGraph")

test_that("Initialize",{
      g <- InitializeProgressionGraph()
      expect_equal(class(g),"ProgressionGraph")
      expect_equal(edgemode(g$graph),"directed")
      expect_equal(edgeDataDefaults(g$graph,"hazard"),0)
      expect_equal(nodeDataDefaults(g$graph,"visitSchedule"),NewVisitSchedule())
      expect_false(nodeDataDefaults(g$graph,"isVisitNode"))
      expect_false(edgeDataDefaults(g$graph,"isVisitEdge"))
      expect_false(edgeDataDefaults(g$graph,"isResetEdge"))
      expect_equal(1,edgeDataDefaults(g$graph,"shape"))
      
    }
)


test_that("IsEmpty",{
   g <- InitializeProgressionGraph() 
   expect_true(IsEmpty.ProgressionGraph(g))
   g <- AddNode.ProgressionGraph(g,"1")
   expect_false(IsEmpty.ProgressionGraph(g))
   g <- AddNode.ProgressionGraph(g,c("2"))
   g <- AddEdge.ProgressionGraph(g,from="1",to="2")
   expect_false(IsEmpty.ProgressionGraph(g))
})

test_that("Add Node",{
    g <- InitializeProgressionGraph()
    expect_equal(nodes(g$graph),vector("character"))
    g <- AddNode.ProgressionGraph(g,"1")
    expect_equal(nodes(g$graph),c("1"))
    expect_equal(length(edgeMatrix(g$graph)),0)
    g <- AddNode.ProgressionGraph(g,"2")
    expect_equal(nodes(g$graph),c("1","2"))
    expect_error(AddNode.ProgressionGraph(g,c("3","3")))
    expect_error(AddNode.ProgressionGraph(g,"1"))
    g <- AddNode.ProgressionGraph(g,c("a","b"))
    expect_equal(nodes(g$graph),c("1","2","a","b"))
    expect_error(AddNode.ProgressionGraph(g,"all"))
}
)

test_that("is.DAG",{
    g <- graphNEL()
    expect_false(is.DAG(g)) #Edges not directed
    g <- graphNEL(edgemode="directed")
    expect_true(is.DAG(g)) # Empty graph is DAG
    
    g <- addNode(node=c("1","2","3","4","5"),g)
    expect_true(is.DAG(g)) # No edges is DAG
    
    h <- addEdge(from="1",to="1",g)
    expect_false(is.DAG(h))
    
    g <- addEdge(from="1",to="2",g)
    expect_true(is.DAG(g))
    
    h <- addEdge(from=c("2","4","5"),to=c("3","3","4"),g)
    expect_true(is.DAG(h))
    
    i <- addEdge(from=c("1","5"),to=c("3","3"),h)
    expect_true(is.DAG(i))
    
    h <- addEdge(from="2",to="1",h)
    expect_false(is.DAG(h))
    
    h <- addEdge(from=c("2","3","4"),to=c("3","4","5"),g)
    expect_true(is.DAG(h))
    
    i <- addEdge(from=c("3"),to=c("1"),h)
    expect_false(is.DAG(i))
    
    i <- addEdge(from=c("5"),to=c("1"), h)
    expect_false(is.DAG(i))
    
    i <- addEdge(from=c("3"),to=c("5"),i)
    expect_false(is.DAG(i))
    
  
})

test_that("Add Edge",{
    g <- InitializeProgressionGraph()
    g <- AddNode.ProgressionGraph(g,c("a","b","c","d"))
    expect_equal(length(edgeMatrix(g$graph)),0)
    
    g <- AddEdge.ProgressionGraph(g,from="a",to="b")
    expect_equal(edges(g$graph)$a,"b")
    expect_equal(length(edges(g$graph)$b),0)
    expect_equal(edgeData(g$graph,from="a",to="b",attr="hazard")[[1]],0)
    
    k <- AddEdge.ProgressionGraph(g,from=c("c","c"),to=c("d","d")) #No error
    expect_warning(AddEdge.ProgressionGraph(k,from=c("c"),to=c("d"))) #warning
        
    expect_error(AddEdge.ProgressionGraph(g,from=c("c","c"),to=c("d")))
    
    g <- AddEdge.ProgressionGraph(g,from=c("a","c"),to=c("d","d"))
    expect_equal(edges(g$graph)$a,c("b","d"))
    expect_equal(length(edges(g$graph)$b),0)
    expect_equal(edges(g$graph)$c,"d")
    expect_equal(length(edges(g$graph)$d),0)
    
    
    
    expect_warning(h <- AddEdge.ProgressionGraph(g,from="a",to="a"))
    h <- AddEdge.ProgressionGraph(h,from="d",to="a") #No warning here
    expect_error(AddEdge.ProgressionGraph(g,from="d",to="1"))
})

test_that("Add Arm",{
    g <- InitializeProgressionGraph()
    expect_error(AddArm.ProgressionGraph(g,"name","far",deathNode="boo"))
    expect_error(AddArm.ProgressionGraph(g,"","far",deathNode="edge"))
    
    g <- AddNode.ProgressionGraph(g,"death")
    expect_error(AddArm.ProgressionGraph(g,"name",c(),deathNode="death"))
    expect_error(AddArm.ProgressionGraph(g,"name",c(),deathNode="death",edges="noone"))
    
    expect_error(AddArm.ProgressionGraph(g,"name",c("progression","progression"),deathNode="death"))
    expect_error(AddArm.ProgressionGraph(g,c("name","name2"),c("progression","progressed"),deathNode="death"))
    
    g <- AddArm.ProgressionGraph(g,"name",c("progression","progressed"),deathNode="death")
    
    expect_equal(nodes(g$graph),c("death","name.progression","name.progressed"))
    expect_equal(length(edges(g$graph)$death),0)
    expect_equal(edges(g$graph)$name.progression,"name.progressed")
    expect_equal(edges(g$graph)$name.progressed,"death")
    
    expect_error(AddArm.ProgressionGraph(g,"name",c("progressed"),deathNode="death"))
    
    g <- AddArm.ProgressionGraph(g,"name",c("a","b","c","d"),deathNode="name.progressed",edges="death")
    expect_equal(edges(g$graph)$name.a,c("name.b","name.progressed"))
    expect_equal(edges(g$graph)$name.b,c("name.c","name.progressed"))
    expect_equal(edges(g$graph)$name.c,c("name.d","name.progressed"))
    expect_equal(edges(g$graph)$name.d,c("name.progressed"))
    expect_equal(edges(g$graph)$name.progression,"name.progressed")
    
    oldEdges <- length(edgeMatrix(g$graph)/2)
    
    g <- AddArm.ProgressionGraph(g,"boo",c("a","b","c","d"),deathNode="death",edges="none")
    expect_equal(nodes(g$graph),c("death","name.progression","name.progressed","name.a","name.b","name.c","name.d"
                                  ,"boo.a","boo.b","boo.c","boo.d"))
    
    expect_equal(oldEdges,length(edgeMatrix(g$graph)/2))
    g <- AddArm.ProgressionGraph(g,"boomore",c("a","b","c","d"),deathNode="death",edges="all")
    
    expect_equal(edges(g$graph)$boomore.a,c("boomore.b","boomore.c", "boomore.d","death"))
    expect_equal(edges(g$graph)$boomore.b,c("boomore.c","boomore.d","death"))
    expect_equal(edges(g$graph)$boomore.c,c("boomore.d","death"))
    expect_equal(edges(g$graph)$boomore.d,c("death"))
    
    
    
})


test_that("SimpleStudyProgressionGraph",{
  g <- SimpleStudyProgressionGraph(arms=c("control"),
                                   armProgression=c("progressing","progressed"),edge="death")
  
  expect_equal(nodes(g$graph),c("death","control.progressing","control.progressed" ))
  expect_equal(edges(g$graph)$control.progressed,c("death"))
  expect_equal(edges(g$graph)$control.progressing,c("control.progressed","death"))
  
  expect_error(SimpleStudyProgressionGraph(arms=c("control"),
                                           armProgression=c("progressing","progressing")))
  
  expect_error(SimpleStudyProgressionGraph(arms=c("control","control"),
                                           armProgression=c("progressing","progressed")))
  crossOver=c(1,2,3)
  expect_error(SimpleStudyProgressionGraph(arms=c("control","active"),
                                           armProgression=c("progressing","progressed"),crossOver=crossOver))
  
  crossOver=c("control.progressing","control.progress")
  expect_error(SimpleStudyProgressionGraph(arms=c("control","active"),
                                           armProgression=c("progressing","progressed"),crossOver=crossOver))
  
  g <- SimpleStudyProgressionGraph(arms=c("a1","a2","a3"),
                                   armProgression=c("a","b","c"),edge="immediate")
  
  expect_equal(nodes(g$graph),c("death","a1.a","a1.b","a1.c","a2.a","a2.b","a2.c","a3.a","a3.b","a3.c"))
  expect_equal(length(edges(g$graph)$death),0)
  expect_equal(edges(g$graph)$a1.a,c("a1.b"))
  expect_equal(edges(g$graph)$a2.a,c("a2.b"))
  expect_equal(edges(g$graph)$a3.b,c("a3.c"))
  expect_equal(edges(g$graph)$a3.c,c("death"))
  
  crossOver <- c("a1.b","a2.c")
  g <- SimpleStudyProgressionGraph(arms=c("a1","a2","a3"),
                                   armProgression=c("a","b","c"),edge="immediate",crossOver=crossOver)
  expect_equal(edges(g$graph)$a1.b,c("a1.c","a2.c"))
  expect_equal(edges(g$graph)$a2.c,c("death"))
  
  crossOver <- c("a1.b","a2.c","a2.c","a1.b")
  expect_warning(SimpleStudyProgressionGraph(arms=c("a1","a2","a3"),
                                   armProgression=c("a","b","c"),edge="immediate",crossOver=crossOver))
  
  crossOver <- c("a1.b","a2.c","a2.c","a3.b")
  g <- SimpleStudyProgressionGraph(arms=c("a1","a2","a3"),
                                   armProgression=c("a","b","c"),edge="immediate",crossOver=crossOver)
  expect_equal(edges(g$graph)$a1.b,c("a1.c","a2.c"))
  expect_equal(edges(g$graph)$a2.c,c("death","a3.b"))
  expect_equal(edges(g$graph)$a3.b,c("a3.c"))
  
})



test_that("InsertRate.ProgressionGraph",{
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"),
                                   armProgression=c("1","2","3"),edge="death")
  
  g <- InsertRate.ProgressionGraph(g,"a.1","a.2",5)
  
  expect_equal(5,edgeData(g$graph,from="a.1",to="a.2",attr="hazard")[[1]])
  expect_equal(0,edgeData(g$graph,from="a.1",to="death",attr="hazard")[[1]])
  expect_equal(0,edgeData(g$graph,from="a.2",to="a.3",attr="hazard")[[1]])
  
  expect_error(InsertRate.ProgressionGraph(g,"a.1","a.3",-5))
  expect_error(InsertRate.ProgressionGraph(g,"a.1","a.3",5))
  expect_error(InsertRate.ProgressionGraph(g,"a.1","a.1",5))
  expect_error(InsertRate.ProgressionGraph(g,"a.1","a2",5))
  
  expect_error(InsertRate.ProgressionGraph(g,"a.1","a2",list()))
    
  f <- function(pars){pars["x"]*pars["y"]}
  g <- InsertRate.ProgressionGraph(g,"a.2","a.3",f)
    
  expect_equal(f,edgeData(g$graph,from="a.2",to="a.3",attr="hazard")[[1]])
  expect_error(InsertRate.ProgressionGraph(g,"a.1","a2",f,TRUE))
  
})


test_that("EvaluateFormula",{
  
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"),
                                   armProgression=c("1","2","3"),edge="death")
  
  h <- EvaluateFormula.ProgressionGraph(g,NULL) #No formula, nothing happens
  expect_equal(g,h)
  
  g <- InsertRate.ProgressionGraph(g,"a.1","a.2",5)
  h <- EvaluateFormula.ProgressionGraph(g,NULL) #No formula, nothing happens
  expect_equal(g,h)
  
  f <- function(pars){(1+pars["x"])*pars["y"]}
  g <- InsertRate.ProgressionGraph(g,"a.2","a.3",f)
  
  expect_error(EvaluateFormula.ProgressionGraph(g,NULL))
  
  l <- c(1,2)
  expect_error(EvaluateFormula.ProgressionGraph(g,l))
  
  names(l) <- c("x","r")
  expect_error(EvaluateFormula.ProgressionGraph(g,l))
  names(l) <- c("x","y")
  
  h <- EvaluateFormula.ProgressionGraph(g,l)
  expect_equal(4,edgeData(h$graph,from="a.2",to="a.3",attr="hazard")[[1]])
  
  f2 <- function(pars){"boo"}
  h <- InsertRate.ProgressionGraph(g,"b.2","b.3",f2)
  expect_error( EvaluateFormula.ProgressionGraph(h,l))
  
  f3 <- function(pars){(pars["x"]-2) }
  h <- InsertRate.ProgressionGraph(g,"b.2","b.3",f3)
  
  expect_error(EvaluateFormula.ProgressionGraph(h,l)) #Negative value of formula
  l[["x"]] <- 4
  
  j <- EvaluateFormula.ProgressionGraph(h,l)
  expect_equal(2,edgeData(j$graph,from="b.2",to="b.3",attr="hazard")[[1]])
  expect_equal(10,edgeData(j$graph,from="a.2",to="a.3",attr="hazard")[[1]])
  expect_equal(5,edgeData(j$graph,from="a.1",to="a.2",attr="hazard")[[1]])
  expect_equal(0,edgeData(j$graph,from="a.2",to="death",attr="hazard")[[1]])
})


test_that("IsEdge",{
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"), 
                                   armProgression=c("1","2","3"),edge="death")
  
  expect_error(IsEdge.ProgressionGraph(g,"a.1",c("b.1","c.1")))
  expect_false(IsEdge.ProgressionGraph(g,"a","b.1"))
  
  expect_true(IsEdge.ProgressionGraph(g,"a.1","a.2"))
  expect_true(IsEdge.ProgressionGraph(g,"a.1","death"))
  expect_false(IsEdge.ProgressionGraph(g,"a.2","a.1"))
  expect_true(IsEdge.ProgressionGraph(g,c("a.1","b.2"),c("death","b.3")))
  expect_false(IsEdge.ProgressionGraph(g,c("a.1","b.2"),c("death","b.2")))
  
})


test_that("GeneratRateMatrix",{
  
  g <- InitializeProgressionGraph()
  g <- AddNode.ProgressionGraph(g,"death")
  g <- AddArm.ProgressionGraph(g,armName="a",armProgression=c("1","2","3"),deathNode="death",edges="all")
  
  f <- function(pars){return(pars[["x"]])}
  
  g <- InsertRate.ProgressionGraph(g,"a.1","a.2",1)
  g <- InsertRate.ProgressionGraph(g,"a.1","a.3",2)
  g <- InsertRate.ProgressionGraph(g,"a.1","death",3)
  g <- InsertRate.ProgressionGraph(g,"a.2","a.3",4)
  g <- InsertRate.ProgressionGraph(g,"a.2","death",5)
  g <- InsertRate.ProgressionGraph(g,"a.3","death",f)
  
  expect_error(GeneratRateMatrix.ProgressionGraph(g)) #formula not evaluated
  
  l <- c(2.4)
  names(l) <- c("x")
  g <- EvaluateFormula.ProgressionGraph(g,l)
  
  m <- GeneratRateMatrix.ProgressionGraph(g)
  
  mtrue <- matrix(rep(0,16),nrow=4)
  
  mtrue[2,1] <- 3
  mtrue[2,3] <- 1
  mtrue[2,4] <- 2
  mtrue[3,1] <- 5
  mtrue[3,4] <- 4
  mtrue[4,1] <- 2.4
  
  expect_equal(mtrue,m)
  

})


test_that("SetIsVisitNode + other isvisitnode testing",{
  g <- InitializeProgressionGraph()
  g <- AddNode.ProgressionGraph(g,c("1","2","3"))

  expect_error(SetIsVisitNode.ProgressionGraph(g,4,nodeNames="1"))
  expect_error(SetIsVisitNode.ProgressionGraph(g,c(TRUE,FALSE),nodeNames="1"))
  
  expect_false(nodeData(g$graph,"1","isVisitNode")[[1]])
  
  h <- SetIsVisitNode.ProgressionGraph(g,TRUE) #all
  expect_true(nodeData(h$graph,"1","isVisitNode")[[1]])
  expect_true(nodeData(h$graph,"2","isVisitNode")[[1]])
  expect_true(nodeData(h$graph,"3","isVisitNode")[[1]])
  
  h <- SetIsVisitNode.ProgressionGraph(h,FALSE,nodeNames=c("1","2"))
  expect_false(nodeData(h$graph,"1","isVisitNode")[[1]])
  expect_false(nodeData(h$graph,"2","isVisitNode")[[1]])
  expect_true(nodeData(h$graph,"3","isVisitNode")[[1]])
  
  h <- AddNode.ProgressionGraph(g,c("A","B","C"),isVisitNode=TRUE)
  expect_false(nodeData(h$graph,"1","isVisitNode")[[1]])
  expect_false(nodeData(h$graph,"2","isVisitNode")[[1]])
  expect_false(nodeData(h$graph,"3","isVisitNode")[[1]])
  expect_true(nodeData(h$graph,"A","isVisitNode")[[1]])
  expect_true(nodeData(h$graph,"B","isVisitNode")[[1]])
  expect_true(nodeData(h$graph,"C","isVisitNode")[[1]])
  
  h <- AddNode.ProgressionGraph(g,c("A2","B2","C2"),isVisitNode=FALSE)
  expect_false(nodeData(h$graph,"A2","isVisitNode")[[1]])
  expect_false(nodeData(h$graph,"B2","isVisitNode")[[1]])
  expect_false(nodeData(h$graph,"C2","isVisitNode")[[1]])
  
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"), 
                                   armProgression=c("1","2","3"),edge="death")
  expect_false(nodeData(g$graph,"a.2","isVisitNode")[[1]])
  expect_false(nodeData(g$graph,"death","isVisitNode")[[1]])
  
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"), 
                                   armProgression=c("1","2","3"),edge="death",isVisitNode=TRUE)
  expect_true(nodeData(g$graph,"a.2","isVisitNode")[[1]])
  expect_true(nodeData(g$graph,"death","isVisitNode")[[1]])
  
})

test_that("AddVisitSchedule + other visit schedule testing",{
  g <- InitializeProgressionGraph()
  
  v1 <- NewVisitSchedule()
  v2 <- NewVisitSchedule(frequencies=c(2,4,10),numberVisits=c(4,1,Inf))
  g <- AddNode.ProgressionGraph(g,c("1","2","3"))

  expect_error(AddVisitSchedule.ProgressionGraph(g,3,nodeNames="1"))
  expect_error(AddVisitSchedule.ProgressionGraph(g,c(v1,v2),nodeNames="1"))
  expect_error(AddVisitSchedule.ProgressionGraph(g,v1,nodeNames="death"))
  
  h <- AddVisitSchedule.ProgressionGraph(g,v2) #nodeNames = "all"
  expect_equal(nodeData(h$graph,"1","visitSchedule")[[1]],v2)
  expect_equal(nodeData(h$graph,"2","visitSchedule")[[1]],v2)
  expect_equal(nodeData(h$graph,"3","visitSchedule")[[1]],v2)
  
  h <- AddVisitSchedule.ProgressionGraph(h,v1,nodeNames=c("1","2"))
  expect_equal(nodeData(h$graph,"1","visitSchedule")[[1]],v1)
  expect_equal(nodeData(h$graph,"2","visitSchedule")[[1]],v1)
  expect_equal(nodeData(h$graph,"3","visitSchedule")[[1]],v2) 
  
  g <- AddNode.ProgressionGraph(g,c("A","B","C"),visitSchedule=v2)
  expect_equal(nodeData(g$graph,"1","visitSchedule")[[1]],v1)
  expect_equal(nodeData(g$graph,"2","visitSchedule")[[1]],v1)
  expect_equal(nodeData(g$graph,"3","visitSchedule")[[1]],v1) 
  expect_equal(nodeData(g$graph,"A","visitSchedule")[[1]],v2)
  expect_equal(nodeData(g$graph,"B","visitSchedule")[[1]],v2)
  expect_equal(nodeData(g$graph,"C","visitSchedule")[[1]],v2) 
  
  v3 <- NewVisitSchedule(frequencies=c(4,10),numberVisits=c(4,Inf))
  
  h <- AddArm.ProgressionGraph(g,armName="a",armProgression=c("1","2","3"),deathNode="A",edges="all")
  expect_equal(nodeData(h$graph,"a.1","visitSchedule")[[1]],v1)
  
  h <- AddArm.ProgressionGraph(g,armName="a",armProgression=c("1","2","3"),deathNode="A",edges="all",visitSchedule=v3)
  expect_equal(nodeData(h$graph,"a.1","visitSchedule")[[1]],v3)
  expect_equal(nodeData(h$graph,"a.3","visitSchedule")[[1]],v3)
  expect_equal(nodeData(h$graph,"A","visitSchedule")[[1]],v2)
  
  
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"), 
                                   armProgression=c("1","2","3"),edge="death")
  expect_equal(nodeData(g$graph,"a.1","visitSchedule")[[1]],v1)
  expect_equal(nodeData(g$graph,"death","visitSchedule")[[1]],v1)
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"), 
                                   armProgression=c("1","2","3"),edge="death",visitSchedule=v3)
  expect_equal(nodeData(g$graph,"a.1","visitSchedule")[[1]],v3)
  expect_equal(nodeData(g$graph,"death","visitSchedule")[[1]],v3)
})


test_that("SetShape",{
  g <- InitializeProgressionGraph()
  g <- AddNode.ProgressionGraph(g,"death")
  g <- AddArm.ProgressionGraph(g,armName="a",armProgression=c("1","2","3"),deathNode="death",edges="all")
  
  expect_equal(edgeData(g$graph,"a.1","a.2","shape")[[1]],1)
  expect_equal(edgeData(g$graph,"a.2","death","shape")[[1]],1)
  expect_error(SetShape.ProgressionGraph(g,"a.1",shape=6))
  expect_error(SetShape.ProgressionGraph(g,from="a.1",to="death",c(5,6,7)))
  
  expect_error(SetShape.ProgressionGraph(g,from="a.1",to="death",-5))
  g <- SetShape.ProgressionGraph(g,from="a.1",to="death",0.5)
  g <- SetShape.ProgressionGraph(g,from="a.2",to="death",1.5)      
  
  expect_equal(edgeData(g$graph,from="a.1",to="death","shape")[[1]],0.5)
  expect_equal(edgeData(g$graph,from="a.2",to="death","shape")[[1]],1.5)
  expect_equal(edgeData(g$graph,from="a.3",to="death","shape")[[1]],1)
  expect_equal(edgeData(g$graph,from="a.2",to="a.3","shape")[[1]],1)
  
  
  
})

test_that("IsResetEdge",{
  g <- InitializeProgressionGraph()
  g <- AddNode.ProgressionGraph(g,"death")
  g <- AddArm.ProgressionGraph(g,armName="a",armProgression=c("1","2","3"),deathNode="death",edges="all")
  
  expect_false(edgeData(g$graph,"a.1","a.2","isResetEdge")[[1]])
  
  expect_error(SetIsResetEdge(g,c("a.1","a.1"),c("a.2","death"),c(TRUE,FALSE)))
  expect_error(SetIsResetEdge(g,c("a.1","a.1"),c("a.2"),TRUE))
  
  expect_equal(0,length(GetResetEdges.ProgressionGraph(g)))
  
  g <- SetIsResetEdge.ProgressionGraph(g,c("a.1","a.1"),c("a.2","death"),TRUE)
  expect_true(edgeData(g$graph,"a.1","a.2","isResetEdge")[[1]])
  expect_true(edgeData(g$graph,"a.1","death","isResetEdge")[[1]])
  expect_false(edgeData(g$graph,"a.1","a.3","isResetEdge")[[1]])
  
  expect_equal(c(2,3,2,1),GetResetEdges.ProgressionGraph(g))
  
  g <- AddNode.ProgressionGraph(g,c("boo"))
  g <- AddEdge.ProgressionGraph(g,"boo","death",isResetEdge=TRUE)
  expect_true(edgeData(g$graph,"boo","death","isResetEdge")[[1]])
  
  expect_equal(c(2,3,2,1,5,1),GetResetEdges.ProgressionGraph(g))
  
  g <- SetIsResetEdge.ProgressionGraph(g,c("a.1"),c("a.2"),FALSE)
  expect_false(edgeData(g$graph,"a.1","a.2","isResetEdge")[[1]])
  expect_true(edgeData(g$graph,"a.1","death","isResetEdge")[[1]])
  expect_false(edgeData(g$graph,"a.1","a.3","isResetEdge")[[1]])
  
  expect_equal(c(2,1,5,1),GetResetEdges.ProgressionGraph(g))
  
  g <- SimpleStudyProgressionGraph(arms=c("a","b","c"), 
                                   armProgression=c("1","2","3"),edge="death",
                                   crossOver=c("a.1","b.2","b.3","c.3"))
  
  expect_true(edgeData(g$graph,"a.1","b.2","isResetEdge")[[1]])
  expect_true(edgeData(g$graph,"b.3","c.3","isResetEdge")[[1]])
  expect_false(edgeData(g$graph,"a.1","a.2","isResetEdge")[[1]])

  expect_equal(c(2,6,7,10),GetResetEdges.ProgressionGraph(g))

})


test_that("MaxOneInfPerRow",{
  x <- matrix(rep(0,25),nrow=5)
  expect_true(MaxOneInfPerRow(x))
  
  x[1,1] <- 500
  x[1,2] <- 30
  expect_true(MaxOneInfPerRow(x))
  
  x[1,4] <- Inf
  expect_true(MaxOneInfPerRow(x))
  x[5,4] <- Inf
  expect_true(MaxOneInfPerRow(x))
  x[2,2] <- Inf
  x[3,2] <- Inf
  expect_true(MaxOneInfPerRow(x))
  x[4,1] <- Inf
  expect_true(MaxOneInfPerRow(x))
  x[3,3] <- Inf
  expect_false(MaxOneInfPerRow(x))
  
  x <- matrix(rep(0,25),nrow=5)
  x[1,1] <- Inf
  x[1,2] <- Inf
  expect_false(MaxOneInfPerRow(x))
})
