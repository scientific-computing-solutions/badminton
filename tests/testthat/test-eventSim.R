context("event sim tests")

test_that("GenerateStartStateVector",{
  
  nN <- c("a","b","c")
  
  expect_error(GenerateStartStateVector(nN,NULL))
  
  expect_equal(c(1,1,1,1,2,2),GenerateStartStateVector(nN,c("a","4","b","2")))
  expect_equal(c(2,3,1),GenerateStartStateVector(nN,c("b","1","c","1","a",1)))
  expect_equal(c(2,1),GenerateStartStateVector(nN,c("b","1","c",0,"a",1)))
  
  expect_error(GenerateStartStateVector(nN,c("d","1","c",0,"a",1)))
  expect_error(GenerateStartStateVector(nN,c("a","-1","c",-1,"b",1)))
  expect_error(GenerateStartStateVector(nN,c("a","1","c",1.5,"b",1)))
  expect_error(GenerateStartStateVector(nN,c("a","1","c",1,"b")))
  
  expect_equal(c(rep(1,100),rep(2,50),rep(3,50)),GenerateStartStateVector(nN,c("a",100,"b",50,"c",50)))
  expect_equal(c(1,1,1,3,1),GenerateStartStateVector(nN,c("a",3,"c",1,"a",1,"b",0)))
  

})



test_that("InitializeEventSim",{
  pG <- SimpleStudyProgressionGraph(arms=c("a","b","c"),
                                          armProgression=c("1","2","3"),edge="death")
  s <- InitializeStudySwitches(c(10,20))
  s <-SetSubjectSwitchTimes.Switches(s,20,c(10,20,30,40))
  
  rM <- simpleAccrual(4, 0.5)
  
  expect_error(InitializeEventSim(InitializeProgressionGraph(),s,rM))
  
  ES <- InitializeEventSim(pG,s,rM)
  
  expect_equal(7,ES$numberRateMatrices)
  expect_equal(s,ES$switches)
  expect_equal(rM,ES$recruitmentModel)
  expect_equal(rep(pG,7),ES$progressionGraph)
  expect_equal(list(),ES$params)
 
 
})


test_that("InsertParameter+GetParameters",{
  pG <- SimpleStudyProgressionGraph(arms=c("a","b","c"),
                                    armProgression=c("1","2","3"),edge="death")
  s <- InitializeStudySwitches(c(10,20))
  s <-SetSubjectSwitchTimes.Switches(s,20,c(10,20,30,40))
  
  rM <- simpleAccrual(4, 0.5)
  
  ES <- InitializeEventSim(pG,s,rM)
  
  expect_error( GetParameters.EventSim(ES,c(2)))
  
  ES <- InsertParameter.EventSim(ES,c("a","b"))
  expect_equal(list("a","b"),ES$params)
  
  expect_error(GetParameters.EventSim(ES,c(5,1,10)))
  
  g <- c(4,6.7)
  g2 <- GetParameters.EventSim(ES,g)
  names(g) <- c("a","b")
  expect_equal(g2,g)
  
  
  expect_error(InsertParameter.EventSim(ES,c("a")))
  
  ES <- InsertParameter.EventSim(ES,c(5,"d"))
  expect_equal(list("a","b","5","d"),ES$params)
  
  expect_error(GetParameters.EventSim(ES,g))
  g <- c(1,2,3,4)
  g2 <- GetParameters.EventSim(ES,g)
  names(g) <- c("a","b","5","d")
  expect_equal(g2,g)
  
})


test_that("system_level_test_rates",{
  pG <- SimpleStudyProgressionGraph(arms=c("a","b"),
                                    armProgression=c("1","2","3"),edge="all",crossOver=c("a.1","b.3"))
  
  s <- InitializeStudySwitches(c(10,20))
  s <-SetSubjectSwitchTimes.Switches(s,20,40)
  
  rM <- simpleAccrual(4, 0.5)
  
  ES <- InitializeEventSim(pG,s,rM)
  
  
  v1 <- 1
  v2 <- 2
  v3 <- 3
  v4 <- 4
  
  ES <- InsertRate.EventSim(ES,"a.1","death",calendarStartTime=NULL,patientStartTime=NULL,rate=v1)
  ES <- InsertRate.EventSim(ES,"a.1","a.2",calendarStartTime=NULL,patientStartTime=NULL,rate=v2)
  ES <- InsertRate.EventSim(ES,"a.1","a.3",calendarStartTime=NULL,patientStartTime=NULL,rate=v3)
  ES <- InsertRate.EventSim(ES,"a.1","b.3",calendarStartTime=NULL,patientStartTime=NULL,rate=v4)
  ES <- InsertRate.EventSim(ES,"a.2","death",calendarStartTime=NULL,patientStartTime=NULL,rate=v1)
  ES <- InsertRate.EventSim(ES,"a.2","a.3",calendarStartTime=NULL,patientStartTime=NULL,rate=v2)
  ES <- InsertRate.EventSim(ES,"a.3","death",calendarStartTime=NULL,patientStartTime=NULL,rate=v3)
  ES <- InsertRate.EventSim(ES,"b.1","death",calendarStartTime=NULL,patientStartTime=NULL,rate=v4)
  
  expect_error(InsertRate.EventSim(ES,"b.1","death",calendarStartTime=NULL,patientStartTime=40,rate=v4))
  expect_error(InsertRate.EventSim(ES,"b.1","death",calendarStartTime=0,patientStartTime=NULL,rate=v4))
  
  ES <- InsertRate.EventSim(ES,"b.1","b.2",calendarStartTime=0,patientStartTime=0,rate=0.5)
  ES <- InsertRate.EventSim(ES,"b.1","b.2",calendarStartTime=10,patientStartTime=0,rate=1.5)
  ES <- InsertRate.EventSim(ES,"b.1","b.2",calendarStartTime=20,patientStartTime=0,rate=2.5)
  ES <- InsertRate.EventSim(ES,"b.1","b.2",calendarStartTime=20,patientStartTime=40,rate=3.5)
  
  ES <- InsertRate.EventSim(ES,"b.1","b.3",calendarStartTime=0,patientStartTime=0,rate=4.5)
  ES <- InsertRate.EventSim(ES,"b.1","b.3",calendarStartTime=10,patientStartTime=0,rate=5.5)
  ES <- InsertRate.EventSim(ES,"b.1","b.3",calendarStartTime=20,patientStartTime=0,rate=6.5)
  ES <- InsertRate.EventSim(ES,"b.1","b.3",calendarStartTime=20,patientStartTime=40,rate=7.5)
  
  ES <- InsertRate.EventSim(ES,"b.2","death",calendarStartTime=0,patientStartTime=0,rate=8.5)
  ES <- InsertRate.EventSim(ES,"b.2","death",calendarStartTime=10,patientStartTime=0,rate=9.5)
  ES <- InsertRate.EventSim(ES,"b.2","death",calendarStartTime=20,patientStartTime=0,rate=10.5)
  ES <- InsertRate.EventSim(ES,"b.2","death",calendarStartTime=20,patientStartTime=40,rate=11.5)
  
  ES <- InsertRate.EventSim(ES,"b.2","b.3",calendarStartTime=0,patientStartTime=0,rate=12.5)
  ES <- InsertRate.EventSim(ES,"b.2","b.3",calendarStartTime=10,patientStartTime=0,rate=13.5)
  ES <- InsertRate.EventSim(ES,"b.2","b.3",calendarStartTime=20,patientStartTime=0,rate=14.5)
  ES <- InsertRate.EventSim(ES,"b.2","b.3",calendarStartTime=20,patientStartTime=40,rate=15.5)
  
  ES <- InsertParameter.EventSim(ES,c("x","y"))
  f1 <- function(pars){pars["x"]*pars["y"]}
  f2 <- function(pars){pars["x"]+pars["y"]}
  f3 <- function(pars){(1-pars["x"])*pars["y"]}
  f4 <- function(pars){(1-pars["x"])+pars["y"]}

  ES <- InsertRate.EventSim(ES,"b.3","death",calendarStartTime=0,patientStartTime=0,rate=f1)
  ES <- InsertRate.EventSim(ES,"b.3","death",calendarStartTime=10,patientStartTime=0,rate=f2)
  ES <- InsertRate.EventSim(ES,"b.3","death",calendarStartTime=20,patientStartTime=0,rate=f3)
  ES <- InsertRate.EventSim(ES,"b.3","death",calendarStartTime=20,patientStartTime=40,rate=f4)
  
  pars <- GetParameters.EventSim(ES,values=c(0.2,0.1))
    
  ES <- EvaluateFormula.EventSim(ES,pars)
    
  rates <- GenerateRates.EventSim(ES)
  expect_equal(196,length(rates))
  
  expect_equal(rep(0,28),c(rates[1:7],rates[50:56],rates[99:105],rates[148:154]))
  expect_equal(rep(v1,8),c(rates[8],rates[15],rates[57],rates[64],rates[106],rates[113],rates[155],rates[162]))
  expect_equal(rep(v4,8),c(rates[14],rates[29],rates[63],rates[78],rates[112],rates[127],rates[161],rates[176]))
  expect_equal(c(0.5,1.5,2.5,3.5),c(rates[34],rates[83],rates[132],rates[181]))
  
  expect_equal(0.02,rates[43])
  expect_equal(0.3,rates[92])
  expect_equal(0.08,rates[141])
  expect_equal(0.9,rates[190])
  
  
})


test_that("crossOver_functionals",{
  
  f1 <- crossOver_functional("h",rate=5)
  f2 <- non_crossOver_functional("h",rate=5)
  
  x <- c(0.5)
  names(x) <- "h"
  
  expect_equal(2.5,f1(x))
  expect_equal(2.5,f2(x))
  
  x[["h"]] <- 0
  expect_equal(0,f1(x))
  expect_equal(5,f2(x))
  
  x[["h"]] <- 1
  expect_equal(5,f1(x))
  expect_equal(0,f2(x))
  
  f1 <- crossOver_functional("h",rate=5,shape=2)
  f2 <- non_crossOver_functional("h",rate=5,shape=2)
  
  x[["h"]] <- 0.5

    
  expect_equal(sqrt(0.5)*5,f1(x))
  expect_equal(sqrt(0.5)*5,f2(x))
  
  x[["h"]] <- 0
  expect_equal(0,f1(x))
  expect_equal(5,f2(x))
  
  x[["h"]] <- 1
  expect_equal(5,f1(x))
  expect_equal(0,f2(x))
  
  x[["h"]] <- 0.75
  expect_equal(sqrt(0.75)*5,f1(x))
  expect_equal(sqrt(0.25)*5,f2(x))
  
  f1 <- crossOver_functional("h",rate=5,shape=0.9)
  f2 <- non_crossOver_functional("h",rate=5,shape=0.9)
  
  expect_equal(0.75^(1/0.9)*5,f1(x))
  expect_equal(0.25^(1/0.9)*5,f2(x))
  
})

