context("test Switches")

test_that("ValidateSwitchTimes",{
    times <- c(4,5,3.5)
    expect_equal(ValidateSwitchTimes(times),c(0,3.5,4,5))
    times <- c("x")
    expect_error(ValidateSwitchTimes(times))
    times <- c(-5,10)
    expect_error(ValidateSwitchTimes(times))
    times <- c(0,1,2,3)
    expect_error(ValidateSwitchTimes(times))
    times <- c(Inf,4,5,6)
    expect_error(ValidateSwitchTimes(times))
    times <- c(4,5,6,8,4)
    expect_error(ValidateSwitchTimes(times))
    times <- c(20,50,40,30,1000.256,0.5)
    expect_equal(ValidateSwitchTimes(times),c(0,0.5,20,30,40,50,1000.256))
})

test_that("Match with exception",{
   f <- c(1,2,3,4,5,6,6.5,7)
   expect_equal(MatchWithException(1,f),1)
   expect_equal(MatchWithException(7,f),8)
   expect_error(MatchWithException(8,f,errMsg="boo"))
   expect_equal(MatchWithException("7",f,errMsg="boo"),8)
   expect_error(MatchWithException(6.999,f,errMsg="boo"))
   #No testing of what happens if f contains duplicates as not used 
  
})


test_that("InitializeStudySwitches",{
    s <- InitializeStudySwitches() #default no switches
    expect_equal(s$calendarTimes,0)
    expect_equal(length(s$patientTimes),1)
    expect_equal(s$patientTimes[[1]],0)

    
    expect_error(InitializeStudySwitches(Inf))
    expect_error(InitializeStudySwitches(-6))
    expect_error(InitializeStudySwitches(c(0,5)))
    
    s <- InitializeStudySwitches(10)
    expect_equal(s$calendarTimes,c(0,10))
    expect_equal(length(s$patientTimes),2)
    expect_equal(s$patientTimes[[1]],0)
    expect_equal(s$patientTimes[[2]],0)
    
    
    s <- InitializeStudySwitches(c(10,20))
    expect_equal(s$calendarTimes,c(0,10,20))
    expect_equal(length(s$patientTimes),3)
    expect_equal(s$patientTimes[[1]],0)
    expect_equal(s$patientTimes[[2]],0)
    expect_equal(s$patientTimes[[3]],0)
}
)

test_that("SetSubjectSwitchTimes",{
    s <- InitializeStudySwitches(c(10,20))
    expect_error(SetSubjectSwitchTimes.Switches(s,1))
    expect_error(SetSubjectSwitchTimes.Switches(s,10,c(Inf)))
    expect_error(SetSubjectSwitchTimes.Switches(s,10,0))
    
    s <-SetSubjectSwitchTimes.Switches(s,0,2.5)
    expect_equal(s$patientTimes[[1]],c(0,2.5))
    
    s <-SetSubjectSwitchTimes.Switches(s,10,c(2.5,1))
    expect_equal(s$patientTimes[[2]],c(0,1,2.5))
    
    s <-SetSubjectSwitchTimes.Switches(s,10)
    expect_equal(s$patientTimes[[2]],0)
    
})


test_that("GetCalendarSwitchIndexes and numberRateMatrices",{
  s <- InitializeStudySwitches()
  expect_equal(GetCalendarSwitchIndexes.Switches(s),c(1))
  expect_equal(NumberOfRateMatrices.Switches(s),1)
  
  s <- InitializeStudySwitches(c(10,20))
  expect_equal(GetCalendarSwitchIndexes.Switches(s),c(1,2,3))
  expect_equal(NumberOfRateMatrices.Switches(s),3)
  
  s <-SetSubjectSwitchTimes.Switches(s,10,c(2,1))
  expect_equal(GetCalendarSwitchIndexes.Switches(s),c(1,4,5))
  expect_equal(NumberOfRateMatrices.Switches(s),5)
  
  s <-SetSubjectSwitchTimes.Switches(s,0,c(3,4,5,2,1))
  expect_equal(GetCalendarSwitchIndexes.Switches(s),c(6,9,10))
  expect_equal(NumberOfRateMatrices.Switches(s),10)
  
  s <-SetSubjectSwitchTimes.Switches(s,20,c(3,4,5,2,1))
  expect_equal(GetCalendarSwitchIndexes.Switches(s),c(6,9,15))
  expect_equal(NumberOfRateMatrices.Switches(s),15)
})

test_that("GetCalendar+PatientEndTimes+IndexSwitches",{
    s <- InitializeStudySwitches()
    expect_equal(GetCalendarEndTimes.Switches(s),Inf)
    expect_equal(GetPatientEndTimes.Switches(s),Inf)
    
    expect_equal(GetIndex.Switches(s,0,0),1)
        
    s <- InitializeStudySwitches(c(10,20))
    expect_equal(GetCalendarEndTimes.Switches(s),c(10,20,Inf))
    expect_equal(GetPatientEndTimes.Switches(s),c(Inf,Inf,Inf))
    
    expect_equal(GetIndex.Switches(s,0,0),1)
    expect_equal(GetIndex.Switches(s,10,0),2)
    expect_equal(GetIndex.Switches(s,20,0),3)
    
    s <- InitializeStudySwitches(c(10,20,50))
    expect_equal(GetCalendarEndTimes.Switches(s),c(10,20,50,Inf))
    expect_equal(GetPatientEndTimes.Switches(s),c(Inf,Inf,Inf,Inf))
    
    expect_equal(GetIndex.Switches(s,0,0),1)
    expect_equal(GetIndex.Switches(s,10,0),2)
    expect_equal(GetIndex.Switches(s,50,0),4)
    
    s <-SetSubjectSwitchTimes.Switches(s,20,c(10,20,30,40))
    expect_equal(GetCalendarEndTimes.Switches(s),c(10,20,50,50,50,50,50,Inf))
    expect_equal(GetPatientEndTimes.Switches(s),c(Inf,Inf,10,20,30,40,Inf,Inf))
    
    expect_equal(GetIndex.Switches(s,0,0),1)
    expect_equal(GetIndex.Switches(s,20,0),3)
    expect_equal(GetIndex.Switches(s,20,20),5)
    expect_equal(GetIndex.Switches(s,20,40),7)
    expect_equal(GetIndex.Switches(s,50,0),8)
    
    s <-SetSubjectSwitchTimes.Switches(s,0,c(1))
    expect_equal(GetCalendarEndTimes.Switches(s),c(10,10,20,50,50,50,50,50,Inf))
    expect_equal(GetPatientEndTimes.Switches(s),c(1,Inf,Inf,10,20,30,40,Inf,Inf))
    
    expect_equal(GetIndex.Switches(s,0,1),2)
    expect_equal(GetIndex.Switches(s,20,0),4)
    expect_equal(GetIndex.Switches(s,20,20),6)
    expect_equal(GetIndex.Switches(s,20,40),8)
    expect_equal(GetIndex.Switches(s,50,0),9)
    
    expect_error(GetIndex.Switches(s,Inf,0))
    expect_error(GetIndex.Switches(s,11,0))
    expect_error(GetIndex.Switches(s,20,1))
    expect_error(GetIndex.Switches(s,0,Inf))
    
    s <-SetSubjectSwitchTimes.Switches(s,50,c(30,40))
    expect_equal(GetCalendarEndTimes.Switches(s),c(10,10,20,50,50,50,50,50,Inf,Inf,Inf))
    expect_equal(GetPatientEndTimes.Switches(s),c(1,Inf,Inf,10,20,30,40,Inf,30,40,Inf))
    
    expect_equal(GetIndex.Switches(s,0,1),2)
    expect_equal(GetIndex.Switches(s,20,0),4)
    expect_equal(GetIndex.Switches(s,20,20),6)
    expect_equal(GetIndex.Switches(s,20,40),8)
    expect_equal(GetIndex.Switches(s,50,0),9)
    expect_equal(GetIndex.Switches(s,50,30),10)
    expect_equal(GetIndex.Switches(s,50,40),11)
    
    s <-SetSubjectSwitchTimes.Switches(s,10,c(1,30,40))
    expect_equal(GetCalendarEndTimes.Switches(s),c(10,10,20,20,20,20,50,50,50,50,50,Inf,Inf,Inf))
    expect_equal(GetPatientEndTimes.Switches(s),c(1,Inf,1,30,40,Inf,10,20,30,40,Inf,30,40,Inf))
    
    s <-SetSubjectSwitchTimes.Switches(s,10)
    expect_equal(GetCalendarEndTimes.Switches(s),c(10,10,20,50,50,50,50,50,Inf,Inf,Inf))
    expect_equal(GetPatientEndTimes.Switches(s),c(1,Inf,Inf,10,20,30,40,Inf,30,40,Inf))
})

