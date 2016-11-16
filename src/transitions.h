#include<vector>
#include<limits>
#include<Rcpp.h>

#include"subjectTime.h"

/* A class which contains all the rates for the event simulator
It calculates the amount of time left until the rate functions change,
and can return the current rate functions given a state, currentPatientTime
and patient Recruitment Time*/
class Transitions{
public:
    /*Constructor, takes arguments directly from the arguments passed from the R code */
    Transitions(const int n_state,
		Rcpp::NumericVector rates,
		Rcpp::NumericVector patientEndTimes,
                Rcpp::NumericVector calendarEndTimes,
		Rcpp::NumericMatrix shape,
		Rcpp::NumericVector resetEdges);
    
    /*This returns amount of time left until the current rate matrix changes */
    double getNextSwitch(const SubjectTime & sTime );
    
       
    //expects state numbered 1,2...
    double getShape(int from_state, int to_state){return shape(from_state-1,to_state-1);}
    
    //have we crossed an edge with isResetEdge =TRUE?
    bool resetPatientTimeForSwitches(int fromState, int toState);
    
    /*The function proposes a transition (if this would occur before the next rate switch
    point then the transition will occur). The proposed state is returned and the 
    out_time variable will contain the amount of time from currentPatientTime until
    the proposed transition would occur
    */
    int ProposeNewState(double & out_time, const SubjectTime & sTime, int current_state);
  
 
    
private:
    int n_state; //The number of states on the underlying DAG
    Rcpp::NumericMatrix shape; // The Weibull shape parameters see gillespie.cpp
    Rcpp::NumericVector patientIndexTimes; //See gillespie.cpp
    Rcpp::NumericVector calendarIndexTimes; // See gillespie.cpp
    Rcpp::NumericVector rates; // See  gillespie.cpp
    
    std::vector<double> calendarTimes; // The unique values in calendarIndexTimes 
    std::vector<int> calendarSwitches; //which indexes in calendarIndexTimes, represent patient Time = 0
    
    std::vector<int> resetEdgesFrom; 
    std::vector<int> resetEdgesTo;
    
    
    double getNextPatientSwitch(double currentPatientTime,
				double currentCalendarTime);
    double getNextCalendarSwitch(double currentCalendarTime);
    int getCalendarStartPos(double currentCalendarTime);
    int getIndex(double currentPatientTime, double currentCalendarTime);
    
    /*  Sample from a Weibull distribution with rate, shape parameters subject to 
    *   the value being greater than currentTime. We then take off currentTime
    *   so that the proposed transition time (used in gillespie) is given by the return value of this
    *   function + currentPatientTime
    *  
    *   The formula is given by {[(currentTime*rate)^shape - log(U)]^[1/shape]}/rate - currentTime
    *   where U is U[0,1]
    *   
    */
    double conditionalWeibull_timeToTransition(double rate,
					       double shape,
					       double currentTime);
    
    /* Get the appropriate rate value from the rates vector for current_state -> out_state
    at sTime.get[Patient/Calendar]Time().
    */
    double getRate(const SubjectTime & sTime, const int current_state, int out_state);
    
};
