#include<algorithm>
#include<iostream>

#include"transitions.h"

Transitions::Transitions(const int n_state, Rcpp::NumericVector rates, Rcpp::NumericVector patientEndTimes,
Rcpp::NumericVector calendarEndTimes, Rcpp::NumericMatrix shape, Rcpp::NumericVector resetEdges):
n_state(n_state),
shape(shape),
patientIndexTimes(patientEndTimes),
calendarIndexTimes(calendarEndTimes),
rates(rates)
{
    if(shape.nrow() != n_state || shape.ncol() != n_state){
        throw std::runtime_error("invalid number of shape parameters");
    }
    
    if (patientEndTimes.length() != calendarEndTimes.length()) {
        throw std::runtime_error("patient end times and calendar end times size mismatch");
    }
    
    int num_rates = patientEndTimes.length() * n_state * n_state;
    
    if(num_rates != rates.size()){
        throw std::runtime_error("incorrect number of rates");  
    }
    
    if(resetEdges.length() % 2 != 0){
        throw std::runtime_error("incorrect length of resetEdges");
    }
    
    //calendarSwitches = which indexes in calendarIndexTimes, represent patient Time = 0 
    calendarSwitches.push_back(0); 
    for(R_len_t i=0; i + 1 != calendarEndTimes.length(); ++i){
        if(calendarEndTimes[i] != calendarEndTimes[i+1] ){
          calendarSwitches.push_back(i+1);
        }  
    }  
    
    // The unique values in calendarIndexTimes
    for(R_len_t i=0; i + 1 != calendarEndTimes.length(); ++i){
      if(calendarEndTimes[i] != calendarEndTimes[i+1] ){
          double toPush = ( calendarEndTimes[i] == R_PosInf ? INFINITY : calendarEndTimes[i]) ;
          calendarTimes.push_back(toPush);
         
      }
    }
    if(calendarTimes.empty()|| calendarTimes.back() != INFINITY)  calendarTimes.push_back(INFINITY);
    
    for(int i = 0; i < resetEdges.length(); i+=2 ){
        resetEdgesFrom.push_back(resetEdges[i]);
        resetEdgesTo.push_back(resetEdges[i+1]);
    }
}


bool Transitions::resetPatientTimeForSwitches(int fromState, int toState){
    for(int i = 0; i < resetEdgesFrom.size(); i++) {
      if(resetEdgesFrom[i] == fromState && resetEdgesTo[i] == toState) return true;
    }
    return false;
}



double Transitions::getRate(const SubjectTime & sTime, const int current_state, int out_state){
  double currentCalendarTime = sTime.getCurrentCalendarTime();
  double timeSinceLastSwitchReset = sTime.getTimeSinceLastSwitchReset();
  
  int index = getIndex(timeSinceLastSwitchReset,currentCalendarTime);                                           

  return(rates[index * n_state*n_state + n_state*(current_state-1)+ out_state -1]);
}



//which rate matrix should be used for the given patient and calendar time
int Transitions::getIndex(double currentPatientTime, double currentCalendarTime){
   int pos = getCalendarStartPos(currentCalendarTime); //index for patientTime = 0 for currentCalendarTime
    while(pos < patientIndexTimes.length()){
       if(currentPatientTime <  patientIndexTimes[pos]) return pos;
       pos++;
   }
   throw std::runtime_error("bad index");
}

//time until next switchpoint
double Transitions::getNextSwitch(const SubjectTime & sTime ){
    
    double currentCalendarTime = sTime.getCurrentCalendarTime();
    double timeSinceLastSwitchReset = sTime.getTimeSinceLastSwitchReset();
    
    return std::min(getNextPatientSwitch(timeSinceLastSwitchReset,currentCalendarTime),
                     getNextCalendarSwitch(currentCalendarTime) );
    

}

//time until next switchpoint due to patient time crossing a boundary
double Transitions::getNextPatientSwitch(double currentPatientTime ,double currentCalendarTime){
    int startPos = getCalendarStartPos(currentCalendarTime); //index for patientTime = 0 for currentCalendarTime
    for(int i = startPos; patientIndexTimes[i] != R_PosInf; i++){
        if(currentPatientTime < patientIndexTimes[i] ){
            return patientIndexTimes[i] - currentPatientTime;
        }
      
      
    }
    
    return INFINITY;
  
   
  
}


//time until next switchpoint due to calendar time crossing a boundary
double Transitions::getNextCalendarSwitch(double currentCalendarTime){
    for(int i = 0; i < calendarTimes.size()-1; i++){
        if(currentCalendarTime < calendarTimes[i]){
            return calendarTimes[i] - currentCalendarTime;
        }
    
    }
    return INFINITY;
}


//return the index of the rate matrix for the given currentCalendarTime
//with patientTime = 0
int Transitions::getCalendarStartPos(double currentCalendarTime){
    for(int i = 0; i < calendarTimes.size(); i++){
        if(currentCalendarTime < calendarTimes[i]){
            return calendarSwitches[i]; 
        }
    }
}


int Transitions::ProposeNewState(double & out_time, const SubjectTime & sTime, int current_state){
  out_time = INFINITY;
  
  int ans = 0;
  for(int i = 1; i <= n_state; i++){
     double rate = getRate(sTime, current_state, i);
     
     if(rate != 0){
       double shape = getShape(current_state,i);
       double proposed_out_time = conditionalWeibull_timeToTransition(rate,shape,sTime.getCurrentPatientTime());
     
       if(proposed_out_time < out_time){
         out_time = proposed_out_time;
         ans = i;
       }
     
     }
    
  }
  return ans;

}


//see transitions.h file
double Transitions::conditionalWeibull_timeToTransition(double rate, double shape, double currentTime){
  if(rate == INFINITY){
    return 0;
  }
  if(shape == 1){ //for efficiency, could remove these three lines and code is unchanged 
     return R::exp_rand() / rate;
  }
  double ans = pow(currentTime*rate,shape);
  ans += R::exp_rand();
  ans = pow(ans,1.0/shape);
  ans /= rate;
  ans -= currentTime;
  return ans;
  
}

