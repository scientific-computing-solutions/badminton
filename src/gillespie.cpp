#include <exception>
#include <vector>
#include <numeric>

#include <iostream>

#include <Rcpp.h>
#include "transitions.h"


//' The Gillespie algorithm for simulating continuous time Markov chains.
//'
//' This function is called by \code{\link{Simulate.EventSim}} 
//'
//' @param start_states an integer vector giving the start states for each patient
//' @param start_times a numeric vector containing the calendar time at which each patient is recruited 
//' @param n_state an integer, the number of states on the underlying DAG 
//' @param rates A list of \code{n_state * n_state * length(patientEndTime)} transition rates
//' @param patientEndTimes The ith \code{n_state*n_state} block of rates represents the transition
//' rate matrix until patient time patientEndTimes[i]
//' @param calendarEndTimes The ith \code{n_state*n_state} block of rates represents the transition
//' rate matrix until calendar time calendarEndTimes[i]
//' @param shape The shape parameter for the edges in the DAG. A matrix where shape[i,j] is the Weibull
//' shape parameter for the edge from node i to node j. If there is no edge between i and j
//' then shape[i,j] = 0.
//' @param resetEdges A vector (from1,to1,from2,to2,...) of edges for which if a subject traverses 
//' one of these edges then patient time switches are reset. See \code{SetIsResetEdge.ProgressionGraph} for further 
//' details
//' @param duration The total (patient) time each subject is to be simulated 
//' @return A data frame with columns "id", "state" and "patient_transition_time" listing the (patient) time at
//' which patients transition into new states. For example
//' 
//' id state patient_transition_time
//' 1   1      0.0
//' 1   2      1.4
//' 
//' patient 1 transitions to state 1 at time 0 (w.r.t. patient time) and then at time 1.4 transitions
//' to state 2
//[[Rcpp::export]]
Rcpp::DataFrame gillespie(Rcpp::IntegerVector start_states,Rcpp::NumericVector start_times,
                          const int n_state,Rcpp::NumericVector rates, Rcpp::NumericVector patientEndTimes,
                          Rcpp::NumericVector calendarEndTimes,Rcpp::NumericMatrix shape, Rcpp::NumericVector
                          resetEdges, const double duration){
    //set up output vectors
    std::vector<int> Id;
    std::vector<int> transition_to_states;
    std::vector<double> transition_times;
    
    //Random number generator
    Rcpp::RNGScope scope;
    
    //lots of validation and set up here
    Transitions transitions(n_state,rates,patientEndTimes,calendarEndTimes,shape,resetEdges);
    
    //for each subject
    for(R_len_t subject_id=0; subject_id != start_states.length(); ++subject_id){
        
        //Initialize patient-specific variables
        int current_Id = subject_id+1;
        int current_state = start_states[subject_id];
                
        SubjectTime subjectTime(start_times[subject_id]);
              
        //store subject recruitment in output vectors
        Id.push_back(current_Id);
        transition_to_states.push_back(current_state);
        transition_times.push_back(0);
    
        //get intial transition from current state         
        double out_time=0; 
        double time_left_before_switch = transitions.getNextSwitch(subjectTime);
        int proposed_new_state = transitions.ProposeNewState(out_time,subjectTime, current_state);
        
            
        //keep going until, no more rate switching and the rate leaving current state = 0
        while(!(out_time == INFINITY && time_left_before_switch == INFINITY ) &&
                subjectTime.getCurrentPatientTime()  <=  duration){
            
            //std::cout << time_left_before_switch << " " << out_time << " " << subjectTime.getCurrentPatientTime() 
            //          << " " << proposed_new_state << std::endl;
                        
            if(out_time >= time_left_before_switch ||
               subjectTime.getCurrentPatientTime() + out_time > duration){ //subject switch rate matrices before transition
                 subjectTime.IncreasePatientTime(time_left_before_switch);   
                            
            }
            else{ //subject transitions at time current_patient_time + out_time
                subjectTime.IncreasePatientTime(out_time);
                
                int old_state = current_state;
               
                current_state = proposed_new_state;
               
                if(current_state != old_state){
                    Id.push_back(current_Id);
                    transition_to_states.push_back(current_state);
                    transition_times.push_back(subjectTime.getCurrentPatientTime());
                }
                //are we reseting the patient time switches (i.e. have we crossed 
                //an edge with isResetEdge = TRUE?)
                if(transitions.resetPatientTimeForSwitches(old_state,current_state)){
                    subjectTime.ResetPatientSwitches();
                }
                          
                
            }
            
             proposed_new_state = transitions.ProposeNewState(out_time,subjectTime, current_state);
             time_left_before_switch = transitions.getNextSwitch(subjectTime );
             
             //numerical fix
             if(time_left_before_switch < 1e-10){
                subjectTime.IncreasePatientTime(1e-10); 
               time_left_before_switch = transitions.getNextSwitch(subjectTime );
               proposed_new_state = transitions.ProposeNewState(out_time,subjectTime, current_state);
             }
             
            
             
        }
    }
    return Rcpp::DataFrame::create(Rcpp::Named("id")= Id, 
                                   Rcpp::Named("state") =transition_to_states, 
                                   Rcpp::Named("patient_transition_time")=transition_times );
}




