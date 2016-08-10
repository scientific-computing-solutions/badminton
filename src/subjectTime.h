#include<Rcpp.h>

/* Class which contains the current patient time for a single patient
simulation. It also contains the (calendar time) a patient is recruited and
the (patient) time at which the patient switches were last reset (i.e. the patient 
transitioned over an edge with isResetEdge property true) 
*/
class SubjectTime{
public:
  SubjectTime(double recruitTime):currentPatientTime(0),
                                  patientRecruitmentTime(recruitTime),
                                  currentPatientResetSwitchTime(0){}

  double getCurrentPatientTime()const{return currentPatientTime; }
  double getPatientRecruitmentTime()const{return patientRecruitmentTime;}  
  double getCurrentPatientResetSwitchTime()const{return currentPatientResetSwitchTime; }

  double getCurrentCalendarTime()const{return currentPatientTime + patientRecruitmentTime;}
  double getTimeSinceLastSwitchReset()const{return currentPatientTime - currentPatientResetSwitchTime; }

  void IncreasePatientTime(double increment){currentPatientTime += increment;}

  void ResetPatientSwitches(){currentPatientResetSwitchTime = currentPatientTime;}

private:
  double currentPatientTime;
  double patientRecruitmentTime;
  double currentPatientResetSwitchTime;
};