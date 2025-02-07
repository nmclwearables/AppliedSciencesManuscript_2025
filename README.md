# AppliedSciencesManuscript_2025

This document provides an overview of the code used in the manuscript titled 
"Feasibility and User Experience of a program of progressive cued activity to
promote funcitonal upper limb activity in the inpatient rehabilitaiton setting
with follow-up at home". The code included with this submission corresponds to
the data, analysis, and figures described in the manuscript.


Abbreviations:
MoCA = Montreal Cognitive Assessment
FMA-UE = Fugl Meyer Assessment - Upper Extremity
IRF = Inpatient Rehabilitaiton Facility
QUEST = Quebec User Experience and Satisfaction with assistive Technology
SUS = System Usability Scale
IMI = Intrinsic Motivation Inventory
NaN = not a number; data were not collected (e.g. participant declined response) or data were missing (e.g. follow-up not completed)


Data files:

1. "CalculatedSurveys_IPRandHome.csv" consists of outcome and survey data. 
Column headers in row 1 are as follows: 

- study_id: the participant's unique study ID for deidentification
- elig_moca: scores from the MoCA (0-30)
- baseline_fm_h_total: scores for the FMA-UE sensation (part H) (0-12)
- baseline_fm_total: scores for the FMA-UE motor and sensation section (0-78) * Note: Fugl Meyer Motor scores were calculated by subtracting fm_h scores from fm_total scores
- survey_ipr_quest_items___n: participant selection for nth item of 12 options for QUEST "most important" section (0 = not selected, 1 = selected) in the IRF
- survey_final_quest_items___n: participant selection for nth item of 12 options for QUEST "most important" section (0 = not selected, 1 = selected) in home settings
- QUEST_IPR_CalcScore: calculated QUEST score in the IRF setting
- QUEST_Home_CalcScore: calculated QUEST score in home settings
- SUS_IPR_CalcScore: calculated SUS score in the IRF setting
- SUS_Home_CalcScore: calculated SUS score in home settings
- IMI_IPR_(Category)_CalcScore: calculated IMI score for the designated category in the IRF setting
	Interest = interest/enjoyment
	Effort = effort/importance
	Value = value/usefulness
	Choice = perceived choice
	Competence = perceived competence
	Pressure = pressure/tension

2. ForwardResponses_ByParticipant
- study_id: the participant's unique study ID for deidentification
- GroupCount: the number of days included in the participant's analysis
- mean_ForwardResponsesMatrixn: the average rate that activity was detected in the correct limb(s) in the 5 seconds following the 'n'th cue delivery

2. BackwardResponses_ByParticipant
- study_id: the participant's unique study ID for deidentification
- GroupCount: the number of days included in the participant's analysis
- mean_BackwardResponsesMatrixn: the average rate that activity was detected in limbs in the 5 seconds prior to the 'n'th cue delivery

3. ResponseRatesBySubject
- Workspace_labels: the participant's unique study ID for deidentification
- GroupCount: the number of days included in the participant's analysis
- mean_PreCue_RR: the participant's average rate of activity that was detected in the limbs in the 5 seconds prior to cue delivery across all days and cues
- mean_PostCue_RR: the participant's average rate of activity that was detected in the correct limbs in the 5 seconds following cue delivery across all days and cues
- mean_Diff_RR: the difference between mean_PreCue_RR and mean_PostCueRR

Data Analysis and Visualization files:

1. Script_DataAnalysis_Visualization.R
- this script will read the necessary packages and data files listed above to reproduce the statistical analysis and figures used in the submitted manuscript titled 
"Feasibility and User Experience of a program of progressive cued activity to promote functional upper limb activity in the inpatient rehabilitation setting with follow-up at home".
Note: the R packages must be installed prior to running the script.

2. calculate_CI_SEM.R
- this function is called from Script_DataAnalysis_Visualization.R and calculates the standard error of the mean and confidence intervals for one-tailed t-tests.


For any questions related to this work, please contact:
- Dr. Kim Bassindale, PT, DPT at kimberly.bassindale@marquette.edu or
- Dr. Bob Scheidt, PhD at robert.scheidt@marquette.edu
