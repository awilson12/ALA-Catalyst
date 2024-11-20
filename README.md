# ALA-Catalyst

# App-1
Contains all files related to the submission to Indoor Air.

# App.R
Runs the most up-to-date version of the model

# case_study_runs.R
Runs the case scenarios presented in the Indoor Air paper

These files run defining_parameters.R and defining_parameters_v2.R, where the only difference is class.duration flexibilitiy for the case studies while it is assumed a full day in the risk app at this time.

defining_rates.R and defining_probabilities.R set up the transitional probabilities for the Markov chain approach used in risk_model.R and risk_model_v2.R. These then provide doses that are used to estimate infection risks through Dose-response and data sum.R.
