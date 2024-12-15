# Mpox-Vac-RiskChanges
This repository contains files and code used for a modelling study investigating the impact of hypothetical future introductions of mpox cases among men who have sex with men (MSM) in the Netherlands. We investigated the impact of new introductions assuming no vaccinations after 2023 or with additional vaccinations in 2024-2025. The model accounts for 3 levels of risk to get infected, based on the number of sexual partners: low, medium, and high risk. Furthermore, we accounted for possible transitions of MSM between risk groups: men with low risk may have later medium or high risk and vice versa. 

Methods and results are presented in a manuscript (in preparation). 

The computer code was written in R version 4.3.2 (2023-10-31).

The following folders are included: 
“data”: with data included in the calculations.
“output”: text files with numerical results obtained from the model; plots from the model. 
“param”: sets of parameter values generated and those selected via model fitting. 
“scripts”: R files for functions and procedures. 

The code uses R base packages and the following packages: deSolve, ggplot2, reshape2, dplyr, lhs, readxl.

License: EUPL v1.2.
 
