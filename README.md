# Mpox-Vac-RiskChanges
This repository contains files and code used for a modelling study investigating the impact of hypothetical future introductions of mpox cases among men who have sex with men (MSM) in the Netherlands. 
We investigated the impact of new introductions assuming no vaccinations after 2023 or with additional vaccinations in 2024-2025. 
The model accounts for 3 levels of sexual activity, based on the number of sexual partners: low, medium, and high levels. 
Furthermore, we accounted for possible transitions of MSM between the sexual activity groups: men with a low level of sexual activity may have later a medium or high level and vice versa. 

Methods and results are presented in a manuscript (in preparation). 

The computer code was written in R version 4.4.3.

The following folders are included: 
“data”: with data included in the calculations.
“output”: text files with numerical results obtained from the model; plots from the model. 
“param”: sets of parameter values generated and those selected via model fitting. 
“scripts”: R files for functions and procedures. 

The code uses R base packages and the following packages: deSolve, ggplot2, reshape2, dplyr, lhs, readxl.

License: EUPL v1.2.

Data: 
Data with the numbers of daily mpox cases in the Netherlands are available on the website of the National Institute of Public Health and Environment of the Netherlands: https://www.rivm.nl/en/mpox/current-information-about-mpox. Data until 2 April 2024 were used in the code. 
The weekly numbers of mpox vaccinations are available in the report “Sexually transmitted infections in the Netherlands in 2023” (page 152) of Kayaert L, et al. (RIVM report 2024-0038; Center for Infectious Diseases Control, National Institute of Public Health and the Environment (RIVM); Bilthoven, 2024. Available online: https://www.rivm.nl/bibliotheek/rapporten/2024-0038.pdf). 
Data from the PrEP pilot are not available for replication, because they are third-party data and are not freely available. They are data from the Dutch national registration of Sexual Health Centre consultations (SOAP). Pseudonymized individual participant data can be requested for scientific use with a methodologically sound proposal submitted to the SOAP registration committee for approval. Information can be requested from the authors.

Last update: 26 May 2025. 