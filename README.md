# TPESS Network Study 1
This repository contains the files for the study titled "Transdiagnostic Associations Between the Core Vulnerability and Internalizing Symptoms: A Cross-Sectional and a Confirmatory Network Analysis". Deidentified data to replicate the analyses performed in this study are available in ./data/exploratory and ./data/confirmatory.

Under ./scripts you will find the R codes used to wrangle and analyse the data:
- ./scripts/datacleaning.R contains the code used to prepare the raw data for analysis. As the identified raw data will not be provided here, this file is not executable. It, however, should give the reader a sense of what was done to the original data file.
- ./scripts/expnet.R and ./scripts/cfmnet.R contains the executable code to perform the exploratory and confirmatory analyses conducted here (respectively).
- ./scripts/suppmat.Rmd contains the executable code to generate the supplementary material.