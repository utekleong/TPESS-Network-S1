# Associations Between a Transdiagnostic Core Vulnerability and Internalizing Symptoms: A Network Analysis

This repository contains the data and code for the study titled "Associations Between a Transdiagnostic Core Vulnerability and Internalizing Symptoms: A Network Analysis". Deidentified data to replicate the analyses performed in this study are available in ./data/exploratory and ./data/confirmatory. 

./data/variablebook.xlsx is the code book of the primary study (Hong et al., 2024).

Under ./scripts you will find the R codes used to wrangle and analyse the data:
- ./scripts/datacleaning.R contains the code used to prepare the raw data for analysis. As the identified raw data will not be provided here, this file is not executable. It, however, should give the reader a sense of what was done to the original data file.
- ./scripts/expnet.R contains the executable code to perform the study's main analysis.
- ./scripts/suppmat.Rmd contains the executable code to generate the supplementary material. Please check out ./scripts/cfmnet.R for an easier-to-read version of the confirmatory network analysis code.
