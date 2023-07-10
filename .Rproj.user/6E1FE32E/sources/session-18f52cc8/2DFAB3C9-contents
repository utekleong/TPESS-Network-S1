# loading packages
library(tidyverse)
library(haven)

# importing singapore sample
tpess_sg_data <- read_sav("./data/tpess_sg.sav")

# importing item label and content
variables <- as.data.frame(readxl::read_xlsx("./data/variablebook.xlsx")) %>% 
  select("variablename", "variable_description_short", "variable_description_clean")

variables_tpessstart <- which(variables$variablename == "tpess1")
variables_tpessend <- which(variables$variablename == "tpess40")
variables$variablename[variables_tpessstart:variables_tpessend] <-c('rnt1', 'rnt2', 'rnt3', 'rnt4', 'rnt5', 'rnt6', 
                                                                    'iu1', 'iu2', 'iu3', 'iu4', 'iu5', 'iu6', 
                                                                    'as1', 'as2', 'as3', 'as4', 'as5', 'as6', 
                                                                    'ai1', 'ai2', 'ai3', 'ai4', 'ai5', 'ai6', 
                                                                    'rdt1', 'rdt2', 'rdt3', 'rdt4', 'rdt5', 'rdt6', 
                                                                    'ca1', 'ca2', 'ca3', 'ca4', 'ca5', 'ca6',
                                                                    'nsr1', 'nsr2', 'nsr3', 'nsr4', 'nsr5', 'nsr6',
                                                                    'nsa1', 'nsa2', 'nsa3', 'nsa4', 'nsa5', 'nsa6',
                                                                    'pel1', 'pel2', 'pel3', 'pel4', 'pel5', 'pel6', 
                                                                    'uee1', 'uee2', 'uee3', 'uee4')

# subsetting TPESS subscales and symptom measures items
cleandata <- tpess_sg_data %>%
  select(Age, starts_with("docs"), starts_with("gad"), starts_with("phq"), starts_with("smpd"), starts_with("smsp"), tpess) %>%
  select(-docs_sum, -gad_sum, -phq_sum, -smpd_mean, -smsp_mean) %>% 
  mutate(docs_con = docs1 + docs2 + docs3 + docs4 + docs5,
         docs_res = docs6 + docs7 + docs8 + docs9 + docs10,
         docs_ut = docs11 + docs12 + docs13 + docs14 + docs15,
         docs_sym = docs16 + docs17 + docs18 + docs19 + docs20,
         .keep = "unused",
         .after = smsp10)

# data for 57-node network (all symptom item data, no symptom subscale scores)
cleandata_full <- tpess_sg_data %>%
  select(starts_with("gad"), starts_with("phq"), starts_with("smpd"), starts_with("smsp"), starts_with("docs"), tpess) %>%
  select(-docs_sum, -gad_sum, -phq_sum, -smpd_mean, -smsp_mean)

# data for calculating reliability
cleandata_reliability <- tpess_sg_data %>% 
  select(starts_with("docs"), starts_with("tpess")) %>% 
  select(-docs_sum, -tpess)

# saving cleaned data file
cleandata %>% write.csv("./data/networkdata_sg.csv", row.names = FALSE)
cleandata_full %>% write.csv("./data/networkdata_allitems_sg.csv", row.names = FALSE)
cleandata_reliability %>% write.csv("./data/networkdata_reliability_sg.csv", row.names = FALSE)
