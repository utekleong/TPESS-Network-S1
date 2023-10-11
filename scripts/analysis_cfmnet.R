#################################################################
##                            Setup                            ##
#################################################################
set.seed(2022)

# loading packages
library(haven)
library(psych)
library(bootnet)
library(tidyverse)
library(flextable)
library(qgraph)
library(psychonetrics)

# importing clean data for reliability analyses
networkdata_reliability_us <- read.csv("./data/confirmatory/networkdata_reliability_us.csv")

# importing clean data for sample characteristics and network estimation
data_us <- read.csv("./data/confirmatory/networkdata_us.csv")
networkdata_us <- read.csv("./data/confirmatory/networkdata_us.csv") %>% 
  select(-Age)

# importing codebook
variables <- as.data.frame(readxl::read_xlsx("./data/variablebook.xlsx")) %>% 
  select("label", "variablename", "variable_description_short", "variable_description_clean")

#################################################################
##                         Reliability                         ##
#################################################################
#calculating reliability for docs subscales (raw_alpha of output)

## docs_con
networkdata_reliability_us %>% 
  select(docs1, docs2, docs3, docs4, docs5) %>% 
  psych::alpha()

## docs_res
networkdata_reliability_us %>% 
  select(docs6, docs7, docs8, docs9, docs10) %>% 
  psych::alpha()

## docs_ut
networkdata_reliability_us %>% 
  select(docs11, docs12, docs13, docs14, docs15) %>% 
  psych::alpha()

## docs_sym
networkdata_reliability_us %>% 
  select(docs16, docs17, docs18, docs19, docs20) %>% 
  psych::alpha()

#calculating reliability for tpess (raw_alpha of output)
networkdata_reliability_us %>% 
  select(starts_with("tpess")) %>% 
  psych::alpha()

##################################################################
##                    Sample characteristics                    ##
##################################################################
# mean age
mean(data_us$Age, na.rm = TRUE) %>% 
  round(digits = 2)

#################################################################
##               Confirmatory network estimation               ##
#################################################################
# pulling adjacency matrix from exploratory network
adjmatrix <- as.matrix(read.csv("./data/confirmatory/adjmatrix.csv", row.names = 1))

# obtaining average pairwise sample size for US sample
noNA <- !is.na(networkdata_us)
noNAmat <- t(noNA) %*% noNA
n_pairwise <- mean(noNAmat[lower.tri(noNAmat)])

# fitting confirmatory network model with psychonetrics
# since exploratory model used spearman, should use spearman in confirmatory model for consistency (supply covs and nobs arguments instead of data)
cfmnetwork <- ggm(covs = cor(networkdata_us, use = "pairwise.complete.obs", method = "spearman"),
                  nobs = n_pairwise,
                  omega = adjmatrix)
results_cfmnetwork <- cfmnetwork %>% runmodel() #takes a long time!

#################################################################
##                Plotting confirmatory network                ##
#################################################################
# custom function for defining legend vectors
# DO NOT use if there is only one element
# var_start argument should be argument for dplyr::starts_with
def_legend <- function(data, var_start){
  
  data %>% 
    select(starts_with(var_start)) %>% 
    colnames() %>%  
    match(colnames(data))
}

# defining grouping for legend
group_subscale <- list("GAD" = def_legend(networkdata_us,"gad"),
                       "PHQ-9" = def_legend(networkdata_us,"phq"),
                       "SMPD" = def_legend(networkdata_us,"smpd"),
                       "SMSAD" = def_legend(networkdata_us,"smsp"),
                       "DOCS" = def_legend(networkdata_us,"docs"),
                       "TPESS" = c(which(colnames(networkdata_us) == "tpess")))

# importing nodelabels
nodelabels <- read.csv("./data/nodelabels.csv")

# pulling plot layout of exploratory network
plotlayout <- as.matrix(read.csv("./data/confirmatory/plotlayout.csv"))

#plotting confirmatory network
plot_us <- qgraph(getmatrix(results_cfmnetwork, "omega", threshold = TRUE, alpha = 0.01),
                groups = group_subscale,
                layout = plotlayout,
                cut = 0,
                palette = "pastel",
                vsize = 3,
                labels = nodelabels$label,
                label.cex = 1.5,
                border.width = 0.75,
                legend.cex = 0.95,
                nodeNames = nodelabels$variable_description_short,
                filename = "study1_cfmnetwork", filetype = "jpeg", width = 20, height = 20,
                theme = "colorblind")
