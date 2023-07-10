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

# importing clean data for reliability analyses
networkdata_reliability_sg <- read.csv("./data/networkdata_reliability_sg.csv")

# importing clean data for sample characteristics and network estimation
data_sg <- read.csv("./data/networkdata_sg.csv")
networkdata_sg <- read.csv("./data/networkdata_sg.csv") %>% 
  select(-Age)

# importing codebook
variables <- as.data.frame(readxl::read_xlsx("./data/variablebook.xlsx")) %>% 
  select("label", "variablename", "variable_description_short", "variable_description_clean")

#################################################################
##                         Reliability                         ##
#################################################################

#calculating reliability for docs subscales

  ## docs_con
  networkdata_reliability_sg %>% 
    select(docs1, docs2, docs3, docs4, docs5) %>% 
    psych::alpha()
  
  ## docs_res
  networkdata_reliability_sg %>% 
    select(docs6, docs7, docs8, docs9, docs10) %>% 
    psych::alpha()
  
  ## docs_ut
  networkdata_reliability_sg %>% 
    select(docs11, docs12, docs13, docs14, docs15) %>% 
    psych::alpha()
  
  ## docs_sym
  networkdata_reliability_sg %>% 
    select(docs16, docs17, docs18, docs19, docs20) %>% 
    psych::alpha()

#calculating reliability for tpess (raw_alpha)
networkdata_reliability_sg %>% 
  select(starts_with("tpess")) %>% 
  psych::alpha()

##################################################################
##                    Sample characteristics                    ##
##################################################################

# mean age
mean(data_sg$Age, na.rm = TRUE) %>% 
  round(digits = 2)

##################################################################
##                      Network estimation                      ##
##################################################################

# defining node labels for graph
nodelabels <- data.frame(variablename = colnames(networkdata_sg)) %>% 
  left_join(variables, by = "variablename") %>% 
  select(-variable_description_clean)

# estimating network
network_sg <- estimateNetwork(networkdata_sg,
                             default = "EBICglasso",
                             corMethod = "spearman",
                             labels = nodelabels$label,
                             missing = "pairwise",
                             sampleSize = "pairwise_average",
                             tuning = 0.5)

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
group_subscale <- list("GAD" = def_legend(networkdata_sg,"gad"),
                       "PHQ-9" = def_legend(networkdata_sg,"phq"),
                       "SMPD" = def_legend(networkdata_sg,"smpd"),
                       "SMSAD" = def_legend(networkdata_sg,"smsp"),
                       "DOCS" = def_legend(networkdata_sg,"docs"),
                       "TPESS" = c(which(colnames(networkdata_sg) == "tpess")))

# defining node names for legend
itemnames <- data.frame(variablename = colnames(networkdata_sg)) %>% 
  left_join(variables, by = "variablename") %>%
  select(variable_description_short)

# printing plot
plot_sg <- plot(network_sg,
     groups = group_subscale,
     layout = "spring",
     cut = 0,
     palette = "pastel",
     vsize = 3,
     labels = nodelabels$label,
     label.cex = 1.5,
     border.width = 0.75,
     legend.cex = 0.95,
     nodeNames = itemnames$variable_description_short,
     filename = "study1_network", filetype = "jpeg", width = 20, height = 20)

##################################################################
##                      Edge weight tables                      ##
##################################################################

# Edge Weight Table (Full)
edgeweight_full <- data.frame(network_sg$graph) %>%
  round(digits = 2)
row.names(edgeweight_full) <- nodelabels$label
names(edgeweight_full) <- nodelabels$label
# write.csv(edgeweight_full, file = "edgeweight.csv")

# Edge Weight Table (TPESS-Symptoms only)
edgeweight_tpess <- data.frame(network_sg$graph[,41]) %>%
  round(digits = 2) %>% 
  rownames_to_column()
names(edgeweight_tpess) <- c("variablename", "weight")
edgeweight_tpess <- left_join(edgeweight_tpess, nodelabels, by = "variablename") %>% 
  select(label, variable_description_short, weight) %>% 
  filter(label == "D6"|label == "D7"|label == "D9"|label == "A7"|label == "S3"|label == "O3") %>% 
  arrange(desc(weight))

edgeweight_tpess_table <- edgeweight_tpess %>% 
  flextable() %>% 
  set_header_labels(label = "Node Label",
                    variable_description_short = "Description",
                    weight = "Edge Weight") %>% 
  autofit()
#print(edgeweight_tpess_table, preview = "docx")

