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
library(lavaan)

# importing factor scores
tpess_factorscores <- read_sav("./data/fsnetwork/fscores.sav") %>% 
  select(1)
names(tpess_factorscores) <- "tpess_fs"

# importing clean data for network estimation
networkdata_sg_fs <- read.csv("./data/exploratory/networkdata_allitems_sg.csv")

# importing codebook
variables <- as.data.frame(readxl::read_xlsx("./data/variablebook.xlsx")) %>% 
  select("label", "variablename", "variable_description_short", "variable_description_clean")

##################################################################
##          Computing factor scores for DOCS subscales          ##
##################################################################
# subsetting DOCS data
docs_data <- networkdata_sg_fs %>% 
  select(starts_with("docs"))

# specifying and estimating CFA model
docs_structure <-"
docs_con =~ docs1 + docs2 + docs3 + docs4 + docs5
docs_res =~ docs6 + docs7 + docs8 + docs9 + docs10
docs_ut =~ docs11 + docs12 + docs13 + docs14 + docs15
docs_sym =~ docs16 + docs17 + docs18 + docs19 + docs20

# Second-order factor
docs_general =~ docs_con + docs_res + docs_ut + docs_sym
"
docs_model <- cfa(docs_structure, docs_data, missing = "ml")
summary(docs_model, fit.measures = TRUE)

# extracting factor scores
docs_factorscores <- as.data.frame(lavPredict(docs_model, method = "regression")) %>% 
  select(-docs_general)

##################################################################
##                Exploratory network estimation                ##
##################################################################
# merging factor scores with rest of the data
networkdata_sg_fs <- networkdata_sg_fs %>% 
  select(-tpess, -starts_with("docs")) %>% 
  cbind(docs_factorscores) %>% 
  cbind(tpess_factorscores)

# defining node labels for graph
nodelabels <- data.frame(variablename = colnames(networkdata_sg_fs)) %>% 
  left_join(variables, by = "variablename") %>% 
  select(-variable_description_clean)
nodelabels[nodelabels$variablename == "tpess_fs",2] <- "T"
nodelabels[nodelabels$variablename == "tpess_fs",3] <- "TPESS factor score"
nodelabels[nodelabels$variablename == "docs_con",3] <- "Contamination factor score"
nodelabels[nodelabels$variablename == "docs_res",3] <- "Responsibility factor score"
nodelabels[nodelabels$variablename == "docs_ut",3] <- "Unacceptable Thoughts factor score"
nodelabels[nodelabels$variablename == "docs_sym",3] <- "Symmetry factor score"

# estimating network
network_fs <- estimateNetwork(networkdata_sg_fs,
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
group_subscale <- list("GAD" = def_legend(networkdata_sg_fs,"gad"),
                       "PHQ-9" = def_legend(networkdata_sg_fs,"phq"),
                       "SMPD" = def_legend(networkdata_sg_fs,"smpd"),
                       "SMSAD" = def_legend(networkdata_sg_fs,"smsp"),
                       "DOCS" = def_legend(networkdata_sg_fs,"docs"),
                       "TPESS" = c(which(colnames(networkdata_sg_fs) == "tpess_fs")))

# importing plot layout from original exploratory network for ease of comparison
plotlayout <- as.matrix(read.csv("./data/confirmatory/plotlayout.csv"))

# printing plot
plot_fs <- plot(network_fs,
                groups = group_subscale,
                layout = plotlayout, #layout constrained to that of original exploratory network for ease of comparison
                cut = 0,
                palette = "pastel",
                vsize = 3,
                labels = nodelabels$label,
                label.cex = 1.5,
                border.width = 0.75,
                legend.cex = 0.95,
                nodeNames = nodelabels$variable_description_short,
                filename = "fsnetwork", filetype = "png", width = 20, height = 20)
