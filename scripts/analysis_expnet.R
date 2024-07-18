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
networkdata_reliability_sg <- read.csv("./data/exploratory/networkdata_reliability_sg.csv")

# importing clean data for sample characteristics and network estimation
data_sg <- read.csv("./data/exploratory/networkdata_sg.csv")
networkdata_sg <- read.csv("./data/exploratory/networkdata_sg.csv") %>% 
  select(-Age)

# importing codebook
variables <- as.data.frame(readxl::read_xlsx("./data/variablebook.xlsx")) %>% 
  select("label", "variablename", "variable_description_short", "variable_description_clean")

#################################################################
##                         Reliability                         ##
#################################################################
#calculating reliability for docs subscales (raw_alpha of output)

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

#calculating reliability for tpess (raw_alpha of output)
networkdata_reliability_sg %>% 
  select(starts_with("tpess")) %>% 
  psych::alpha()

##################################################################
##                    Sample characteristics                    ##
##################################################################
# mean age
mean(data_sg$Age, na.rm = TRUE) %>% 
  round(digits = 2)

# SD of age
sd(data_sg$Age, na.rm = TRUE) %>% 
  round(digits = 2)

##################################################################
##                Exploratory network estimation                ##
##################################################################
# defining node labels for graph
nodelabels <- data.frame(variablename = colnames(networkdata_sg)) %>% 
  left_join(variables, by = "variablename") %>% 
  select(-variable_description_clean)
# write.csv(nodelabels, file = "./data/nodelabels.csv", row.names = FALSE)

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
     nodeNames = nodelabels$variable_description_short,
     filename = "expnetwork", filetype = "png", width = 20, height = 20)

# extracting adjacency matrix from the exploratory network to be used in confirmatory network analysis
adjmatrix <- 1*(network_sg$graph !=0)
# write.csv(adjmatrix, file = "./data/confirmatory/adjmatrix.csv", row.names = TRUE)

# extracting plot layout from the exploratory network to be used in confirmatory network analysis
plotlayout <- plot_sg$layout
# write.csv(plotlayout, file = "./data/confirmatory/plotlayout.csv", row.names = FALSE)

#extracting weights matrix from the exploratory network to be used in the factor score network
weightmatrix <- network_sg$graph
# write.csv(weightmatrix, file = "./data/fsnetwork/weightmatrix.csv", row.names = FALSE)

#saving plot object for appendix
#saveRDS(plot_sg, file = "./data/appendix/rds/plot_sg.rds")

#saving bootnet object for appendix
#saveRDS(network_sg, file = "./data/appendix/rds/boot_sg.rds")


#################################################################
##                 Sympptom descriptives table                 ##
#################################################################
# Symptom descriptive table
sympdesc <- data.frame(psych::describe(data_sg, skew = FALSE, ranges = FALSE, na.rm = TRUE)) %>% 
  rownames_to_column(var = "variablename") %>% 
  select(-vars, -se, -n) %>% 
  filter(variablename != "Age")
sympdesc[,c("mean", "sd")] <- round(sympdesc[,c("mean","sd")], digits =2)

sympdesc_table <- nodelabels %>%
  left_join(sympdesc, by = "variablename") %>%
  select(-variablename) %>%
  flextable() %>% 
  set_header_labels(label = "Node label",
                    variable_description_short = "Description",
                    mean = "Mean",
                    sd = "SD") %>%
  autofit()

#print(sympdesc_table, preview = "docx")

##################################################################
##                      Edge weight tables                      ##
##################################################################
# Edge Weight Table (Full)
edgeweight_full <- data.frame(network_sg$graph) %>%
  round(digits = 2)
row.names(edgeweight_full) <- nodelabels$label
names(edgeweight_full) <- nodelabels$label
#write.csv(edgeweight_full, file = "edgeweight.csv")

# extracting edge weights (TPESS-Symptoms only)
edgeweight_tpess <- data.frame(network_sg$graph[,41]) %>%
  round(digits = 2) %>% 
  rownames_to_column()
names(edgeweight_tpess) <- c("variablename", "weight")
edgeweight_tpess <- left_join(edgeweight_tpess, nodelabels, by = "variablename") %>% 
  select(label, variable_description_short, weight) %>% 
  filter(label == "D6"|label == "D7"|label == "D9"|label == "A7"|label == "S3"|label == "O3") 

#performing non-parametric bootstrapping
# computationally intensive; code below was saved to a .rds file, to be loaded with readRDS
#nonparboot <- bootnet(network_sg, nBoots = 1000, nCores = 8)
#saveRDS(nonparboot, file = "./data/exploratory/rds/nonparboot.rds")
nonparboot <- readRDS(file = "./data/exploratory/rds/nonparboot.rds")

# extracting bootstrapped CIs
nonparboot_ci <- summary(nonparboot) %>%
  filter(type == "edge", node2 == "T", (node1 == "D6"|node1 == "D7"|node1 == "D9"|node1 == "A7"|node1 == "S3"|node1 == "O3")) %>% 
  ungroup() %>% 
  select(node1, CIlower, CIupper)
nonparboot_ci$CIlower <- round(nonparboot_ci$CIlower, digits = 2)
nonparboot_ci$CIupper <- round(nonparboot_ci$CIupper, digits = 2)
nonparboot_ci <- mutate(nonparboot_ci, CI = paste0("[",CIlower,",",CIupper,"]"), .keep = "unused")
names(nonparboot_ci) <- c("label", "BCI")

# extracting inclusion proportions
nonparboot_ip <- bootInclude(nonparboot)
nonparboot_ip<- nonparboot_ip$graph %>% data.frame() %>% 
  select(tpess) %>% 
  rownames_to_column()
names(nonparboot_ip) <- c("variablename", "ip")
nonparboot_ip <- left_join(nonparboot_ip, nodelabels, by = "variablename") %>% 
  select(label, ip) %>% 
  filter(label == "D6"|label == "D7"|label == "D9"|label == "A7"|label == "S3"|label == "O3") 

# edge weight differences
difference_edgelist <- c("A7--T","D7--T","D9--T","O3--T", "S3--T")
difference_df <- data.frame()
for (i in difference_edgelist) {
  difference_edge <- differenceTest(nonparboot, "D6--T", i, measure = "edge")
  difference_df <- rbind(difference_df, difference_edge)
}

difference_df <- cbind(label = c("A7", "D7", "D9", "O3", "S3"), difference_df)

difference_df <- select(difference_df, label, lower, upper)
difference_df$lower <- round(difference_df$lower, digits = 2)
difference_df$upper <- round(difference_df$upper, digits = 2)
difference_df <- mutate(difference_df, CI = paste0("[",lower,",",upper,"]"), .keep = "unused")
difference_df <- rbind(c("D6","-"), difference_df)
names(difference_df) <- c("label", "diff_CI")


# edge weights (TPESS-Symptoms only)) + bootstrapped CI + inclusion proportion
edgeweight_tpess_table <- left_join(edgeweight_tpess, nonparboot_ci, by = "label") %>% 
  left_join(nonparboot_ip, by = "label") %>%
  left_join(difference_df, by = "label") %>% 
  arrange(desc(weight)) %>% 
  flextable() %>% 
  set_header_labels(label = "Node label",
                    variable_description_short = "Description",
                    weight = "Edge weight",
                    BCI = "Bootstrapped CI",
                    ip = "Inclusion proportion",
                    diff_CI = "Bootstrapped difference test CI") %>% 
  autofit()
#print(edgeweight_tpess_table, preview = "docx")
