setwd("/Users/wonny/Downloads/CHMC/Pain AUC/DATA")
library(openxlsx)
library(readxl)
library(dplyr)
library(chron)
library(plyr)
library(data.table)
library(lubridate)

raw_data <- read_excel("auc_use_11172022.xlsx") %>% as.data.frame
ID <- unique(raw_data$record_id)

subset(raw_data, record_id==ID[38])$POD

## count # of observations in each period
# tabulate does not count pod2 if there's no pod2
# manual count

count_ftn <- function(id_input, data_input){
  
  pod_vector <- subset(data_input, record_id==id_input)$POD
  
  count0 <- sum(pod_vector==0)
  count1 <- sum(pod_vector==1)
  count2 <- sum(pod_vector==2)
  count012 <- c(count0, count1, count2)
  
  return(count012)
}

count_list <- lapply(ID, count_ftn, data_input = raw_data)

# count matrix: Npod0, Npod1, Npod2
count_matrix <- matrix(0, nrow = length(ID), ncol = 3)
for(i in 1:length(ID)){
  count_matrix[i,] <- count_list[[i]]
}







