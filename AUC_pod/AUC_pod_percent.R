rm(list=ls())
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
n <- length(ID)


## count # of observations in each period
# tabulate does not count pod2 if there's no pod2
# manual count
source('percent_time_auc_ftns.R')
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

## AUC function ##
AUC_ftn <- function(data_input, count_input){
  
  
  # imputation function
  imput_ftn <- function(p1_input, p2_input, acc1_input, acc2_input, pod_input, pod0_duration_input){
    pod_indicator <- pod_input + 1
    cuttime <- switch (pod_indicator,
                       pod0_duration_input,
                       pod0_duration_input + 24,
                       pod0_duration_input + 48
    )
    acc1_input <- as.numeric(acc1_input);acc2_input <- as.numeric(acc2_input)
    if(p2_input >= p1_input){
      x <- (p2_input-p1_input)*(cuttime-acc1_input)/(acc2_input-acc1_input) + p1_input
    }else{
      x <- (p1_input-p2_input)*(acc2_input-cuttime)/(acc2_input-acc1_input) + p2_input
    }
    
    return(as.numeric(x))
  }
  
  sur_endtime <- data_input$set_new[1]
  pod0_duration <- difftime(data_input$dos[1]+3600*24, sur_endtime, units = "hours")
  pod1_duration <- pod0_duration + 24
  pod2_duration <- pod0_duration + 48   ## accumulative, so this is the total duration
  
  pod <- data_input$POD # pod
  time_vec <- data_input$painDT # pain recorded time
  ps_vec <- data_input$pain_score # pain score
  
  
  # order
  temp_order <- data.frame(ps=ps_vec, ts=time_vec, pod=pod)
  temp_order <- temp_order[order(temp_order$ts),]
  ps_vec <- temp_order$ps; time_vec <- temp_order$ts; pod <- temp_order$pod
  
  # remove the pain scores and time < surgery end time
  temp <- difftime(sur_endtime, time_vec)
  if(sum(temp>0)>=1){
    time_vec <- time_vec[-which(temp>0)]
    ps_vec <- ps_vec[-which(temp>0)]
    pod <- pod[-which(temp>0)]
  }
  
  ps0 <- ps_vec[pod==0]
  ps1 <- ps_vec[pod==1]
  ps2 <- ps_vec[pod==2]
  
  time0 <- time_vec[pod==0]
  time1 <- time_vec[pod==1]
  time2 <- time_vec[pod==2]
  
  
  
  auc0 <- 0
  auc1 <- 0
  auc2 <- 0
  
  auc0_low <- auc0_mid <- auc0_high <- 0
  auc1_low <- auc1_mid <- auc1_high <- 0
  auc2_low <- auc2_mid <- auc2_high <- 0
  
  time0_low <- time0_mid <- time0_high <- 0
  time1_low <- time1_mid <- time1_high <- 0
  time2_low <- time2_mid <- time2_high <- 0
  
  
  Npod0 <- sum(pod==0)
  Npod1 <- sum(pod==1)
  Npod2 <- sum(pod==2)
  
  # print(c(Npod0,Npod1,Npod2))
  
  
  auc_save <- 0
  acc_time <- 0
  # POD0
  if(Npod0!=0){ # if 0 -> auc_work = 0
    # print("POD0 start")
    
    # first one
    lag_time <- difftime(time0[1], sur_endtime, units="hours")
    auc_work <- ps0[1] * lag_time
    
    case <- case_gen_ftn(ps0[1],ps0[1])
    temp <- percent_auc_ftn(case, ps0[1],ps0[1], lag_time, auc_work)
    
    auc0_low <- auc0_low + temp$auc_vector[1]
    auc0_mid <- auc0_mid + temp$auc_vector[2]
    auc0_high <- auc0_high + temp$auc_vector[3]
    
    time0_low <- time0_low + temp$time_vector[1]
    time0_mid <- time0_mid + temp$time_vector[2]
    time0_high <- time0_high + temp$time_vector[3]
    
    
    auc_save <- auc_save + auc_work
    acc_time <- acc_time + lag_time
    
    
    # if only one ps
    if(Npod0==1){
      
      # this should be the last one
      if(Npod1==0){
        lag_time <- pod0_duration - difftime(time0[1], sur_endtime, units="hours")
        auc_work <- ps0[1] * lag_time
        
        case <- case_gen_ftn(ps0[1],ps0[1])
        temp <- percent_auc_ftn(case, ps0[1],ps0[1], lag_time, auc_work)
        
        auc0_low <- auc0_low + temp$auc_vector[1]
        auc0_mid <- auc0_mid + temp$auc_vector[2]
        auc0_high <- auc0_high + temp$auc_vector[3]
        
        time0_low <- time0_low + temp$time_vector[1]
        time0_mid <- time0_mid + temp$time_vector[2]
        time0_high <- time0_high + temp$time_vector[3]
        
      }else{ # imputation
        lag_time <- pod0_duration - difftime(time0[Npod0], sur_endtime, units="hours")
        acc1 <- acc_time
        acc2 <- acc_time + difftime(time1[1], time0[Npod0], units="hours")
        
        imputed_score <- imput_ftn(ps0[Npod0], ps1[1], acc1, acc2, 0, pod0_duration)
        auc_work <-  (ps0[Npod0] + imputed_score) * lag_time /2
        
        case <- case_gen_ftn(ps0[Npod0], imputed_score)
        temp <- percent_auc_ftn(case, ps0[Npod0], imputed_score, lag_time, auc_work)
        
        auc0_low <- auc0_low + temp$auc_vector[1]
        auc0_mid <- auc0_mid + temp$auc_vector[2]
        auc0_high <- auc0_high + temp$auc_vector[3]
        
        time0_low <- time0_low + temp$time_vector[1]
        time0_mid <- time0_mid + temp$time_vector[2]
        time0_high <- time0_high + temp$time_vector[3]
        
        
      }
      
      auc_save <- auc_save + auc_work
      acc_time <- acc_time + lag_time
      
      
    }else{
      # else -> we need a middle part
      
      for(i in 1:(Npod0-1)){

        lag_time <- difftime(time0[i+1], time0[i], units="hours")
        auc_work <- (ps0[i+1]+ps0[i]) * lag_time / 2
        
        case <- case_gen_ftn(ps0[i], ps0[i+1])
        temp <- percent_auc_ftn(case, ps0[i], ps0[i+1], lag_time, auc_work)
        
        auc0_low <- auc0_low + temp$auc_vector[1]
        auc0_mid <- auc0_mid + temp$auc_vector[2]
        auc0_high <- auc0_high + temp$auc_vector[3]
        
        time0_low <- time0_low + temp$time_vector[1]
        time0_mid <- time0_mid + temp$time_vector[2]
        time0_high <- time0_high + temp$time_vector[3]
        
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
      }
      
      # last : imputation if Npod1!=0, not if Npod1==0
      if(Npod1!=0){

        lag_time <- pod0_duration - difftime(time0[Npod0], sur_endtime, units="hours")
        acc1 <- acc_time
        acc2 <- acc_time + difftime(time1[1], time0[Npod0], units = "hours")
        
        imputed_score <- imput_ftn(ps0[Npod0], ps1[1], acc1, acc2, 0, pod0_duration)
        auc_work <-  (ps0[Npod0] + imputed_score) * lag_time /2
        
        case <- case_gen_ftn(ps0[Npod0], imputed_score)
        temp <- percent_auc_ftn(case, ps0[Npod0], imputed_score, lag_time, auc_work)
        
        auc0_low <- auc0_low + temp$auc_vector[1]
        auc0_mid <- auc0_mid + temp$auc_vector[2]
        auc0_high <- auc0_high + temp$auc_vector[3]
        
        time0_low <- time0_low + temp$time_vector[1]
        time0_mid <- time0_mid + temp$time_vector[2]
        time0_high <- time0_high + temp$time_vector[3]
        
        
        
      }else{

        lag_time <- pod0_duration - difftime(time0[Npod0], sur_endtime, units="hours")
        auc_work <- ps0[Npod0] * lag_time
        
        case <- case_gen_ftn(ps0[Npod0], ps0[Npod0])
        temp <- percent_auc_ftn(case, ps0[Npod0], ps0[Npod0], lag_time, auc_work)
        
        auc0_low <- auc0_low + temp$auc_vector[1]
        auc0_mid <- auc0_mid + temp$auc_vector[2]
        auc0_high <- auc0_high + temp$auc_vector[3]
        
        time0_low <- time0_low + temp$time_vector[1]
        time0_mid <- time0_mid + temp$time_vector[2]
        time0_high <- time0_high + temp$time_vector[3]
        
      }

      auc_save <- auc_save + auc_work
      acc_time <- pod0_duration
    }

    auc0 <- auc_save
    
  } # POD0 end ################################################

  # POD1 start ###################################################
  if(Npod1!=0){

    # first: Npod0=0 -> no imputation
    if(Npod0==0){
      lag_time <- difftime(time1[1], sur_endtime, units = "hours") - acc_time
      auc_work <- ps1[1] * lag_time
      
      case <- case_gen_ftn(ps1[1], ps1[1])
      temp <- percent_auc_ftn(case, ps1[1], ps1[1], lag_time, auc_work)
      
      auc1_low <- auc1_low + temp$auc_vector[1]
      auc1_mid <- auc1_mid + temp$auc_vector[2]
      auc1_high <- auc1_high + temp$auc_vector[3]
      
      time1_low <- time1_low + temp$time_vector[1]
      time1_mid <- time1_mid + temp$time_vector[2]
      time1_high <- time1_high + temp$time_vector[3]
      
    }else{ # imputation
      lag_time <- difftime(time1[1], sur_endtime, units = "hours") - pod0_duration
      auc_work <- (imputed_score + ps1[1]) * lag_time/2
      
      case <- case_gen_ftn(ps1[1], imputed_score)
      temp <- percent_auc_ftn(case, ps1[1], imputed_score, lag_time, auc_work)
      
      auc1_low <- auc1_low + temp$auc_vector[1]
      auc1_mid <- auc1_mid + temp$auc_vector[2]
      auc1_high <- auc1_high + temp$auc_vector[3]
      
      time1_low <- time1_low + temp$time_vector[1]
      time1_mid <- time1_mid + temp$time_vector[2]
      time1_high <- time1_high + temp$time_vector[3]
      
    } # first area end
    
    auc_save <- auc_save + auc_work
    acc_time <- acc_time + lag_time
    
    # if there's only one -> check POD2 and this is the last
    if(Npod1==1){
      
      if(Npod2!=0){ # imputation
        
        lag_time <- pod0_duration + 24 - difftime(time1[Npod1], sur_endtime, units = "hours")
        acc1 <- acc_time
        acc2 <- acc_time + difftime(time2[1], time1[Npod1], units = "hours")
        
        imputed_score <- imput_ftn(ps1[Npod1], ps2[1], acc1, acc2, 1, pod0_duration)
        auc_work <-  (ps1[Npod1] + imputed_score) * lag_time /2
        
        case <- case_gen_ftn(ps1[Npod1], imputed_score)
        temp <- percent_auc_ftn(case, ps1[Npod1], imputed_score, lag_time, auc_work)
        
        auc1_low <- auc1_low + temp$auc_vector[1]
        auc1_mid <- auc1_mid + temp$auc_vector[2]
        auc1_high <- auc1_high + temp$auc_vector[3]
        
        time1_low <- time1_low + temp$time_vector[1]
        time1_mid <- time1_mid + temp$time_vector[2]
        time1_high <- time1_high + temp$time_vector[3]
        
        
      }else{  # no imputation
        lag_time <- pod1_duration - difftime(time1[Npod1], sur_endtime, units = "hours")
        auc_work <- ps1[Npod1] * lag_time
        
        case <- case_gen_ftn(ps1[Npod1], ps1[Npod1])
        temp <- percent_auc_ftn(case, ps1[Npod1], ps1[Npod1], lag_time, auc_work)
        
        auc1_low <- auc1_low + temp$auc_vector[1]
        auc1_mid <- auc1_mid + temp$auc_vector[2]
        auc1_high <- auc1_high + temp$auc_vector[3]
        
        time1_low <- time1_low + temp$time_vector[1]
        time1_mid <- time1_mid + temp$time_vector[2]
        time1_high <- time1_high + temp$time_vector[3]
        
      }
      acc_time <- acc_time + lag_time
      auc_save <- auc_save + auc_work
      
    }else{ # if Npod1 >1
      
      for(i in 1:(Npod1-1)){
        lag_time <- difftime(time1[i+1], time1[i], units="hours")
        auc_work <- (ps1[i+1]+ps1[i]) * lag_time / 2
        
        
        case <- case_gen_ftn(ps1[i], ps1[i+1])
        temp <- percent_auc_ftn(case, ps1[i], ps1[i+1], lag_time, auc_work)
        
        auc1_low <- auc1_low + temp$auc_vector[1]
        auc1_mid <- auc1_mid + temp$auc_vector[2]
        auc1_high <- auc1_high + temp$auc_vector[3]
        
        time1_low <- time1_low + temp$time_vector[1]
        time1_mid <- time1_mid + temp$time_vector[2]
        time1_high <- time1_high + temp$time_vector[3]
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
      }
    }
    
    # last part
    if(Npod2!=0){ # imputation
      
      lag_time <- pod0_duration + 24 - difftime(time1[Npod1], sur_endtime, units = "hours")
      acc1 <- acc_time
      acc2 <- acc_time + difftime(time2[1], time1[Npod1], units = "hours")
      
      imputed_score <- imput_ftn(ps1[Npod1], ps2[1], acc1, acc2, 1, pod0_duration)
      auc_work <-  (ps1[Npod1] + imputed_score) * lag_time /2
      
      case <- case_gen_ftn(ps1[Npod1], imputed_score)
      temp <- percent_auc_ftn(case, ps1[Npod1], imputed_score, lag_time, auc_work)
      
      auc1_low <- auc1_low + temp$auc_vector[1]
      auc1_mid <- auc1_mid + temp$auc_vector[2]
      auc1_high <- auc1_high + temp$auc_vector[3]
      
      time1_low <- time1_low + temp$time_vector[1]
      time1_mid <- time1_mid + temp$time_vector[2]
      time1_high <- time1_high + temp$time_vector[3]
      
      
    }else{  # no imputation
      lag_time <- pod1_duration - difftime(time1[Npod1], sur_endtime, units = "hours")
      auc_work <- ps1[Npod1] * lag_time
      
      case <- case_gen_ftn(ps1[Npod1], ps1[Npod1])
      temp <- percent_auc_ftn(case, ps1[Npod1], ps1[Npod1], lag_time, auc_work)
      
      auc1_low <- auc1_low + temp$auc_vector[1]
      auc1_mid <- auc1_mid + temp$auc_vector[2]
      auc1_high <- auc1_high + temp$auc_vector[3]
      
      time1_low <- time1_low + temp$time_vector[1]
      time1_mid <- time1_mid + temp$time_vector[2]
      time1_high <- time1_high + temp$time_vector[3]
      
    }
    acc_time <- acc_time + lag_time
    auc_save <- auc_save + auc_work
    
    auc1 <- auc_save - auc0
  } # POD1 end

  if(Npod2!=0){
    if(Npod2==1){
      auc_work <- ps2[1] * 24
      
      case <- case_gen_ftn(ps2[1], ps2[1])
      temp <- percent_auc_ftn(case, ps2[1], ps2[1], lag_time, auc_work)
      
      auc2_low <- auc2_low + temp$auc_vector[1]
      auc2_mid <- auc2_mid + temp$auc_vector[2]
      auc2_high <- auc2_high + temp$auc_vector[3]
      
      time2_low <- time2_low + temp$time_vector[1]
      time2_mid <- time2_mid + temp$time_vector[2]
      time2_high <- time2_high + temp$time_vector[3]
      
      auc_save <- auc_save + auc_work
    }else{
      
      # first: Npod1=0 -> no imputation
      if(Npod1==0){
        lag_time <- difftime(time2[1], sur_endtime, units = "hours") - acc_time
        auc_work <- ps2[1] * lag_time
        
        case <- case_gen_ftn(ps2[1], ps2[1])
        temp <- percent_auc_ftn(case, ps2[1], ps2[1], lag_time, auc_work)
        
        auc2_low <- auc2_low + temp$auc_vector[1]
        auc2_mid <- auc2_mid + temp$auc_vector[2]
        auc2_high <- auc2_high + temp$auc_vector[3]
        
        time2_low <- time2_low + temp$time_vector[1]
        time2_mid <- time2_mid + temp$time_vector[2]
        time2_high <- time2_high + temp$time_vector[3]
        
      }else{ # imputation
        lag_time <- difftime(time2[1], sur_endtime, units = "hours") - (pod0_duration + 24)
        auc_work <- (imputed_score + ps2[1]) * lag_time/2
        
        case <- case_gen_ftn(ps2[1], imputed_score)
        temp <- percent_auc_ftn(case, ps2[1], imputed_score, lag_time, auc_work)
        
        auc2_low <- auc2_low + temp$auc_vector[1]
        auc2_mid <- auc2_mid + temp$auc_vector[2]
        auc2_high <- auc2_high + temp$auc_vector[3]
        
        time2_low <- time2_low + temp$time_vector[1]
        time2_mid <- time2_mid + temp$time_vector[2]
        time2_high <- time2_high + temp$time_vector[3]
        
      }
      
      auc_save <- auc_save + auc_work
      acc_time <- acc_time + lag_time
      
      # middle
      for(i in 1:(Npod2-1)){
        lag_time <- difftime(time2[i+1], time2[i], units="hours")
        auc_work <- (ps2[i+1]+ps2[i]) * lag_time / 2
        
        case <- case_gen_ftn(ps2[i], ps2[i+1])
        temp <- percent_auc_ftn(case, ps2[i], ps2[i+1], lag_time, auc_work)
        
        auc2_low <- auc2_low + temp$auc_vector[1]
        auc2_mid <- auc2_mid + temp$auc_vector[2]
        auc2_high <- auc2_high + temp$auc_vector[3]
        
        time2_low <- time2_low + temp$time_vector[1]
        time2_mid <- time2_mid + temp$time_vector[2]
        time2_high <- time2_high + temp$time_vector[3]
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
      }
      
      
      # last
      lag_time <- pod2_duration - difftime(time2[Npod2], sur_endtime, units = "hours")
      auc_work <- ps2[Npod2] * lag_time
      
      
      case <- case_gen_ftn(ps2[Npod2], ps2[Npod2])
      temp <- percent_auc_ftn(case, ps2[Npod2], ps2[Npod2], lag_time, auc_work)
      
      auc2_low <- auc2_low + temp$auc_vector[1]
      auc2_mid <- auc2_mid + temp$auc_vector[2]
      auc2_high <- auc2_high + temp$auc_vector[3]
      
      time2_low <- time2_low + temp$time_vector[1]
      time2_mid <- time2_mid + temp$time_vector[2]
      time2_high <- time2_high + temp$time_vector[3]
      
      
      auc_save <- auc_save + auc_work
      acc_time <- acc_time + lag_time
      
      
    }
    auc2 <- auc_save - (auc0 + auc1)

    
  } # POD2 end

  
  
  pod0_output <- c(auc0_low, auc0_mid, auc0_high, time0_low, time0_mid, time0_high)
  pod1_output <- c(auc1_low, auc1_mid, auc1_high, time1_low, time1_mid, time1_high)
  pod2_output <- c(auc2_low, auc2_mid, auc2_high, time2_low, time2_mid, time2_high)
  
  return(list(pod0_output=pod0_output, pod1_output=pod1_output, pod2_output=pod2_output))
} # function end

POD0matrix <- POD1matrix <- POD2matrix <- matrix(0,n,6)

colnames(POD0matrix) <- c("AUC_low","AUC_mid","AUC_high","time_low","time_mid","time_high")
colnames(POD1matrix) <- c("AUC_low","AUC_mid","AUC_high","time_low","time_mid","time_high")
colnames(POD2matrix) <- c("AUC_low","AUC_mid","AUC_high","time_low","time_mid","time_high")


for(i in 1:n){
  temp_data <- subset(raw_data, record_id==ID[i])
  temp <- AUC_ftn(temp_data, count_matrix[i,])
  
  POD0matrix[i,] <- temp$pod0_output
  POD1matrix[i,] <- temp$pod1_output
  POD2matrix[i,] <- temp$pod2_output
  
  print(i)
}

load("pod_result.rda")

POD0matrix <- as.data.frame(POD0matrix)
POD1matrix <- as.data.frame(POD1matrix)
POD2matrix <- as.data.frame(POD2matrix)


check_ftn <- function(set_input, pod_input, result_input, n_input){
  
  pod <- pod_input + 1
  
  test_value <- round(apply(set_input[,1:3],1,sum))
  true_value <- round(result_input[,pod])
  
  indi <- ifelse(sum(test_value==true_value)==n_input, 1, 0)
  
  return(indi)

}

check_ftn(POD0matrix, 0, result, n)
check_ftn(POD1matrix, 1, result, n)
check_ftn(POD2matrix, 2, result, n)

## little bit of numerical issue but the plots say it's true ... 


# POD2matrix$AUC2 <- result$AUC2
# POD2matrix$AUC2_test <- apply(POD2matrix[,1:3],1,sum)
# 
# which(round(POD2matrix$AUC2)!=round(POD2matrix$AUC2_test)) # 280 335

# > round(POD2matrix$AUC2[335])
# [1] 156
# > round(POD2matrix$AUC2_test[335])
# [1] 157





