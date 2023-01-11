rm(list=ls())
library(openxlsx)
library(readxl)
library(dplyr)
library(chron)
library(plyr)
library(data.table)
library(lubridate)

setwd("/Users/wonny/Downloads/CHMC/Pain AUC/pod_race_result")
load(file="raw_data_nrs.rda")
n <- length(unique(raw_data$newID))
ID <- unique(raw_data$newID)


## count # of observations in each period
# tabulate does not count pod2 if there's no pod2
# manual count
source('percent_time_auc_ftns.R')
count_ftn <- function(id_input, data_input){
  
  pod_vector <- subset(data_input, PAT_ID==id_input)$POD
  
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
  
  sur_endtime <- data_input$TIME_IN_OOR_2[1]
  pod0_duration <- difftime(data_input$SURGERY_DATE[1]+3600*24, sur_endtime, units = "hours")
  
  if(pod0_duration<0){
    pod0_duration <- difftime(as.Date(ymd_hms(sur_endtime)), sur_endtime, units = "hours")+24
  }
  
  
  pod1_duration <- pod0_duration + 24
  pod2_duration <- pod0_duration + 48   ## accumulative, so this is the total duration
  
  pod <- data_input$POD # pod
  time_vec <- data_input$RECORDED_TIME2 # pain recorded time
  ps_vec <- data_input$NRS_SCORE # pain score
  
  
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
    
  }else{
    acc_time <- pod0_duration
  }
  # POD0 end ################################################
  
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
    }
    
    
    
    auc1 <- auc_save - auc0
  }else{
    acc_time <- pod1_duration
  } # POD1 end
  
  
  ## POD2 start
  if(Npod2!=0){
    if(Npod2==1){
      
      ###########
      if(Npod1==0){
        auc_work <- ps2[1] * 24
        
        case <- case_gen_ftn(ps2[1], ps2[1])
        temp <- percent_auc_ftn(case, ps2[1], ps2[1], 24, auc_work)
        
        auc2_low <- auc2_low + temp$auc_vector[1]
        auc2_mid <- auc2_mid + temp$auc_vector[2]
        auc2_high <- auc2_high + temp$auc_vector[3]
        
        time2_low <- time2_low + temp$time_vector[1]
        time2_mid <- time2_mid + temp$time_vector[2]
        time2_high <- time2_high + temp$time_vector[3]
        
        auc_save <- auc_save + auc_work
      }else{
        # imputation and last one
        lag_time <-  difftime(time2[1], sur_endtime, units = "hours") - pod1_duration
        auc_work <- (imputed_score + ps2[1]) * lag_time/2
        
        
        case <- case_gen_ftn(ps2[1], imputed_score)
        temp <- percent_auc_ftn(case, ps2[1], imputed_score, lag_time, auc_work)
        
        auc2_low <- auc2_low + temp$auc_vector[1]
        auc2_mid <- auc2_mid + temp$auc_vector[2]
        auc2_high <- auc2_high + temp$auc_vector[3]
        
        time2_low <- time2_low + temp$time_vector[1]
        time2_mid <- time2_mid + temp$time_vector[2]
        time2_high <- time2_high + temp$time_vector[3]
        
        
        auc_save <- auc_save + auc_work
        acc_time <- acc_time + lag_time
        
        # last one
        
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
      
      
      ##########
      
      
    }else{
      
      # first: Npod1=0 -> no imputation
      if(Npod1==0){
        lag_time <- difftime(time2[1], sur_endtime, units = "hours") - pod1_duration
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
        lag_time <- difftime(time2[1], sur_endtime, units = "hours") - pod1_duration
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

colnames(POD0matrix) <- paste0(c("AUC_mild_POD", "AUC_mod_POD", "AUC_sev_POD", "time_mild_POD", "time_mod_POD", "time_sev_POD"), 0)
colnames(POD1matrix) <- paste0(c("AUC_mild_POD", "AUC_mod_POD", "AUC_sev_POD", "time_mild_POD", "time_mod_POD", "time_sev_POD"), 1)
colnames(POD2matrix) <- paste0(c("AUC_mild_POD", "AUC_mod_POD", "AUC_sev_POD", "time_mild_POD", "time_mod_POD", "time_sev_POD"), 2)

#surgery_end_DT <- numeric(n)
for(i in 1:n){
  
  if(i==1422){
    POD0matrix[i,] <- rep(0,6)
    POD1matrix[i,] <- rep(0,6)
    POD2matrix[i,] <- rep(0,6)
  }else{
    temp_data <- subset(raw_data, newID==ID[i])
    #surgery_end_DT[i] <- as.POSIXct(unique(temp_data$set_new))
    temp <- AUC_ftn(temp_data, count_matrix[i,])
    
    temp$pod0_output[temp$pod0_output<0 & temp$pod0_output> -10^-5] <- 0
    temp$pod1_output[temp$pod1_output<0 & temp$pod1_output> -10^-5] <- 0
    temp$pod2_output[temp$pod2_output<0 & temp$pod2_output> -10^-5] <- 0
    
    POD0matrix[i,] <- temp$pod0_output
    POD1matrix[i,] <- temp$pod1_output
    POD2matrix[i,] <- temp$pod2_output
  }
  
  print(i)
}


POD0matrix <- as.data.frame(POD0matrix)
POD1matrix <- as.data.frame(POD1matrix)
POD2matrix <- as.data.frame(POD2matrix)


# apply(POD2matrix, 2, range)


# check_ftn <- function(set_input, pod_input, result_input, n_input){
#   
#   pod <- pod_input + 1
#   
#   test_value <- round(apply(set_input[,1:3],1,sum))
#   true_value <- round(result_input[,pod])
#   
#   indi <- ifelse(sum(test_value==true_value)==n_input, 1, 0)
#   
#   return(indi)
# 
# }

# check_ftn(POD0matrix, 0, result, n)
# check_ftn(POD1matrix, 1, result, n)
# check_ftn(POD2matrix, 2, result, n)

## little bit of numerical issue but the plots say it's true ... 

# POD0matrix$AUC0_real <- result$AUC0
# POD0matrix$AUC0_test <- apply(POD0matrix[,1:3],1,sum)
# POD0matrix$duration_real <- result$Duration0
# POD0matrix$duration_test <- apply(POD0matrix[,4:6],1,sum)
# 
# 
# POD1matrix$AUC1_real <- result$AUC1
# POD1matrix$AUC1_test <- apply(POD1matrix[,1:3],1,sum)
# POD1matrix$duration_real <- result$Duration1
# POD1matrix$duration_test <- apply(POD1matrix[,4:6],1,sum)
# 
# POD2matrix$AUC2_real <- result$AUC2
# POD2matrix$AUC2_test <- apply(POD2matrix[,1:3],1,sum)
# POD2matrix$duration_real <- result$Duration2
# POD2matrix$duration_test <- apply(POD2matrix[,4:6],1,sum)

# 
# which(round(POD2matrix$AUC2)!=round(POD2matrix$AUC2_test)) # 280 335

# > round(POD2matrix$AUC2[335])
# [1] 156
# > round(POD2matrix$AUC2_test[335])
# [1] 157

# POD0matrix$duration_test <- apply(POD0matrix[,4:6],1,sum)
# POD0matrix$actual_duration <- result$Duration0
# POD0matrix$auc_test <- apply(POD0matrix[,1:3],1,sum)
# POD0matrix$actual_auc <- result$AUC0
# 
# 
# plot(POD0matrix$duration_test[POD0matrix$duration_test!=0])
# sum(POD0matrix$duration_test!=0)
# points(1:661, POD0matrix$duration_rea[POD0matrix$duration_test!=0], col="red",cex=0.5)
# 
# POD0matrix$actual_auc[POD0matrix$duration_test==0] 
# which(POD0matrix$duration_test!=POD0matrix$actual_duration)


#POD0matrix <- cbind(unique(raw_data[,c("record_id","set_new")]), POD0matrix)
# final data
load("race_auc_nrs.rda")
POD_list <- list(POD0matrix, POD1matrix, POD2matrix)

save_list <- list()
for(i in 1:3){
  
  temp_data <- POD_list[[i]]
  pod <- i-1
  
  temp_data <- cbind(temp_data, result[,c(paste0("AUC",pod), paste0("Duration",pod), paste0("Missing",pod))])
  
  # percent_AUC
  temp_AUC <- temp_data[,1:3]
  temp_pct <- matrix(0, n, 3)
  for(j in 1:n){
    
    if(temp_data[,paste0("Missing",pod)][j]==1){
      temp_pct[j,] <- rep(0,3)
    }else{
      
      if(temp_data[,paste0("AUC",pod)][j]==0){
        temp_pct[j,] <- c(1,0,0)
      }else{
        temp_pct[j,] <- as.numeric(temp_AUC[j,] / temp_data[,paste0("AUC",pod)][j])
      }
      
    }
    temp_pct[j,] <- temp_pct[j,] * 100
    
  } #percent end
  
  # matrix-> data frame/ colnames change and bind
  temp_pct <- as.data.frame(temp_pct)
  colnames(temp_pct) <- c(paste0("pctAUC_mild_POD",pod), paste0("pctAUC_mod_POD",pod), paste0("pctAUC_sev_POD",pod))
  
  temp_data <- cbind(temp_data, temp_pct)
  
  
  # percent time : similar
  temp_time <- temp_data[,4:6]
  temp_pct <- matrix(0, n, 3)
  for(j in 1:n){
    
    if(temp_data[,paste0("Missing",pod)][j]==1){
      temp_pct[j,] <- rep(0,3)
    }else{
      temp_pct[j,] <- as.numeric(temp_time[j,] / temp_data[,paste0("Duration",pod)][j])
    }
    temp_pct[j,] <- temp_pct[j,] * 100
  }
  
  
  temp_pct <- as.data.frame(temp_pct)
  colnames(temp_pct) <- c(paste0("pctTime_mild_POD",pod), paste0("pctTime_mod_POD",pod), paste0("pctTime_sev_POD",pod))
  
  temp_data <- cbind(temp_data, temp_pct)
  
  # standardize AUC -> only pod 0
  if(pod==0){
    
    stand_AUC_POD0 <- numeric(n)
    for(j in 1:n){
      if(temp_data[,paste0("Missing",pod)][j]==1){
        stand_AUC_POD0[j] <- 0
      }else{
        stand_AUC_POD0[j] <- (temp_data[,paste0("AUC",pod)][j] * 24) / temp_data[,paste0("Duration",pod)][j]
      }
      
    }
    temp_data <- cbind(temp_data, stand_AUC_POD0)
  }
  
  # if missing =1 -> duration = 0
  
  for(j in 1:n){
    if(temp_data[,paste0("Missing",pod)][j]==1){
      temp_data[,paste0("Duration",pod)][j] <- 0
    }
  }
  
  save_list[[i]] <- temp_data
  
}

Merge_data <- cbind(save_list[[1]], save_list[[2]])
Merge_data <- cbind(Merge_data, save_list[[3]])

apply(Merge_data, 2, range)

# id and surgery date
Merge_data <- cbind(unique(raw_data[,c("PAT_ID","SURGERY_DATE")]), Merge_data)
Merge_data$newID <- ID
# add surgery end time
temp_data <- subset(raw_data, select = c(TIME_IN_OOR_2, newID ))
unique_set <- unique(temp_data[,c("TIME_IN_OOR_2","newID")])
n 
dim(unique_set)
which(table(unique_set$newID)==2)

Merge_temp <- left_join(Merge_data, unique_set, by="newID")
which(Merge_temp$newID=="ID288")
Merge_temp <- Merge_temp[-289,]


# IDduplicateSet_nrs <- subset(raw_data, newID=="ID288")
# save(IDduplicateSet_nrs, file="/Users/wonny/Downloads/CHMC/Pain AUC/pod_race_result/diffendtime_nrs.rda")
## 
Merge_data <- subset(Merge_temp, select=-newID)

# apply(Merge_data,2,range)
# export
setwd("/Users/wonny/Downloads/CHMC/AUC pod race/result")
write.xlsx(Merge_data, sheetName="sheet1", file="AUC_pod_nrs.xlsx")


