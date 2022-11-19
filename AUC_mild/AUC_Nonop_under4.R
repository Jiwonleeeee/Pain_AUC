setwd("/Users/wonny/Downloads/CHMC/Pain AUC/DATA")
library(openxlsx)
library(readxl)
library(dplyr)
library(chron)
library(plyr)
library(data.table)
library(lubridate)

Nop_general <- read_excel("AUC_NonOP_Combined_General_Data.xlsx") %>% as.data.frame
Nop_pain <- read_excel("AUC_NonOP_Combined_Anesthesia_Pain.xlsx") %>% as.data.frame


# discharge time
temp_time <- Nop_general$Discharge_Time %>% as.character %>% as.ITime
Discharge_DT <- numeric(nrow(Nop_general))
for(i in 1:nrow(Nop_general)){
  Discharge_DT[i] <- paste(as.character(Nop_general$DISCHARGE_DATE[i]),as.character(temp_time[i])) 
  
}
Nop_general$Disch_DT <- strptime(Discharge_DT, "%Y-%m-%d %H:%M:%S")


# merge
op_general <- read_excel("AUC_OP_Combined_General_Data.xlsx") %>% as.data.frame
op_pain <- read_excel("AUC_OP_Combined_Anesthesia_Pain.xlsx") %>% as.data.frame


# discharge time
temp_time <- op_general$Discharge_Time %>% as.character %>% as.ITime
Discharge_DT <- numeric(nrow(op_general))
for(i in 1:nrow(op_general)){
  Discharge_DT[i] <- paste(as.character(op_general$DISCHARGE_DATE[i]),as.character(temp_time[i])) 
  
}
op_general$Disch_DT <- as.POSIXct(Discharge_DT)

# ID 2179, 7380, 4527, 2592-> move to the NonOP set, so excluded here
temp_gen <- filter(op_general, ID %in% c(2179, 7380, 4527, 2592))
temp_pain <- filter(op_pain, ID %in% c(2179, 7380, 4527, 2592))

# merge
Nop_general <- rbind(Nop_general,temp_gen)
Nop_pain <- rbind(Nop_pain,temp_pain)


# indicator return function
indi_return_ftn <- function(ps1_input, ps2_input){
  
  if(ps1_input<=4 && ps2_input<=4){
    indi <- 1
  }else if(ps1_input>=4 && ps2_input>=4){
    indi <- 2
  }else if(ps1_input<4 && ps2_input>4){
    indi <- 3
  }else{
    indi <- 4
  }
  return(indi)
}

# new auc under 4
cutoff_ftn <- function(indicator_input,ps1_input,ps2_input,acc1_input,acc2_input,aucwork_input){
  
  # indi == 1 : both are less than or equal to 4
  if(indicator_input==1){
    auc <- aucwork_input # no change
  }else if(indicator_input==2){ # both are greater than 4
    auc <- (acc2_input-acc1_input) * 4
  }else if(indicator_input==3){ # ps1 < 4< ps2
    auc <- aucwork_input - (((acc2_input-acc1_input)*(ps2_input-4)^2 )/(2*(ps2_input-ps1_input)))
  }else{ # ps1 > 4 > ps2
    auc <- aucwork_input - (((acc2_input-acc1_input)*(ps1_input-4)^2 )/(2*(ps1_input-ps2_input)))
  } 

  return(auc)
}




# auc funtion
auc_under4_ftn <- function(df_input, discharge_input){
  
  
  time_vec <- subset(df_input, select=c("PAIN_RECORDED_TIME"))[,1]
  ps_vec <- subset(df_input,select=c("PAIN_MEAS_VALUE"))[,1]
  discharge_time <- discharge_input
  
  #remove NA
  time_vec <- time_vec[!is.na(time_vec)]
  ps_vec <- ps_vec[!is.na(ps_vec)]
  
  # sort
  
  temp_order <- data.frame(ps=ps_vec, ts=time_vec)
  temp_order <- temp_order[order(temp_order$ts),]
  
  ps_vec <- temp_order$ps; time_vec <- temp_order$ts
  
  # imputation function
  imput_ftn <- function(p1_input, p2_input, acc1_input, acc2_input){
    
    acc1_input <- as.numeric(acc1_input);acc2_input <- as.numeric(acc2_input)
    if(p2_input >= p1_input){
      x <- (p2_input-p1_input)*(48-acc1_input)/(acc2_input-acc1_input) + p1_input
    }else{
      x <- (p1_input-p2_input)*(acc2_input-48)/(acc2_input-acc1_input) + p2_input
    }
    
    return(x)
  }
  
  auc_save <- 0
  acc_time <- 0
  done <- 0
  
  last_index <- length(time_vec)
  
  break_indi <- 0
  for(k in 1:(last_index-1)){
    lag_time <- difftime(time_vec[k+1],time_vec[k],units="hours")
    
    if((acc_time+lag_time) > 48){ 
      break_indi <- 1
      break_index <- k
      break # for statement break
    }
    
    #break x then
    
    auc_work <- (ps_vec[k+1]+ps_vec[k]) * lag_time /2
    acc2 <- acc_time + lag_time
    acc1 <- acc_time
    indi <- indi_return_ftn(ps_vec[k],ps_vec[k+1])
    auc_work <- cutoff_ftn(indi,ps_vec[k],ps_vec[k+1],acc1,acc2,auc_work)
    
    # and save
    acc_time <- acc_time + lag_time
    auc_save <- auc_save + auc_work
    
  }# for end
  
  # if break -> imputation / not -> min(dis,48)
  if(break_indi==1){
    acc1 <- acc_time
    acc2 <- acc_time + lag_time
    ps1 <- ps_vec[break_index]; ps2 <- ps_vec[break_index+1]
    imputed_score <- imput_ftn(ps1,ps2,acc1,acc2)
    
    auc_work <- (ps1 + imputed_score)*(48-acc1)/2
    
    indi <- indi_return_ftn(ps_vec[break_index],imputed_score) # ps2 = imputed_score
    auc_work <- cutoff_ftn(indi,ps_vec[break_index],imputed_score,acc1,48,auc_work) # acc2 = 48
    
    acc_time <- 48
    auc_save <- auc_save + auc_work
  }else{
    # break x -> 2 cases
    if(difftime(discharge_time,time_vec[last_index],units = "hours") < (48-acc_time)){
      lag_time <- difftime(discharge_time,time_vec[last_index],units = "hours")
      
      if(ps_vec[last_index]>4){
        auc_work <- 4 * lag_time
      }else{
        auc_work <- ps_vec[last_index] * lag_time
      }
      
    }else{
      lag_time <- 48-acc_time
      
      if(ps_vec[last_index]>4){
        auc_work <- 4 * lag_time
      }else{
        auc_work <- ps_vec[last_index] * lag_time
      }
    }
    
    auc_save <- auc_save + auc_work
    acc_time <- acc_time + lag_time
  }
  
  
  return(list(auc=auc_save,duration=acc_time))
  
}# function end

n <- nrow(Nop_general)
auc_under4_final <- duration_final <- numeric(n)
for(t in 1:n){
  
  df_input <- subset(Nop_pain, ID==Nop_general$ID[t])
  discharge_input <- Nop_general$Disch_DT[t]
  
  auc_under4_final[t] <- auc_under4_ftn(df_input,discharge_input)$auc
  duration_final[t] <- auc_under4_ftn(df_input,discharge_input)$duration
  
  print(t)
  
}

range(duration_final)
range(auc_under4_final)

# % calculation
load("AUC_Nonop.rda");auc_nonOP_normal <- temp_data
auc_nonOP_merge <- cbind(auc_nonOP_normal,auc_under4_final)

auc_nonOP_merge$percent <- numeric(nrow(auc_nonOP_merge))
for(t in 1:nrow(auc_nonOP_merge)){
  if(auc_nonOP_merge$auc_under4_final[t]==0){
    auc_nonOP_merge$percent[t] <- 1
  }else{
    auc_nonOP_merge$percent[t] <- auc_nonOP_merge$auc_under4_final[t]/auc_nonOP_merge$AUC[t]
  }
  print(t)
}

auc_nonOP_merge$percent <- auc_nonOP_merge$percent * 100

# merge data

# left_join
Nop_mild_pain_merge <- left_join(Nop_pain,auc_nonOP_merge,by="ID")


# setwd("/Users/wonny/Downloads/CHMC/Pain AUC/Result")
#   write.xlsx(Nop_mild_pain_merge, sheetName="sheet1", file="NonOP_mild_AUC_combined_data_final.xlsx")

