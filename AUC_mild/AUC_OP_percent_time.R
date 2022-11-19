setwd("/Users/wonny/Downloads/CHMC/Pain AUC/DATA")
library(openxlsx)
library(readxl)
library(dplyr)
library(chron)
library(plyr)
library(data.table)
library(lubridate)

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
op_general <- op_general %>% filter(!ID %in% c(2179, 7380, 4527, 2592))
op_pain <- op_pain %>% filter(!ID %in% c(2179, 7380, 4527, 2592))

# ID 150: delete the 2nd anesthesia infor
op_pain[op_pain$ID==150,]$AN_START_DATETIME[2] <- NA
op_pain[op_pain$ID==150,]$AN_STOP_DATETIME[2] <- NA

# total number of patients
n <- nrow(op_general)

# make the indicator variable

indi_gen_ftn <- function(df_input){
  time_vec <- subset(df_input, select=c("PAIN_RECORDED_TIME"))[,1]
  an_start <- subset(df_input, select=c("AN_START_DATETIME"))[,1]
  an_stop <- subset(df_input, select=c("AN_STOP_DATETIME"))[,1]
  
  # remove NA
  an_start <- an_start[!is.na(an_start)]
  an_stop <- an_stop[!is.na(an_stop)]
  time_vec <- time_vec[!is.na(time_vec)]
  
  
  # sort
  an_start <- an_start[order(an_start)]
  an_stop <- an_stop[order(an_stop)]
  time_vec <- time_vec[order(time_vec)] 
  
  n_op <- length(an_start)
  n_time <- length(time_vec)
  indi_matrix <- matrix(0,nrow=n_time,ncol=(n_op+1))
  
  for(i in 1:(n_op+1)){
    
    for(j in 1:n_time){
      
      if(i==1){
        indi_matrix[j,i] <- ifelse(difftime(an_start[i],time_vec[j])>0, 1, 0)
      }else if(1<i && i<(n_op+1)){
        indi_matrix[j,i] <- ifelse( difftime(an_start[i],time_vec[j])>0 && difftime(time_vec[j],an_stop[i-1])>0,1,0 )
      }else{
        indi_matrix[j,i] <- ifelse(difftime(time_vec[j],an_stop[i-1])>0, 1, 0)
      }
    }
    
  }
  
  return(indi_matrix)
}

# indicator matrix for all patients
indi_list <- list()
check <- numeric(n)

for(t in 1:n){
  
  temp_df <- subset(op_pain, ID==op_general$ID[t])
  indi_list[[t]] <- indi_gen_ftn(temp_df)
  check[t] <- sum(apply(indi_list[[t]],1,sum)==0)
  print(t)
}


# remove time & pain_score under anesthesia & if there's only one row -> remove that patient
remove_indi <- numeric(n)
for(t in 1:n){
  
  indi_mat <- indi_list[[t]]
  indi_mat <- indi_mat[apply(indi_mat, 1, sum)==1,]
  remove_indi[t] <- ifelse(is.null(dim(indi_mat)),1,0)
}
table(remove_indi)
which(remove_indi==1) #1488

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

percent_time_ftn <- function(indicator_input,ps1_input,ps2_input,acc1_input,acc2_input){
  lag_input <- acc2_input - acc1_input
  if(indicator_input==1){
    pt <- lag_input
  }else if(indicator_input==2){ # both are greater than 4
    pt <- 0
  }else if(indicator_input==3){ # ps1 < 4< ps2
    pt <- lag_input - ( lag_input * (ps2_input-4) /(ps2_input-ps1_input))
  }else{ # ps1 > 4 > ps2
    pt <- lag_input - ( lag_input * (ps1_input-4) /(ps1_input-ps2_input))
  } 
  
  return(pt)
  
}




#auc ftn
auc_under4_ftn <- function(df_input,discharge_input,indi_mat_input){
  time_vec <- subset(df_input, select=c("PAIN_RECORDED_TIME"))[,1]
  an_start <- subset(df_input, select=c("AN_START_DATETIME"))[,1]
  an_stop <- subset(df_input, select=c("AN_STOP_DATETIME"))[,1]
  ps_vec <- subset(df_input,select=c("PAIN_MEAS_VALUE"))[,1]
  discharge_time <- discharge_input
  
  # remove NA
  an_start <- an_start[!is.na(an_start)]
  an_stop <- an_stop[!is.na(an_stop)]
  time_vec <- time_vec[!is.na(time_vec)]
  ps_vec <- ps_vec[!is.na(ps_vec)]
  
  
  
  temp_order <- data.frame(ps=ps_vec, ts=time_vec)
  temp_order <- temp_order[order(temp_order$ts),]
  
  ps_vec <- temp_order$ps; time_vec <- temp_order$ts
  
  
  # sort
  an_start <- an_start[order(an_start)]
  an_stop <- an_stop[order(an_stop)]
  
  n_op <- length(an_start)
  
  
  indi_mat <- indi_mat_input
  
  time_vec <- time_vec[apply(indi_mat, 1, sum)==1]
  ps_vec <- ps_vec[apply(indi_mat, 1, sum)==1]
  indi_mat <- indi_mat[apply(indi_mat, 1, sum)==1,]
  
  n_time <- length(time_vec)
  
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
  percent_time_save <- 0
  
  
  for(op in 1:(n_op+1)){
    # done==1 -> for loop should stop
    if(done!=1){
      colsum <- apply(indi_mat,2,sum)
      if(colsum[op]>=2){ # colsum[op]= # of pain scores in that period
        
        if(op<=n_op){
          
          first_index <- which(indi_mat[,op]==1)[1]
          last_index <- which(indi_mat[,op]==1)[length(which(indi_mat[,op]==1))]
          
          break_indi <- 0
          for(k in first_index:(last_index-1)){ # calculate auc
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
            pt_work <- percent_time_ftn(indi,ps_vec[k],ps_vec[k+1],acc1,acc2)
            
            
            # and save
            acc_time <- acc_time + lag_time
            auc_save <- auc_save + auc_work
            percent_time_save <- percent_time_save + pt_work
            
            
          }# for edn
          if(break_indi==1){
            done <- 1 # if break -> already acc > 48 -> imputation and done
            
            acc1 <- acc_time
            acc2 <- acc_time + lag_time
            ps1 <- ps_vec[break_index]; ps2 <- ps_vec[break_index+1]
            imputed_score <- imput_ftn(ps1,ps2,acc1,acc2)
            
            auc_work <- (ps1 + imputed_score)*(48-acc1)/2
            
            indi <- indi_return_ftn(ps_vec[break_index],imputed_score) # ps2 = imputed_score
            auc_work <- cutoff_ftn(indi,ps_vec[break_index],imputed_score,acc1,48,auc_work) # acc2 = 48
            pt_work <- percent_time_ftn(indi,ps_vec[break_index],imputed_score,acc1,48)
            
            
            acc_time <- 48
            auc_save <- auc_save + auc_work
            percent_time_save <- percent_time_save + pt_work
            
          } # break -> imputation -> done
          
          
          
        }else{ # last period
          
          
          first_index <- which(indi_mat[,op]==1)[1]
          last_index <- which(indi_mat[,op]==1)[length(which(indi_mat[,op]==1))]
          break_indi <- 0
          
          for(k in first_index:(last_index-1)){
            lag_time <- difftime(time_vec[k+1],time_vec[k],units="hours")
            
            if((acc_time+lag_time) > 48){ 
              break_indi <- 1
              break_index <- k
              break # for statement break
            }
            auc_work <- (ps_vec[k+1]+ps_vec[k]) * lag_time /2
            acc2 <- acc_time + lag_time
            acc1 <- acc_time
            indi <- indi_return_ftn(ps_vec[k],ps_vec[k+1])
            auc_work <- cutoff_ftn(indi,ps_vec[k],ps_vec[k+1],acc1,acc2,auc_work)
            pt_work <- percent_time_ftn(indi,ps_vec[k],ps_vec[k+1],acc1,acc2)
            

            # and save
            acc_time <- acc_time + lag_time
            auc_save <- auc_save + auc_work
            percent_time_save <- percent_time_save + pt_work
            
          } # for end
          
          # break -> imputation for the last one/ break x -> next
          
          if(break_indi==1){ # imputation
            acc1 <- acc_time
            acc2 <- acc_time + lag_time
            ps1 <- ps_vec[break_index]; ps2 <- ps_vec[break_index+1]
            imputed_score <- imput_ftn(ps1,ps2,acc1,acc2)
            
            auc_work <- (ps1 + imputed_score)*(48-acc1)/2
            
            indi <- indi_return_ftn(ps_vec[break_index],imputed_score) # ps2 = imputed_score
            auc_work <- cutoff_ftn(indi,ps_vec[break_index],imputed_score,acc1,48,auc_work) # acc2 = 48
            pt_work <- percent_time_ftn(indi,ps_vec[break_index],imputed_score,acc1,48)
            
            
            acc_time <- 48
            auc_save <- auc_save + auc_work
            percent_time_save <- percent_time_save + pt_work
            
          }else{
            # break x -> there are two cases
            
            if(difftime(discharge_time,time_vec[last_index],units = "hours") < (48-acc_time)){
              lag_time <- 0
              if(ps_vec[last_index]>4){
                auc_work <- 4 * lag_time
              }else{
                auc_work <- ps_vec[last_index] * lag_time
              }
            }else{
              lag_time <- 0
              if(ps_vec[last_index]>4){
                auc_work <- 4 * lag_time
              }else{
                auc_work <- ps_vec[last_index] * lag_time
              }
            }
            
            auc_save <- auc_save + auc_work
            acc_time <- acc_time + lag_time
            
          }
          
          
          
        }
        
        
        
        
      }else{
        auc_save <- auc_save
        acc_time <- acc_time # no change 
      }
      
      
    }
    
    
  }
  return(list(auc_save=auc_save,acc_time=acc_time,pt=percent_time_save))
}

auc_under4_final <- duration_final <- pt_under4_final <- numeric(n)
for(t in 1:n){
  
  if(t!=1488){ # remove 1488th patient
    df_input <- subset(op_pain, ID==op_general$ID[t])
    discharge_input <- op_general$Disch_DT[t]
    indi_mat_input <- indi_list[[t]]
    
    auc_under4_final[t] <- auc_under4_ftn(df_input,discharge_input,indi_mat_input)$auc_save
    duration_final[t] <- auc_under4_ftn(df_input,discharge_input,indi_mat_input)$acc_time
    pt_under4_final[t] <- auc_under4_ftn(df_input,discharge_input,indi_mat_input)$pt
    
  }
  print(t)
  
}

range(duration_final)
range(auc_under4_final)
range(pt_under4_final)



# % calculation
load("AUC_op.rda");auc_OP_normal <- temp_data
auc_OP_merge <- cbind(auc_OP_normal,auc_under4_final)

auc_OP_merge$percent_auc <- numeric(nrow(auc_OP_merge))
for(t in 1:nrow(auc_OP_merge)){
  if(auc_OP_merge$auc_under4_final[t]==0){
    auc_OP_merge$percent_auc[t] <- 1
  }else{
    auc_OP_merge$percent_auc[t] <- auc_OP_merge$auc_under4_final[t]/auc_OP_merge$AUC[t]
  }
  print(t)
}

auc_OP_merge$percent_auc <- auc_OP_merge$percent_auc * 100
auc_OP_merge$percent_time <- pt_under4_final
# merge data

# left_join
op_mild_pain_merge <- left_join(op_pain,auc_OP_merge,by="ID")


# setwd("/Users/wonny/Downloads/CHMC/Pain AUC/Result")
# write.xlsx(op_mild_pain_merge, sheetName="sheet1", file="OP_mild_AUC_combined_data_final.xlsx")

