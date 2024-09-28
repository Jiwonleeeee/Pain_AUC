
case_gen_ftn <- function(ps1_input, ps2_input){
  
  if(ps1_input==7 & ps2_input==4){
    case <- 11
  }else if(ps1_input==0 & ps2_input==0){
    case <- 10
  }else if(ps1_input<=4 & ps2_input<=4){
    case <- 1
  }else if(ps1_input<=4 & ps2_input>4 & ps2_input<=7){
    case <- 2
  }else if(ps1_input<=4 & ps2_input>7){
    case <- 3
  }else if(ps1_input>4 & ps1_input<=7 & ps2_input<=4){
    case <- 4
  }else if(ps1_input>4 & ps1_input<=7 & ps2_input>4 & ps2_input<=7){
    case <- 5
  }else if(ps1_input>4 & ps1_input<=7 & ps2_input>7){
    case <- 6
  }else if(ps1_input>7 & ps2_input<=4){
    case <- 7
  }else if(ps1_input>7 & ps2_input>4 & ps2_input<=7){
    case <- 8
  }else{
    case <- 9
  }
    
  return(case)
  
}
# two partitions case
two_partitions_ftn <- function(ps1_input, ps2_input, lag_time_input, cutoff_input){
  
  if(ps1_input<=ps2_input){
    greater <- ps2_input
    less <- ps1_input
  }else{
    greater <- ps1_input
    less <- ps2_input
  }
  
  unknownX <- (greater-cutoff_input) * lag_time_input / (greater-less)
  
  auc_below <- (less + cutoff_input) * (lag_time_input-unknownX)/2
  auc_above <- (greater + cutoff_input) * unknownX/2
  
  time_below <- lag_time_input - unknownX
  time_above <- unknownX
  
  auc_vector <- c(auc_below, auc_above)
  time_vector <- c(time_below, time_above)
  
  return(list(auc_vector=auc_vector, time_vector=time_vector))
  
}

three_partitions_ftn <- function(ps1_input, ps2_input, lag_time_input, auc_work_input, cutoff1_input, cutoff2_input){
  
  if(ps1_input<=ps2_input){
    greater <- ps2_input
    less <- ps1_input
  }else{
    greater <- ps1_input
    less <- ps2_input
  }
  
  unknownX1 <- (greater-cutoff2_input) * lag_time_input / (greater-less)
  unknownX2 <- (greater-cutoff1_input) * lag_time_input / (greater-less)
  
  auc_high <- (greater + cutoff2_input) * unknownX1/2
  auc_mid <- (cutoff1_input + cutoff2_input) * (unknownX2 - unknownX1)/2
  auc_low <- auc_work_input - (auc_high + auc_mid)
  
  time_high <- unknownX1
  time_mid <- unknownX2 - unknownX1
  time_low <- lag_time_input - unknownX2
  
  auc_vector <- c(auc_low, auc_mid, auc_high)
  time_vector <- c(time_low, time_mid, time_high)
  
  return(list(auc_vector=auc_vector, time_vector=time_vector))
  
}




# return low/mid/high
percent_auc_ftn <- function(case_input, ps1_input, ps2_input, lag_time_input, auc_work_input){
  
  case <- case_input
  auc_low <- auc_mid <- auc_high <- 0
  time_low <- time_mid <- time_high <- 0
  
  if(case==11){
    auc_mid <- auc_work_input
    time_mid <- lag_time_input
  }
  
  if(case==10){
    time_low <- lag_time_input
  }

  # case = 1,5,9
  if(case==1){
    auc_low <- auc_work_input
    time_low <- lag_time_input
  }
  
  if(case==5){
    auc_mid <- auc_work_input
    time_mid <- lag_time_input
  }
  
  if(case==9){
    auc_high <- auc_work_input
    time_high <- lag_time_input
  }
  
  # 2 partitions cases
  if(case==2|case==4){ # high = 0
    temp <- two_partitions_ftn(ps1_input, ps2_input, lag_time_input, 4)
    auc_low <- temp$auc_vector[1]
    auc_mid <- temp$auc_vector[2]
    time_low <- temp$time_vector[1]
    time_mid <- temp$time_vector[2]
  }
  
  if(case==6|case==8){ # low = 0
    temp <- two_partitions_ftn(ps1_input, ps2_input, lag_time_input, 7)
    auc_mid <- temp$auc_vector[1]
    auc_high <- temp$auc_vector[2]
    time_mid <- temp$time_vector[1]
    time_high <- temp$time_vector[2]
  }
  
  # 3 partitions case
  if(case==3|case==7){
    temp <- three_partitions_ftn(ps1_input, ps2_input, lag_time_input,auc_work_input, 4, 7)
    auc_low <- temp$auc_vector[1]
    auc_mid <- temp$auc_vector[2]
    auc_high <- temp$auc_vector[3]
    time_low <- temp$time_vector[1]
    time_mid <- temp$time_vector[2]
    time_high <- temp$time_vector[3]
  
  }
  
  
  
  auc_vector <- c(auc_low, auc_mid, auc_high)
  time_vector <- c(time_low, time_mid, time_high)
  
  return(list(auc_vector=auc_vector, time_vector=time_vector))
  
}

# example
# percent_auc_ftn(3, 2, 8, 5, 25)
# sum(percent_auc_ftn(3, 2, 8, 5, 25)$auc_vector) # 25
# sum(percent_auc_ftn(3, 2, 8, 5, 25)$time_vector) # 5

# $auc_vector
# [1]  5.00 13.75  6.25
# 
# $time_vector
# [1] 1.6666667 2.5000000 0.8333333

