library(readxl)
library(compositions)
general_data <- read_excel("/Users/wonny/Documents/Pain_AUC/AUC_0307/AUC_NonOP_Combined_General_Data.xlsx")
load("/Users/wonny/Documents/Pain_AUC/AUC_0307/AUC_Nonop.rda")
temp_set <- auc_nonOP_merge[,c("ID","percent_time")]
merge_set <- left_join(general_data, temp_set, by="ID")


# Function to calculate sample size, conduct model fitting for two group compositions with one categorical variable
lm.comp.ct.grp2 <- function(Y, X){
  # sample size
  df <- data.frame(Y, X)
  n <- table(na.omit(df)$X) # sample size
  
  # model fitting
  lm.c <- lm(alr(Y) ~ X)
  anova(lm.c)
  p <- anova(lm.c)[1, 5] # p-value
  
  # Geometric means
  alrInv(coef(lm.c),orig=Y)
  a <- alrInv(coef(lm.c)[1],orig=Y)   # intercept - geometric means for reference group
  b <- alrInv(rbind(0, matrix(coef(lm.c)[2:length(coef(lm.c))])),orig=Y) # geometric means for each group with the first one for the reference group
  m <- a+b # geometric means
  
  data.frame(sample.size = n, anova.p = p, geometric.mean=m) # output
}


lm.comp.cn.grp2 <- function(Y, X){
  # sample size
  df <- data.frame(Y, X)
  n <- dim(na.omit(df))[1] # sample size
  
  # model fitting
  Y <- na.omit(df)[, 1:2]
  X <- na.omit(df)[,3]
  lm.c <- lm(alr(Y) ~ X)
  anova(lm.c)
  p <- anova(lm.c)[1,5] # p-value
  
  # Geometric mean
  alrInv(coef(lm.c),orig=Y)
  a <- alrInv(coef(lm.c)[1],orig=Y)   # intercept 
  b <- alrInv(rbind(0, mean(X) * coef(lm.c)[2]),orig=Y) 
  c <- alrInv(rbind(0, (mean(X)+sd(X))*coef(lm.c)[2]),orig=Y)
  
  m <- (a+b)[2,] # geometric mean
  msd <- (a+c)[2,] # geometric mean + sd
  
  data.frame(sample.size = n, anova.p = p, 
             geometric.mean.lt4 = m[1], geometric.msd.lt4 = msd[1],
             geometric.mean.gt4 = m[2], geometric.msd.gt4 = msd[2]) # output
}


pt_lt_4 <- merge_set$percent_time
pt_gt_4 <- 100-pt_lt_4
pt_lt_4[pt_lt_4==0] <- 0.000010
pt_gt_4[pt_gt_4==0] <- 0.000010

y <- cbind(pt_lt_4, pt_gt_4)

y <- as.data.frame(y)
Y <- acomp(y)

# age
age <- merge_set$Age
df<-lm.comp.cn.grp2(Y, age)

X = age
lm.c <- lm(alr(Y) ~ X)
anova(lm.c)
p <- anova(lm.c)[1,5] # p-value

# Geometric mean
alrInv(coef(lm.c),orig=Y)
a <- alrInv(coef(lm.c)[1], orig=Y)   # intercept -> beta0만을 이용한 L, G의 추정값?
b <- alrInv(rbind(0, mean(X) * coef(lm.c)[2]),orig=Y) # beta1만을 이용한 L, G의 추정값?

c <- alrInv(rbind(0, (mean(X)+sd(X))*coef(lm.c)[2]),orig=Y)

alrInv(coef(lm.c)[1] +mean(X) * coef(lm.c)[2], orig=Y )

m <- (a+b)[2,] # geometric mean
msd <- (a+c)[2,] # geometric mean + sd


L_vector <- Y[,1]
mu_g <- alrInv(coef(lm.c)[1] +mean(X) * coef(lm.c)[2], orig=Y )[1]


log(exp(sqrt(sum(log(L_vector/mu_g)^2)/length(L_vector) ) ))





# > a
# pt_lt_4      pt_gt_4 
# "0.95143165" "0.04856835" 
# attr(,"class")
# [1] "acomp"

# > b
# pt_lt_4     pt_gt_4    
# [1,] "0.5000000" "0.5000000"
# [2,] "0.1560553" "0.8439447"
# attr(,"class")
# [1] "acomp"

## 3/14: geometric mean이랑 p-value df output에 있는거 갖다 쓴거네 



# sex (1 = male, 0 = female)
sex <- merge_set$sex
df <- lm.comp.ct.grp2(Y,sex)
df
# race_num (1 = w, non-his / 2=b, non-his / 3=his/4=other)
race_num <- merge_set$race_num
df <- lm.comp.ct.grp2(Y,race_num)
df
# -	FnlInsur ( 1 = commercial, 2 = medicaid, 3 = other, 4 = self-pay)
FnlInsure <- merge_set$FnlInsur
df <- lm.comp.ct.grp2(Y,FnlInsure)
df
# -	dep_index (Census-tract level deprivation index
dep_index <- merge_set$dep_index
df <- lm.comp.cn.grp2(Y, dep_index)
df

# -	Injury Year – year admitted for the injury 
# need to extract year
# class(merge_set$Injury_Date[1])
# ex <- merge_set$Injury_Date[1]
# format(ex, "%Y") %>% class

convert_ftn <- function(x_input){
  if(!is.na(x_input)){
    re <- as.numeric(format(x_input, "%Y"))
    return(re)
  }else{
    return(NA)
  }

}

injury_year <- sapply(merge_set$Injury_Date, convert_ftn)

df <- lm.comp.ct.grp2(Y, injury_year)
df
# MOI
head(merge_set$MOI_Group3)
df<-lm.comp.ct.grp2(Y,merge_set$MOI_Group3)
df
# intent
df<-lm.comp.ct.grp2(Y,merge_set$intent)
df

# ISS_NUM
head(merge_set$ISS_Num)
df <- lm.comp.cn.grp2(Y, merge_set$ISS_Num)
df
# GCS_T_NUM
head(merge_set$GCS_T_Num)
df<-lm.comp.cn.grp2(Y, merge_set$GCS_T_Num)
df
# Admitting_Service_Group
head(merge_set$Admitting_Service_Group)
df<-lm.comp.ct.grp2(Y, merge_set$Admitting_Service_Group)
df

# Chronic_Pain_Hx
head(merge_set$Chronic_Pain_Hx)
table(merge_set$Chronic_Pain_Hx)
df<-lm.comp.ct.grp2(Y, merge_set$Chronic_Pain_Hx)
df
# ADHD_HISTORY
head(merge_set$ADHD_HISTORY)
df<-lm.comp.ct.grp2(Y, merge_set$ADHD_HISTORY)
df
# Anxiety_Disorder_Hx
head(merge_set$Anxiety_Disorder_Hx)
df<-lm.comp.ct.grp2(Y, merge_set$Anxiety_Disorder_Hx)
df
# Depressive_Disorder_Hx
head(merge_set$Depressive_Disorder_Hx)
df<-lm.comp.ct.grp2(Y, merge_set$Depressive_Disorder_Hx)
df
# Drug_Use_Disorder_Hx
head(merge_set$Drug_Use_Disorder_Hx)
df<-lm.comp.ct.grp2(Y, merge_set$Drug_Use_Disorder_Hx)
df

# > head(merge_set$Injury_Date)
# [1] "2012-03-18 UTC" "2017-02-08 UTC" NA               "2016-12-21 UTC" "2011-09-17 UTC" "2013-11-03 UTC"
# > head(injury_year)
# [1] 2012 2017   NA 2016 2011 2013






# -	MOI (see separate documentation detailing grouping below)  use MOI_Group3
# Intent ( 1 = unintentional, 2 = self-inflicted, 3 = abuse/assault, 4 = unknown)
# -	ISS  use ISS_Num
# -	GCS (Total, Eye, Verbal, Motor)  use GCS_T_Num





# ## check
# Y = Y.2grp.pod0
# X = merge_set$Age
# lm.c <- lm(alr(Y) ~ X) ## alr(Y) = log(percent time less than 4/ percent time greater than 4)
# summary(lm.c)
# 
# anova(lm.c)
# p <- anova(lm.c)[1,5] # p-value
# 
# ## what is alrInv?
ex <- alr(Y)
ex2 <- alrInv(ex)
# 
# 
# # Geometric mean
# alrInv(coef(lm.c),orig=Y)
# a <- alrInv(coef(lm.c)[1],orig=Y)   # intercept 
# b <- alrInv(rbind(0, mean(X) * coef(lm.c)[2]),orig=Y) 
# c <- alrInv(rbind(0, (mean(X)+sd(X))*coef(lm.c)[2]),orig=Y)
# 
# m <- (a+b)[2,] # geometric mean
# msd <- (a+c)[2,] # geometric mean + sd

## descriptive


summary(merge_set)
str(merge_set)
merge_set$injury_year <- injury_year


pt_lt_0 <- pt_gt_0 <- numeric(nrow(merge_set))
for(i in 1:nrow(merge_set)){
  if(merge_set$percent_time[i]==0){
    pt_lt_0[i] <- 1
  }else if(merge_set$percent_time[i]==100){
    pt_gt_0[i] <- 1
  }
}

merge_set$pt_lt_0 <- pt_lt_0
merge_set$pt_gt_0 <- pt_gt_0

write.xlsx(merge_set, sheetName="sheet1", file="/Users/wonny/Documents/Pain_AUC/AUC_0307/NonOP_merge_data.xlsx")

