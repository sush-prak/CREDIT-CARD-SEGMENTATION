rm(list = ls(all = T))
setwd("C:/Users/admin/Desktop/Sush/R")

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#read the data
cc <- read.csv("credit-card-data.csv")

### Exploring the data

View(cc)
str(cc)
summary(cc)
names(cc)

cc$CUST_ID <- NULL


############ creating new variables and validating the values ################

#(1) Monthly average purchase and cash advance amount
cc$m_avg_purchase<- cc$PURCHASES/cc$TENURE
cc$m_avg_cash_adamount <- cc$CASH_ADVANCE/cc$TENURE

#(2) Purchases by type
cc$PURCHASE_TYPE <- ifelse(cc$ONEOFF_PURCHASES>0 & 
                             cc$INSTALLMENTS_PURCHASES==0,0,
                           ifelse(cc$ONEOFF_PURCHASES==0 & 
                                    cc$INSTALLMENTS_PURCHASES >0,1,2))

#(3) Average amount per purchaseand Cash advance transaction

cc$avg_amt_per_purchase <- cc$PURCHASES/cc$PURCHASES_TRX
cc$avg_amt_per_purchase[is.na(cc$avg_amt_per_purchase)] <- 0

cc$avg_amt_per_purchase[!is.finite(cc$avg_amt_per_purchase)] <- 0


cc$avg_cash_advance_amt <- cc$CASH_ADVANCE/cc$CASH_ADVANCE_TRX
cc$avg_cash_advance_amt[!is.finite(cc$avg_cash_advance_amt)] <- 0

#(4) Balance to credit limit ratio

cc$limit_usage <-cc$BALANCE/cc$CREDIT_LIMIT

# (5) Payments to minimum payments ratio

cc$pay_ratio <- cc$PAYMENTS/cc$MINIMUM_PAYMENTS

View(cc)

# Creating user defined function for descriptive statistics

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  UC1 <- m+2*s
  LC1 <- m-2*s
  UC2 <- m+3*s
  LC2 <- m-3*s
  outlier_flag1<- max>UC1 | min<LC1
  outlier_flag2<- max>UC2 | min<LC2
  return(c(n=n, nmiss=nmiss, outlier_flag1=outlier_flag1, outlier_flag2=outlier_flag2, mean=m, stdev=s,min = min, pctls=pctls,max=max, UC1=UC1, LC1=LC1, UC2=UC2, LC2=LC2))
}

vars <- c("BALANCE","BALANCE_FREQUENCY",
        "PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
        "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY",
        "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
        "PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
        "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE",
        "m_avg_purchase","m_avg_cash_adamount","avg_amt_per_purchase","avg_cash_advance_amt","limit_usage",
        "pay_ratio")

#vars<- sapply(cc,as.numeric)


diagnostic_stats <- t(data.frame(apply(cc[vars], 2, mystats)))

write.csv(diagnostic_stats, "diagnostic_stats.csv")

# Outliers Treatment
udf2<- function(x){
  p5<- quantile(x,0.05,na.rm = T)
  p95<- quantile(x,0.95,na.rm = T)
  x[x<p5]<- p5   # Any value less than p5 are treated as Outlier
  x[x>p95]<- p95 # Any value greater than p95 are treated as Outlier
  return(x)
}
cc[vars]<- data.frame(apply(cc[vars],2,udf2))

#cc$BALANCE[cc$BALANCE>5727.538587]<-5727.538587
#cc$BALANCE_FREQUENCY[cc$BALANCE_FREQUENCY>1.3510787]<-1.3510787
#cc$PURCHASES[cc$PURCHASES>5276.474397]<-5276.474397
#cc$ONEOFF_PURCHASES[cc$ONEOFF_PURCHASES>3912.213206]<-3912.213206
#cc$INSTALLMENTS_PURCHASES[cc$INSTALLMENTS_PURCHASES>2219.743875]<-2219.743875
#cc$CASH_ADVANCE[cc$CASH_ADVANCE>5173.198866]<-5173.198866
#cc$ONEOFF_PURCHASES_FREQUENCY[cc$ONEOFF_PURCHASES_FREQUENCY>0.799129814]<-0.799129814
#cc$CASH_ADVANCE_FREQUENCY[cc$CASH_ADVANCE_FREQUENCY>0.535386977]<-0.535386697
#cc$CASH_ADVANCE_TRX[cc$CASH_ADVANCE_TRX>16.8981203]<-16.8981203
#cc$PURCHASES_TRX[cc$PURCHASES_TRX>64.4251306]<-64.4251306
#cc$CREDIT_LIMIT[cc$CREDIT_LIMIT>11772.09]<-11772.09
#cc$PAYMENTS[cc$PAYMENTS>7523.26]<-7523.26
#cc$MINIMUM_PAYMENTS[cc$MINIMUM_PAYMENTS>5609.1065423]<-5609.1065423
#cc$PRC_FULL_PAYMENT[cc$PRC_FULL_PAYMENT>0.738713]<-0.738713
#cc$TENURE[cc$TENURE>14.19398]<-14.19398
#cc$m_avg_purchase [cc$m_avg_purchase>447.192746] <- 447.192746
#cc$m_avg_cash_adamount[cc$m_avg_cash_adamount>475.2502132] <- 475.2502132
#cc$avg_amt_per_purchase [cc$avg_amt_per_purchase>394.9205613] <- 394.9205613
#cc$avg_cash_advance_amt [cc$avg_cash_advance_amt>1280.216151] <- 1280.216151
#cc$limit_usage[cc$limit_usage >1.16837] <- 1.16837
#cc$pay_ratio[cc$pay_ratio>249.923] <- 249.923


####Missing_value_Analysis########

missing_val = data.frame(apply(cc,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(cc)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

missing_values = data.frame(apply(cc,2,function(x){sum(is.na(x))}))

missing_values

write.csv(missing_values, "Miising_perc.csv", row.names = F)

#Imputuing with mean ####3

cc$CREDIT_LIMIT[which(is.na(cc$CREDIT_LIMIT))] <- mean(cc$CREDIT_LIMIT, na.rm = T)

cc$MINIMUM_PAYMENTS[which(is.na(cc$MINIMUM_PAYMENTS))] <- mean(cc$MINIMUM_PAYMENTS, na.rm = T)

cc$limit_usage[which(is.na(cc$limit_usage))] <- mean(cc$limit_usage, na.rm = T)


cc$pay_ratio[which(is.na(cc$pay_ratio))] <- mean(cc$pay_ratio, na.rm = T)

#cc$CREDIT_LIMIT[which(is.na(cc$CREDIT_LIMIT))] <- 4494.44
#cc$MINIMUM_PAYMENTS[which(is.na(cc$MINIMUM_PAYMENTS))] <- 864.20654
#cc$limit_usage[which(is.na(cc$limit_usage))] <- 0.388926
#cc$pay_ratio[which(is.na(cc$pay_ratio))] <- 9.350070

sum(is.na(cc))


View(cc)



## FACTOR ANALYSIS 

corrm<- cor(cc)  ### CORRELATION MATRIX

#install.packages("psych")
library("psych")

#install.packages("GPArotation")
library("GPArotation")


#Exporting Correlation matrix

#install.packages("writexl")
library("writexl")

write.csv(corrm,"corrm.csv")


#DECIDING NUMBER OF FACTORS USING SCREE PLOT AND EIGEN VALUES

#SCREE PLOT

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) 

#CALCULATING EIGEN VALUES & VARIANCE

#install.packages("dplyr")
library("dplyr")

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       ,pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       ,cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  

write.csv(eigen_values, "eigenvalue.csv", row.names = F)

#CONDUCTING 7 FACTOR ANALYSIS BASED ON RESULTS OF EIGEN VALUES

FA<-fa(r=corrm,7, rotate="varimax", fm="ml")               
print(FA)  


FA_SORT<-fa.sort(FA)    

ls(FA_SORT)     
FA_SORT$loadings

loadings <- data.frame(FA_SORT$loadings[1:ncol(cc),])

write.csv(loadings, "loadings.csv", row.names = T)

#### Cluster analysis ####

#Preparing final Data

Selected_vars <- c("ONEOFF_PURCHASES",
                   "ONEOFF_PURCHASES_FREQUENCY",
                   "PAYMENTS",
                   "CREDIT_LIMIT",
                   "CASH_ADVANCE_TRX",
                   "CASH_ADVANCE",
                   "PURCHASES_INSTALLMENTS_FREQUENCY",
                   "MINIMUM_PAYMENTS", 
                   "BALANCE_FREQUENCY",
                   "INSTALLMENTS_PURCHASES",
                   "TENURE"
)

#standardizing the data

cc1 <- scale(cc[Selected_vars])
View (cc1)

#Building clusters using k-means clustering 

cluster_three <- kmeans(cc1,3)
cluster_four <- kmeans(cc1,4)
cluster_five <- kmeans(cc1,5)
cluster_six <- kmeans(cc1,6)

cluster_new<-cbind(cc,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster,km_clust_6=cluster_six$cluster)
View(cluster_new)

#Graph based on k-means - Optional

#install.packages("cluster")
library("cluster")

clusplot(cc1, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         shade = TRUE, # Lines in clusters
         lines =5, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)


##### Profiling


### Converting into factors

cluster_new$km_clust_3=factor(cluster_new$km_clust_3)
cluster_new$km_clust_4=factor(cluster_new$km_clust_4)
cluster_new$km_clust_5=factor(cluster_new$km_clust_5)
cluster_new$km_clust_6=factor(cluster_new$km_clust_6)


#### preparing profiling sheet

#install.packages("tables")
library("tables")
#options(scipen = 111)


profile<-tabular(1+ONEOFF_PURCHASES+ONEOFF_PURCHASES_FREQUENCY+PAYMENTS+CREDIT_LIMIT
                 +CASH_ADVANCE_TRX+CASH_ADVANCE
                 +PURCHASES_INSTALLMENTS_FREQUENCY+MINIMUM_PAYMENTS+BALANCE_FREQUENCY
                 +INSTALLMENTS_PURCHASES+TENURE~mean+(mean*km_clust_3)+(mean*km_clust_4)
                 +(mean*km_clust_5)+(mean*km_clust_6),data=cluster_new)

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)


profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)
                 +(length*km_clust_6),data=cluster_new)


profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)


