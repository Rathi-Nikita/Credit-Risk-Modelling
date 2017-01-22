credge_data <- read.csv(file.choose(),header = T,na.strings=c(""," ","- ","_","NA"))
credge_data <- read.csv(file.choose(),header = T)

View(credge_data)
names(credge_data)
attach(credge_data)
str(credge_data)
#keeping copy 
data <-credge_data 
attach(data)
library(gplots) 
library(ROCR) 
library(Rcpp) 
library(ggplot2) 
library(pROC)
str(data) 
summary(data) 
attach(data)
##deleting unwanted columns
data[,c(1,2,3,5,7,13)] <- data.frame(apply(data[c(1,2,3,5,7,13)],2,as.null))
#converting sales and profit to int
#data$Sales<- (data$Sales*1000)/10000000
#profit in crores
#data$Net.Profit<-(data$Net.Profit*1000)/10000000
##Years in business
data$yearsRating <- 1
summary(data$yearsRating)
attach(data)
names(data)
colnames(data)[5] <- "yrs_in_bus"
data$yearsRating[is.na(data$yrs_in_bus)] <- 0 
data$yearsRating[data$yrs_in_bus == "0 - 1"] <- 2 
data$yearsRating[data$yrs_in_bus == "2 - 3"] <- 3
data$yearsRating[data$yrs_in_bus == "4 - 6"] <- 6 
data$yearsRating[data$yrs_in_bus == "13 - 20"] <- 9
data$yearsRating[data$yrs_in_bus == "21 - Above"] <- 10
###business_exp
names(data)
summary(data$business_exp)
levels(data$business_exp)
data$expRating <- 1
colnames(data)[11]<-"business_exp"
attach(data)
data$expRating[is.na(data$business_exp)] <- 0 
data$expRating[data$business_exp == "0-1"] <- 1 
data$expRating[data$business_exp == "0 - 1"] <- 1 
data$expRating[data$business_exp == "2 - 3"] <- 2 
data$expRating[data$business_exp == "4 - 6"] <- 4 
data$expRating[data$business_exp == "7 - 10"] <- 7 
data$expRating[data$business_exp == "15-20"] <- 9 
data$expRating[data$business_exp == "21-25"] <- 9 
data$expRating[data$business_exp == "21-50"] <- 9 
data$expRating[data$business_exp == "Owned"] <- 9 
data$expRating[data$business_exp == " 26-Above"] <- 10
data$expRating = as.factor(data$expRating) 
levels(data$expRating)
summary(data$expRating) 
summary(data$business_exp)
###score ownofc 
names(data)
colnames(data)[4]<-"type_of_ownership"
data$ofcRating<- 1
levels(data$type_of_ownership)
data$ofcRating[is.na(data$type_of_ownership)] <- 0 
data$ofcRating[data$type_of_ownership == "13 - 20"] <- 0 
data$ofcRating[data$type_of_ownership == "Rented"] <- 4 
data$ofcRating[data$type_of_ownership == "rented"] <- 4 
data$ofcRating[data$type_of_ownership == "Occupied"] <- 4
data$ofcRating[data$type_of_ownership == "Leased"] <- 9
data$ofcRating[data$type_of_ownership == "Owned"] <- 10 
data$ofcRating = as.factor(data$ofcRating)
levels(data$type_of_ownership)
levels(data$ofcRating)
head(data$ofcRating)
###score number of employees
names(data)
colnames(data)[6]<-"employees"
summary(data$employees) 
levels(data$employees)
data$empRating <- 1 
data$empRating[is.na(data$employees)] <- 0 
data$empRating[data$employees == "1-20"] <- 2 
data$empRating[data$employees == "Inadequate"] <- 2
data$empRating[data$employees == "21-50"] <- 4 
data$empRating[data$employees == "51-70"] <- 6 
data$empRating[data$employees == "71-100"] <- 8
data$empRating[data$employees == "101-150"] <- 10
data$empRating[data$employees == "100-150"] <- 10
data$empRating[data$employees == "151 and above"] <-12
data$empRating[data$employees == "Adequate"] <- 8
data$empRating[data$employees == "Not Available"] <- 0
data$empRating<-as.factor(data$empRating)
levels(data$empRating)
levels(data$employees)
##score suppliers base
names(data)
data$supplierRating <- 1
colnames(data)[7]<-"supplier"
data$supplierRating[is.na(data$supplier)] <- 0 
data$supplierRating[data$supplier == "<1.5L"]<- 0
data$supplierRating[data$supplier == "1.5L-3L"] <- 0 
data$supplierRating[data$supplier == "Concentrated Unknown"] <- 4 
data$supplierRating[data$supplier == "Concentrated Reputed"] <- 6 
data$supplierRating[data$supplier == "Dispersed Unknown"] <- 8 
data$supplierRating[data$supplier == "Dispersed Reputed"] <- 10 
data$supplierRating = as.factor(data$supplierRating)
levels(data$supplierRating)
##score buyers base
names(data)
colnames(data)[8]<-"buyers"
summary(data$buyers) 
levels(data$buyers)
data$buyerRating <- 1
data$buyerRating [is.na(data$buyers)] <- 0 
data$buyerRating[data$buyers == "Stiff Credit Terms"] <- 0 
data$buyerRating[data$buyers == "Yes"] <- 0 
data$buyerRating[data$buyers == "No"] <- 0
data$buyerRating[data$buyers == "Concentrated with bad reputation"] <-2 
data$buyerRating[data$buyers == "Concentrated Unknown"]<- 4 
data$buyerRating[data$buyers == "Concentrated Reputed"]<- 6 
data$buyerRating[data$buyers == "Dispersed Unknown"] <-8
data$buyerRating[data$buyers == "Dispersed Reputed"] <-10
data$buyerRating= as.factor(data$buyerRating)
summary(data$buyers) 
summary(data$buyerRating)
##score qualification
names(data)
colnames(data)[10]<-"qualification"
summary(data$qualification) 
levels(data$qualification)
data$qualRating <- 1
data$qualRating[data$qualification == "NULL"] <- 0
data$qualRating[data$qualification == "Under Graduate"] <- 4
data$qualRating[data$qualification == "Professional"] <- 6 
data$qualRating[data$qualification == "Graduate/ No Qualification Available"] <- 6
data$qualRating[data$qualification == "Graduate"] <- 6
data$qualRating[data$qualification == "Post Graduate"] <- 8 
data$qualRating[data$qualification == "Diploma"] <- 8 
data$qualRating[data$qualification == "PG Diploma"] <- 8 
data$qualRating[data$qualification == "Engineering and Management"] <-10 
data$qualRating[data$qualification == "Other Profession higher of above"] <- 10 
data$qualRating= as.factor(data$qualRating)
summary(data$qualRating) 
summary(data$qualification)
##score generation
names(data)
colnames(data)[12]<-"generation"
summary(data$generation) 
levels(data$generation)
data$genRating <- 1
data$genRating[is.na(data$generation)] <- 0 
data$genRating[data$generation == "First Generations"] <- 4 
data$genRating[data$generation == "Second Generations"] <- 8 
data$genRating[data$generation == "Third Generations"] <- 2 
data$genRating[data$generation == "First & Second generations together"] <- 10
data$genRating[data$generation == "Second & Third Generation together"] <- 6 
data$genRating= as.factor(data$genRating)
summary(data$genRating) 
summary(data$generation)
summary(data$supplierRating) 
summary(data$supplier)
###financial scores
# score sales trend
names(data)
colnames(data)[15]<-"trendsales"
summary(data$trendsales) 
levels(data$trendsales)
data$saletrn_Rating <- 1
data$saletrn_Rating[data$trendsales == "NULL"] <- 0
data$saletrn_Rating[data$trendsales == "No previous trend"] <- 0 
data$saletrn_Rating[data$trendsales == "Negative"] <- 2
data$saletrn_Rating[data$trendsales == "Positive"] <- 10 
data$saletrn_Rating[data$trendsales == "Stagnant over last three years"] <- 4 
data$saletrn_Rating[data$trendsales == "increase earlier but decreased last year"] <- 5 
data$saletrn_Rating[data$trendsales == "Decrease earlier but increased last year"] <- 7 
data$saletrn_Rating= as.factor(data$saletrn_Rating)
levels(data$saletrn_Rating)
summary(data$saletrn_Rating) 
summary(data$trendsales)


#score profit trend
names(data)
colnames(data)[17]<-"trendprofit"
summary(data$trendprofit) 
levels(data$trendprofit)
data$profittrn_Rating <- 1
data$profittrn_Rating[data$trendprofit == "NULL"] <- 0 
data$profittrn_Rating[data$trendprofit == "No previous trend"] <- 0 
data$profittrn_Rating[data$trendprofit == "0"] <- 2
data$profittrn_Rating[data$trendprofit == "Negative"] <- 2 
data$profittrn_Rating[data$trendprofit == "Positive"] <- 10 
data$profittrn_Rating[data$trendprofit == "Stagnant over last three years"] <- 4 
data$profittrn_Rating[data$trendprofit == "increase earlier but decreased last year"] <- 5 
data$profittrn_Rating[data$trendprofit == "Decrease earlier but increased last year"] <- 7 
data$profittrn_Rating= as.factor(data$profittrn_Rating)
summary(data$profittrn_Rating) 
summary(data$trendprofit)
#score icr
names(data)
colnames(data)[18]<-"icr"
str(data$icr)
class(data$icr)
#data$icr<-as.numeric(data$icr)
data$icr<-as.double(as.character(data$icr))
data$icrRating[data$icr < 1] <- 0 
data$icrRating[data$icr >= 1 & data$icr <= 1.28] <- 1
data$icrRating[data$icr >= 1.29 & data$icr <= 1.56] <- 2 
data$icrRating[data$icr >= 1.57 & data$icr <= 1.84] <- 3
data$icrRating[data$icr >= 1.85 & data$icr <= 2.12] <- 4 
data$icrRating[data$icr >= 2.13 & data$icr <= 2.42] <- 5 
data$icrRating[data$icr >= 2.43 & data$icr <= 2.72] <- 6 
data$icrRating[data$icr >= 2.73 & data$icr <= 3.02] <- 7 
data$icrRating[data$icr >= 3.03 & data$icr <= 3.32] <- 8 
data$icrRating[data$icr >= 3.33 & data$icr <= 3.62] <- 9 
data$icrRating[data$icr >= 3.63] <- 10
data$icrRating<-as.factor(data$icrRating)
levels(data$icrRating)
summary(data$icrRating)
#score sales to inventory
names(data)
colnames(data)[19]<-"Invturnover"
summary(data$Invturnover) 
data$invRating <- NA
data$Invturnover<-as.double(as.character(data$Invturnover))
data$invRating[data$Invturnover < 3] <- 0 
data$invRating[data$Invturnover >= 3 & data$Invturnover <= 4.15] <- 1 
data$invRating[data$Invturnover >= 4.16 & data$Invturnover <= 5.28] <- 2 
data$invRating[data$Invturnover >= 5.29 & data$Invturnover <= 6.41] <- 3 
data$invRating[data$Invturnover >= 6.42 & data$Invturnover <= 7.54] <- 4 
data$invRating[data$Invturnover >= 7.55 & data$Invturnover <= 8.79] <- 5 
data$invRating[data$Invturnover >= 8.8 & data$Invturnover <= 10.03] <- 6 
data$invRating[data$Invturnover >= 10.04 & data$Invturnover <= 11.29] <- 7 
data$invRating[data$Invturnover >= 11.30 & data$Invturnover <= 12.54] <- 8 
data$invRating[data$Invturnover >= 12.55 & data$Invturnover <= 13.79] <- 9 
data$invRating[data$Invturnover >= 13.8] <- 10
data$invRating= as.factor(data$invRating) 
summary(data$invRating) 
summary(data$Invturnover)
#score collection collection_days 
names(data)
colnames(data)[20]<-"collection_days"
summary(data$collection_days) 
str(data$collection_days)
data$collection_daysRating <- NA
class(data$collection_days)
data$collection_days<-as.double(as.character(data$collection_days))
data$collection_daysRating[data$collection_days > 180] <- 0 
data$collection_daysRating[data$collection_days >= 127 & data$collection_days <= 180] <- 1
data$collection_daysRating[data$collection_days >= 106 & data$collection_days <= 126.99] <- 2
data$collection_daysRating[data$collection_days >= 87 & data$collection_days <= 105.99] <- 3 
data$collection_daysRating[data$collection_days >= 67 & data$collection_days <= 86.99] <- 4 
data$collection_daysRating[data$collection_days >= 61 & data$collection_days <= 66.99] <- 5
data$collection_daysRating[data$collection_days >= 56 & data$collection_days <= 60.99] <- 6 
data$collection_daysRating[data$collection_days >= 51 & data$collection_days <= 55.99] <- 7 
data$collection_daysRating[data$collection_days >= 46 & data$collection_days <= 50.99] <- 8 
data$collection_daysRating[data$collection_days >= 40 & data$collection_days <= 45.99] <- 9 
data$collection_daysRating[data$collection_days < 40] <- 10
data$collection_daysRating = as.factor(data$collection_daysRating) 
summary(data$collection_daysRating) 
summary(data$collection_days)
##score ROA summary(data$roa)
names(data)
colnames(data)[22]<-"roa"
summary(data$roa)
data$roa<-as.double(as.character(data$roa))
data$roaRating <- NA
data$roaRating[data$roa <= 0 ] <- 0 
data$roaRating[data$roa >= 0.01 & data$roa <= 0.82] <- 1 
data$roaRating[data$roa >= 0.83 & data$roa <= 1.66] <- 2 
data$roaRating[data$roa >= 1.67 & data$roa <= 2.50] <- 3 
data$roaRating[data$roa >= 2.51 & data$roa <= 3.34] <- 4 
data$roaRating[data$roa >= 3.35 & data$roa <= 4.19] <- 5 
data$roaRating[data$roa >= 4.20 & data$roa <= 5.19] <- 6 
data$roaRating[data$roa >= 5.20 & data$roa <= 6.19] <- 7 
data$roaRating[data$roa >= 6.20 & data$roa <= 7.19] <- 8 
data$roaRating[data$roa >= 7.20 & data$roa <= 8.19] <- 9 
data$roaRating[data$roa >= 8.20] <- 10
data$roaRating = as.factor(data$roaRating) 
summary(data$roaRating) 
summary(data$roa)
##score NetMargin
names(data)
colnames(data)[23]<-"netmargin"
summary(data$netmargin) 
data$nmRating <- NA
data$netmargin<-as.double(as.character(data$netmargin))
data$nmRating[data$netmargin < 0 ] <- 0 
data$nmRating[data$netmargin >= 0.00 & data$netmargin <= 0.069] <- 1 
data$nmRating[data$netmargin >= 0.07 & data$netmargin <= 1.39] <- 2 
data$nmRating[data$netmargin >= 1.40 & data$netmargin <= 2.09] <- 3 
data$nmRating[data$netmargin >= 2.10 & data$netmargin <= 2.79] <- 4
data$nmRating[data$netmargin >= 2.80 & data$netmargin <= 3.35] <- 5 
data$nmRating[data$netmargin >= 3.36 & data$netmargin <= 3.91] <- 6 
data$nmRating[data$netmargin >= 3.92 & data$netmargin <= 4.47] <- 7 
data$nmRating[data$netmargin >= 4.48 & data$netmargin <= 5.03] <- 8 
data$nmRating[data$netmargin >= 5.04 & data$netmargin <= 5.59] <- 9 
data$nmRating[data$netmargin >= 5.60] <- 10
data$nmRating = as.factor(data$nmRating) 
summary(data$nmRating) 
summary(data$netmargin)
#score DTE
names(data)
colnames(data)[21]<-"dte"
summary(data$dte) 
data$dte<-as.double(as.character(data$dte))
data$dteRating <- NA
data$dteRating[data$dte >= 2.74] <- 0 
data$dteRating[data$dte >= 2.47 & data$dte <= 2.73] <- 1 
data$dteRating[data$dte >= 2.19 & data$dte <= 2.46] <- 2 
data$dteRating[data$dte >= 1.91 & data$dte <= 2.18] <- 3 
data$dteRating[data$dte >= 1.63 & data$dte <= 1.90] <- 4 
data$dteRating[data$dte >= 1.35 & data$dte <= 1.62] <- 5 
data$dteRating[data$dte >= 1.09 & data$dte <= 1.34] <- 6 
data$dteRating[data$dte >= .82 & data$dte <= 1.08] <- 7 
data$dteRating[data$dte >= .55 & data$dte <= .81] <- 8
data$dteRating[data$dte >= .28 & data$dte <= .54] <- 9
data$dteRating[data$dte <= .27] <- 10
data$dteRating = as.factor(data$dteRating) 
summary(data$dteRating) 
###score Sales summary(data$Sales)
names(data)
data$SalesRating <- NA
data$Sales<-as.numeric(as.character(data$Sales))
#Sales in lakhs
data$Sales <-round(((data$Sales/100000)),digits = 2)
##Sales Rating
data$SalesRating[data$Sales < 0.5 ] <- 0 
data$SalesRating[data$Sales >= 0.5 & data$Sales <= 2.39 ] <- 1 
data$SalesRating[data$Sales >= 2.4 & data$Sales <= 5.69] <- 2 
data$SalesRating[data$Sales >= 5.7 & data$Sales <= 8.99 ] <- 3 
data$SalesRating[data$Sales >= 9 & data$Sales <= 12.29 ] <- 4 
data$SalesRating[data$Sales >= 12.3 & data$Sales <= 15.59 ] <- 5 
data$SalesRating[data$Sales >= 15.1 & data$Sales <= 18.89 ] <- 6 
data$SalesRating[data$Sales >= 18.9 & data$Sales <= 22.19 ] <- 7 
data$SalesRating[data$Sales >= 22.2 & data$Sales <= 25.49 ] <- 8 
data$SalesRating[data$Sales >= 25.5 & data$Sales <= 28.79 ] <- 9 
data$SalesRating[data$Sales >= 28.8 ] <- 10
data$SalesRating = as.factor(data$SalesRating)
summary(data$SalesRating) 
summary(data$Sales)

#profit in lakhs
names(data)
colnames(data)[16]<-"netprofit"
class(data$netprofit)
data$netprofit<-as.numeric(as.character(data$netprofit))
data$netprofit <-round(((data$netprofit/100000)),digits = 2)
#rating net profit
data$profitRating <- NA
data$profitRating[data$netprofit < -10 ] <- -4 
data$profitRating[data$netprofit >= -9.99 & data$netprofit <= 0] <- -2 
data$profitRating[data$netprofit >= 0 & data$netprofit <= 0.05] <- 2 
data$profitRating[data$netprofit >= 0.06 & data$netprofit <= 0.1 ] <- 3 
data$profitRating[data$netprofit >= 0.11 & data$netprofit <= 0.15] <- 4 
data$profitRating[data$netprofit >= 0.16 & data$netprofit <= 0.20 ] <- 5 
data$profitRating[data$netprofit >= 0.21 & data$netprofit <= 0.25 ] <- 6 
data$profitRating[data$netprofit >= 0.26 & data$netprofit <= 0.30 ] <- 7 
data$profitRating[data$netprofit >= 0.31 & data$netprofit <= 0.35 ] <- 8 
data$profitRating[data$netprofit >= 0.36 ] <- 10
data$profitRating = as.factor(data$profitRating) 
summary(data$profitRating) 
summary(data$netprofit)
levels(data$profitRating)
####
Performance_Capability<-c()
attach(data)
Performance_Capability<-ifelse((Rating.NSIC=="SE 1A")| (Rating.NSIC=="SE 1B")|(Rating.NSIC=="SE 1C"),append(Performance_Capability,"Highest"),ifelse((Rating.NSIC=="SE 2A")|(Rating.NSIC=="SE 2B")|(Rating.NSIC=="SE 2C"),append(Performance_Capability,"High"),ifelse((Rating.NSIC=="SE 3A")|(Rating.NSIC=="SE 3B")|(Rating.NSIC=="SE 3C"  ),append(Performance_Capability,"moderate"),ifelse((Rating.NSIC=="SE 4A")|(Rating.NSIC=="SE 4B")|(Rating.NSIC=="SE 4C"),append(Performance_Capability,"weak"),ifelse((Rating.NSIC=="SE 5A")|(Rating.NSIC=="SE 5B")|(Rating.NSIC=="SE 5C"),append(Performance_Capability,"Poor"),append(Performance_Capability,"NULL"))))))
Performance_Capability  
Financial_Strength<-c()
Financial_Strength<-ifelse((Rating.NSIC=="SE 1A")| (Rating.NSIC=="SE 2A")|(Rating.NSIC=="SE 3A")|(Rating.NSIC=="SE 4A")|(Rating.NSIC=="SE 5A"),append(Financial_Strength,"High"),ifelse((Rating.NSIC=="SE 1B")| (Rating.NSIC=="SE 2B")|(Rating.NSIC=="SE 3B")|(Rating.NSIC=="SE 4B")|(Rating.NSIC=="SE 5B"),append(Financial_Strength,"Moderate"),ifelse((Rating.NSIC=="SE 1C")|(Rating.NSIC=="SE 2C")|(Rating.NSIC=="SE 3C")|(Rating.NSIC=="SE 4C")|(Rating.NSIC=="SE 5C"),append(Financial_Strength,"Low"),append(Financial_Strength,"Null"))))
data<-data.frame(data,Financial_Strength,Performance_Capability)
data$Perf_cap<-NA
data$Perf_cap[data$Performance_Capability=="Highest"]<-10
data$Perf_cap[data$Performance_Capability=="High"]<-8
data$Perf_cap[data$Performance_Capability=="moderate"]<-6
data$Perf_cap[data$Performance_Capability=="weak"]<-4
data$Perf_cap[data$Performance_Capability=="poor"]<-2
data$Perf_cap<-as.factor(data$Perf_cap)
levels(data$Performance_Capability)
levels(data$Perf_cap)
summary(data$Perf_cap)
summary(data$Performance_Capability)
data$fs_rating<-NA
data$fs_rating[data$Financial_Strength=="High"]<-10
data$fs_rating[data$Financial_Strength=="Moderate"]<-6
data$fs_rating[data$Financial_Strength=="Low"]<-2
data$fs_rating<-as.factor(data$fs_rating)
levels(data$fs_rating)
summary(data$Financial_Strength)
summary(data$fs_rating)
#creating a new dataset with rating
names(data)
sub_data = data[,c(24:42,45,46)] 
summary(sub_data)
sub_data = na.omit(sub_data)
##chi squared chi_data = data4[,-21]
chi_data = sub_data[,-1]
Users <- as.list(colnames(chi_data))
chi_square <- matrix(data = NA , nrow = ncol(chi_data), ncol = ncol(chi_data), dimnames = list(c(Users),c(Users)))
for(i in 1:length(chi_data)){ for(j in 1:length(chi_data)){
  a = chisq.test(table(chi_data[,i],chi_data[,j])) chi_square[i,j] = a$p.value
  }
}

#feature selection with Boruta library(ranger) library(Boruta)
View(sub_data)
dim(sub_data)
attach(sub_data)
install.packages("ranger") 
library("ranger")
install.packages("Boruta")
library(Boruta)
set.seed(123)
?Boruta
sub_data$Default<-as.factor(sub_data$Default)
boruta.train <- Boruta(Default~., doTrace=2,data=sub_data) 
print(boruta.train)
final.boruta <- TentativeRoughFix(boruta.train) 
print(final.boruta)
a = getSelectedAttributes(final.boruta, withTentative = F) 
boruta.df <- attStats(final.boruta) 
class(boruta.df)
boruta.df

###factor analysis techniques
##new data frame
sub_data2 = sub_data[,c(a)]
names(sub_data2)
##dummy creation 
library(lattice) 
library(caret)
dummy1 <- dummyVars("~.", data = sub_data2,fullRank = T)
df_pred = data.frame(predict(dummy1,newdata = sub_data2))
##adding default
sub_data2$Default<-sub_data$Default 
summary(sub_data2$Default) 
#training and testing dataset
set.seed(1)
ind <- sample(2,nrow(sub_data2),replace = T,prob = c(0.75,0.25)) 
dim(sub_data2)
training <- sub_data2[ind == 1,1:15]
test <- sub_data2[ind==2,1:15]
trainlabels <- sub_data2[ind==1,15] 
testlabels <- sub_data2[ind==2,15] 
training$Default = trainlabels 
test$Default = testlabels 
test1<-data.matrix(test[,-15]) 
actual<-test[,15]
sub_data3=data.matrix(training[,-15])
k=training$Default

#logistic regression
model1 = glm(training$Default~.,data = training, family = "binomial") 
summary(model1)
library("leaps")
model1 = regsubsets(training$Default~.,data = training, nvmax=10,method="backward") 
summary(model1)
model1 = glm(training$Default~icrRating+empRating,data = training, family = "binomial") 
summary(model1)
pred0 = predict.glm(model1,newdata = test,type = "response") 
predicted=rep("N",1815)
predicted[pred0 > 0.75]="Y"
t0 <- table(actual,predicted)
t0
##Accuracy
accuracy_log<-sum(diag(t0)/sum(t0))
### 0.9707989
##Miscalssification rate
Error<-(t0[1,2]+t0[2,1])/sum(t0)
Error
#### 0.0292011
##Sensitivity(TP/TP+FN)
ST0 <- t0[2,2]/(t0[2,2]+t0[1,2])
ST0
## 0.6842105
##Specificity(TN/TN+FP)
SP0 <- t0[1,1]/(t0[1,1]+t0[2,1])
SP0
### 0.9769274
###Prevalence(Actualyes/Total)
"how often does yes condition appears in our sample"
PV0<- t0[2,2]/sum(t0)
PV0
#### 0.01432507
roc.model1=roc(predicted,as.numeric(actual))
plot(roc.model1,col = "blue")
roc.model1

####lasso
set.seed(1)
library(Matrix)
library(foreach)
library(glmnet)
library(ISLR)
####lasso regression
model.lasso = glmnet(sub_data3,y=k,alpha=1,family="binomial")
coef(model.lasso)
plot(model.lasso,xvar = "dev") 
summary(model.lasso) 
model.lasso
grid()
#####cv.lambda
model = cv.glmnet(sub_data3,y=k, family = "binomial",alpha=1)
plot(model)
model$lambda.min
#predict on test set
predicted = predict(model, s='lambda.min', newx=test1, type="class")
t1 <- table(actual,predicted)
t1
##Miscalssification rate
ER1 = (t1[1,2]+t1[2,1])/sum(t1)
ER1
##Accuracy[(TP+TN)/TP+TN+FP+FN ]
A1 <- sum(diag(t1))/sum(t1)
A1
##Sensitivity(TP/TP+FN)
ST1 <- t1[2,2]/(t1[2,2]+t1[2,1])
ST1
##Specificity(TN/TN+FP)
SP1 <- t1[1,1]/(t1[1,1]+t1[1,2])
SP1
###Prevalence(Actualyes/Total)
"how often does yes condition appears in our sample"
PV1<- t1[2,2]/sum(t1)
PV1
#####ROC_lasso
library(pROC)
roc.lasso=roc(predicted,as.numeric(actual)) 
plot(roc.lasso,col = "blue")
##ridge regression
model.ridge = glmnet(x=sub_data3,y=k,alpha=0,family='binomial') 
coef(model.ridge)
plot(model.ridge,xvar = "dev") 
summary(model.ridge) 
model.ridge
grid() 
predicted=predict(model.ridge, newx = test1, type = "class",s=0.05)
table(predicted,actual)

######cvridge
cv.model.ridge <- cv.glmnet(sub_data3,y=k,family = "binomial", type.measure = "class",alpha =0)
plot(cv.model.ridge)
cv.model.ridge$lambda.min
coef(cv.model.ridge, s = "lambda.min")
predicted = predict(cv.model.ridge, newx = test1, s = "lambda.min",type = "class") 
t2 <- table(actual,predicted)
t2
##Miscalssification rate
ER2= (t2[1,2]+t2[2,1])/sum(t2)
ER2
##Accuracy[(TP+TN)/TP+TN+FP+FN ]
A2 <- sum(diag(t2))/sum(t2)
##Sensitivity(TP/TP+FN)
ST2 <- t2[2,2]/(t2[2,2]+t2[2,1])
ST2
##Specificity(TN/TN+FP)
SP2 <- t2[1,1]/(t2[1,1]+t2[1,2])
SP2
###Prevalence(Actualyes/Total)
"how often does yes condition appears in our sample"
PV2<- t2[2,2]/sum(t2)
PV2
###roc_ridge
library(pROC)
roc.ridge=roc(predicted,as.numeric(actual))
plot(roc.ridge,col = "blue")

##decision trees
library(rpart)
library(rpart.plot)
library(RColorBrewer)
install.packages("grid")
library(partykit)
library(rattle)
form <- as.formula(training$Default ~ .)
tree.1 <- rpart(form,data=training,control=rpart.control(cp=0.001))
plot(tree.1)
text(tree.1)
prp(tree.1,varlen=3)
##Interatively prune the tree
#new.tree.1 <- prp(tree.1,snip=TRUE)$obj 
#display the new tree
prp(new.tree.1) 
# A more reasonable tree 
tree.2 <- rpart(form,training,parms = list(loss =matrix(c(0,10,1,0),ncol=2)),control=rpart.control(cp=0.001))

prp(tree.2)	# A fast plot
fancyRpartPlot(tree.2)	# A fancy plot from rattle
pred.tree = predict(tree.2,test) 
pred.tree
predicted =rep("N",1815)
predicted[pred.tree[,2] > 0.80]="Y" 
t3=table(predicted,actual)
t3
###Miscalssification rate
ER3= (t3[1,2]+t3[2,1])/sum(t3)
ER3
"zero miss classification"
##Accuracy[(TP+TN)/TP+TN+FP+FN ]
A3 <-sum(diag(t3))/sum(t3)
A3
##Sensitivity(TP/TP+FN)
ST3 <- t3[1,1]/(t3[1,1]+t3[1,2])
ST3
##Specificity(TN/TN+FP)
SP3 <- t3[2,2]/(t3[2,2]+t3[2,1])
SP3

#roc_tree
roc.tree=roc(predicted,as.numeric(actual)) 
plot(roc.tree,col = "blue")

#	##without rating variable
ind <- sample(2,nrow(data),replace = T,prob = c(0.75,0.25)) 
test_tree <- data[ind==2,4:24]
training_tree <- data[ind == 1,4:24]
actual_tree<-test_tree[,21]
training1 = training_tree[,c(1:21)]
form1 <- as.formula(training1$Default ~ .)
tree.3 <- rpart(form1,data=training1,control=rpart.control(minsplit=20,cp=0))
plot(tree.3)
text(tree.3)
#prp(tree.3,varlen=3)
tree.4 <- rpart(form1,training1)	# A more reasonable tree
#prp(tree.4)	# A fast plot
#fancyRpartPlot(tree.4)	# A fancy plot from rattle
pred.tree.2 = predict(tree.4,test_tree)
predicted =rep("N",2139)
predicted[pred.tree.2[,2] > 0.917]="Y"
##confusion matrix
t4 <- table(actual_tree,predicted)
t4
##misclassification rate
ER4 = (t4[1,2]+t4[2,1])/sum(t4)
##Accuracy[(TP+TN)/TP+TN+FP+FN ]
A4 <- sum(diag(t4))/sum(t4)
A4
##Sensitivity(TP/TP+FN)
ST4 <- t4[2,2]/(t4[2,2]+t4[1,2]) 
ST4
##Specificity(TN/TN+FP)
SP4 <- t4[1,1]/(t4[1,1]+t4[2,1])
SP4
##Prevalence
PV4<-t4[2,2]/sum(t4)
PV4
#roc_tree
roc.tree=roc(predicted,as.numeric(actual_tree)) 
plot(roc.tree,col = "blue")

###########################################
#SVM
library(e1071)
svm.model = svm(Default~., data = training, cost =100,gamma =.01) 
summary(svm.model)
pred.svm=predict(svm.model,test)
t5 = table(actual,pred.svm)
t5

##Misclassification error[(FP+FN)/TP+TN+FP+FN]
ER5 = (t5[1,2]+t5[2,1])/sum(t5)
ER5

##Accuracy[(TP+TN)/TP+TN+FP+FN ]
A5 <- sum(diag(t5))/sum(t5)
A5

##Sensitivity(TP/TP+FN)
SP5 <- t5[1,1]/(t5[1,1]+t5[1,2])
SP5

##Specificity(TN/TN+FP)
ST5 <- t5[2,2]/(t5[2,2]+t5[2,1])
ST5
##Prevalence
PV5<-t5[2,2]/sum(t5)
###ROC_svm
roc.svm=roc(pred.svm,as.numeric(actual)) 
plot(roc.svm,col = "blue")


## Random Forest 
set.seed(1) 
library(randomForest)
# Fitting model
fit <- randomForest(Default ~ .,training,ntree=400)
summary(fit)
#Predict Output 
predicted= predict(fit,test)
t6 <- table(predicted,actual)
t6
##Misclassification error[(FP+FN)/TP+TN+FP+FN]
ER6 = (t6[1,2]+t6[2,1])/sum(t6)
ER6
##Accuracy[(TP+TN)/TP+TN+FP+FN ]
A6 <-sum(diag(t6))/sum(t6)
A6
##Sensitivity(TP/TP+FN)
ST6 <- t6[2,2]/(t6[2,2]+t6[2,1])
ST6
##Specificity(TN/TN+FP)
SP6 <- t6[1,1]/(t6[1,1]+t6[1,2])
SP6
#Prevalence
PV6<-t6[2,2]/sum(t6)
#############################
##roc_randomforest
library(pROC)
roc.random_forest=roc(predicted,as.numeric(actual)) 
plot(roc.random_forest,col = "blue") 
lines.roc(roc.svm,col = "cyan")
lines.roc(roc.tree,col = "green") 
lines.roc(roc.model1,col = "red") 
lines.roc(roc.ridge,col = "black") 
lines.roc(roc.lasso,col = "purple")


