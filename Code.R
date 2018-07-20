## THIS SHOULD BE YOUR CODE ##
rm(list=ls(all=TRUE))

setwd("E:/MiTH")
data <- read.csv("Train.csv")
names(data)
#-------------------------------------------------------------------------------------------
summary(data)
str(data)
sum(is.na(data)==TRUE)

# --------
# Seperating Location.Geo(, as delimiter)
Loc<-do.call(rbind,strsplit(as.character(data$Location.Geo),','))
Loc.lat <- as.numeric(Loc[,1])
Loc.lon <- as.numeric(Loc[,2])
data$Loc.lat <- Loc.lat
data$Loc.lon <- Loc.lon
data$Location.Geo <- NULL

# Loc_all <- data[,c("Location.Geo", "Loc.lat", "Loc.lon")]
# rm(Loc_all)
rm(Loc,Loc.lat,Loc.lon)
# --------

# ---------------------------------------
# Replacing ? in Income with NA
data$Income <- as.character(data$Income)
data$Income <- ifelse(data$Income == "?",NA,data$Income)
# -----------------------------------

str(data)
# Convert attribute to appropriate type  
attr <- names(data)
cat_Attr = c("Coverage" , "Education","EmploymentStatus","Gender","Location.Code",
             "Marital.Status","Policy.Type","Policy","Renew.Offer.Type",
             "Sales.Channel","Vehicle.Class","Vehicle.Size")
num_Attr = setdiff(attr, cat_Attr)
rm(attr)

cat_Data <- data.frame(sapply(data[,cat_Attr], as.factor))
num_Data <- data.frame(sapply(data[,num_Attr], as.numeric))

data = cbind(num_Data, cat_Data)
rm(cat_Data, num_Data)

summary(data)

#--------------------------------------------------------------------------------------------
# Deleting records with more than 30% of values missing

table(is.na(data))
sapply(data, function(x) sum(is.na(x)))

data_noNAs <- data
data_noNAs <- data_noNAs[c(rowMeans(is.na(data_noNAs)) < 0.3),]

table(is.na(data_noNAs))
sapply(data_noNAs, function(x) sum(is.na(x)))
# Remove records with more than 6 values missing
# sum(((rowSums(is.na(data))/21)*100) > 30)
#write.csv(Per,file = "Per.csv")

#--------------------------------------------------------------------------------------------
# Replacing NAs in Policy.Type using Policy
str(data_noNAs)
library(qdap) #for beg2char function
data_noNAs$Policy.Type <- as.character(data_noNAs$Policy.Type)
data_noNAs$Policy <- as.character(data_noNAs$Policy)

sum(is.na(data_noNAs$Policy.Type))
for(i in 1:nrow(data_noNAs)){
  if(is.na(data_noNAs[i,18])){
    data_noNAs[i,18] <- paste(beg2char(data_noNAs[i,19], " "),"Auto")
  } 
}

data_noNAs$Policy.Type <- as.factor(data_noNAs$Policy.Type)
data_noNAs$Policy <- as.factor(data_noNAs$Policy)
summary(data_noNAs)
#-------------------------------------------------------------------------------------------------
# data_noNAs[is.na(data_noNAs$Number.of.Open.Complaints)] <- 0   

data_noNAs[["Number.of.Open.Complaints"]][is.na(data_noNAs[["Number.of.Open.Complaints"]])] <- 0
summary(data_noNAs)

fdata <- data_noNAs
#fdata <- fdata[,-c(1)]

library(DMwR)
fdata <- knnImputation(fdata,k=3)
summary(fdata)
str(fdata)

cdata <- fdata
cdata <- cdata[-1]
#write.csv(fdata, file = "cdata.csv")
rm(data,cat_Attr,i,num_Attr, fdata)
###########################End of Data Preprocessing#####################################################################

#Vis1: Corelleogram-----------------

NoLoc <- cdata[,-c(9,10)]
num.cols <- sapply(NoLoc, is.numeric)
cor.data <- cor(NoLoc[,num.cols])
cor.data

#install.packages("corrplot")
#install.packages("corrgram")
library(corrplot)
library(corrgram)
corrplot(cor.data,method='color')

#Vis2: Understanding CLV-----------
hist(cdata$Customer.Lifetime.Value, col="green",breaks=50) # Right Skewed Target
rug(cdata$Customer.Lifetime.Value)
abline(v=median(cdata$Customer.Lifetime.Value), col="magenta", lwd=2)

#Vis3: Understanding Employment status and CLV-----------
boxplot( Customer.Lifetime.Value ~ EmploymentStatus, data = cdata, col="red")
summary(cdata$EmploymentStatus)

#Vis4: Understanding CLV by Gender--------------------------------------
barplot(table(cdata$Gender), col='Wheat', main="#Customers by Gender")

#------------Vis5: Customer Spread: On Map-------------------------------------------------------------------------------
install.packages("maps")
library("ggmap")
library(maptools)
library(maps)

map("world", fill=TRUE, col="white", bg="lightblue",xlim=c(60, 100), ylim=c(0, 40), mar=c(0,0,0,0))
cdataP <-subset(cdata, Coverage == "Premium")
cdataB <-subset(cdata, Coverage == "Basic")
cdataE <-subset(cdata, Coverage == "Extended")

points(cdataP$Loc.lon, cdataP$Loc.lat, col="green", pch=20)
points(cdataB$Loc.lon, cdataB$Loc.lat, col="blue", pch=20)
points(cdataE$Loc.lon, cdataE$Loc.lat, col="yellow", pch=20)

cdataR <-subset(cdata, Location.Code == "Rural")
cdataS <-subset(cdata, Location.Code == "Suburban")
cdataU <-subset(cdata, Location.Code == "Urban")

points(cdataR$Loc.lon, cdataR$Loc.lat, col="green", pch=20)
points(cdataS$Loc.lon, cdataS$Loc.lat, col="blue", pch=20)
points(cdataU$Loc.lon, cdataU$Loc.lat, col="yellow", pch=20)

rm(cor.data,NoLoc,num.cols)
summary(cdata)
###########################End of Visualization#####################################################################

#-------------Clustering-------------------
# Clustering
hdata <- cdata[,c(9,10)]

### add row names
rownames(hdata) <- data_noNAs$CustomerID

distxy <- dist(hdata)
hClustering <- hclust(distxy,method = "ward.D")
plot(hClustering,labels=rownames(hdata))
member <- cutree(hClustering, k = 5)
member
rect.hclust(hClustering, k=5, border="blue")

Cluster <- cbind(hdata,cdata$Customer.Lifetime.Value, cluster5 = member)

rm(hClustering,hdata,distxy,member)
###########################End of Hierarchial Clustering############################################################
cdatalm <- cdata[,-c(9,10)]
cdatalm$Number.of.Policies <- as.factor(cdatalm$Number.of.Policies)
#-------------Split the dataset into test and train - Stratified Sampling------------------------
library(caret)

set.seed(7777)
inTrain=createDataPartition(y=cdatalm$Customer.Lifetime.Value,p=0.7, list=FALSE)
train <- cdatalm[ inTrain,]
test <- cdatalm[-inTrain,]

#Moving Target
tempTrainTRG <- train$Customer.Lifetime.Value
train$Customer.Lifetime.Value <- NULL
tempTestTRG <- test$Customer.Lifetime.Value
test$Customer.Lifetime.Value <- NULL

#------------------Linear Regression-----------------------------------------------------
FinalTest<-read.csv("Test.csv")
summary(FinalTest)
summary(cdata)

summary(cdatalm$Vehicle.Size)

summary(FinalTest$Vehicle.Size)
FinalTest$Number.of.Policies <- as.factor(FinalTest$Number.of.Policies)

convertVSTest <- function(x) {
  if (x == "Large")
    num <- "1"
  else if (x == "Medsize")
    num = "2"
  else 
    num <- "3"
  return(num)
}

FinalTest$Vehicle.Size <- mapply(convertVSTest,FinalTest$Vehicle.Size)
FinalTest$Vehicle.Size <- as.factor(FinalTest$Vehicle.Size)
# cdatalm$Vehicle.Size = as.factor(cdatalm$Vehicle.Size,labels=c("Large","Medsize","Small"),ordered=TRUE)

lin_mod <- lm(tempTrainTRG~.,data = train)
lin_mod1 <- lm(log10(tempTrainTRG)~.,data = train)

summary(lin_mod)      #checking summary of modelsummary(lin_mod1) 
summary(lin_mod1)
#plot(tempTrainTRG)
par(mfrow=c(2,2))     
plot(lin_mod)       #checking for the assumption of linear model
par(mfrow=c(1,1))      # reseting the plotting area
hist(lin_mod$residuals)      
hist(lin_mod1$residuals) 

PredRev_lm <- predict(lin_mod,test)
PredRev_lm1 <- 10^(predict(lin_mod1,test))
library(DMwR)
regr.eval(tempTrainTRG,lin_mod$fitted.values)   #error verification
regr.eval(tempTestTRG,PredRev_lm1)


PredRev_lmT <- predict(lin_mod,FinalTest)
PredRev_lm1T <- predict(lin_mod1,10^FinalTest)

#------------------------Linear Regression Ends-------------------------------------------

#------------------Random Forrest---------------------------------------------------------

cdatarf <- cdata[,-c(9,10)]
str(cdatarf$Number.of.Policies)
cdatarf$Number.of.Policies <- as.factor(cdatarf$Number.of.Policies)
#-------------Split the dataset into test and train - Stratified Sampling------------------------
library(caret)

# set.seed(7777)
# inTrain=createDataPartition(y=cdatarf$Customer.Lifetime.Value,p=0.7, list=FALSE)
# train <- cdatarf[ inTrain,]
# test <- cdatarf[-inTrain,]

#Moving Target
tempTrainTRG <- cdatarf$Customer.Lifetime.Value
cdatarf$Customer.Lifetime.Value <- NULL
# tempTestTRG <- test$Customer.Lifetime.Value
# test$Customer.Lifetime.Value <- NULL

# FinalTest<-read.csv("Test.csv")
# summary(FinalTest)
# summary(cdata)

summary(cdatarf$Vehicle.Size)

# summary(FinalTest$Vehicle.Size)
# FinalTest$Number.of.Policies <- as.factor(FinalTest$Number.of.Policies)


# cdatalm$Vehicle.Size = as.factor(cdatalm$Vehicle.Size,labels=c("Large","Medsize","Small"),ordered=TRUE)
#----------------------------------------------------------------------------------------------------------------


# str(train)
# str(test)
library(dummies)
names(cdatarf)
cattrain<- subset(cdatarf, select =c(Number.of.Policies,Coverage,Education,EmploymentStatus,Gender,Location.Code,Marital.Status,Policy.Type,Policy))    
cattrain<- dummy.data.frame(cattrain)
numtrain <- subset(cdatarf, select = c(Income, Monthly.Premium.Auto, Months.Since.Last.Claim, 
                                       Months.Since.Policy.Inception, Number.of.Open.Complaints, Total.Claim.Amount)) 
numtrain <- scale(numtrain)
cdatarf <- cbind(cattrain,numtrain)

# cattest<- subset(cdatarf, select =c(Number.of.Policies,Coverage,Education,EmploymentStatus,Gender,Location.Code,Marital.Status,Policy.Type,Policy))    
# cattest<- dummy.data.frame(cattest)
# numtest <- subset(test, select = c(Income, Monthly.Premium.Auto, Months.Since.Last.Claim, 
#                                      Months.Since.Policy.Inception, Number.of.Open.Complaints, Total.Claim.Amount)) 
# numtest <- scale(numtest)
# test <- cbind(cattest,numtest)

columnNames <- names(cdatarf)
i=0
for(columnName in columnNames){
  i=i+1
  columnNames[i]<- gsub(" ", ".",columnName,fixed = TRUE )
  columnNames[i]<- gsub("-", ".",columnNames[i],fixed = TRUE )
  
  
}

colnames(cdatarf) <- columnNames 
cdatarf <- cdatarf[,-21]
#install.packages("randomForest")
library(randomForest)
model_rf <- randomForest(tempTrainTRG ~ . , cdatarf,ntree = 50,mtry = 20)

summary(model_rf)
importance(model_rf)
varImpPlot(model_rf)

library(DMwR)

regr.eval(tempTrainTRG, model_rf$predicted)


#-----------------------------------------------------
FinalTest <- read.csv("Test.csv")
names(FinalTest)
CustomerID<-FinalTest[,1]
FinalTest <- knnImputation(FinalTest,k=4)
FinalTest <- FinalTest[,-c(1,7)]
FinalTest$Number.of.Policies <- as.factor(FinalTest$Number.of.Policies)
FinalTest$Location.Geo <- NULL

convertVSTest <- function(x) {
  if (x == "Large")
    num <- "1"
  else if (x == "Medsize")
    num = "2"
  else 
    num <- "3"
  return(num)
}

FinalTest$Vehicle.Size <- mapply(convertVSTest,FinalTest$Vehicle.Size)
FinalTest$Vehicle.Size <- as.factor(FinalTest$Vehicle.Size)
catFinalTest<- subset(FinalTest, select =c(Number.of.Policies,Coverage,Education,EmploymentStatus,Gender,Location.Code,Marital.Status,Policy.Type,Policy))    
catFinalTest<- dummy.data.frame(catFinalTest)
numFinalTest <- subset(FinalTest, select = c(Income, Monthly.Premium.Auto, Months.Since.Last.Claim, 
                                             Months.Since.Policy.Inception, Number.of.Open.Complaints, Total.Claim.Amount)) 
numFinalTest <- scale(numFinalTest)
FinalTest <- cbind(catFinalTest,numFinalTest)


columnNames <- names(FinalTest)
i=0
for(columnName in columnNames){
  i=i+1
  columnNames[i]<- gsub(" ", ".",columnName,fixed = TRUE )
  columnNames[i]<- gsub("-", ".",columnNames[i],fixed = TRUE )
  
  
}

colnames(FinalTest) <- columnNames

preds_rf <- predict(model_rf, FinalTest)
Predictions<-cbind(CustID,preds_rf)
colnames(Predictions)<- c("CustomerID","Customer.Lifetime.Value")
write.csv(Predictions,"Predictions.csv",row.names = FALSE)
# regr.eval(tempTestTRG,preds_rf)
##################Random Forrest Ends##############################################################################

library(xgboost)
# Constructing the Dense matrix on the train and test data
dtrain = xgb.DMatrix(data = as.matrix(cdatarf),
                     label = tempTrainTRG)


# fit the model
model = xgboost(data = dtrain, max.depth = 4, 
                eta = 0.1, nthread = 4, nround = 40, 
                objective = "reg:linear", verbose = 1)

importance <- xgb.importance(feature_names = names(cdatarf), model = model)
print(importance)
xgb.plot.importance(importance_matrix = importance)

pred <- predict(model, as.matrix(FinalTest))
Predictions1<-cbind(CustID,preds_rf)
colnames(Predictions)<- c("CustomerID","Customer.Lifetime.Value")
write.csv(Predictions,"Predictions.csv",row.names = FALSE)
