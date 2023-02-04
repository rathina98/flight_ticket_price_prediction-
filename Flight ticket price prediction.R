#LOADING THE REQUIRED LIBRARIES
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggrepel)
library(caret)
library(caretensemble)
library(xgboost)
library(glmnet)
library(doparallel)

#SETTING UP THE WORKING DIRECTORY
setwd("C:/Users/Admin/Documents/flight train.csv")
setwd("C:/Users/Admin/Documents/flight_test.csv")
ft_train<-read.csv("flight train.csv")
ft_test<-read.csv("flight_test.csv")

#CREATING THE TARGET VARIABLE IN TEST DATA
ft_test$Price<-0

#COMBINING TRAIN AND TEST DATA
ft_all = rbind(ft_train, ft_test)
ft_all$set = c(rep("ft_train", nrow(ft_train)), rep("ft_test", nrow(ft_test)))
head(ft_all)
tail(ft_all)
str(ft_all)
dim(ft_all)

#CHECKING FOR NULL VALUES
is.na(ft_all)
table(is.na(ft_all))
dim(ft_all)
summary(ft_train)

#FEATURE ENGINEERING ON THE COMBINED DATA
ft_all<-tidyr::separate(ft_all, Date_of_Journey , c('Day', 'Month', 'Year'), sep = "/",remove = FALSE)
ft_all$Day<-as.integer(ft_all$Day)
ft_all$Month<-as.integer(ft_all$Month)
ft_all$Year<-as.integer(ft_all$Year)

ft_all<-tidyr::separate(ft_all, Dep_Time , c('Dep_Hour','Dep_Minute'), sep = ":",remove = FALSE)
ft_all$Dep_Hour<-as.integer(ft_all$Dep_Hour)
ft_all$Dep_Minute<-as.integer(ft_all$Dep_Minute)

h_in_str <- grepl("h", ft_all$Duration)
ft_all$Duration_Hours <- ifelse(h_in_str, sub("h.*", "",ft_all$Duration), 0)
m_in_str <- grepl("m",ft_all$Duration)
ft_all$Duration_Minutes <- ifelse(m_in_str, 
                             gsub("([0-9]+).*$", "\\1", sub(".*h", "",ft_all$Duration)), 0)
ft_all$Duration_Hours<-as.integer(ft_all$Duration_Hours)
ft_all$Duration_Minutes<-as.integer(ft_all$Duration_Minutes)

ft_all<-ft_all%>%mutate(Duration_Total_Minutes=(Duration_Hours*60)+(Duration_Minutes))

ft_all$Total_Stops <- str_replace(ft_all$Total_Stops, "non-stop", "0")
ft_all$Total_Stops <- regmatches(ft_all$Total_Stops, gregexpr("[[:digit:]]+",ft_all$Total_Stops))
as.numeric(unlist(ft_all$Total_Stops))
ft_all$Total_Stops<-as.integer(ft_all$Total_Stops)
is.na(ft_all$Total_Stops)
table(is.na(ft_all$Total_Stops))

sapply(ft_all, function(x) sum(is.na(x)))
ft_all<-ft_all[complete.cases(ft_all),]
sapply(ft_all, function(x) sum(is.na(x)))

ft_all$Airline<-as.factor(ft_all$Airline)
ft_all$Source<-as.factor(ft_all$Source)
ft_all$Destination<-as.factor(ft_all$Destination)

str(ft_all)
dim(ft_all)

#SPLITTING THE TRAIN AND TEST DATA
df_train =ft_all %>% filter(set == "ft_train")
df_test = ft_all %>% filter(set == "ft_test")
dim(df_train)
str(df_train)
view(df_test)

#SELECTING THE REQUIRED FEATURES FOR DATA VISUALISATION
df_dv<-df_train%>%select(c(Airline,Day,Month,Source,Destination, Dep_Hour,Total_Stops,Price,Duration_Total_Minutes ))
dim(df_dv)

ggplot(data = df_dv, aes(x = Price)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "lightblue") +
  labs(x = "Price", y = "Count")
ggplot(data = df_dv, aes(x = 1, y = Price)) +
  geom_boxplot(color = "black") +
  labs(x = "", y = "Price") +
  scale_x_discrete(limits = c(1), labels = "")
ggplot(data = df_dv, aes(x = Price)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "lightblue") +
  labs(x = "Price", y = "Count")+
  facet_wrap(~ Total_Stops, ncol = 1)

ggplot(data = df_dv, aes(x = Price)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "lightblue") +
  labs(x = "Price", y = "Count")+
  facet_wrap(~ Airline, ncol = 1)

df_mean <- aggregate(Price ~ Dep_Hour, data = df_dv, mean)
ggplot(df_mean, aes(x = Dep_Hour, y = Price)) +
  geom_bar(stat = "identity") +
  xlab("Dep_Hour") +
  ylab("Mean Price") +
  ggtitle("Average ticket fare with respect to Departure time")

df_mean <- aggregate(Price ~ Airline, data = df_dv, mean)
ggplot(df_mean, aes(x = Airline, y = Price)) +
  geom_bar(stat = "identity") +
  xlab("Airline")+
  ylab("Mean Price") +
  ggtitle("Average ticket fare of Arilines")+
  geom_text_repel(aes(label = Airline), size = 3, direction = "y", segment.color = "grey")

df_mean <- aggregate(Price ~ Month, data = df_dv, mean)
ggplot(df_mean, aes(x = Month, y = Price)) +
  geom_bar(stat = "identity") +
  xlab("Month")+
  ylab("Mean Price") +
  ggtitle("Average ticket fare as per Month")+
  geom_text_repel(aes(label = Month), size = 3, direction = "y", segment.color = "grey")

df_mean <- aggregate(Price ~Duration_Total_Minutes, data = df_dv, mean)
ggplot(df_dv, aes(x =Duration_Total_Minutes, y = Price)) +
  geom_point() +
  xlab("Duration_Total_Minutes") +
  ylab("Mean Price") +
  ggtitle("Avg. Price with respect to Duration time")

#SELECTING THE REQUIRED FEATURES FOR DATA MANIPULATION
unique(ft_all$Airline)
df_dm<-df_train%>%select(c(Airline,Date_of_Journey,Source,Destination,Route,Dep_Time,Arrival_Time,Duration,Total_Stops,Additional_Info,Price))
dim(df_dm)
str(df_dm)

#dplyr Package
a<-df_train%>%
  select(Airline,Date_of_Journey,Source,Destination,Route,Dep_Hour,Duration,Total_Stops,Additional_Info,Price)%>%
  group_by(Airline)%>%summarise(MaxPrice=max(Price))
a%>%arrange(desc(MaxPrice))
df_train%>%filter(Price==79512)
b<-df_train%>%filter(Airline=="Trujet")
count(b)
c<-df_train%>%group_by(Airline)
count(c)%>%arrange(desc(n))
d<-df_train%>%
  select(Airline,Date_of_Journey,Source,Destination,Route,Dep_Hour,Duration,Total_Stops,Additional_Info,Price)%>%
  group_by(Total_Stops)%>%summarise(MaxPrice=max(Price))
d%>%arrange(desc(MaxPrice))
e<-df_train%>%
  select(Airline,Date_of_Journey,Source,Destination,Route,Dep_Hour,Duration,Total_Stops,Additional_Info,Price)%>%
  group_by(Total_Stops)%>%summarise(MinPrice=min(Price))
e%>%arrange(MinPrice)
f<-df_train%>%
  select(Airline,Date_of_Journey,Source,Destination,Route,Dep_Hour,Duration,Total_Stops,Additional_Info,Price)%>%
  group_by(Total_Stops)%>%summarise(MinPrice=min(Price))
df_train%>%pull(Price) %>%  min()
df_train%>%select(Airline,Date_of_Journey,Source,Destination,Route,Dep_Hour,Duration,Total_Stops,Additional_Info,Price)%>%
  filter(Price==1759)
f<-df_train%>%
  filter(Airline=="Jet Airways"& Source=="Mumbai"&Destination=="Hyderabad")%>%arrange(desc(Price))
head(f)  
h<-df_dm%>%filter(Dep_Time>=22&Dep_Time<=6)%>%summarise(mean(Price))
(h)
df_dm%>%summarise(mean(Price))
g<-df_dm%>%select(Airline,Price)%>%
  filter(Airline=="Jet Airways Business")
g%>%arrange(Price)

#FEATURE SELECTION FOR MODEL BUILDING
m_train<-df_train%>%select(Airline,Day,Month,Source,Destination,Dep_Hour,Duration_Total_Minutes,Total_Stops,Price)
str(m_train)
dim(m_train)

nums = unlist(lapply(m_train, is.numeric))
empnums = m_train[,nums]
head(empnums)
varstodummy = m_train[,sapply(m_train, is.factor)]
head(varstodummy)

#CREATING DUMMY VARIABLES
dummies = dummyVars( ~ ., data = varstodummy)
empdummy = predict(dummies, newdata = varstodummy)
colnames(empdummy)
empfull = data.frame(empdummy, empnums)

# REMOVING NEAR ZERO VARIABLES
removecols = nearZeroVar(empfull, names = TRUE)
removecols
allcols = names(empfull)
empfinal= empfull[ , setdiff(allcols, removecols)]

#SPLITTING THE TRAIN DATASET
ind = createDataPartition(empfinal$Price,p = 0.75,list = FALSE)
ind
ctrain=empfinal[ind,]
ctest=empfinal[-ind,]
dim(ctrain)

#LINEAR REGRESSION
lm<-lm(Price~.,ctrain)
summary(lm)

#RANDOM FOREST
model_rf <- train(Price ~ ., data = ctrain, method = "rf", trControl = trainControl(method = "cv", number = 5), verbose = TRUE)
pred <- predict(model_rf, newdata = ctest)
rmse_value <-sqrt(mean((pred - ctest$Price)^2))
r_squared_value <- 1 - sum((ctest$Price - pred)^2)/sum((ctest$Price - mean(ctest$Price))^2)
print(paste("RMSE:", rmse_value))
print(paste("R-squared:", r_squared_value))

# XGBOOST
fitControl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(nrounds = 100, max_depth = c(2, 3), eta = 0.1, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 1)
fitXGBoost <- train(Price ~ ., data = ctrain, method = "xgbTree", trControl = fitControl, tuneGrid = grid, verbose = FALSE)
predXGBoost <- predict(fitXGBoost, newdata = ctest)
rmse_value <-sqrt(mean((predXGBoost - ctest$Price)^2))
r_squared_value <- 1 - sum((ctest$Price - predXGBoost)^2)/sum((ctest$Price - mean(ctest$Price))^2)
print(paste("RMSE:", rmse_value))
print(paste("R-squared:", r_squared_value))

#RIDGE REGRESSION
fitControl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(alpha = 0.5, lambda = seq(0.001, 1, 0.01))
fitRidge <- train(Price ~ ., data = ctrain, method = "glmnet", trControl = fitControl, tuneGrid = grid, verbose = FALSE)
predRidge <- predict(fitRidge, newdata = ctest)
rmse_value <-sqrt(mean((predRidge - ctest$Price)^2))
r_squared_value <- 1 - sum((ctest$Price - predRidge)^2)/sum((ctest$Price - mean(ctest$Price))^2)
print(paste("RMSE:", rmse_value))
print(paste("R-squared:", r_squared_value))

# ENSEMBLE
models <- c( "rf", "glmnet", "xgbTree")
fitEnsemble <- caretList(Price ~ ., data = ctrain, trControl = trainControl(method = "cv", number = 5), methodList = models, parallel=TRUE)
predEnsemble <- predict(fitEnsemble, newdata = ctest)
rmse_value <-sqrt(mean((predEnsemble - ctest$Price)^2))
r_squared_value <- 1 - sum((ctest$Price - predEnsemble)^2)/sum((ctest$Price - mean(ctest$Price))^2)
print(paste("RMSE:", rmse_value))
print(paste("R-squared:", r_squared_value))

#PREDICTING THE TICKET FARE FOR THE TEST DATASET
m_test<-df_test%>%select(Airline,Day,Month,Source,Destination,Dep_Hour,Duration_Total_Minutes,Total_Stops,Price)
str(m_test)
dim(m_test)
nums = unlist(lapply(m_test, is.numeric))
empnums = m_test[,nums]
head(empnums)
varstodummy = m_test[,sapply(m_test, is.factor)]
head(varstodummy)
dummies = dummyVars( ~ ., data = varstodummy)
empdummy = predict(dummies, newdata = varstodummy)
colnames(empdummy)
empfull = data.frame(empdummy, empnums)
removecols = nearZeroVar(empfull, names = TRUE)
removecols
allcols = names(empfull)
empfinal= empfull[ , setdiff(allcols, removecols)]
predicted_Price <- predict(model_rf, newdata = empfinal)
summary(predicted_Price)

#TEST DATAFRAME WITH PREDICTED PRICE
result<-data.frame(df_test,predicted_Price)
view(result)
summary(result)
