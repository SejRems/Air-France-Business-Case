library(readxl)
airfrance <- read_excel("/Users/viviansoo/Desktop/Air France Case Spreadsheet Supplement.xls", 
                        sheet = "DoubleClick")
View(airfrance)

head(airfrance)
str(airfrance)

summary(airfrance)

str(airfrance$`Publisher ID` =="K2615")
airfrance[which(airfrance$`Publisher ID` =="K2615"),]
summary(airfrance[which(airfrance$`Publisher ID` =="K2615"),])

library(ggplot2)

#######

airfrance[which(airfrance$Amount == "0"),]
#4142 out of 4152 have no revenues

airfrance[which(airfrance$`Total Cost` > "0"),]
# but 4509 rows all have total costs spent

airfrance[which(airfrance$Impressions > "10"),]


#########################################
########NORMALIZE
##################

normal <- function(var1) {
  my_normal <- (var1 - min(var1)/(max(var1) - min(var1)))
  return(my_normal)
}
max(airfrance$`Total Volume of Bookings`)
normal(var1 = airfrance$`Total Volume of Bookings`)
###
airfrance$total_bookings_norm == airfrance$`Total Volume of Bookings`
airfrance$clicks_norm == airfrance$Clicks
airfrance$avg_pos_norm == airfrance$`Avg. Pos.`
airfrance$engine_click_thru_norm == airfrance$`Engine Click Thru %`
###

airfrance$total_bookings_norm <- normal(var1 = airfrance$`Total Volume of Bookings`)

airfrance$total_costs_norm <- normal(var1 = airfrance$`Total Cost`)

airfrance$revenue_norm <- normal(var1 = airfrance$Amount)

airfrance$total_cost_per_trans_norm <- normal(var1 = airfrance$`Total Cost/ Trans.`)

airfrance$avg_pos_norm <- normal(var1 = airfrance$`Avg. Pos.`)

airfrance$engine_click_thru_norm <- normal(var1 = airfrance$`Engine Click Thru %`)

airfrance$impressions_norm <- normal(var1 = airfrance$Impressions)

airfrance$avg_cost_per_click_norm <- normal(var1 = airfrance$`Avg. Cost per Click`)

airfrance$click_charges_norm <- normal(var1 = airfrance$`Click Charges`)

airfrance$clicks_norm <- normal(var1=airfrance$Clicks)

airfrance$search_bid_norm <- normal(var1 = airfrance$`Search Engine Bid`)
min(airfrance$`Search Engine Bid`)



####################
###### Weighted version TEST SCORE
#####################

team_score <- function(var1,var2,var3,var4,w1,w2,w3,w4){
  my_score<-w1*var1+w2*var2+w3*var3+w4*var4
}### function


airfrance$Totalscore<- team_score(var1 = airfrance$total_costs_norm, w1=-0.3, 
                      var2 = airfrance$impressions_norm, w2=0.2,
                      var3 = airfrance$avg_cost_per_click_norm, w3=-0.2,
                      var4 = airfrance$revenue_norm, w4=0.3)

which(airfrance$Totalscore<20)

summary(airfrance$Totalscore)
##########################################
############PLot
###############################################
library(ggplot2)
ggplot(data=airfrance, aes(x=avg_cost_per_click_norm, y=Totalscore))+ 
  scale_x_continuous(trans = 'log2') + scale_y_continuous(trans = 'log2')+
  geom_point()+
  geom_smooth()





#axis(side=1, at=seq(0,50000,500), labels=seq(0,50000,500))

#################################################
########TRAIN DATA
#########################################


train_index <- sample(1:nrow(airfrance), 0.8*nrow(airfrance))
airfrance_train <- airfrance[train_index,]
airfrance_test <- airfrance[-train_index,]

train_index_units <- sample(1:nrow(airfrance), 0.8*nrow(airfrance))
airfrance_train_units <- airfrance[train_index,]
airfrance_test_units <- airfrance[-train_index,]

#####################
#####Converting TOTAL SCORE to BINARY
######################
airfrance$score_goodbad<-airfrance$Totalscore

for(i in 1:nrow(airfrance)){
 if(airfrance$Totalscore[i]>=median(airfrance$Totalscore)){
    airfrance$score_goodbad[i]<-"good"} else{airfrance$score_goodbad[i]<-"bad"
   
 }#closing if
}#closing loop

airfrance$score_binary<- gsub("good", "1", airfrance$score_goodbad)
airfrance$score_binary<- gsub("bad", "0", airfrance$score_binary )
airfrance$score_binary<- as.numeric(airfrance$score_binary)

#############################################
#############LOGISTIC REGRESSION
#########################################
my_logit_airunit<-glm(score_binary~`Total Cost`+Impressions+`Avg. Cost per Click`+Amount
                      ,data=airfrance_train, family="binomial")
summary(my_logit_airunit)
exp(my_logit_airunit$coefficients[2])-1
#for every additional unit of Total Cost, the odds of business success decrease by ?%
exp(5.143)-1
exp(-8.212)-1
#for every additional unit of Total Cost, the odds of business success decrease by ?%

my_logit_airunit$coefficients[3]

my_logit_air<-glm(score_binary~total_costs_norm+impressions_norm+avg_cost_per_click_norm+revenue_norm
      ,data=airfrance_train, family="binomial")

summary(my_logit_air)


#####

library(caret)

my_prediction_testing <- predict(my_logit_air, airfrance_test, type="response")

confusionMatrix(data=as.factor(as.numeric(my_prediction_testing > 0.5)),
                reference = as.factor(as.numeric(airfrance_test$score_binary)))

#creating confusion matrix for training data

my_prediction_training <- predict(my_logit_air, airfrance_train, type="response")

confusionMatrix(data=as.factor(as.numeric(my_prediction_training > 0.5)),
                reference = as.factor(as.numeric(airfrance_train$score_binary)))

#make sure the accuracy for training and testing data are the same or constant +/- 5~10%

#creating the AUC ROC framework
#install.packages("ROCR")
library(ROCR)

my_prediction <- my_prediction_training

#converting to ROCR format?
pred_val_logit <- prediction(my_prediction, airfrance_train$score_binary)

perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_logit)
#noticed line is rigidy

#creating a challenger decision tree for german credit card data
library(rpart)
library(rpart.plot)


my_tree <- rpart(score_binary~total_costs_norm+impressions_norm+avg_cost_per_click_norm+revenue_norm, 
                 data=airfrance_train, method="class",
                 cp=0.0001)

rpart.plot(my_tree, extra=1, type=1)



#session 9 comparing model performance

my_tree_predict_test <- predict(my_tree, airfrance_test, type="prob")

my_tree_predict_train <- predict(my_tree, airfrance_train, type="prob")

my_tree_prediction <- prediction(my_tree_predict_train[,2], airfrance_train$score_binary)
#we want the business success probability, so we do the 2nd variable

my_tree_performance <- performance(my_tree_prediction, "tpr", "fpr")

plot(my_tree_performance, col="black")
plot(perf_logit, col="red", add=TRUE)


