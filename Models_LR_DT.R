
##Backward Stepwise Logistic Regression

library(dplyr)
library(ggplot2)
library(ROCR)
library(pROC)

data <- read.csv("survey.csv")
data <- data[,-1] #Removing S. No. column

data_noNAs <- data %>% 
  select(Age:Country, self_employed:obs_consequence) %>% 
  filter(!is.na(work_interfere)) %>% filter(!is.na(self_employed)) %>% filter(Gender != "") %>% 
  mutate(Gender = factor(as.character(Gender))) #%>% select(-work_interfere)


results <- as.data.frame(NULL)

for(i in 1:100) {
  set.seed(i)
  rand <- runif(nrow(data_noNAs))
  training <- data_noNAs[rand <= 0.7, ]
  testing <- data_noNAs[rand > 0.7,]
  
  logistic <- glm(treatment ~ ., data = training, family = "binomial")
  stepwise_logistic <- step(logistic, direction = "backward", k = log(nrow(training)))
  
  
  stepwise_prediction_training <- predict(stepwise_logistic, type = "response")
  stepwise_prediction_testing <- predict(stepwise_logistic, type = "response", newdata = testing)
  
  analysis <- roc(response=training$treatment, predictor = stepwise_prediction_training)
  e <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities)
  opt_t <- subset(e,e[,2]==max(e[,2]))[,1]
  analysis2 <- roc(response=testing$treatment, predictor = stepwise_prediction_testing)
  
  pred_training <- ifelse(stepwise_prediction_training >= opt_t, 1, 0)
  pred_testing <- ifelse(stepwise_prediction_testing >= opt_t, 1, 0)
  
  tprate <- sum(ifelse(testing$treatment == "Yes" & pred_testing == 1, 1, 0)) / nrow(testing[testing$treatment == "Yes", ])
  fprate <- sum(ifelse(testing$treatment == "No" & pred_testing == 1, 1, 0)) / nrow(testing[testing$treatment == "No", ])
  misclassification_testing <- 1 - sum(ifelse(testing$treatment == "Yes" & pred_testing == 1 | testing$treatment == "No" & pred_testing == 0 , 1, 0)) / nrow(testing)
  misclassification_training <- 1 - sum(ifelse(training$treatment == "Yes" & pred_training == 1 | training$treatment == "No" & pred_training == 0 , 1, 0)) / nrow(training)
  naivepredtraining <- sum(training$treatment == "Yes") / nrow(training)
  naivepredtesting <- sum(testing$treatment == "Yes") / nrow(testing)
  
  results[i, 'nrow_train'] <- nrow(training)
  results[i, 'nrow_testing'] <- nrow(testing)
  results[i, 'threshold'] <- opt_t
  results[i, 'training_auc'] <- analysis$auc
  results[i, 'testing_auc'] <- analysis2$auc
  results[i, 'training_sensitivity'] <- analysis$sensitivities[which.max(analysis$thresholds == opt_t)]
  results[i, 'training_specificity'] <- analysis$specificities[which.max(analysis$thresholds == opt_t)]
  results[i, 'testing_sensitivity'] <- tprate
  results[i, 'testing_specificity'] <- 1 - fprate
  results[i, 'training_misclassification_model'] <- misclassification_training
  results[i, 'testing_misclassification_model'] <- misclassification_testing
  results[i, 'training_misclassification_naive'] <- 1-naivepredtraining
  results[i, 'testing_misclassification_naive'] <- 1-naivepredtesting
  results[i, 'VarsChosen'] <- paste(unlist(t(rownames(as.data.frame(coef(stepwise_logistic))))), collapse = " ")
  results[i, 'Coefficients'] <- paste(as.character(unlist(round(coef(stepwise_logistic),2))), collapse = " ",sep = " ")
}

results %>% group_by(VarsChosen) %>% summarize(freq = n()) %>% arrange

##DT Model

library(ISLR)
library(tidyr)
library(dplyr)
library(ggplot2)

survey = read.csv("survey.csv")

survey
names(survey)
dim(survey)
summary(survey)

survey_data = survey %>%
  filter(Gender != ""  & !is.na(self_employed) & !is.na(work_interfere)) %>%
  dplyr::select(Age:Country, self_employed:obs_consequence) %>%
  mutate(Gender = factor(as.character(Gender)))

attach(survey_data)

survey_data2 = survey_data %>% dplyr::select(Age:Gender,self_employed:obs_consequence)

for(i in 1:10) {
  set.seed(i)
  rand <- runif(nrow(survey_data2)) ##random unification
  training2 <- survey_data2[rand <= 0.7, ]
  testing2 <- survey_data2[rand > 0.7,]
  
  tree.data = tree(treatment~.,training2)
  summary(tree.data)
  tree_predict_testing = predict(tree.data, type = "class", newdata = testing2)
  
  #pred_testing2 <- ifelse(tree_predict_testing >= 0.5, 1, 0)
  
  ##confusion matrix
  #table(tree_predict_testing, testing2$treatment)
  correctpredperc2 <- sum(ifelse(testing2$treatment == "Yes" & tree_predict_testing == "Yes" | testing2$treatment == "No" & tree_predict_testing == "No" , 1, 0)) / nrow(testing2)
  naivepred2 <- sum(testing2$treatment == "Yes") / nrow(testing2) ##giving all yes
  assign(paste("result",i,sep=""), paste("Correct classification DT % = ", round(correctpredperc2,2), " and naive prediction DT % = ", round(naivepred2,2), sep = ""))
}

result1
result2
result3
result4
result5
result6
result7
result8
result9
result10

set.seed(6)
rand <- runif(nrow(survey_data2)) ##generating random uniform numbers
training2 <- survey_data2[rand <= 0.7, ]
testing2 <- survey_data2[rand > 0.7,]

tree.data = tree(treatment~.,training2)
summary(tree.data)
tree_predict_testing = predict(tree.data, type = "class", newdata = testing2)

#pred_testing2 <- ifelse(tree_predict_testing >= 0.5, 1, 0)

##confusion matrix
table(tree_predict_testing, testing2$treatment)
correctpredperc2 <- sum(ifelse(testing2$treatment == "Yes" & tree_predict_testing == "Yes" | testing2$treatment == "No" & tree_predict_testing == "No" , 1, 0)) / nrow(testing2)
naivepred2 <- sum(testing2$treatment == "Yes") / nrow(testing2) ##giving all yes

assign(paste("result6",sep=""), paste("Correct classification DT % = ", round(correctpredperc2,2), " and naive prediction DT % = ", round(naivepred2,2), sep = "")) 

result6

#plot
par(mar=c(1,1,1,1))
plot(tree.data)
text(tree.data,pretty = 0)