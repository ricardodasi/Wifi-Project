#Wifi location project

#load packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,)


#load the training and validation data set ----


raw_training_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

raw_validation_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')

#data exploration ----

str(raw_training_data_set)

#19937 observations, 529 variables

#Colnames for non WAP obervations

colnames(raw_training_data_set[,521:529])

# "LONGITUDE"        "LATITUDE"         "FLOOR"            "BUILDINGID"       "SPACEID"         
# "RELATIVEPOSITION" "USERID"           "PHONEID"          "TIMESTAMP" 


#Plotting data distribution for waps

options(scipen=999)

list_of_waps <- stack(raw_training_data_set[,1:520])

list_of_waps <- list_of_waps %>% filter(values < 100 ) 

ggplot(list_of_waps, aes(x=values))+
  geom_histogram(color = 'black',
                  fill='white',
                  binwidth = 1)+
  geom_vline(aes(xintercept=mean(values)),
                  color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.2, fill="#FF6666") 


#the mean of the value is around -80 dbm, while the mode is closer to the -95 dbm. there seems
#to be values that are separated from this distribution that  are >-25 dbm, however the data
#set information states that there will be 1.7% of the data in the range of -40 dbm to 0 dbm.


outlier_waps <- list_of_waps %>% filter(values >= -40,values <= 0)
  
#2560 obs / 19937*520 obs total data -> 0.0002 

#observations are in the tolerance range

#exploring total observations per column 

waps <- raw_training_data_set[,1:520]

waps$count <- apply(waps,1, function(x) sum(x!=100))


ggplot(waps, aes(x=count))+
  geom_histogram(color = 'black',
                 fill='white',
                 binwidth = 1)+
  geom_vline(aes(xintercept=mean(count)),
             color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.2, fill="#FF6666") 

#mean observations around 18, mode closer to 16 for observations related to waps detected

location_data <- raw_training_data_set %>% select('LONGITUDE','LATITUDE','FLOOR')


location_data_gg_wraped = ggplot(location_data) + 
  geom_point(aes(x=LONGITUDE,color=FLOOR,y=LATITUDE),size=2) +
  scale_color_continuous(limits=c(0,4)) +
  ggtitle("Points by floor") +
  theme(title = element_text(size=8),
        text = element_text(size=12))+
  facet_wrap(FLOOR~.)

location_data_gg = ggplot(location_data) + 
  geom_point(aes(x=LONGITUDE,color=FLOOR,y=LATITUDE),size=2) +
  scale_color_continuous(limits=c(0,4)) +
  ggtitle("Points by floor") +
  theme(title = element_text(size=8),
        text = element_text(size=12))


plot_ly(x=raw_training_data_set$LATITUDE, 
        y=raw_training_data_set$LONGITUDE, 
        z=raw_training_data_set$FLOOR, 
        type="scatter3d", 
        mode="markers", 
        size=3)


#it seems that the 4th floor only has measures for the last building and that density seems to 
#change from one floor to another in specific points probably due to the building structure 

#Cheking building distribution 

building_distribution <- raw_training_data_set %>% select(BUILDINGID) %>% 
  group_by(BUILDINGID)

ggplot(building_distribution,aes(x=BUILDINGID))+ geom_bar()

#most of the observations are on the 2nd building, while observations on building 0 and 1 are almost 
#equal

#exploring labels to predict

individual_ids <- raw_training_data_set %>% select(BUILDINGID,SPACEID) %>% 
  group_by(SPACEID,BUILDINGID) %>% 
  summarise(total = sum(n()))

individual_ids <- individual_ids[order(individual_ids$SPACEID),]

duplicated_ids <- individual_ids[which(duplicated(individual_ids$SPACEID)==T),]

duplicated_ids <- unique(duplicated_ids)

#there are 138 factors that are duplicated for the the space id, will need to 
#add to this data the column of the building to be able to work with it
#as predicting individual ID would confuse the model with different kind of
#observations pointing to the same place

#Checking the data distribution by floor

histogram(raw_training_data_set$FLOOR)

#percent of the data by floor seems pretty stable except for the 4th floor which has
#the lowest percent of the total of the data. this is consistent for previous plots
#as the building number two is the only one that has a 4th floor.


#Preprocessing ----

#transforming categorical columns into factor

raw_training_data_set$FLOOR <- as.factor(raw_training_data_set$FLOOR)

raw_training_data_set$BUILDINGID <- as.factor(raw_training_data_set$BUILDINGID)

raw_training_data_set$SPACEID <- as.factor(raw_training_data_set$SPACEID)

raw_training_data_set$RELATIVEPOSITION <- as.factor(raw_training_data_set$RELATIVEPOSITION)

raw_training_data_set$USERID <- as.factor(raw_training_data_set$USERID)

raw_training_data_set$PHONEID <- as.factor(raw_training_data_set$PHONEID)


#given the repeated spaceid number, we are creating a new variable to be predicted
#that is a factor of building mixed with space id,adding and additional data set
#for testing with in/out of the space variable

base_data_for_classification <- raw_training_data_set

base_data_for_classification_in_out <- raw_training_data_set

base_data_for_classification$building_space <- as.factor(paste(base_data_for_classification$BUILDINGID,
                                                     base_data_for_classification$SPACEID,
                                                     sep = '-'))

base_data_for_classification_in_out$building_space__in_out <- as.factor(paste(base_data_for_classification_in_out$BUILDINGID,
                                                                              base_data_for_classification_in_out$RELATIVEPOSITION,
                                                                              base_data_for_classification_in_out$SPACEID,
                                                                              sep = '-'))


#removing columns with no information to make the modelling faster

#starting with the non wap data, we take out the variables for regression and the id's

base_data_for_classification <-base_data_for_classification[,-c(529,528,527,526,525,524,523,522,521)]

base_data_for_classification_in_out <-base_data_for_classification_in_out[,-c(529,528,527,526,525,524,523,522,521)]


#checking for columns that add no information and model would disregard as they are
#constants

base_data_for_classification <- base_data_for_classification[,-c(nearZeroVar(base_data_for_classification, uniqueCut = 0))]

base_data_for_classification_in_out <- base_data_for_classification_in_out[,-c(nearZeroVar(base_data_for_classification_in_out, uniqueCut = 0))]


#Random Forest fist model approach ----


#making a first approach to the classification problem with the full data set to 
#evaluate possible next steps

intraining_raw <- createDataPartition(raw_training_data_set$SPACEID,
                                      p=0.70,
                                      list = FALSE) 

training_raw <- raw_training_data_set[intraining_raw,]

testing_raw <- raw_training_data_set[-intraining_raw,]


# first_approach_random_forest <- ranger(formula = SPACEID~.,
#                                        data = training_raw,
#                                        verbose = T)

first_approach_random_forest <- readRDS("c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/first_approach_random_forest")


first_approach_predictions <- predict(first_approach_random_forest,testing_raw)

confusionMatrix(first_approach_predictions$predictions,testing_raw$SPACEID)

# Accuracy : 0.8797               
# 95% CI : (0.8712, 0.8879)     
# No Information Rate : 0.0244               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.8781      


#We can observe that base accuracy is fairly good with a high kappa score
#but we need to take into account that this measure those not include building and 
#in/out measure.

#trying to predict real scenario with full location factor created


training_raw$full_location <- as.factor(paste(training_raw$BUILDINGID,
                                     training_raw$RELATIVEPOSITION,
                                     training_raw$SPACEID,
                                     sep ='-'))

testing_raw$full_location <- as.factor(paste(testing_raw$BUILDINGID,
                                   testing_raw$RELATIVEPOSITION,
                                   testing_raw$SPACEID,
                                    sep ='-'))

#removing the variables not used for predictions

training_raw <- training_raw[,-c(521:529)]

testing_raw <- testing_raw[,-c(521:529)]

#modelling

# second_approach_random_forest <- ranger(formula = full_location~.,
#                                        data = training_raw,
#                                        verbose = T)


second_approach_random_forest <- readRDS("c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/second_approach_random_forest.rds")


second_approach_predictions <- predict(second_approach_random_forest,testing_raw)

postResample(second_approach_predictions$predictions,testing_raw$full_location)

# Accuracy : 0.9249               
# 95% CI : (0.9179, 0.9315)     
# No Information Rate : 0.0096               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9245               

#accuracy actually increases when the specific labels are added, testing accuracy
#and kappa against second available data set


raw_validation_data_set$full_location <- as.factor(paste(raw_validation_data_set$BUILDINGID,
                                                         raw_validation_data_set$RELATIVEPOSITION,
                                                         raw_validation_data_set$SPACEID,
                                                         sep ='-'))

raw_validation_predictions <- predict(second_approach_random_forest,raw_validation_data_set)

postResample(raw_validation_predictions$predictions,raw_validation_data_set$full_location)


#Only building and floor are available in the validation data set, given this, we need to change
#the approach for a classification problem as it is not possible to measure the predictions 
#without this data.


#switching approach to predictions, will work with longitude and latitude 

#trying to determine what's the most succesfull metric to predict to add information to the 
#table by phases for predictions


#target variables 
#LATITUDE
#LONGITUDE
#BUILDINGID
#FLOOR

#building quick models for this

#returning training raw and testing raw to base state for the new predictions

intraining_raw <- createDataPartition(raw_training_data_set$SPACEID,
                                      p=0.70,
                                      list = FALSE) 

training_raw <- raw_training_data_set[intraining_raw,]



testing_raw <- raw_training_data_set[-intraining_raw,]



#predicting variables

k <-c(522,521,524,523)


for (i in k) { 
  
  training_raw_2 <- training_raw[,c(1:520,i)]
  testing_raw_2 <- testing_raw[,c(1:520,i)]

  models_for_comparison <- ranger(formula = training_raw_2[,521]~.,
                                         data = training_raw_2[,1:520],
                                         verbose = T,
                                         importance = 'impurity')

  predictions_for_comparison <- predict(models_for_comparison,testing_raw_2)

  results <- postResample(predictions_for_comparison$predictions,testing_raw_2[,521])

  print(colnames(training_raw_2[521]))
  print(results)

}

#Random Forest result for each of the variables as predictors

# [1] "LATITUDE"
#      RMSE  Rsquared       MAE 
# 6.7863857 0.9898378 4.4333349 
# [1] "LONGITUDE"
#      RMSE  Rsquared       MAE 
# 9.0825324 0.9946211 5.4436726 
# [1] "BUILDINGID"
# Accuracy     Kappa 
# 0.9984841 0.9976169 
# [1] "FLOOR"
# Accuracy     Kappa 
# 0.9907361 0.9880252 




#General model approach with random forest seems pretty accurate, will train the model 
#and test against real world behaviour with the validation set

#assigning nominal variables as factor for predictions

raw_training_data_set$FLOOR <- as.factor(raw_training_data_set$FLOOR)

raw_training_data_set$BUILDINGID <- as.factor(raw_training_data_set$BUILDINGID)

raw_validation_data_set$FLOOR <- as.factor(raw_validation_data_set$FLOOR)

raw_validation_data_set$BUILDINGID <- as.factor(raw_validation_data_set$BUILDINGID)

k <-c(522,521,524,523)

for (i in k) {

  full_train_data <- raw_training_data_set[,c(1:520,i)]
  test_validation_data <- raw_validation_data_set[,c(1:520,i)]


  models_for_comparison <- ranger(formula = full_train_data[,521]~.,
                                  data = full_train_data[,1:520],
                                  verbose = T,importance = 'impurity')

  predictions_for_comparison <- predict(models_for_comparison,test_validation_data)

  results_phase_3 <- postResample(predictions_for_comparison$predictions,test_validation_data[,521])

  print(colnames(full_train_data[521]))
  print(results_phase_3)

}

# [1] "LATITUDE"
#       RMSE   Rsquared        MAE 
# 11.1871928  0.9771573  7.5487570 
# [1] "LONGITUDE"
#       RMSE   Rsquared        MAE 
# 12.0921702  0.9901277  8.6260217 
# [1] "BUILDINGID"
# Accuracy    Kappa 
#         1        1 
# [1] "FLOOR"
# Accuracy     Kappa 
# 0.8676868 0.8156839 


#Confussion matrix for floor given that it was the least accurate

#                     predicted
# true    0    1    2    3    4
#    0 4321   11    0   37    0
#    1   28 4965    7    2    0
#    2    1   18 4377   20    0
#    3    1    0    8 5039    0
#    4    0    0    0    3 1099

#we can see that the error is spread out all buildings, however, third floor was the worse floor
#to be predicted and it also gets the third floor predicted as the fourth floor. Having additional
#information from the buildings will probably help the models discriminate better between floors

#I will now review different models with it's basic componets to see which one might be the best
#one for this problem.



#Normal KNN model approach ----

#predicting latitude with the training set

# knn_base_model_latitude <- knn.reg(train = training_raw[,1:520],
#                           y = training_raw[,522],
#                           k=3,
#                           test = testing_raw[,1:520])
# 
# saveRDS(knn_base_model_latitude, 'knn_base_model_latitude.rds')

knn_base_model_latitude <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_model_latitude.rds')

knn_predictions_base_latitude <- data.frame(predicted = knn_base_model_latitude$pred,
                              real_values = testing_raw$LATITUDE)

postResample(knn_predictions_base_latitude$predicted,
             knn_predictions_base_latitude$real_values)

#      RMSE  Rsquared       MAE 
# 8.2785240 0.9847465 3.0302466 

#taking note on the difference between MAE and RMSE, outlier preprocessing might be benefical.

#base error seem to be performing worse than random forest against train set, predicting 
#validation set


#predicting latitude with the validation set

# knn_base_validation_model_latitude <- knn.reg(train = raw_training_data_set[,1:520],
#                                                 y = raw_training_data_set[,522],
#                                                 k=3,
#                                                 test = raw_validation_data_set[,1:520])
# 
# saveRDS(knn_base_validation_model_latitude, 'knn_base_validation_model_latitude.rds')

knn_base_validation_model_latitude <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_validation_model_latitude.rds')

knn_predictions_base_validations_latitude <- data.frame(predicted = knn_base_validation_model_latitude$pred,
                                                          real_values = raw_validation_data_set$LATITUDE)

postResample(knn_predictions_base_validations_latitude$predicted,
             knn_predictions_base_validations_latitude$real_values)


#       RMSE   Rsquared        MAE 
# 13.9293919  0.9615796  8.2390610 

#Viewing the error against the validation set

knn_predictions_base_validations_latitude$percentual_error <- (knn_predictions_base_validations_latitude$real_values-knn_predictions_base_validations_latitude$predicted)/knn_predictions_base_validations_latitude$real_values

plot(knn_predictions_base_validations_latitude$percentual_error)


#there doesnt seem to be clear patterns, just some outliers

knn_predictions_base_validations_latitude$real_error <- (knn_predictions_base_validations_latitude$real_values-knn_predictions_base_validations_latitude$predicted)

plot(knn_predictions_base_validations_latitude$real_error)

#observation index bigger and smaller than than 800 and 400 seem to be where the biggest errors are


#predicting longitude with the training set

# knn_base_model_longitude <- knn.reg(train = training_raw[,1:520],
#                           y = training_raw[,521],
#                           k=3,
#                           test = testing_raw[,1:520])

#saveRDS(knn_base_model_longitude, 'knn_base_model_longitude.rds')

knn_base_model_longitude <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_model_longitude.rds')

knn_predictions_base_longitude <- data.frame(predicted = knn_base_model_longitude$pred,
                                            real_values = testing_raw$LONGITUDE)

postResample(knn_predictions_base_longitude$predicted,
             knn_predictions_base_longitude$real_values)


# RMSE  Rsquared       MAE 
# 8.8630784 0.9948535 3.5483600 

#predicting longitude with the validation set


# knn_base_validation_model_longitude <- knn.reg(train = raw_training_data_set[,1:520],
#                                                 y = raw_training_data_set[,521],
#                                                 k=3,
#                                                 test = raw_validation_data_set[,1:520])
# 
# saveRDS(knn_base_validation_model_longitude, 'knn_base_validation_model_longitude.rds')

knn_base_validation_model_longitude <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_validation_model_longitude.rds')

knn_predictions_base_validations_longitude <- data.frame(predicted = knn_base_validation_model_longitude$pred,
                                                        real_values = raw_validation_data_set$LONGITUDE)

postResample(knn_predictions_base_validations_longitude$predicted,
             knn_predictions_base_validations_longitude$real_values)


# RMSE   Rsquared        MAE 
# 16.9320687  0.9803688  9.0332732

#bigger error but with a good R squared.


#predicting building ID

# knn_base_model_building_id <- knn(train = training_raw[,1:520],
#                                    cl = training_raw[,524],
#                                    test = testing_raw[1:520],
#                                    k = 3)
# 
# saveRDS(knn_base_model_building_id, 'knn_base_model_building_id.rds')

knn_base_model_building_id <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_model_building_id.rds')

knn_predictions_base_building_id <- data.frame(predicted = knn_base_model_building_id,
                                                         real_values = testing_raw$BUILDINGID)

confusionMatrix(knn_predictions_base_building_id$predicted,
                knn_predictions_base_building_id$real_values)

# Confusion Matrix and Statistics

#                  Reference
# Prediction      0    1    2
#            0 1562    0    0
#            1    0 1528    1
#            2    0    9 2837
# 
# Overall Statistics
# 
# Accuracy : 0.9983               
# 95% CI : (0.9969, 0.9992)     
# No Information Rate : 0.478                
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9973

#Error is low with good kappa, mistakes for building id are in building 1 and 2,
#specially in bulding number 1. similar results against test with random forest



#predicting building id against validation set

# knn_base_model_validation_building_id <- knn(train = raw_training_data_set[,1:520],
#                                                cl = raw_training_data_set[,524],
#                                                test = raw_validation_data_set[1:520],
#                                                k = 3)

#saveRDS(knn_base_model_validation_building_id, 'knn_base_model_validation_building_id.rds')

knn_base_model_validation_building_id <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_model_validation_building_id.rds')

knn_predictions_base_validation_building_id <- data.frame(predicted = knn_base_model_validation_building_id,
                                                          real_values = raw_validation_data_set$BUILDINGID)

confusionMatrix(knn_predictions_base_validation_building_id$predicted,
                knn_predictions_base_validation_building_id$real_values)

# Confusion Matrix and Statistics

#                   Reference
# Prediction       0   1   2
#              0 535   0   0
#              1   0 303   0
#              2   1   4 268
# 
# Overall Statistics
# 
# Accuracy : 0.9955               
# 95% CI : (0.9895, 0.9985)     
# No Information Rate : 0.4824               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9929          

#good results but performs worse than random forest


#predicting floor with normal knn


# knn_base_model_floor <- knn(train = training_raw[,1:520],
#                                    cl = training_raw[,523],
#                                    test = testing_raw[1:520],
#                                    k = 3)
# 
# saveRDS(knn_base_model_floor, 'knn_base_model_floor.rds')

knn_base_model_floor<- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_model_floor.rds')

knn_predictions_base_FLOOR <- data.frame(predicted = knn_base_model_floor,
                                               real_values = testing_raw$FLOOR)

confusionMatrix(knn_predictions_base_FLOOR$predicted,
                knn_predictions_base_FLOOR $real_values)

# Confusion Matrix and Statistics

#                           Reference
# Prediction         0    1    2    3    4
#               0 1250   60    6   11    1
#               1    9 1424   13    4    0
#               2    2   13 1275   20    0
#               3    0    0   59 1459    3
#               4    0    0    0    2  326
# 
# Overall Statistics
# 
# Accuracy : 0.9658               
# 95% CI : (0.9609, 0.9703)     
# No Information Rate : 0.2521               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9558

#model seems to be having issues determining floors given the trianing data set, 
#being the third floor the worst. doesnt make many 4th floor mistakes.


#predicting floor with against validation data 

# knn_base_model_validation_floor <- knn(train = raw_training_data_set[,1:520],
#                                                 cl = raw_training_data_set[,523],
#                                                 test = raw_validation_data_set[1:520],
#                                                 k = 3)
# 
# saveRDS(knn_base_model_validation_floor, 'knn_base_model_validation_floor.rds')

knn_base_model_validation_floor <- readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/models/knn_base_model_validation_floor.rds')

knn_predictions_base_validation_floor <- data.frame(predicted = knn_base_model_validation_floor,
                                                          real_values = raw_validation_data_set$FLOOR)

confusionMatrix(knn_predictions_base_validation_floor$predicted,
                knn_predictions_base_validation_floor$real_values)


# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0 113  71  15   6   5
#          1  11 331   8   0   1
#          2   6  58 199  11   0
#          3   2   2  84 152  11
#          4   0   0   0   3  22
# 
# Overall Statistics
# 
# Accuracy : 0.7354               
# 95% CI : (0.7084, 0.7611)     
# No Information Rate : 0.4158               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.6436


#KNN is considerably worse at predicting the floor with lower accuracy, kappa and 
#and confuses predictions between floors a lot more.


#given that the models seem to have fairly similar prediction rates with the first
#three predicted variables, I will be testing the next models directly with the 
#most complicated variable instead: 'FLOOR'

#Gradient boosted tree model approach ----

#trying H20 approach first to predict floor

#predicting with train set

h2o.init()

# training_raw_h2o <- as.h2o(training_raw)
# 
# 
# h2o_gradient_boosted_tree_training_model <- h2o.gbm(x = 1:520,
#                                                     y = 523,
#                                                     training_frame = training_raw_h2o,
#                                                     nfolds = 10,
#                                                     ntrees = 50)
# 
# saveRDS(h2o_gradient_boosted_tree_training_model,'h2o_gradient_boosted_tree_training_model')

h2o_gradient_boosted_tree_training_model <- readRDS('c://Users/riqui/desktop/Ubiqum course/Project 9/Wifi Project/models/h2o_gradient_boosted_tree_training_model')

testing_raw_h2o <- as.h2o(testing_raw)

h2o_base_training_predictions <- h2o.predict(object = h2o_gradient_boosted_tree_training_model
                                              ,newdata = testing_raw_h2o)

h2o_testing_results <- as.vector(h2o_base_training_predictions$predict)


h2o_test_results <-   data.frame(predicted = h2o_testing_results,
                                 real = testing_raw$FLOOR)

confusionMatrix(h2o_test_results$predicted,h2o_test_results$real)

# Confusion Matrix and Statistics

#                           Reference
# Prediction    0    1    2    3    4
#          0 1276   19    0    2    0
#          1   29 1480   14   10    0
#          2    2   25 1245   11    0
#          3    0    4   22 1467    4
#          4    0    0    0    1  326
# 
# Overall Statistics
# 
# Accuracy : 0.9759               
# 95% CI : (0.9717, 0.9797)     
# No Information Rate : 0.2574               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9688


#predicting floor against validation set

training_raw_full_h2o <- as.h2o(raw_training_data_set)

# h2o_gradient_boosted_tree_validation_model <- h2o.gbm(x = 1:520,
#                                                     y = 523,
#                                                     training_frame = training_raw_full_h2o,
#                                                     nfolds = 10,
#                                                     ntrees = 50)
# 
# saveRDS(h2o_gradient_boosted_tree_validation_model,'h2o_gradient_boosted_tree_validation_model')

h2o_gradient_boosted_tree_validation_model<- readRDS('c://Users/riqui/desktop/Ubiqum course/Project 9/Wifi Project/models/h2o_gradient_boosted_tree_validation_model')

testing_raw_validation_h2o <- as.h2o(raw_validation_data_set)

h2o_base_validation_predictions <- h2o.predict(object = h2o_gradient_boosted_tree_validation_model
                                             ,newdata = testing_raw_validation_h2o )

h2o_validation_results <- as.vector(h2o_base_validation_predictions$predict)


h2o_validation_results <-   data.frame(predicted = h2o_validation_results,
                                 real = raw_validation_data_set$FLOOR)

confusionMatrix(h2o_validation_results$predicted,h2o_validation_results$real)

# Confusion Matrix and Statistics
#  
#                       Reference
# Prediction    0   1   2   3   4
#           0 112  15   1   0   1
#           1  12 381  13   2   1
#           2   6  57 257  10   0
#           3   2   9  35 156  14
#           4   0   0   0   4  23
# 
# Overall Statistics
# 
# Accuracy : 0.8362               
# 95% CI : (0.8131, 0.8575)     
# No Information Rate : 0.4158               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.7726


#lower results for the gradient boosted tree.


#Weighted KNN  approach ----

#test set

# kknn_model_tesrting <- kknn(formula = formula(training_raw[,523]~.),
#                             train = training_raw[,1:520],
#                             test = testing_raw,
#                             k = 3,
#                             distance = 1) 

#saveRDS(kknn_model_tesrting ,'kknn_model_tesrting.rds')

kknn_model_tesrting <- readRDS('c://Users/riqui/desktop/Ubiqum course/Project 9/Wifi Project/models/kknn_model_tesrting.rds ')

fit_knn_training <- fitted(kknn_model_tesrting)

confusionMatrix(fit_knn_training,testing_raw$FLOOR)

# Confusion Matrix and Statistics
# 
#                           Reference
# Prediction    0    1    2    3    4
#          0 1290   31    2   10    0
#          1    6 1449   16    1    0
#          2    0    3 1277    9    0
#          3    0    0   31 1487    0
#          4    0    0    0    0  325
# 
# Overall Statistics
# 
# Accuracy : 0.9816               
# 95% CI : (0.9779, 0.9849)     
# No Information Rate : 0.2538               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9762  


#testing against validation set


# kknn_model_validation <- kknn(formula = formula(raw_training_data_set[,523]~.),
#                                train = raw_training_data_set[,1:520],
#                                test = raw_validation_data_set,
#                                k = 3,
#                                distance = 1)
# 
# saveRDS(kknn_model_validation ,'kknn_model_validation.rds')


kknn_model_validation <- readRDS('c://Users/riqui/desktop/Ubiqum course/Project 9/Wifi Project/models/kknn_model_validation.rds ')

fit_knn_validation <- fitted(kknn_model_validation)

confusionMatrix(fit_knn_validation,raw_validation_data_set$FLOOR)

# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0 112  49  11   7   7
#          1  10 345  14   3   0
#          2   9  67 206  15   0
#          3   1   1  75 145  11
#          4   0   0   0   2  21
# 
# Overall Statistics
# 
# Accuracy : 0.7462               
# 95% CI : (0.7195, 0.7715)     
# No Information Rate : 0.4158               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.6542 


#basic results against validation are not very good, testing one last method before
#proceeding to cascade models, as I suspect that providing the additional information
#and preprocessing can boost the performance of floor prediction which is the 
#biggest target variable









#Multi label classification approach

