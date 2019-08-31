#Train plus validation approach 

#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,plyr,anchors, psycho, data.table,
               stats)


#load the training and validation data set ----

raw_training_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

raw_validation_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')

#creating cascading data set

full_raw_data_set <- rbind(raw_training_data_set,raw_validation_data_set)



#preprocessing ----


#checking for columns with zero variance

which(apply(full_raw_data_set,2,var)==0)

#there are no columns with zero variance, however there are rows with zero variance, we remove
#those

full_raw_data_set <- 
  full_raw_data_set[-which(apply(full_raw_data_set[,1:520],1,min)==100),]




#changing the scales and the missing values cardinality.


#We verified that there are zero values in the data set, this is a measurement error, 
#exploring the error

# list_of_waps <- stack(full_raw_data_set[,1:520])
# 
# list_of_waps <- list_of_waps %>% filter(values == 0 )
# 
# waps <- unique(list_of_waps$ind)
# 
# waps <- full_raw_data_set[,c(which((colnames(full_raw_data_set) %in% waps)== T),521:529)]
# 
# b <- c()
# 
# zero_signal <- c()
# 
#  for (i in 1:25){
#   
#   a <- filter(waps, waps[,i]==0)
#   
#   zero_signal <- rbind(zero_signal,a) 
#   
# } 
# 
# zero_signal$user_phone <-  paste(zero_signal$USERID,zero_signal$PHONEID, sep = ' ')
# 
# ggplot(zero_signal, aes(x =user_phone ))+ geom_bar()


#all the error observations belong to 1 user in building 2. removing the user

full_raw_data_set$user_phone <-  paste(full_raw_data_set$USERID,
                                       full_raw_data_set$PHONEID,
                                       sep = ' ')


full_raw_data_set <- full_raw_data_set %>%  filter(user_phone != '6 19' )



#changing the scale to positive values 

full_raw_data_set[,1:520] <- full_raw_data_set[,1:520]+105

full_raw_data_set <- replace.value( data = full_raw_data_set,
                                           names = colnames(full_raw_data_set[1:520]),
                                           from = 205 ,
                                           to  = 0,
                                           verbose = FALSE)


#removing variables that are not going to be used

full_raw_data_set <-
  full_raw_data_set[,-c(525:530)]



#creating factor variables

full_raw_data_set$BUILDINGID <- as.factor(full_raw_data_set$BUILDINGID)

full_raw_data_set$FLOOR <- as.factor(full_raw_data_set$FLOOR)

#creating train and test ---- 

intraining_raw <- createDataPartition(full_raw_data_set$FLOOR,
                                      p=0.70,
                                      list = FALSE) 

training_raw <- full_raw_data_set[intraining_raw,]

testing_raw <- full_raw_data_set[-intraining_raw,]



#Ranger cascade modelling ----
#Predicting building ----

# rf_building_model <-  ranger(formula = training_raw[,524]~.,
#                                data = training_raw[,1:520],
#                                verbose = T,importance = 'impurity')
# 
# saveRDS(rf_building_model, 'rf_building_model.rds')

rf_building_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_building_model.rds')

rf_building_prediction <- predict(rf_building_model,testing_raw)

confusionMatrix(rf_building_prediction$predictions,
                testing_raw$BUILDINGID)



# Confusion Matrix and Statistics
# 
#                 Reference
# Prediction    0    1    2
#          0 1730    0    0
#          1    0 1638    0
#          2    0    0 2627
# 
# Overall Statistics
# 
# Accuracy : 1          
# 95% CI : (0.9994, 1)
# No Information Rate : 0.4382     
# P-Value [Acc > NIR] : < 2.2e-16  
# 
# Kappa : 1   


#adding predictions 

training_raw$predicted_building <- rf_building_model$predictions


testing_raw$predicted_building <-  rf_building_prediction$predictions


#predicting latitude by building ---- 


#separate the respective train and tests


#train

train_predicted_building_0 <- training_raw %>% filter(predicted_building == 0)

train_predicted_building_1 <- training_raw %>% filter(predicted_building == 1)

train_predicted_building_2 <- training_raw %>% filter(predicted_building == 2)

#test

test_predicted_building_0 <- testing_raw %>% filter(predicted_building == 0)

test_predicted_building_1 <- testing_raw %>% filter(predicted_building == 1)

test_predicted_building_2 <- testing_raw %>% filter(predicted_building == 2)


#predicting building 0

# rf_latitude_building_0_model <-  
#   ranger(formula = train_predicted_building_0[,522]~.,
#            data = train_predicted_building_0[,c(1:520,525)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_latitude_building_0_model, 'rf_latitude_building_0_model.rds')
# 

rf_latitude_building_0_model <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_latitude_building_0_model.rds')


rf_latitude_building_0_prediction <- predict(rf_latitude_building_0_model,
                                             test_predicted_building_0)


postResample(rf_latitude_building_0_prediction$predictions,
             test_predicted_building_0$LATITUDE)


#error analysis 

latitude_error_building_0 <- 
  data.frame(real_latitude = test_predicted_building_0$LATITUDE,
             predicted_latitude = rf_latitude_building_0_prediction$predictions,
             real_error = (test_predicted_building_0$LATITUDE - rf_latitude_building_0_prediction$predictions),
             percentual_error = (test_predicted_building_0$LATITUDE - rf_latitude_building_0_prediction$predictions)/test_predicted_building_0$LATITUDE,
             real_building = test_predicted_building_0$BUILDINGID,
             real_longitude = test_predicted_building_0$LONGITUDE)


#error plots 


ggplot(latitude_error_building_0, aes(x = real_latitude,
                                      y = real_error, 
                                      color = real_building, 
                                      shape = real_building)) +geom_point()


ggplot(latitude_error_building_0, aes(x = real_latitude,
                                      y = percentual_error, 
                                      color = real_building, 
                                      shape = real_building)) + geom_point()


ggplot(latitude_error_building_0, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(latitude_error_building_0, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')


ggplot(latitude_error_building_0, aes(x = real_longitude,
                                      y = real_latitude, 
                                      color = real_error, 
                                      shape = real_building)) + geom_point()


#first try

# RMSE Rsquared      MAE 
# 3.711824 0.987923 2.498176 



#adding predictions 


train_predicted_building_0$predicted_latitude <-
  rf_latitude_building_0_model$predictions

test_predicted_building_0$predicted_latitude <-
  rf_latitude_building_0_prediction$predictions




#predicting building 1

# rf_latitude_building_1_model <-
#   ranger(formula = train_predicted_building_1[,522]~.,
#            data = train_predicted_building_1[,c(1:520,525)],
#            verbose = T,importance = 'impurity')
# 
# 
# saveRDS(rf_latitude_building_1_model,'rf_latitude_building_1_model.rds')

rf_latitude_building_1_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_latitude_building_1_model.rds')

rf_latitude_building_1_prediction <- predict(rf_latitude_building_1_model,
                                             test_predicted_building_1)


postResample(rf_latitude_building_1_prediction$predictions,
             test_predicted_building_1$LATITUDE)


#error analysis

latitude_error_building_1 <- 
  data.frame(real_latitude = test_predicted_building_1$LATITUDE,
             predicted_latitude = rf_latitude_building_1_prediction$predictions,
             real_error = (test_predicted_building_1$LATITUDE - rf_latitude_building_1_prediction$predictions),
             percentual_error = (test_predicted_building_1$LATITUDE - rf_latitude_building_1_prediction$predictions)/test_predicted_building_1$LATITUDE,
             real_building = test_predicted_building_1$BUILDINGID,
             real_longitude = test_predicted_building_1$LONGITUDE)


#error plots 


ggplot(latitude_error_building_1, aes(x = real_latitude,
                                      y = real_error, 
                                      color = real_building, 
                                      shape = real_building)) +geom_point()


ggplot(latitude_error_building_1, aes(x = real_latitude,
                                      y = percentual_error, 
                                      color = real_building, 
                                      shape = real_building)) + geom_point()


ggplot(latitude_error_building_1, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(latitude_error_building_1, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')


ggplot(latitude_error_building_1, aes(x = real_longitude,
                                      y = real_latitude, 
                                      color = real_error, 
                                      shape = real_building)) + geom_point()


#first try

# RMSE  Rsquared       MAE 
# 5.7350414 0.9765925 4.1059700 


#adding predictions 


train_predicted_building_1$predicted_latitude <-
  rf_latitude_building_1_model$predictions

test_predicted_building_1$predicted_latitude <-
  rf_latitude_building_1_prediction$predictions




#predicting building 2


# rf_latitude_building_2_model <-
#   ranger(formula = train_predicted_building_2[,522]~.,
#            data = train_predicted_building_2[,c(1:520,525)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_latitude_building_2_model,'rf_latitude_building_2_model.rds')

rf_latitude_building_2_model <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_latitude_building_2_model.rds')


rf_latitude_building_2_prediction <- predict(rf_latitude_building_2_model,
                                             test_predicted_building_2)

postResample(rf_latitude_building_2_prediction$predictions,
             test_predicted_building_2$LATITUDE)


#error analysis 


latitude_error_building_2 <- 
  data.frame(real_latitude = test_predicted_building_2$LATITUDE,
             predicted_latitude = rf_latitude_building_2_prediction$predictions,
             real_error = (test_predicted_building_2$LATITUDE - rf_latitude_building_2_prediction$predictions),
             percentual_error = (test_predicted_building_2$LATITUDE - rf_latitude_building_2_prediction$predictions)/test_predicted_building_2$LATITUDE,
             real_building = test_predicted_building_2$BUILDINGID,
             real_longitude = test_predicted_building_2$LONGITUDE)


#error plots 


ggplot(latitude_error_building_2, aes(x = real_latitude,
                                      y = real_error, 
                                      color = real_building, 
                                      shape = real_building)) +geom_point()


ggplot(latitude_error_building_2, aes(x = real_latitude,
                                      y = percentual_error, 
                                      color = real_building, 
                                      shape = real_building)) + geom_point()


ggplot(latitude_error_building_2, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(latitude_error_building_2, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')


ggplot(latitude_error_building_2, aes(x = real_longitude,
                                      y = real_latitude, 
                                      color = real_error, 
                                      shape = real_building)) + geom_point()


#first try

#      RMSE  Rsquared       MAE 
# 4.8661312 0.9723335 3.3322332

#adding predictions 


train_predicted_building_2$predicted_latitude <-
  rf_latitude_building_2_model$predictions

test_predicted_building_2$predicted_latitude <-
  rf_latitude_building_2_prediction$predictions



#predicting longitude ----


#predicting building 0


# rf_longitude_building_0_model <-
#   ranger(formula = train_predicted_building_0[,521]~.,
#            data = train_predicted_building_0[,c(1:520,525,526)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_longitude_building_0_model, 'rf_longitude_building_0_model.rds')

rf_longitude_building_0_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_longitude_building_0_model.rds')


rf_longitude_building_0_prediction <-predict(rf_longitude_building_0_model,
                                             test_predicted_building_0)


postResample(rf_longitude_building_0_prediction$predictions,
             test_predicted_building_0$LONGITUDE)



#error analysis 

longitude_error_building_0 <- 
  data.frame(real_longitude = test_predicted_building_0$LONGITUDE,
             predicted_longitude = rf_longitude_building_0_prediction$predictions,
             real_error = (test_predicted_building_0$LONGITUDE - rf_longitude_building_0_prediction$predictions),
             percentual_error = (test_predicted_building_0$LONGITUDE - rf_longitude_building_0_prediction$predictions)/test_predicted_building_0$LONGITUDE,
             real_building = test_predicted_building_0$BUILDINGID,
             real_latitude = test_predicted_building_0$LATITUDE)


#error plots 


ggplot(longitude_error_building_0, aes(x = real_longitude,
                                      y = real_error, 
                                      color = real_building, 
                                      shape = real_building)) +geom_point()


ggplot(longitude_error_building_0, aes(x = real_longitude,
                                      y = percentual_error, 
                                      color = real_building, 
                                      shape = real_building)) + geom_point()


ggplot(longitude_error_building_0, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(longitude_error_building_0, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')


ggplot(longitude_error_building_0, aes(x = real_longitude,
                                      y = real_latitude, 
                                      color = real_error, 
                                      shape = real_building)) + geom_point()




#first try 

#      RMSE  Rsquared       MAE 
# 4.0546058 0.9759302 2.7683114 


#adding predictions

train_predicted_building_0$predicted_longitude <-
  rf_longitude_building_0_model$predictions

test_predicted_building_0$predicted_longitude <-
  rf_longitude_building_0_prediction$predictions




#predicting building 1

# rf_longitude_building_1_model <-
#   ranger(formula = train_predicted_building_1[,521]~.,
#            data = train_predicted_building_1[,c(1:520,525,526)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_longitude_building_1_model, 'rf_longitude_building_1_model.rds')

rf_longitude_building_1_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_longitude_building_1_model.rds')


rf_longitude_building_1_prediction <-predict(rf_longitude_building_1_model,
                                             test_predicted_building_1)

postResample(rf_longitude_building_1_prediction$predictions,
             test_predicted_building_1$LONGITUDE)


#error analysis 

longitude_error_building_1 <- 
  data.frame(real_longitude = test_predicted_building_1$LONGITUDE,
             predicted_longitude = rf_longitude_building_1_prediction$predictions,
             real_error = (test_predicted_building_1$LONGITUDE - rf_longitude_building_1_prediction$predictions),
             percentual_error = (test_predicted_building_1$LONGITUDE - rf_longitude_building_1_prediction$predictions)/test_predicted_building_1$LONGITUDE,
             real_building = test_predicted_building_1$BUILDINGID,
             real_latitude = test_predicted_building_1$LATITUDE)


#error plots 


ggplot(longitude_error_building_1, aes(x = real_longitude,
                                       y = real_error, 
                                       color = real_building, 
                                       shape = real_building)) +geom_point()


ggplot(longitude_error_building_1, aes(x = real_longitude,
                                       y = percentual_error, 
                                       color = real_building, 
                                       shape = real_building)) + geom_point()


ggplot(longitude_error_building_1, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(longitude_error_building_1, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')


ggplot(longitude_error_building_1, aes(x = real_longitude,
                                       y = real_latitude, 
                                       color = real_error, 
                                       shape = real_building)) + geom_point()



# first try

# RMSE  Rsquared       MAE 
# 6.1099032 0.9846407 4.4719370 



#adding prediction


train_predicted_building_1$predicted_longitude <-
  rf_longitude_building_1_model$predictions

test_predicted_building_1$predicted_longitude <-
  rf_longitude_building_1_prediction$predictions


#predicting building 2


# rf_longitude_building_2_model <-
#   ranger(formula = train_predicted_building_2[,521]~.,
#            data = train_predicted_building_2[,c(1:520,525,526)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_longitude_building_2_model, 'rf_longitude_building_2_model.rds')

rf_longitude_building_2_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_longitude_building_2_model.rds')

rf_longitude_building_2_prediction <-predict(rf_longitude_building_2_model,
                                             test_predicted_building_2)

postResample(rf_longitude_building_2_prediction$predictions,
             test_predicted_building_2$LONGITUDE)


#error analysis 

longitude_error_building_2 <- 
  data.frame(real_longitude = test_predicted_building_2$LONGITUDE,
             predicted_longitude = rf_longitude_building_2_prediction$predictions,
             real_error = (test_predicted_building_2$LONGITUDE - rf_longitude_building_2_prediction$predictions),
             percentual_error = (test_predicted_building_2$LONGITUDE - rf_longitude_building_2_prediction$predictions)/test_predicted_building_2$LONGITUDE,
             real_building = test_predicted_building_2$BUILDINGID,
             real_latitude = test_predicted_building_2$LATITUDE)


#error plots 


ggplot(longitude_error_building_2, aes(x = real_longitude,
                                       y = real_error, 
                                       color = real_building, 
                                       shape = real_building)) +geom_point()


ggplot(longitude_error_building_2, aes(x = real_longitude,
                                       y = percentual_error, 
                                       color = real_building, 
                                       shape = real_building)) + geom_point()


ggplot(longitude_error_building_2, aes(x = real_error)) +
  geom_histogram(colour = 'blue')

ggplot(longitude_error_building_2, aes(x = percentual_error)) +
  geom_histogram(colour = 'blue')


ggplot(longitude_error_building_2, aes(x = real_longitude,
                                       y = real_latitude, 
                                       color = real_error, 
                                       shape = real_building)) + geom_point()



#first try 

#     RMSE  Rsquared       MAE 
# 5.9515864 0.9668039 4.0832449 


#adding prediction


train_predicted_building_2$predicted_longitude <-
  rf_longitude_building_2_model$predictions

test_predicted_building_2$predicted_longitude <-
  rf_longitude_building_2_prediction$predictions




#predicting floor ----


#predicting building 0

# rf_floor_building_0_model <-
#   ranger(formula = train_predicted_building_0[,523]~.,
#            data = train_predicted_building_0[,c(1:520,525,526,527)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_floor_building_0_model, 'rf_floor_building_0_model.rds')

rf_floor_building_0_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_floor_building_0_model.rds')


rf_floor_building_0_prediction <-predict(rf_floor_building_0_model,
                                             test_predicted_building_0)


confusionMatrix(rf_floor_building_0_prediction$predictions,
             test_predicted_building_0$FLOOR)


#first try 


# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0 346   1   0   0   0
#          1   4 449   2   0   0
#          2   0   0 490   0   0
#          3   0   0   1 437   0
#          4   0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.9954               
# 95% CI : (0.9909, 0.998)      
# No Information Rate : 0.285                
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9938 



#predicting building 1

# rf_floor_building_1_model <-
#   ranger(formula = train_predicted_building_1[,523]~.,
#            data = train_predicted_building_1[,c(1:520,525,526,527)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_floor_building_1_model, 'rf_floor_building_1_model.rds')

rf_floor_building_1_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_floor_building_1_model.rds')


rf_floor_building_1_prediction <-predict(rf_floor_building_1_model,
                                         test_predicted_building_1)


confusionMatrix(rf_floor_building_1_prediction$predictions,
                test_predicted_building_1$FLOOR)

#first try 

# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0 402   4   0   0   0
#          1   1 477   0   0   0
#          2   0   5 456   5   0
#          3   1   0   0 287   0
#          4   0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.9902               
# 95% CI : (0.9842, 0.9944)     
# No Information Rate : 0.2967               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9868   



#predicting building 2


# rf_floor_building_2_model <-
#   ranger(formula = train_predicted_building_2[,523]~.,
#            data = train_predicted_building_2[,c(1:520,525,526,527)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(rf_floor_building_2_model, 'rf_floor_building_2_model.rds')

rf_floor_building_2_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/rf_floor_building_2_model.rds')

rf_floor_building_2_prediction <-predict(rf_floor_building_2_model,
                                         test_predicted_building_2)

confusionMatrix(rf_floor_building_2_prediction$predictions,
                test_predicted_building_2$FLOOR)


#first try


# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0 584   0   0   0   0
#          1   1 702   1   0   0
#          2   0   0 466   0   0
#          3   0   0   0 672   7
#          4   0   0   0   0 194
# 
# Overall Statistics
# 
# Accuracy : 0.9966               
# 95% CI : (0.9935, 0.9984)     
# No Information Rate : 0.2672               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9956       

