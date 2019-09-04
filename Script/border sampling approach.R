#Sampled borders approach

#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,plyr,anchors, psycho, data.table,
               stats)


#load the training and validation data set ----

cascading_train_data_set<- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

cascading_validation_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')


#preprocessing ----


# 1. changing factor categories

cascading_train_data_set$BUILDINGID <- as.factor(cascading_train_data_set$BUILDINGID)

cascading_train_data_set$FLOOR <- as.factor(cascading_train_data_set$FLOOR)

cascading_validation_data_set$BUILDINGID <- as.factor(cascading_validation_data_set$BUILDINGID)

cascading_validation_data_set$FLOOR <- as.factor(cascading_validation_data_set$FLOOR)


# 2. checking waps with problem measures 

#waps shouldnt have measures between -30 and 0


waps_values <- stack(cascading_train_data_set[,1:520])

waps_values <- waps_values %>%  filter(values != 100) 

ggplot(waps_values,aes(x = values)) + geom_histogram()

#we can see that there are measures that go up to 0 from -30, these need to be removed

problem_waps <-  waps_values %>%  filter(values <=1,values>= -30)


#65 problem waps out of the 520

problem_waps <- problem_waps %>%  
  group_by(ind) %>% 
  summarise(n = count(ind))

#different frequencies depending to the wap, exploring measures in relation to the users


waps_with_user <- cascading_train_data_set[,c(1:520,527,528)]


waps_with_user <- replace.value( data = waps_with_user,
                                   names = colnames(cascading_train_data_set[1:520]),
                                   from = 100 ,
                                   to  = -500,
                                   verbose = FALSE)


waps_with_user$max <- apply(waps_with_user[,1:520],1,max)

waps_with_user <-  waps_with_user %>%  filter(max>=-30 & max<=0)

waps_with_user$user_phone <- paste(waps_with_user$USERID,
                                   waps_with_user$PHONEID, 
                                   sep = '-')


waps_with_user <- waps_with_user %>% 
  group_by(user_phone) %>% 
  summarize(count = count(user_phone))


#most of the points that are wrong belong to two users in particular, however I have decided to just
#remove the rows that are giving me impossible measures


cascading_train_data_set<-replace.value( data = cascading_train_data_set,
                                           names = colnames(cascading_train_data_set[1:520]),
                                           from = 100 ,
                                           to  = -500,
                                           verbose = FALSE)


cascading_train_data_set$max<- apply(cascading_train_data_set[,1:520],1,max)


cascading_train_data_set <- cascading_train_data_set %>% 
  filter(max < -30)


#rows removed


# 3. removing zero var columns 

#saving the indexes as I will need to remove them from the validation set too

zero_var_indexes <-which(apply(cascading_train_data_set,2,var)==0)

cascading_train_data_set <- cascading_train_data_set[,-zero_var_indexes]


# 4. re scaling 

cascading_train_data_set[,1:464]<- cascading_train_data_set[,1:464] + 105

cascading_train_data_set<-replace.value( data = cascading_train_data_set,
                                          names = colnames(cascading_train_data_set[1:464]),
                                          from = -395 ,
                                          to  = 0,
                                          verbose = FALSE)


cascading_validation_data_set[,1:520]<- cascading_validation_data_set[,1:520] + 105

cascading_validation_data_set<-replace.value( data = cascading_validation_data_set,
                                         names = colnames(cascading_validation_data_set[1:520]),
                                         from = 205 ,
                                         to  = 0,
                                         verbose = FALSE)





# 5. taking out non used variables 


cascading_train_data_set <- cascading_train_data_set[,-c(469:474)] 


cascading_validation_data_set <- cascading_validation_data_set[,-c(525:529)] 


#predicting building ----

# 
# random_forest_borders_building_model <-  ranger(formula = cascading_train_data_set[,468]~.,
#                                            data = cascading_train_data_set[,1:464],
#                                            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_borders_building_model,'random_forest_borders_building_model.rds')

random_forest_borders_building_model <- 
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_borders_building_model.rds')


random_forest_borders_building_prediction <- 
  predict(random_forest_borders_building_model,cascading_validation_data_set)

confusionMatrix(random_forest_borders_building_prediction$predictions,
                cascading_validation_data_set$BUILDINGID)


#results

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1   2
# 0 535   1   0
# 1   1 306   0
# 2   0   0 268
# 
# Overall Statistics
# 
# Accuracy : 0.9982          
# 95% CI : (0.9935, 0.9998)
# No Information Rate : 0.4824          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.9972


#adding predictions


cascading_train_data_set$predicted_building <-
  random_forest_borders_building_model$predictions

cascading_validation_data_set$predicted_building  <-
  random_forest_borders_building_prediction$predictions







#predicting latitude ----

#separatig data sets by predicted building

#train

cascading_train_data_set_predicted_building_0 <-
  cascading_train_data_set %>% filter(predicted_building == 0) 

cascading_train_data_set_predicted_building_1 <-
  cascading_train_data_set %>% filter(predicted_building == 1) 

cascading_train_data_set_predicted_building_2 <- 
  cascading_train_data_set %>% filter(predicted_building == 2) 

#test 

cascading_validation_data_set_predicted_building_0 <- 
  cascading_validation_data_set %>% filter(predicted_building == 0) 

cascading_validation_data_set_predicted_building_1 <- 
  cascading_validation_data_set %>% filter(predicted_building == 1) 

cascading_validation_data_set_predicted_building_2 <-
  cascading_validation_data_set %>% filter(predicted_building == 2) 


#error measurements by building from previous tries

ggplot(cascading_train_data_set,aes(x=LATITUDE, y=LONGITUDE)) +geom_point()



#building 0

index_0 <- 
c(which(cascading_validation_data_set_predicted_building_0$LATITUDE >= 4865000 &
        cascading_validation_data_set_predicted_building_0$LONGITUDE <= -7600),
  which(cascading_validation_data_set_predicted_building_0$LATITUDE <= 4864920))

partition_0 <-createDataPartition(index_0,
                    p=0.70,
                    list = FALSE) 


new_set_0 <- cascading_validation_data_set_predicted_building_0[index_0[partition_0],]

new_set_0 <- new_set_0[,-zero_var_indexes]

cascading_train_data_set_predicted_building_0 <- 
  rbind(cascading_train_data_set_predicted_building_0,
          new_set_0)


cascading_validation_data_set_predicted_building_0 <-
  cascading_validation_data_set_predicted_building_0[-index_0[partition_0],]


#building 1


index_1 <- c(which(cascading_validation_data_set_predicted_building_1$LONGITUDE >= -7450),
              which(cascading_validation_data_set_predicted_building_1$LATITUDE >= 4864950 &
                      cascading_validation_data_set_predicted_building_1$LONGITUDE >= -7550))
              

partition_1 <-createDataPartition(index_1,
                                  p=0.70,
                                  list = FALSE) 


new_set_1 <- cascading_validation_data_set_predicted_building_1[index_1[partition_1],]

new_set_1 <- new_set_1[,-zero_var_indexes]

cascading_train_data_set_predicted_building_1 <- 
  rbind(cascading_train_data_set_predicted_building_1,
        new_set_1)


cascading_validation_data_set_predicted_building_1 <-
  cascading_validation_data_set_predicted_building_1[-index_1[partition_1],]


#building 2


index_2<-c(which(cascading_validation_data_set_predicted_building_2$LONGITUDE >= -7325),
           which(cascading_validation_data_set_predicted_building_2$LONGITUDE <= -7400 &
                   cascading_validation_data_set_predicted_building_2$LATITUDE <= 4864830 ))



partition_2 <-createDataPartition(index_2,
                                  p=0.70,
                                  list = FALSE) 


new_set_2 <- cascading_validation_data_set_predicted_building_2[index_2[partition_2],]

new_set_2 <- new_set_2[,-zero_var_indexes]

cascading_train_data_set_predicted_building_2 <- 
  rbind(cascading_train_data_set_predicted_building_2,
        new_set_2)


cascading_validation_data_set_predicted_building_2 <-
  cascading_validation_data_set_predicted_building_2[-index_2[partition_2],]



#predicting latitude building 0

# random_forest_borders_latitude_model_building_0 <-  
#   ranger(formula = cascading_train_data_set_predicted_building_0[,466]~.,
#            data = cascading_train_data_set_predicted_building_0[,c(1:464,469)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_borders_latitude_model_building_0,
#         'random_forest_borders_latitude_model_building_0.rds')

random_forest_borders_latitude_model_building_0 <-
  readRDS('c://Users/riqui/desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_borders_latitude_model_building_0.rds')

random_forest_borders_latitude_prediction_building_0 <- 
  predict(random_forest_borders_latitude_model_building_0,
          cascading_validation_data_set_predicted_building_0)

postResample(random_forest_borders_latitude_prediction_building_0$predictions,
                cascading_validation_data_set_predicted_building_0$LATITUDE)



#error analysis

#      RMSE  Rsquared       MAE 
# 5.6950910 0.9576607 3.8195156 


latitude_error_building_0 <- 
  data.frame(real_latitude = cascading_validation_data_set_predicted_building_0$LATITUDE,
             predicted_latitude = random_forest_borders_latitude_prediction_building_0$predictions,
             real_error = (cascading_validation_data_set_predicted_building_0$LATITUDE - random_forest_borders_latitude_prediction_building_0$predictions),
             percentual_error = (cascading_validation_data_set_predicted_building_0$LATITUDE - random_forest_borders_latitude_prediction_building_0$predictions)/cascading_validation_data_set_predicted_building_0$LATITUDE,
             real_building = cascading_validation_data_set_predicted_building_0$BUILDINGID,
             real_longitude = cascading_validation_data_set_predicted_building_0$LONGITUDE)


#error plots 

options(scipen=999)

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


#data of the error does become more horizontal, less over and understimation, effective experiment


#adding predictions to the data set


#train
cascading_train_data_set_predicted_building_0$predicted_latitude <- 
  random_forest_borders_latitude_model_building_0$predictions


#validation 

cascading_validation_data_set_predicted_building_0$predicted_latitude <-
  random_forest_borders_latitude_prediction_building_0$predictions





#predicting latitude building 1

# random_forest_borders_latitude_model_building_1 <-
#   ranger(formula = cascading_train_data_set_predicted_building_1[,466]~.,
#            data = cascading_train_data_set_predicted_building_1[,c(1:464,469)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_borders_latitude_model_building_1,
#         'random_forest_borders_latitude_model_building_1.rds')


random_forest_borders_latitude_model_building_1 <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_borders_latitude_model_building_1.rds')

random_forest_borders_latitude_prediction_building_1 <- 
  predict(random_forest_borders_latitude_model_building_1,
          cascading_validation_data_set_predicted_building_1)

postResample(random_forest_borders_latitude_prediction_building_1$predictions,
             cascading_validation_data_set_predicted_building_1$LATITUDE)


#error analysis

#       RMSE   Rsquared        MAE 
# 12.8223782  0.8868584  8.2856714 



latitude_error_building_1 <- 
  data.frame(real_latitude = cascading_validation_data_set_predicted_building_1$LATITUDE,
             predicted_latitude = random_forest_borders_latitude_prediction_building_1$predictions,
             real_error = (cascading_validation_data_set_predicted_building_1$LATITUDE - random_forest_borders_latitude_prediction_building_1$predictions),
             percentual_error = (cascading_validation_data_set_predicted_building_1$LATITUDE - random_forest_borders_latitude_prediction_building_1$predictions)/cascading_validation_data_set_predicted_building_1$LATITUDE,
             real_building = cascading_validation_data_set_predicted_building_1$BUILDINGID,
             real_longitude = cascading_validation_data_set_predicted_building_1$LONGITUDE)


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


#main problem seems to be coming from one outlier point predicted in a wrong way



#adding predictions to the data set


#train
cascading_train_data_set_predicted_building_1$predicted_latitude <- 
  random_forest_borders_latitude_model_building_1$predictions


#validation 

cascading_validation_data_set_predicted_building_1$predicted_latitude <-
  random_forest_borders_latitude_prediction_building_1$predictions



#predicting latitude building 2


random_forest_borders_latitude_model_building_2 <-
  ranger(formula = cascading_train_data_set_predicted_building_2[,466]~.,
           data = cascading_train_data_set_predicted_building_2[,c(1:464,469)],
           verbose = T,importance = 'impurity')



random_forest_borders_latitude_prediction_building_2 <- 
  predict(random_forest_borders_latitude_model_building_2,
          cascading_validation_data_set_predicted_building_2)

postResample(random_forest_borders_latitude_prediction_building_2$predictions,
             cascading_validation_data_set_predicted_building_2$LATITUDE)


#error analysis

#       RMSE   Rsquared        MAE 
# 11.0305063  0.9127256  8.0084747 
 

latitude_error_building_2 <- 
  data.frame(real_latitude = cascading_validation_data_set_predicted_building_2$LATITUDE,
             predicted_latitude = random_forest_borders_latitude_prediction_building_2$predictions,
             real_error = (cascading_validation_data_set_predicted_building_2$LATITUDE - random_forest_borders_latitude_prediction_building_2$predictions),
             percentual_error = (cascading_validation_data_set_predicted_building_2$LATITUDE - random_forest_borders_latitude_prediction_building_2$predictions)/cascading_validation_data_set_predicted_building_2$LATITUDE,
             real_building = cascading_validation_data_set_predicted_building_2$BUILDINGID,
             real_longitude = cascading_validation_data_set_predicted_building_2$LONGITUDE)


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





#increased R squared perfomrance but MAE and RMSE got worse. biggest errors on the sides 
#of the bulding



#adding predictions to the data set


#train
cascading_train_data_set_predicted_building_2$predicted_latitude <- 
  random_forest_borders_latitude_model_building_2$predictions


#validation 

cascading_validation_data_set_predicted_building_2$predicted_latitude <-
  random_forest_borders_latitude_prediction_building_2$predictions



#predicting longitude ----


#predicting longitude building 0

# random_forest_borders_longitude_model_building_0 <-
#   ranger(formula = cascading_train_data_set_predicted_building_0[,465]~.,
#            data = cascading_train_data_set_predicted_building_0[,c(1:464,469,470)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_borders_longitude_model_building_0,
#         'random_forest_borders_longitude_model_building_0.rds')

random_forest_borders_longitude_model_building_0 <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_borders_longitude_model_building_0.rds')

random_forest_borders_longitude_prediction_building_0 <- 
  predict(random_forest_borders_longitude_model_building_0,
          cascading_validation_data_set_predicted_building_0)

postResample(random_forest_borders_longitude_prediction_building_0$predictions,
             cascading_validation_data_set_predicted_building_0$LONGITUDE)


#error analysis 


#      RMSE  Rsquared       MAE 
# 7.9114905 0.9370771 5.4883760 


longitude_error_building_0 <- 
  data.frame(real_longitude = cascading_validation_data_set_predicted_building_0$LONGITUDE,
             predicted_longitude = random_forest_borders_longitude_prediction_building_0$predictions,
             real_error = (cascading_validation_data_set_predicted_building_0$LONGITUDE - random_forest_borders_longitude_prediction_building_0$predictions),
             percentual_error = (cascading_validation_data_set_predicted_building_0$LONGITUDE - random_forest_borders_longitude_prediction_building_0$predictions)/cascading_validation_data_set_predicted_building_0$LONGITUDE,
             real_building = cascading_validation_data_set_predicted_building_0$BUILDINGID,
             real_latitude = cascading_validation_data_set_predicted_building_0$LATITUDE)


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



#Rsquared improved slightly while MAE and RMSE got slightly worse. it seems problem
#is the same, the outliers are what is throwing the measures off.

#adding predictions to the data set


#train
cascading_train_data_set_predicted_building_0$predicted_longitude <- 
  random_forest_borders_longitude_model_building_0$predictions


#validation 

cascading_validation_data_set_predicted_building_0$predicted_longitude <-
  random_forest_borders_longitude_prediction_building_0$predictions




#predicting longitude building 1


# random_forest_borders_longitude_model_building_1 <-
#   ranger(formula = cascading_train_data_set_predicted_building_1[,465]~.,
#            data = cascading_train_data_set_predicted_building_1[,c(1:464,469,470)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_borders_longitude_model_building_1,
#         'random_forest_borders_longitude_model_building_1.rds')

random_forest_borders_longitude_model_building_1 <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_borders_longitude_model_building_1.rds')

random_forest_borders_longitude_prediction_building_1 <- 
  predict(random_forest_borders_longitude_model_building_1,
          cascading_validation_data_set_predicted_building_1)

postResample(random_forest_borders_longitude_prediction_building_1$predictions,
             cascading_validation_data_set_predicted_building_1$LONGITUDE)


#error analysis
 
#       RMSE   Rsquared        MAE 
# 13.3058943  0.8864551  7.3546297 

longitude_error_building_1 <- 
  data.frame(real_longitude = cascading_validation_data_set_predicted_building_1$LONGITUDE,
             predicted_longitude = random_forest_borders_longitude_prediction_building_1$predictions,
             real_error = (cascading_validation_data_set_predicted_building_1$LONGITUDE - random_forest_borders_longitude_prediction_building_1$predictions),
             percentual_error = (cascading_validation_data_set_predicted_building_1$LONGITUDE - random_forest_borders_longitude_prediction_building_1$predictions)/cascading_validation_data_set_predicted_building_1$LONGITUDE,
             real_building = cascading_validation_data_set_predicted_building_1$BUILDINGID,
             real_latitude = cascading_validation_data_set_predicted_building_1$LATITUDE)


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




#results get worse in general, outliers are punishing the measures


#adding predictions to the data set


#train
cascading_train_data_set_predicted_building_1$predicted_longitude <- 
  random_forest_borders_longitude_model_building_1$predictions


#validation 

cascading_validation_data_set_predicted_building_1$predicted_longitude <-
  random_forest_borders_longitude_prediction_building_1$predictions



#predicting longitude building 2


random_forest_borders_longitude_model_building_2 <-
  ranger(formula = cascading_train_data_set_predicted_building_2[,465]~.,
           data = cascading_train_data_set_predicted_building_2[,c(1:464,469,470)],
           verbose = T,importance = 'impurity')



random_forest_borders_longitude_prediction_building_2 <- 
  predict(random_forest_borders_longitude_model_building_2,
          cascading_validation_data_set_predicted_building_2)

postResample(random_forest_borders_longitude_prediction_building_2$predictions,
             cascading_validation_data_set_predicted_building_2$LONGITUDE)


#error analysis

#       RMSE   Rsquared        MAE 
# 11.1008101  0.8172717  7.9957828 


longitude_error_building_2 <- 
  data.frame(real_longitude = cascading_validation_data_set_predicted_building_2$LONGITUDE,
             predicted_longitude = random_forest_borders_longitude_prediction_building_2$predictions,
             real_error = (cascading_validation_data_set_predicted_building_2$LONGITUDE - random_forest_borders_longitude_prediction_building_2$predictions),
             percentual_error = (cascading_validation_data_set_predicted_building_2$LONGITUDE - random_forest_borders_longitude_prediction_building_2$predictions)/cascading_validation_data_set_predicted_building_2$LONGITUDE,
             real_building = cascading_validation_data_set_predicted_building_2$BUILDINGID,
             real_latitude = cascading_validation_data_set_predicted_building_2$LATITUDE)


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





#error metrics improved but Rsquared fell a almost 10 %

#adding predictions to the data set


#train
cascading_train_data_set_predicted_building_2$predicted_longitude <- 
  random_forest_borders_longitude_model_building_2$predictions


#validation 

cascading_validation_data_set_predicted_building_2$predicted_longitude <-
  random_forest_borders_longitude_prediction_building_2$predictions




#predicting floor ----


#predicting floor building 0

# random_forest_borders_floor_model_building_0 <-
#   ranger(formula = cascading_train_data_set_predicted_building_0[,467]~.,
#            data = cascading_train_data_set_predicted_building_0[,c(1:464,469,470,471)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_borders_floor_model_building_0,
#         'random_forest_borders_floor_model_building_0.rds')

random_forest_borders_floor_model_building_0 <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_borders_floor_model_building_0.rds')

random_forest_borders_floor_prediction_building_0 <- 
  predict(random_forest_borders_floor_model_building_0,
          cascading_validation_data_set_predicted_building_0)

confusionMatrix(random_forest_borders_floor_prediction_building_0$predictions,
             cascading_validation_data_set_predicted_building_0$FLOOR)


#results

# Confusion Matrix and Statistics
# 
#                      Reference
# Prediction   0   1   2   3   4
#          0  64   1   1   0   0
#          1   2 184   3   0   0
#          2   2   2 128   1   0
#          3   0   0   0  69   0
#          4   0   0   0   0   0
# 
# Overall Statistics
# 
# Accuracy : 0.9737               
# 95% CI : (0.9546, 0.9864)     
# No Information Rate : 0.4092               
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.9626     


#results are roughly equal



#predicting floor building 1


# random_forest_borders_floor_model_building_1 <-
#   ranger(formula = cascading_train_data_set_predicted_building_1[,467]~.,
#            data = cascading_train_data_set_predicted_building_1[,c(1:464,469,470,471)],
#            verbose = T,importance = 'impurity')
# 
# saveRDS(random_forest_borders_floor_model_building_1,
#         'random_forest_borders_floor_model_building_1.rds')

random_forest_borders_floor_model_building_1 <-
  readRDS('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Script/random_forest_borders_floor_model_building_1.rds')


random_forest_borders_floor_prediction_building_1 <- 
  predict(random_forest_borders_floor_model_building_1,
          cascading_validation_data_set_predicted_building_1)

confusionMatrix(random_forest_borders_floor_prediction_building_1$predictions,
                cascading_validation_data_set_predicted_building_1$FLOOR)


#results

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1  2  3  4
# 0 16  0  0  0  0
# 1  3 85  1  0  0
# 2  3 43 62  3  0
# 3  1  3  2 29  0
# 4  0  0  0  0  0
# 
# Overall Statistics
# 
# Accuracy : 0.7649              
# 95% CI : (0.7075, 0.816)     
# No Information Rate : 0.5219              
# P-Value [Acc > NIR] : 0.000000000000001823
# 
# Kappa : 0.6527 


#results get worse



#predicting building 2

random_forest_borders_floor_model_building_2 <-
  ranger(formula = cascading_train_data_set_predicted_building_2[,467]~.,
           data = cascading_train_data_set_predicted_building_2[,c(1:464,469,470,471)],
           verbose = T,importance = 'impurity')


random_forest_borders_floor_prediction_building_2 <- 
  predict(random_forest_borders_floor_model_building_2,
          cascading_validation_data_set_predicted_building_2)

confusionMatrix(random_forest_borders_floor_prediction_building_2$predictions,
                cascading_validation_data_set_predicted_building_2$FLOOR)


#results
# Confusion Matrix and Statistics
# 
#                 Reference
# Prediction  0  1  2  3  4
#          0 15  0  0  0  1
#          1  3 75  0  0  0
#          2  0  2 38  0  0
#          3  0  0  4 31  8
#          4  0  0  0  1 22
# 
# Overall Statistics
# 
# Accuracy : 0.905                
# 95% CI : (0.8556, 0.9418)     
# No Information Rate : 0.385                
# P-Value [Acc > NIR] : < 0.00000000000000022
# 
# Kappa : 0.8731      
# 


#results get slightly worse



