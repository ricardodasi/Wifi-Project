#models as predictors approach

#Loading packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger,plotly,FNN,h2o,plyr,anchors, psycho, data.table,
               stats,kknn,liquidSVM)



#load the training and validation data set ----

cascading_train_data_set <- 
  read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')

cascading_validation_data_set <-
  read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/validationData.csv')


#preprocessing 

# 1. changing variable categories for the factor

cascading_train_data_set$BUILDINGID <- as.factor(cascading_train_data_set$BUILDINGID)

cascading_train_data_set$FLOOR <- as.factor(cascading_train_data_set$FLOOR)

cascading_validation_data_set$BUILDINGID <- as.factor(cascading_validation_data_set$BUILDINGID)

cascading_validation_data_set$FLOOR <- as.factor(cascading_validation_data_set$FLOOR)


# 2. removing the zero var columns 

cascading_train_data_set <- 
  cascading_train_data_set[,-which(apply(cascading_train_data_set,2,var)==0)]


# 3. removing variables that are not going to be used 

cascading_train_data_set <- 
  cascading_train_data_set[,-c(474,473,472,471,470)]

# 4. re scaling 

cascading_train_data_set[,1:465] <- cascading_train_data_set[,1:465] + 105

cascading_validation_data_set[,1:520] <- cascading_validation_data_set[,1:520] + 105


cascading_train_data_set <- replace.value( data = cascading_train_data_set,
                                           names = colnames(cascading_train_data_set[1:465]),
                                           from = 205 ,
                                           to  = 0,
                                           verbose = FALSE)

cascading_validation_data_set <- replace.value( data = cascading_validation_data_set,
                                                names = colnames(cascading_validation_data_set[1:520]),
                                                from = 205 ,
                                                to  = 0,
                                                verbose = FALSE)












#predicting building ----


# 1. SVM



# 2. Gradient boosted tree

h2o.init()

h2o_gradient_boosted_tree_training_model <- h2o.gbm(x = 1:465,
                                                    y = 469,
                                                    training_frame =as.h2o(cascading_train_data_set),
                                                    nfolds = 10,
                                                    ntrees = 50)

saveRDS(h2o_gradient_boosted_tree_training_model,'h2o_gradient_boosted_tree_training_model')

h2o_gradient_boosted_tree_training_model <- readRDS('c:')

gbm_building_prediction <-
  h2o.predict(object = h2o_gradient_boosted_tree_training_model
            ,newdata = as.h2o(cascading_train_data_set))










