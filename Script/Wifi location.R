#Wifi location project

#load packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,sf,viridis, 
               graphics,ranger)

library(rayshader)

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


plot_gg(location_data_gg, height=3, width=3.5, multicore=TRUE, pointcontract = 0.7, soliddepth=-100)


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


#first model testing for predictions with random forest ----


#making a first approach to the classification problem with the full data set to 
#evaluate possible next steps

intraining_raw <- createDataPartition(raw_training_data_set$SPACEID,
                                      p=0.70,
                                      list = FALSE) 

training_raw <- raw_training_data_set[intraining_raw,]

testing_raw <- raw_training_data_set[-intraining_raw,]


first_approach_random_forest <- ranger(formula = SPACEID~.,
                                       data = training_raw,
                                       verbose = T)

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

second_approach_random_forest <- ranger(formula = full_location~.,
                                       data = training_raw,
                                       verbose = T)

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
