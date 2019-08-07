#Wifi location project

#load packages ----

pacman::p_load(dplyr,caret,ggplot2,tidyr,utils,matrixStats,rayshader,sf,viridis, graphics)

remotes::install_github("tylermorganwall/rayshader")

#load the training data set ----


raw_training_data_set <- read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 9/Wifi Project/Data sets/trainingData.csv')


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
















