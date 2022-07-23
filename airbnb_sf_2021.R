######
#
# Created by Yijun Yang
# 
#       How does San Francisco fit with sharing economy?
#       Empirical analysis of Airbnb listing rental prices.
#
#
#######
#
# 1. load package，library and data
#
#######

library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)


listings_detailed_raw <- read_csv("/Users/yyj/Desktop/data200/emperical_project/sf/listings_detailed.csv")
listings_summary_raw  <- read_csv("/Users/yyj/Desktop/data200/emperical_project/sf/listings_summary.csv")

########
#
# 2. Data Exploration and Selection
# 
########

#see the column names of each file

names(listings_detailed_raw)
names(listings_summary_raw)

#2.1 clean the data

#2.1.1 drop the columns that I don't really need and make a new data frame [listings], that I need.

listings = subset(listings_summary_raw, select=-c(name,host_name,neighbourhood_group,last_review,license,availability_365)) 


#2.1.2 add the columns that maybe needed in the analyze

listings$maximun_nights = listings_detailed_raw$maximum_nights

listings$nights_total   = listings$maximun_nights - listings$minimum_nights

listings[c('ava_30',
           'ava_60',
           'ava_90',
           'ava_365')]  = listings_detailed_raw[c('availability_30',
                                                 'availability_60',
                                                 'availability_90',
                                                 'availability_365')]

listings$bed        = listings_detailed_raw$beds

# rs  - review score

listings[c('rs_rating',
           'rs_accuracy',
           'rs_clean',
           'rs_checkin',
           'rs_communication',
           'rs_location',
           'rs_value')]  = listings_detailed_raw[c('review_scores_rating',
                                                  'review_scores_accuracy',
                                                  'review_scores_cleanliness',
                                                  'review_scores_checkin',
                                                  'review_scores_communication',
                                                  'review_scores_location',
                                                  'review_scores_value')]

listings$instant_bookable = listings_detailed_raw$instant_bookable 


#try to label the listings ava_365 into low_ava(0-90), and high_ava(>90) 
# label evidence: http://insideairbnb.com/amsterdam/?neighbourhood=&filterEntireHomes=false&filterHighlyAvailable=false&filterRecentReviews=false&filterMultiListings=false
listings$ava_label <- ifelse(listings$ava_365 >=90, "high_ava", "low_ava")

#2.1.3 clean the out liers

#remove some the outlier for price, if the price is more than 9900, there are 11 of them
sum(listings$price>= 9900)
listings <- listings[listings$price <= 9900,]


#delete the for nights_total, the ones that are over 9900 nights
listings <- listings[listings$nights_total < 9900, ]

names(listings)



# 2.1.3 check the null values for the data
sum(is.na(listings))

# so now we have some null values, find out where they are
sum(is.na(listings$bed))
sum(is.na(listings$reviews_per_month))

#this might be new add to the list
sum(is.na(listings$rs_rating))
sum(is.na(listings$rs_accuracy))
sum(is.na(listings$rs_clean))
sum(is.na(listings$rs_checkin))
sum(is.na(listings$rs_communication))
sum(is.na(listings$rs_location))
sum(is.na(listings$rs_value))

# but what happended to the bedroom number is na?
bedroom_na <- listings[is.na(listings$bed),]

# so in general, the NA values rows do not affect our model and data analysis. 

############
#
# 3. Data Visulization
#
# 3.1 room type?
# 3.2 neighborhood?
# 3.3 host?
# 3.4 Review scores in general

# 3.5 price
#     3.5.1 different room types
#     3.5.2 different neighborhoods
#     3.5.3 different availability (30,60,90,365)
#     3.5.4 different nights_total
#     3.5.5 different bed number
#     3.5.6 different rs_mean values
#     
############

#3.1 room type distribution: most common seen room type in sf
plot_1 <- ggplot(listings, aes(x = room_type)) + 
                geom_bar()+
                geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2))+
                labs(title = "Bar plot of room types on Airbnb in SF",x = "Avaliable room types", y = "Counts")+
                theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_1
#from this plot, we see that the most common ones are 1) entire home/aprt: 4158 and 2) private room:2180

plot_bed <- ggplot(listings, aes(x = bed)) + 
  geom_bar()+
  geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2))+
  labs(title = "Bar plot of bed on Airbnb in SF",x = "bed number", y = "Counts")+
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_bed



## 3.2 neighborhood distribution: most common seen neighborhood

# plot the listings of airbnb on the map of sf, according to the neighbourhood
plot_2 <-ggplot(data = listings) +
  geom_point(mapping = aes(x = longitude, y = latitude, color=neighbourhood)) +
  xlab("") +
  ylab("") +
  labs(color = NULL,title = "Plot of Airbnb listings on the SF Map") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
plot_2


# find out the top neighbourhood, has more than 100 listings
listings %>%
  count(neighbourhood, sort = TRUE) %>%
  filter(n > 100) %>%
  mutate(neighbourhood = reorder(neighbourhood, n)) %>%
  ggplot(aes(neighbourhood, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Count") +
  coord_flip()


# top 5 nieghbourhood has more than 330 listings
listings %>%
  count(neighbourhood, sort = TRUE) %>%
  filter(n > 330) %>%
  mutate(neighbourhood = reorder(neighbourhood, n)) %>%
  ggplot(aes(neighbourhood, n)) +
  geom_col() +
  xlab(NULL) +
  ylab("Count") +
  coord_flip()

# downdtown/civic center
# mission
# south of market
# western addition
# nob hill

neighbourhood <- table(listings$neighbourhood) 
neighbourhood[order()]
neighbourhood

names(sort(table(listings$neighbourhood)))

end_5_neighbourhood_counts = names(sort(table(listings$neighbourhood)))[1:5]
end_5_neighbourhood_counts
top_5_neighbourhood_counts = names(sort(table(listings$neighbourhood)))[36:32]
top_5_neighbourhood_counts
mid_5_neighbourhood_counts = names(sort(table(listings$neighbourhood)))[16:20]
mid_5_neighbourhood_counts



# 3.3 host distribution. Do we have hosts that has many listings/properties?

host_id <- table(listings$host_id)

# number of hosts have more than 1 listings is: 3402
sum(host_id>= 1)

# number of hosts have more than 5 listings is: 167
sum(host_id>= 5)

# number of hosts have more than 10 listings is: 64
sum(host_id>= 10)

# number of hosts have more than 40 listings is: 9
sum(host_id>= 40)

# plot the hosts that has more than 40 listings
listings %>%
  count(host_id, sort = TRUE) %>%
  filter(n > 40) %>%
  mutate(host_id = reorder(host_id, n)) %>%
  ggplot(aes(host_id, n)) +
  geom_col() +
  xlab("host id") +
  ylab("Count") +
  coord_flip()


# commercial hosts is a huge power in the sf airbnb market


# 3.4 Review scores in general. 

# create a review score in general, which is the average of 7 rs score
listings$rs_mean <- rowMeans(listings[,c('rs_accuracy','rs_checkin','rs_communication',
                                         'rs_location','rs_rating',
                                         'rs_value')],na.rm = TRUE)

# number of overall review score in SF Airbnb is: 4.768
all_sf_listings_mean = mean(listings$rs_mean,na.rm = TRUE)

#distribution of overall review score
plot_rs <- ggplot(listings, aes(x = rs_mean)) + 
  geom_histogram()+
  geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2))+
  labs(title = "histogram of mean review score",x = "mean review score", y = "Counts")+
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_rs


# distribution of neighbourhood and its review score? What are the neighbourhood rank?
neighbourhood_rs <- listings[,c('neighbourhood','rs_location','rs_mean')]
neighbourhood_rs_rank = aggregate(.~neighbourhood, data=neighbourhood_rs, mean)

#top 10 neighbourhood with rs_location value
top_neighbourhood_rs_location <- neighbourhood_rs_rank[with(neighbourhood_rs_rank,order(-rs_location)),]
top_10_neighbourhood_rs_location <- top_neighbourhood_rs_location[1:10,]

#last 10 neighbourhood with rs_location value
end_neighbourhood_rs_location <- neighbourhood_rs_rank[with(neighbourhood_rs_rank,order(rs_location)),]
end_10_neighbourhood_rs_location <- end_neighbourhood_rs_location[1:10,]


#top 10 neighbourhood with rs_mean value
top_neighbourhood_rs_mean <- neighbourhood_rs_rank[with(neighbourhood_rs_rank,order(-rs_mean)),]
top_10_neighbourhood_rs_mean <- top_neighbourhood_rs_mean[1:10,]

#last 10 neighbourhood with rs_mean value
end_neighbourhood_rs_mean <- neighbourhood_rs_rank[with(neighbourhood_rs_rank,order(rs_mean)),]
end_10_neighbourhood_rs_mean <- end_neighbourhood_rs_mean[1:10,]


## 3.5 price

# the distribution of price

plot_price <- ggplot(listings, aes(x = id , y = price )) + 
  geom_point()+
  #geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2))+
  labs(title = "scatter plot of price",x = "id", y = "price ($)")+
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_price



#     3.5.1 different room types

# what are the price distribution for different room types?
plot_3 <- ggplot(listings, aes(price, fill = room_type)) +
  geom_density(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(room_type ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "Price($)", y = "Density", fill = "Room type")+
  theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_3

# remark:   4160 entire home
#            84  hotel room
#            2191 private room
#            131 shared room
#
# so for a shared room, it has a normal distribution and with relative low price range
# what we focus more is the entire home and the private room. 
# obviously two very different market.
# entire home/apt: expensive and the number of choice are overall the same from $80 to $200
# private room:   relative cheap, and more choice from $50 to $80.

#     3.5.2 different neighborhoods

# what are the distributoin of all the neighbourhoods?

# too many of them! So only for the selected head and tails.

#what are the distribution of neighbourhoods that has most listings?
top_5_neighbourhood_counts
top_5_neighbourhood_list <- listings[listings$neighbourhood == 'Downtown/Civic Center'|
                                       listings$neighbourhood == 'Mission' |
                                       listings$neighbourhood == 'South of Market' |
                                       listings$neighbourhood == 'Western Addition' |
                                       listings$neighbourhood == 'Nob Hill', ]

plot_4 <- ggplot(top_5_neighbourhood_list, aes(x = price, fill = neighbourhood)) +
  geom_density(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "Price($)", y = "Density", fill = "Top 5 Neighbourhood")
  
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_4

#what are the distribution of neighbourhoods that has least listings?

end_5_neighbourhood_counts
end_5_neighbourhood_list <- listings[listings$neighbourhood == 'Golden Gate Park'|
                                     listings$neighbourhood == 'Presidio' |
                                     listings$neighbourhood == 'Diamond Heights' |
                                     listings$neighbourhood == 'Seacliff' |
                                     listings$neighbourhood == 'Presidio Heights', ]

plot_5 <- ggplot(end_5_neighbourhood_list, aes(x = price, fill = neighbourhood)) +
  geom_density(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "Price($)", y = "Density", fill = "End 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_5


#what are the distribution of neighbourhoods that has moddle listings?
mid_5_neighbourhood_counts

mid_5_neighbourhood_list <- listings[listings$neighbourhood == 'Inner Sunset'|
                                       listings$neighbourhood == 'Chinatown' |
                                       listings$neighbourhood == 'Outer Richmond' |
                                       listings$neighbourhood == 'Pacific Heights' |
                                       listings$neighbourhood == 'Potrero Hill', ]

plot_6 <- ggplot(mid_5_neighbourhood_list, aes(x = price, fill = neighbourhood)) +
  geom_density(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "Price($)", y = "Density", fill = "Mid 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_6

# So we can see that except the neighbourhood that appear the least in the list,
# there is an obvious normal distribution of price for the neighbourhood.



#     3.5.3 different availability (30,60,90,365)

# first check the avaliability distribution over different neighbourhood
plot_7 <- ggplot(top_5_neighbourhood_list, aes(x = ava_365, fill = neighbourhood)) +
  geom_density(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "ava_365", y = "Density", fill = "Top 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_7

plot_8 <- ggplot(end_5_neighbourhood_list, aes(x = ava_365, fill = neighbourhood)) +
  geom_histogram(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "ava_365", y = "Density", fill = "End 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_8

plot_9 <- ggplot(mid_5_neighbourhood_list, aes(x = ava_365, fill = neighbourhood)) +
  geom_density(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "ava_365", y = "Density", fill = "Mid 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_9



#from the plot, there is some distribution, but not obvious.
#try to label the listings ava_365 into low_ava(0-60), and high_ava(>60) 
# label evidence: http://insideairbnb.com/amsterdam/?neighbourhood=&filterEntireHomes=false&filterHighlyAvailable=false&filterRecentReviews=false&filterMultiListings=false
listings$ava_label <- ifelse(listings$ava_365 >=60, "high_ava", "low_ava")

plot_10 <- ggplot(listings, aes(x = price, fill = ava_label)) +
  geom_histogram(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(ava_label ~.)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200,40))+
  labs(x = "price", y = "counts", fill = "availability label")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_10
# so for low ava and high_ava, the price distribution are similar, 

# also check the low and high ava for selsetec  neighbourhood
plot_11 <- ggplot(mid_5_neighbourhood_list, aes(x = ava_label, fill = neighbourhood)) +
  geom_bar(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "availability label", y = "Counts", fill = "Mid 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_11

plot_12 <- ggplot(top_5_neighbourhood_list, aes(x = ava_label, fill = neighbourhood)) +
  geom_bar(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "availability label", y = "Counts", fill = "Top 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_12

plot_13 <- ggplot(end_5_neighbourhood_list, aes(x = ava_label, fill = neighbourhood)) +
  geom_bar(alpha = 0.85, position = "stack", size = 0.5, col = "gray40")+
  facet_grid(neighbourhood ~.)+
  scale_fill_brewer(palette = "Dark2")+
  labs(x = "availability label", y = "Counts", fill = "End 5 Neighbourhood")
theme(strip.text.y = element_text(size = 0), plot.caption = element_text(color = "gray45", size = 7))
plot_13

# there are much more high_available ones than low_ava ones in the top neighbourhood.
# make sense because those neighbourhoodare the most popular ones
#  BUT!!!!
# remember the rs score!!! they are not the neighbourhood with best scores/reviews!!


#     3.5.4 different nights_total


#plot the nights total distribution
plot_nights_total <- ggplot(listings, aes(x = nights_total)) + 
  geom_density()+
  #geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2))+
  labs(title = "distribution of nights intotal",x = "nights total (days)", y = "density")+
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_nights_total

# plot of relationship between x= nights_total, y = price
plot_nt_price <- ggplot(listings, aes(x = nights_total,y = price)) + 
  geom_point()+
  labs(title = "scatter plot of nights intotal vs price",x = "nights total (days)", y = "price ($)")+
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_nt_price
# a trend but not that obvious but still there is some trend

#     3.5.5 different bed number

# first see the counts of beds
plot_bed <- ggplot(listings, aes(x = bed)) + 
  geom_bar()+
  geom_text(stat = 'count', aes(label = stat(count), vjust = -0.2))+
  labs(title = "Bar plot of beds on Airbnb in SF",x = "Avaliable beds", y = "Counts")+
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_bed


#plot the raw data, x = bed humber, y = price
plot_bd_price <- ggplot(listings, aes(x = bed,y = price)) + 
  geom_point()+
 labs(title = "scatter plot of bed number vs price",x = "bed number", y = "price ($)")+ 
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_bd_price 

#there is some trend.

#3.5.6 rs vs price

# plot the raw data, scatter plot of all review score w.r.t price
plot_rs_price <- ggplot(listings, aes(x = rs_mean,y = price)) + 
  geom_point()+
  labs(title = "scatter plot of mean review score vs price ",x = "Mean review score", y = "price ($)")+
  theme(plot.title=element_text(size = 11.5,hjust = 0.5))
plot_rs_price


#there is some trend, but maybe need a little bit of twist.



###########
#
# 4. Machine Learning, target variable: Price

# x candidates: rs_mean, 
#               number of bed, 
#               in top neighbourhood?
#               availability in 365
#               nights in total
#               room type

#names(listings)

top_5_neighbourhood_counts
mid_5_neighbourhood_counts
end_5_neighbourhood_counts

#sort the neighbourhood with counts, from least appaer to most appear
names(sort(table(listings$neighbourhood)))

# create a a new data frame for machine learning
data = subset(listings, select=c(neighbourhood,
                                 room_type,price,nights_total,bed,rs_mean,ava_label)) 

# scale the data: scale the price and the total_nights
price_scaled = sapply(data[,c("price")],scale)
data$price = price_scaled

nt_scaled = sapply(data[,c("nights_total")],scale)
data$nights_total = nt_scaled

# change the ava_label to a number
# so that, items with high ava is 1
# so that, items with low ava is 0
data$ava_label <- ifelse(data$ava_label %in% c("high_ava"), 1, 0)


#if the item is in the top 5 neighbourhood that appear , it has label as 1, other wise,0
data$neighbourhood <- ifelse(data$neighbourhood %in% c("Downtown/Civic Center","Mission","South of Market",
                                                             "Western Addition","Nob Hill" ),1,0)

#room type label
# it the item is a entire home/apt, it has label as 1, other wise, 0
data$room_type <- ifelse(data$room_type %in% c("Entire home/apt"),1,0)

#

library(broom)
library(tidyverse)
library(caret)

set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, repeats =  4)
# 
# 4.1 Linear Regression Model with cross validation, fold = 5, repeat = 4
# 
#     

# Train the model
model_1 <- train(price ~ neighbourhood + room_type + ava_label + nights_total * bed * rs_mean, data = data,
                 method = "glm",
                 trControl = train.control,na.action=na.exclude)
summary(model_1)
# Summarize the results
print(model_1)

model_m1 <- train(price ~ neighbourhood + room_type  + bed * rs_mean, data = data,
                 method = "glm",
                 trControl = train.control,na.action=na.exclude)
summary(model_m1)
# Summarize the results
print(model_m1)



# 4.2 Random forest with cross validation

set.seed(123) 

# Train the model
model_2 <- train(price ~ neighbourhood + room_type + ava_label + nights_total + bed + rs_mean, data = data,
                 method = "rf" ,
                 trControl = train.control,na.action=na.exclude)


summary(model_2)
# Summarize the results
print(model_2)


#https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train

# 4.3 Boosted regression modee with cross validation
#


model_3 <- train(price ~ neighbourhood + room_type + ava_label + nights_total + bed + rs_mean, data = data, 
                 method = "glmboost" ,
                 trControl = train.control,na.action=na.exclude)


summary(model_3)
# Summarize the results
print(model_3)



# 4.4 

model_4 <- train(price ~ neighbourhood + room_type + ava_label + nights_total + bed + rs_mean, data = data,
                 method = "rlm" ,
                 trControl = train.control,na.action=na.exclude)


summary(model_4)
# Summarize the results
print(model_4)


# what about do the classification?
data_2 <- data

# if the price is high, then label 1, if the price is low, label 0.
data_2$price_lab  <- ifelse(data_2$price >= 0,"expensive","cheap")

# 4.5 doing classification with knn 

model_5 <- train(price_lab ~ neighbourhood + room_type + ava_label + nights_total + bed + rs_mean, 
                 data = data_2,
                 method = "knn" ,
                 trControl = train.control,na.action=na.exclude)


summary(model_5)
# Summarize the results
print(model_5)

#get the confusion matrix
cm_1 <- confusionMatrix(model_5)
cm_1

#The final value used for the model was k = 9.



# 4.6 Random forest, CLASSIFICATION, with cross validation


# Train the model
model_6<- train(price_lab ~ neighbourhood + room_type  + bed + rs_mean, 
                data = data_2,
                 method = "rf" ,
                 trControl = train.control,na.action=na.exclude)

summary(model_6)
print(model_6)
cm_2 <- confusionMatrix(model_6)
cm_2

library(randomForest)
#install.packages("cli")

#library(devtools)
#devtools::install_github('skinner927/reprtree')
#library(reprtree)
#data_2$price_lab = factor(data_2$price_lab)
#model_rf <- randomForest(price_lab ~ neighbourhood + room_type + ava_label + nights_total + bed + rs_mean, 
                         data=data_2, importance=TRUE, ntree=500, 
                         mtry = 2, do.trace=100,trControl = train.control,na.action=na.exclude)
#reprtree:::plot.getTree(model_rf)

#using random forest to do classficiation is good.

# 4.7 nnet 

model_7<- train(price_lab ~ neighbourhood + room_type + ava_label + nights_total + bed + rs_mean, 
                data = data_2,
                method = "nnet" ,
                trControl = train.control,na.action=na.exclude)


summary(model_7)
# Summarize the results
print(model_7)

cm_3<- confusionMatrix(model_7)
cm_3

### ###### k-means culster unsupervise ones with hosts
data_h = subset(listings, select=c(host_id,neighbourhood,
                                 room_type,price,nights_total,bed,rs_mean,ava_label)) 

# scale the data: scale the price and the total_nights
price_scaled_h = sapply(data_h[,c("price")],scale)
data_h$price = price_scaled_h

nt_scaled_h = sapply(data[,c("nights_total")],scale)
data$nights_total_h = nt_scaled_h

# change the ava_label to a number
# so that, items with high ava is 1
# so that, items with low ava is 0
data_h$ava_label <- ifelse(data_h$ava_label %in% c("high_ava"), 1, 0)


#if the item is in the top 5 neighbourhood that appear , it has label as 1, other wise,0
data_h$neighbourhood <- ifelse(data_h$neighbourhood %in% c("Downtown/Civic Center","Mission","South of Market",
                                                       "Western Addition","Nob Hill" ),1,0)

#room type label
# it the item is a entire home/apt, it has label as 1, other wise, 0
data_h$room_type <- ifelse(data_h$room_type %in% c("Entire home/apt"),1,0)

data_h <- na.omit(data_h)
kmeans_basic_h <- kmeans(data_h, centers = 2)
kmeans_basic_table_h <- data.frame(kmeans_basic_h$size, kmeans_basic_h$centers)
kmeans_basic_df_h <- data.frame(Cluster = kmeans_basic_h$cluster, data_h)
plot_kmeans_h <- ggplot(data = kmeans_basic_df_h, aes(y = Cluster)) +
  geom_bar(aes(fill = Cluster)) +
  ggtitle("kmeans cluster for hosts") +
  theme(plot.title = element_text(hjust = 0.5))
plot_kmeans_h
kmeans_basic_table_h
###

#k-means cluster unsupervised ones with listings
# create clean data with no NA
data_3 <- data
data_3 = subset(data_3,select=c(price, nights_total,ava_label))
data_3 <- na.omit(data_3)
kmeans_basic <- kmeans(data_3, centers = 2)
kmeans_basic_table <- data.frame(kmeans_basic$size, kmeans_basic$centers)
kmeans_basic_df <- data.frame(Cluster = kmeans_basic$cluster, data_3)
plot_kmeans <- ggplot(data = kmeans_basic_df, aes(y = Cluster)) +
  geom_bar(aes(fill = Cluster)) +
  ggtitle("kmeans cluster") +
  theme(plot.title = element_text(hjust = 0.5))
plot_kmeans
kmeans_basic_table


##########


