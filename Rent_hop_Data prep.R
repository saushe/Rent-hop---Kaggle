# Rent Hop

library("jsonlite")
library("purrr")
library("dplyr")
library("devtools")
install_github("saushe/RED")
library("RED")

data<- fromJSON("E:/SS BAck Up/Downloads/College/In USA/2. Spring Semester/Competetions/Rent Hop/train.json")
sapply(data, head)

cols <- setdiff(names(data), c("photos", "features"))
data_df <- map_at(data, cols, unlist) %>% tibble::as_tibble(.)                        
data_df<- as.data.frame(data_df)

# test data import
test_data<- fromJSON("E:/SS BAck Up/Downloads/College/In USA/2. Spring Semester/Competetions/Rent Hop/test.json")
sapply(test_data, head)

cols <- setdiff(names(test_data), c("photos", "features"))
test_data_df <- map_at(test_data, cols, unlist) %>% tibble::as_tibble(.)                        
test_data_df<- as.data.frame(test_data_df)

# Primary Data Check
summarise_all(data_df, class)

# Missing vlaue count
View(RED::col_missing_count(data_df))
View(RED::col_missing_count(test_data_df))
# WOW no Misssing Values !!

#Univariate
View(RED::univariate_numvar(data_df))

#2.3% of the prices are beyond 3IQR
plot(data_df$price)

PriceIQR<- quantile(data_df$price, .75) - quantile(data_df$price, .25)
# For now I am treating very high Price Outlier by capping and any ways I am planning to try tree based approach so this should not be much of a problem
data_df$price[(which(data_df$price>(quantile(data_df$price, .75)+(15*PriceIQR))))]<-
  min(data_df$price[(which(data_df$price>(quantile(data_df$price, .75)+(15*PriceIQR))))])
plot(density(data_df$price))

# No need to treat numeric Price variable for outlier in the test data

# Char var univariate
# No univariate required for building_id, cerated,manager_id
# Description, display address, street address and features will be treated separately during feature extraction
# I just want to see distribution of my target variable
prop.table(table(data_df$interest_level))*100
# Only 8% high !!

# latitude and longitude check
length(which(data_df$latitude<40.4))
#[1] 15 - This is impossible
data_df$latitude[which(data_df$latitude<40.4)]<- mean(data_df$latitude)

length(which(data_df$latitude>41.5))
#[1] 15 - This is impossible
data_df$latitude[which(data_df$latitude>41.5)]<- mean(data_df$latitude)

length(which(data_df$longitude>-73.6))
#[1] 34 - This is impossible
data_df$longitude[which(data_df$longitude>-73.6)]<- mean(data_df$longitude)

length(which(data_df$longitude< -74.1))
# 4
data_df$longitude[which(data_df$longitude< -74.1)]<- mean(data_df$longitude)

plot(data_df$longitude, data_df$latitude)
points(data_df$longitude[data_df$interest_level=="low"], data_df$latitude[data_df$interest_level=="low"], col = "blue")
points(data_df$longitude[data_df$interest_level=="medium"], data_df$latitude[data_df$interest_level=="medium"], col = "green")
points(data_df$longitude[data_df$interest_level=="high"], data_df$latitude[data_df$interest_level=="high"], col = "red")

# Feature Extraction from photos, description and features
# =========================================================
# Counting number of photos for train data
f1<- function(x)
{
  p = length(unlist(x))
  return(p)
}
f1(data_df$photos[1])

data_df<- group_by(data_df, listing_id) %>% mutate( photo_count = f1(photos))%>% ungroup()
head(data_df$photo_count)
data_df<- as.data.frame(data_df)

#.# Counting number of photos for test data
test_data_df<- group_by(test_data_df, listing_id) %>% mutate( photo_count = f1(photos))%>% ungroup()
head(test_data_df$photo_count)
test_data_df<- as.data.frame(test_data_df)


# For Bedroom and Bathroom I may try them grouping.But I am not doing that now. I will do after I run ...
# ... 1st iteration. then I will do that to compare chnage in outcome

# Building ID and created date should not have any predictive power, so i will not use them

#===============================================================
# Features - Lets use it to extract some features
f2<- function(l)
{
  p<- as.vector(c(unlist(l)))
  p<-  paste0(p, sep = "<>", collapse = " ")
  return(p)
}

data_df<- group_by(data_df, listing_id) %>% mutate( features_list = f2(features))%>% ungroup()
View(head(data_df$features_list))
data_df<- as.data.frame(data_df)

# creating a dtm out of the feature_list
features<- data_df[,c(9,17)]

# remove white spaces
f3<- function(x){
  y<- gsub(" ", "",x)
  return(y)
}

features$features_list<- apply(features[2],1,f3)

# replace "<>" with white space
f4<- function(x){
  y<- gsub("<>", " ",x)
  return(y)
}
features$features_list<- apply(features[2],1,f4)

# Check the number of features per listing

library(tm)

myReader <- readTabular(mapping=list(content="features_list", id="listing_id"))
corp_feature<- Corpus(DataframeSource(features), readerControl=list(reader=myReader))

dtm_feature<- DocumentTermMatrix(corp_feature)
findFreqTerms(dtm_feature, lowfreq = 100)
#Find the sum of words in each Document
rowTotals <- apply(dtm_feature , 1, sum)

matrix_features<- as.matrix(dtm_feature)
colTotals<- apply(matrix_features, 2, sum)
unique_features<- colnames(matrix_features)
feature_freq<- data.frame(feature = unique_features, count = colTotals)

rm(matrix_features)
write.csv(feature_freq, "ff.csv")

# removing sparse features and new feature mapping
tdm_feature<- TermDocumentMatrix(corp_feature)
df_features<- as.data.frame(as.matrix(tdm_feature))

df_features$features<-rownames(df_features)
ff <- read.csv("C:/Users/Ayasha/ff.csv", stringsAsFactors=FALSE)

ff<- ff[,c(1,3)]
df_features<- merge(df_features, ff, by.x = "features", by.y = "feature", all.x = T)

df_features<- df_features[,-1]

new_df_features<- group_by(df_features, F_map)%>% summarise_each(funs(sum))

T_new_df_features<- t(new_df_features)
T_new_df_features<- data.frame(T_new_df_features)
T_new_df_features$id <- rownames(T_new_df_features)
T_new_df_features<- mutate_each(T_new_df_features, funs(as.character))
x<- as.character(as.vector(T_new_df_features[1,]))
x[61]<-"misc2" 
colnames(T_new_df_features)<- c(x)
T_new_df_features<- T_new_df_features[-1,]
T_new_df_features<- mutate_each(T_new_df_features, funs(as.numeric))
T_new_df_features$other<- T_new_df_features$misc+T_new_df_features$misc
T_new_df_features<- select(T_new_df_features, - misc, -misc2)


# add the newly created features to the data and number of features to teh data
data_df$count_features<- rowTotals 
data_df<- merge(data_df, T_new_df_features, by.x ="listing_id", by.y = "F_map", all.x = T  )
--------------------------------------------------------------------
  #.# Extracting info from the 'features' column for TEST
  # Features - Lets use it to extract some features
  f2<- function(l)
  {
    p<- as.vector(c(unlist(l)))
    p<-  paste0(p, sep = "<>", collapse = " ")
    return(p)
  }

test_data_df<- group_by(test_data_df, listing_id) %>% mutate( features_list = f2(features))%>% ungroup()
View(head(test_data_df$features_list))
test_data_df<- as.data.frame(test_data_df)

# creating a dtm out of the feature_list
test_features<- test_data_df[,c(9,16)]

# remove white spaces
f3<- function(x){
  y<- gsub(" ", "",x)
  return(y)
}

test_features$features_list<- apply(test_features[2],1,f3)

# replace "<>" with white space
f4<- function(x){
  y<- gsub("<>", " ",x)
  return(y)
}
test_features$features_list<- apply(test_features[2],1,f4)

# Check the number of features per listing

library(tm)

myReader <- readTabular(mapping=list(content="features_list", id="listing_id"))
corp_test_features<- Corpus(DataframeSource(test_features), readerControl=list(reader=myReader))

dtm_test_features<- DocumentTermMatrix(corp_test_features)

#Find the sum of words in each Document
rowTotals <- apply(dtm_test_features , 1, sum)

matrix_test_features<- as.matrix(dtm_test_features)
colTotals<- apply(matrix_test_features, 2, sum)
unique_features<- colnames(matrix_test_features)
test_features_freq<- data.frame(feature = unique_features, count = colTotals)

rm(matrix_test_features)

# Map these features with the features extracted from the 
test_features_freq<- merge(test_features_freq, ff, by.x = "feature",
                           by.y = "feature", all.x = T)

# replace na by "other"
test_features_freq[is.na(test_features_freq)==T]<- "other"
test_ff<- test_features_freq[,c(1,3)]

# removing sparse features and new feature mapping
tdm_test_feature<- TermDocumentMatrix(corp_test_features)
df_test_feature<- as.data.frame(as.matrix(tdm_test_feature))

df_test_feature$features<-rownames(df_test_feature)

df_test_feature<- merge(df_test_feature, test_ff, by.x = "features", by.y = "feature", all.x = T)

df_test_feature<- df_test_feature[,-1]

new_df_test_features<- group_by(df_test_feature, F_map)%>% summarise_each(funs(sum))

T_new_df_test_features<- t(new_df_test_features)
T_new_df_test_features<- data.frame(T_new_df_test_features)
T_new_df_test_features$id <- rownames(T_new_df_test_features)
T_new_df_test_features<- mutate_each(T_new_df_test_features, funs(as.character))
x<- as.character(as.vector(T_new_df_test_features[1,]))
colnames(T_new_df_test_features)<- c(x)
T_new_df_test_features<- T_new_df_test_features[-1,]

# add the newly created features to the data and number of features to teh data
test_data_df$count_features<- rowTotals 
test_data_df<- merge(test_data_df, T_new_df_test_features, by.x ="listing_id", by.y = "F_map", all.x = T  )

sum(as.numeric(test_data_df$misc))
test_data_df$other<- as.numeric(test_data_df$other)
test_data_df$misc<- as.numeric(test_data_df$misc)
test_data_df$other<- test_data_df$misc+test_data_df$misc
test_data_df<- select(test_data_df, - misc)


# remove unnecessary variables which have been created
rm(df_desc,df_features,new_df_test_features, df_test_feature, 
   features, T_new_df_features, T_new_df_test_features, test_features)
rm(corp_test_features, corp_feature)
rm(dtm_test_features, dtm_feature)
rm(tdm_feature, tdm_test_feature)

#=============================================================#
# Extracting information associated with locations

mydata<- select(data_df, latitude, longitude)
# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(mydata, 20) # 5 cluster solution


# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)
mydata$int<- data_df$interest_level
mydata$high<- ifelse(mydata$int== "high",1,0)
mydata$medium<- ifelse(mydata$int== "medium",1,0)
mydata$low<- ifelse(mydata$int== "low",1,0)
mydata$id<- data_df$listing_id

mydata_summary<- select(mydata, fit.cluster,high, medium, low)
mydata_summary<- group_by(mydata_summary, fit.cluster)%>% summarise_each(funs(sum))

mydata_summary<- group_by(mydata_summary,fit.cluster)%>% mutate(total = sum(high, medium, low))
mydata_summary$high_per<- mydata_summary$high/mydata_summary$total

# map the newly created clusters in data
mydata2<- select(mydata, id, fit.cluster)
data_df<- merge(data_df, mydata2, by.x = "listing_id", by.y = "id", all.x =T )

-----------------------------------------------------------------
  #.# Accociate each of the test data to the nearest cluster
  
  long_lat_test<- select(test_data_df, listing_id, longitude, latitude)
centroid<- as.data.frame(fit$centers)
long_lat_test[4:23]<- NA

for ( i in 1:20)
{
  long_lat_test[i+3]<- sqrt(((long_lat_test$latitude- centroid$latitude[i])^2)+
                              ((long_lat_test$longitude- centroid$longitude[i])^2))
}

which_min<- function(x)
{
  y = which(x== min(x))
  return(y)
}
long_lat_test$fit.cluster<- apply(long_lat_test[,4:23], 1, which_min)
long_lat_test<- select(long_lat_test, listing_id, fit.cluster)
test_data_df<- merge(test_data_df, long_lat_test, all.x =T) 

rm(long_lat_test)

#===============================================================

# Extracting features from Description

library("tm")
library("dplyr")

df_desc<- select(data_df, listing_id, description)
test_df_desc<- select(test_data_df, listing_id, description)

df_desc<- rbind(df_desc, test_df_desc)

# convert it to corpus and 

reader<- readTabular(mapping = list(content = "description", id = "listing_id"))
desc_corp<- Corpus(DataframeSource(df_desc), readerControl = list(reader= reader))
writeLines(as.character(desc_corp[1:2]))

# cleaning the damm text
desc_corp<- tm_map(desc_corp, content_transformer(tolower))
desc_corp<- tm_map(desc_corp, content_transformer(stripWhitespace))
desc_corp<- tm_map(desc_corp, removeNumbers)
desc_corp<- tm_map(desc_corp, removePunctuation)
desc_corp<- tm_map(desc_corp, removeWords, stopwords("en"))
# everything fine till now. Thank God I still have my ID
# stemming now
desc_corp<- tm_map(desc_corp, stemDocument, language = "english")

# lets create a dtm
desc_dtm<- DocumentTermMatrix(desc_corp, control = list(wordLengths = c(4,20),
                                                        bounds = list(global = c(5000,49352))))
findFreqTerms(desc_dtm, lowfreq = 1000)

row_tota<- apply(desc_dtm, 1, sum)
col_tota<- apply(desc_dtm, 2, sum)

desc_dtm<- desc_dtm[row_tota>0,]


# One way of adding features is to 
library("topicmodels")

# Try LDA to get some topics
#Deciding best K value using Log-likelihood method
best.model <- lapply(seq(1, 50, by = 1), function(d){LDA(desc_dtm, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
#calculating LDA
k = 27;#number of topics
# SEED = 786; # number of tweets used

# running LDA with 27 topics
desc_lda<- LDA(desc_dtm, k = k)
Topics<- topics(desc_lda)
Terms<- as.matrix(terms(desc_lda,10))
Topic_probab<- as.data.frame(desc_lda@gamma)

# Scaling the variables
scale_obj<- function(x){
  avg<- mean(x)
  std<- sd(x)
  y<- (x-avg)/std
  return(y)
}
desc_df_features1<- mutate_each(Topic_probab, funs(scale_obj))

# Another way of adding features from dtm is to simply us the tfidf metics or to use the frequency or tto use the flags
# First I will try with the flags
# I dont want my feature list to get too big, so I will create a new dtm where..
# ... I will include the words occouring in more than 500 documents
desc_dtm2<- DocumentTermMatrix(desc_corp, control = list(wordLengths = c(4,20),
                                                         bounds = list(global = c(5000,49352))))
desc_df_features2<- as.data.frame(as.matrix(desc_dtm2))
desc_df_features2[desc_df_features2>0]<- 1
desc_df_features2$id<- rownames(desc_df_features2)

desc_df_features2<- select(desc_df_features2, -within, -will, -well, -true, -updat, -text, -must, -like, -just, -also) 

# Creating matrix with tf-idf
desc_dtm3<- DocumentTermMatrix(desc_corp, control = list(wordLengths = c(4,20),
                                                         bounds = list(global = c(5000,49352)), 
                                                         weighting = weightTfIdf))
desc_df_features3<- as.data.frame(as.matrix(desc_dtm3))

desc_df_features3$id<- rownames(desc_df_features3)
desc_df_features3<- select(desc_df_features3, -within, -will, -well, -true, -updat, -text, -must, -like, -just, -also) 
# Now I can combine either 'desc_df_features1' or 'desc_df_features3' or 'desc_df_features2' ...
# ... to combine with ourt main data set
rm(desc_dtm3, desc_dtm2)

 # ========================================================================

manager <- select(data_df, manager_id, interest_level)
# Create dummy vars for interest level
dumy_var<- dummyVars(" ~ interest_level", data = manager)
manager<- data.frame(predict(dumy_var, newdata = manager))
colnames(manager)<- c("high", "low", "medium")
manager$tot<- apply(manager, 1, sum)
manager$id<- data_df$manager_id

manager<- group_by(manager, id)%>% summarise_each(funs(sum))

# Extracting Infor from Manager_ID
mamager <- read.csv("C:/Users/sbtrader/Downloads/mamager.csv")
data_df<- merge(data_df, mamager, by.x = "manager_id", by.y = "id", all.x = T)

test_data_df<- merge(test_data_df, mamager, by.x = "manager_id", by.y = "id", all.x = T)
test_data_df$Manager_Flag[is.na(test_data_df$Manager_Flag)==T]<- "other"

# Extracting info from created
data_df$month<- months(as.Date(data_df$created))
test_data_df$month<- months(as.Date(test_data_df$created))


data_df$day<- weekdays(as.Date(data_df$created))
test_data_df$day<- weekdays(as.Date(test_data_df$created))

# Extracting info from price
data_df$price_perBED<- data_df$price/data_df$bedrooms
data_df$price_perBED<- ifelse(data_df$price_perBED == Inf, data_df$price, data_df$price_perBED)
data_df$price_perBATH<- data_df$price/data_df$bathrooms
data_df$price_perBATH<- ifelse(data_df$price_perBATH == Inf, data_df$price, data_df$price_perBATH)

test_data_df$price_perBED<- test_data_df$price/test_data_df$bedrooms
test_data_df$price_perBATH<- test_data_df$price/test_data_df$bathrooms
test_data_df$price_perBED<- ifelse(test_data_df$price_perBED == Inf, test_data_df$price, test_data_df$price_perBED)
test_data_df$price_perBATH<- ifelse(test_data_df$price_perBATH == Inf, test_data_df$price, test_data_df$price_perBATH)

# Scaling Fetures and making the data model ready
# Scaling all the features 
scaled_data<- data_df
scaled_data<- select(scaled_data, -manager_id, - building_id, -description, -created, -display_address, -latitude,
                     - longitude, - street_address, -features_list, - photos, - features)

# Convert cluster to categorical
scaled_data$fit.cluster<- as.character(scaled_data$fit.cluster)

#Convert features to categoruical and replace the  values gt1 by 1
scaled_data_fet<- scaled_data[,c(1,8:66)]

rep1<- function(x)
{
  
  x[x>1 & x<20]<- 1
  return(x)
  
}
scaled_data_fet<- as.data.frame(apply(scaled_data_fet, 2, rep1))
scaled_data_fet<- as.data.frame(apply(scaled_data_fet,2, as.character))
scaled_data<- scaled_data[,-c(8:66)]

scaled_data<- merge(scaled_data, scaled_data_fet, all.x =T) 

rm(scaled_data_fet)

# Scale all the other variables
scaled_data$price<- scale(scaled_data$price)
scaled_data$bathrooms<- scale(scaled_data$bathrooms)
scaled_data$bedrooms<- scale(scaled_data$bedrooms)
scaled_data$photo_count<- scale(scaled_data$photo_count)
scaled_data$count_features<- scale(scaled_data$count_features)
scaled_data$price_perBED<- scale(scaled_data$price_perBED)
scaled_data$price_perBATH<- scale(scaled_data$price_perBATH)

# Adding separate columns for the three cases
scaled_data$high<- ifelse(scaled_data$interest_level == "high", "high", "other")
scaled_data$low<- ifelse(scaled_data$interest_level == "low", "low", "other")
scaled_data$medium<- ifelse(scaled_data$interest_level == "medium", "medium", "other")
scaled_data$t_t<- "train"

#.# --------------------------------------------------------------------
# Repeating the avobe exerscie for test set

test_scaled_data<- test_data_df
test_scaled_data<- select(test_scaled_data, -manager_id, - building_id, -description, -created, -display_address, -latitude,
                          - longitude, - street_address, -features_list, - photos, - features)

# Convert cluster to categorical
test_scaled_data$fit.cluster<- as.character(test_scaled_data$fit.cluster)

#Convert features to categoruical and replace the  values gt1 by 1
test_scaled_data_fet<- test_scaled_data[,c(1,8:66)]
test_scaled_data_fet<- as.data.frame(apply(test_scaled_data_fet, 2,as.numeric))

rep1<- function(x)
{
  
  x[x>1 & x<20]<- 1
  return(x)
  
}
test_scaled_data_fet<- as.data.frame(apply(test_scaled_data_fet, 2, rep1))
test_scaled_data_fet<- as.data.frame(apply(test_scaled_data_fet,2, as.character))
test_scaled_data<- test_scaled_data[,-c(8:66)]

test_scaled_data<- merge(test_scaled_data, test_scaled_data_fet, all.x =T) 

rm(test_scaled_data_fet)

# Scale all the other variables
test_scaled_data$price<- scale(test_scaled_data$price)
test_scaled_data$bathrooms<- scale(test_scaled_data$bathrooms)
test_scaled_data$bedrooms<- scale(test_scaled_data$bedrooms)
test_scaled_data$photo_count<- scale(test_scaled_data$photo_count)
test_scaled_data$count_features<- scale(test_scaled_data$count_features)
test_scaled_data$price_perBED<- scale(test_scaled_data$price_perBED)
test_scaled_data$price_perBATH<- scale(test_scaled_data$price_perBATH)

test_scaled_data$interest_level<- "x"
test_scaled_data$high<- "x"
test_scaled_data$low<- "x"
test_scaled_data$medium<- "x"
test_scaled_data$t_t<- "test"

l <- list(scaled_data, test_scaled_data)

logisctic_data<- do.call(rbind, lapply(l, function(x) x[match(names(l[[1]]), names(x))]))

rm(scaled_data, test_scaled_data, l)


