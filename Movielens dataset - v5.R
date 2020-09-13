# 1/LIBRARIES ---------------------------------------------------------
library(dplyr)
library(caret)
library(lubridate) #date and time manipulation
library(ggplot2)
library(stringr)


# 2/LOADING DATASET ---------------------------------------------------
load('edx.Rda') #previously saved with function save, because re run is longer

#edx_base, a copy of edx, is used in the following
edx_base <- edx 


# 3/DATA CLEANING -----------------------------------------------------
##seeking for 'NA' in edx dataset
test1 <- ifelse(edx == "NA", 1, 0)
##seeking for blank cells in edx dataset
test2 <- ifelse(edx == "" | edx == " ", 1, 0)

sum(test1)
sum(test2) #zero in both cases indicates no "NA" and no blank cells

##add date and time in readable format in edx_base
dt <- as_datetime(edx_base$timestamp)
edx_base <- edx_base %>% mutate(dt)


# 4/DATA EXPLORATION --------------------------------------------------

##4.1/GENERAL INFORMATION

###number of rows in edx_base
edx_n <- edx_base %>% nrow()

###number of movies rated in edx_base
edx_m <- edx_base %>% group_by(movieId) %>%
  summarize(n()) %>% nrow()

###number of users in edx_base
edx_u <- edx_base %>% group_by(userId) %>%
  summarize(n()) %>% nrow()

###average rating
edx_ra <- mean(edx_base$rating)

###period of time covered by edx_base
oldest <- min(edx_base$timestamp)
oldest <- as_datetime(oldest)
mrecent <- max(edx_base$timestamp)
mrecent <- as_datetime(mrecent)

duration <- interval(oldest, mrecent)

###genders : number of classifications (multi-gender items) in the dataset
edx_g <- edx_base %>% group_by(genres) %>%
  summarize(n()) %>% nrow()

###printing results
#number of rows
edx_n
#number of movies
edx_m
#number of users
edx_u
#average rating
edx_ra
#period of time
duration
#multi-gender items
edx_g


##4.2/GRAPHICAL REPRESENTATIONS

###4.2.1/movies vs rating : the idea is to examine the rating distribution for all movies
gr1 <- edx_base %>% group_by(movieId) %>%
  summarize(avg_rating = mean(rating)) %>% mutate()

gindex_movieId <- seq(1,edx_m,1) #creates a new index because movieId is not continuous (edx_m = total number of movies)
gr1 <- data.frame(gindex_movieId, gr1)

grt <- gr1 %>%
  ggplot(aes(x=gindex_movieId, y=avg_rating)) +
           geom_point(size=0.5) +
           geom_hline(yintercept=edx_ra, colour = "blue") #draw a blue line for average rating
grt #draw graph

###
###4.2.2/users vs gender : the idea is to see if users have particular preferences concerning movie genders
gr2 <- edx_base %>% group_by(userId, genres) %>% #group by user and by gender(s)
  summarize() %>% mutate()

gr2s <- gr2[1:1500,]  #test only on 1500 first rows in the dataset

grt <- gr2s %>%
  ggplot(aes(x=userId, y=genres, color=genres)) +  #every color represents a gender category
  scale_x_continuous(limits = c(1,15)) +           #print only the first 15 users
  theme(legend.position='none') +                  #delete legend
  theme(axis.text.y = element_blank()) +           #delete description of every gender
  geom_point(size=2)                               
grt #draw graph

###
###4.2.3/new movies by years
spl <- str_extract(edx_base$title, "\\(\\d{4}\\)")
year <- str_extract(spl, "\\d{4}")

gr3 <- edx_base %>% mutate(year) %>% select(title, year)

gr3b <- gr3 %>% group_by(year, title) %>%
  summarize() %>% mutate()

grt <- gr3b %>% ggplot(aes(x = factor(year))) +
  theme(axis.text.x = element_blank()) +
  geom_bar(fill="steelblue")
grt #draw graph

###
###4.2.4/time effect on rating, for a specific movie - test with movieId 296
datey <- year(edx_base$dt) #extract year of ratings
test <- edx_base %>% mutate(datey) %>% filter (movieId ==296)

gr4 <- test %>% group_by(datey) %>%
  summarize(avg_rating_by_year = mean(rating)) %>% mutate()

grt <- gr4 %>%
  ggplot(aes(x=datey, y=avg_rating_by_year)) +
  geom_line(size=1.5, color="purple")
grt #draw graph


##4.3/EFFECTS ANALYSIS (one by one)

### create training and test set
test_index <- createDataPartition(edx_base$rating, times=1, p=0.5, list=FALSE)
trainset <- edx_base[-test_index,]
testset <- edx_base[test_index,]
testset <- testset[1:4500027,] #with this, testset has exactly the same number of rows than trainset

str(trainset) #check structure
str(testset)

train1 <- trainset
train2 <- trainset
test1 <- testset
test2 <- testset

mu <- mean(trainset$rating)
train1 <- train1 %>% mutate(mu)     #add 'mu' to train1
train2 <- train2 %>% mutate(mu)     #add 'mu' to train2
trainset <- trainset %>% mutate(mu) #add 'mu' to trainset

###
###4.3.1/MOVIE EFFECT (version 1) (fil)
feffect <- train1 %>% group_by(movieId) %>%
  summarize(fil = mean(rating-mu)) %>% #'fil' is the movie effect, calculated as the average difference between rating and mu
  mutate()
#RMSE
test1 <- test1 %>% mutate(mu) #add a column 'mu' to test1

ftest1 <- test1 %>% 
  left_join(feffect, by='movieId')

ftest1[is.na(ftest1)] <- 0 #replace NA by 0 in the dataframe ftest1

preds <- ftest1$mu + ftest1$fil
f1 <- RMSE(preds, ftest1$rating)

###
###4.3.2/MOVIE EFFECT (version 2) (fil2)
#I apply a regularization linked to the relative weight of each movie in the total number of ratings
#Hypothesis : if the movie has few ratings, the 'movie effect' is less significant

#create a dataframe with number of ratings per movie
table <- train2 %>% group_by(movieId) %>%
  summarize(nbr = n()) %>% mutate()

med <- median(table$nbr) #median
#hypothesis  : if the number of ratings for one movie is less than the median, we apply a 'penalty' of 10% (x 0.9) on movie effect
table <- table %>% mutate(med)
penalty <- ifelse(table$nbr < table$med, 0.9, 1)
table <- table %>% mutate(penalty)

table <- table %>% left_join(feffect, by='movieId') #feffect is the same as in MOVIE EFFECT V1 (train1 = train2)
fil2 <- table$penalty * table$fil
table <- table %>% mutate(fil2)

f2effect <- table %>% select(movieId, fil2) #we have the new movie effect, with penalty for some rows
#RMSE
test2 <- test2 %>% mutate(mu) #add a column 'mu' to test2

ftest2 <- test2 %>% 
  left_join(f2effect, by='movieId')

ftest2[is.na(ftest2)] <- 0 #replace NA by 0 in the dataframe ftest2

preds <- ftest2$mu + ftest2$fil2
f2 <- RMSE(preds, ftest2$rating)

###
###4.3.3/USER EFFECT (usr)
ueffect <- trainset %>% group_by(userId) %>%
  summarize(usr = mean(rating-mu)) %>% #'usr' is the user effect, calculated as the average difference between rating and mu
  mutate()
#RMSE
testset <- testset %>% mutate(mu) #add a column 'mu' to testset

ftestset <- testset %>% 
  left_join(ueffect, by='userId')

ftestset[is.na(ftestset)] <- 0 #replace NA by 0 in the dataframe ftestset

preds <- ftestset$mu + ftestset$usr
u <- RMSE(preds, ftestset$rating)

###
###4.3.4/(MULTI)GENDER EFFECT (gen)
geffect <- trainset %>% group_by(genres) %>%
  summarize(gen = mean(rating-mu)) %>% #'gen' is the gender effect, calculated as the average difference between rating and mu
  mutate()
#RMSE
ftestset <- testset %>% 
  left_join(geffect, by='genres')

ftestset[is.na(ftestset)] <- 0 #replace NA by 0 in the dataframe ftestset

preds <- ftestset$mu + ftestset$gen
g <- RMSE(preds, ftestset$rating)

###
###4.3.5/"(MULTI)GENDER PREFERRED BY USER" EFFECT
gueffect <- trainset %>% group_by(genres, userId) %>%
  summarize(gu = mean(rating-mu)) %>% #'gu' is the gender preferred by user effect
  mutate()
#RMSE
ftestset <- testset %>% 
  left_join(gueffect, by=c('genres', 'userId'))

ftestset[is.na(ftestset)] <- 0 #replace NA by 0 in the dataframe ftestset

preds <- ftestset$mu + ftestset$gu
gu <- RMSE(preds, ftestset$rating)

###
###4.3.6/IMPACT OF TIME ON RATING
fit <- lm(rating ~ timestamp, trainset) #fit a linear regression
yhat <- predict(fit, testset) #yhat is the prediction

t <- RMSE(yhat, testset$rating) #RMSE

###
###4.3.7/TEST : Time effect on rating, for the most rated film, by user
plus <- trainset %>% group_by(movieId) %>% summarize(nb=n()) %>% mutate()
plus <- data.frame(plus)
plus <- plus[order(-plus$nb),]
maxi <- plus[1,] # maxi gives the movieId for the movie which has the most important number of ratings

edx296 <- trainset %>% filter (movieId ==296)

fit <- lm(rating ~ timestamp + userId, trainset) #fit a linear regression model
yhat <- predict(fit, testset)

tf <- RMSE(yhat, testset$rating) #RMSE

###
###RESULTS (vgg)
vg <- c(mu, f1, f2, u, g, gu, t, tf)
nu <- c("(1) mean", "(2) film v1", "(3) film v2", "(4) user", "(5) gender", "(6) gender by user", "(7) time", "(8) time : movieId 296")
nu2 <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)")
vgg <- data.frame(nu,nu2,vg)
vgg

vgg %>% ggplot(aes(x = nu2 , y = vg , fill=nu)) +
geom_col() +
scale_fill_grey() +
theme_classic() +
#add a line to estimate if the considered effect has to be taken into account for calculus
geom_hline(yintercept=0.99, colour="blue", size=1) 


##4.4/MODELS EVALUATION

## comparison between 4 models : 
## model1 : movie effect version 1 + user effect
## model2 : movie effect version 2 + user effect
## model3 : movie effect version 2 + user effect + gender preferred by user effect
## model4 : movie effect version 2 + user effect + gender effect 

###MODEL 1 (movie effect version 1 + user effect)
#movie effect is already calculated in 4.3.1 : feffect / fil
ftrainset <- trainset %>% left_join(feffect, by='movieId')      #movie effect is added

#usr
ueffect <- ftrainset %>% group_by(userId) %>%                   #ratings are grouped by userId
  summarize(usr = mean(rating-mu-fil)) %>%                     
  mutate()
#'usr' is calculated as the average by user of rating-mu-fil
ftrainset <- ftrainset %>% left_join(ueffect, by='userId')      #user effect is added

#RMSE
ftestset <- testset %>% 
  left_join(feffect, by='movieId') %>%
  left_join(ueffect, by='userId')

ftestset[is.na(ftestset)] <- 0                                  #replace NA by 0 in dataframe ftestset

preds <- ftestset$mu + ftestset$fil + ftestset$usr
model1 <- RMSE(preds, ftestset$rating)                          #calculate RSME for model1

###
###MODEL2 (movie effect version 2 + user effect)
#movie effect is already caculated in 4.3.2 : f2effect / fil2
f2trainset <- trainset %>% left_join(f2effect, by='movieId')    #movie effect is added

#usr
ueffect <- f2trainset %>% group_by(userId) %>%                  #ratings are grouped by userId
  summarize(usr = mean(rating-mu-fil2)) %>%                     
  mutate()
f2trainset <- f2trainset %>% left_join(ueffect, by='userId')    #user effect is added

#RMSE
ftestset <- testset %>% 
  left_join(f2effect, by='movieId') %>%
  left_join(ueffect, by='userId')

ftestset[is.na(ftestset)] <- 0                                  #replace NA by 0 in dataframe ftestset

preds <- ftestset$mu + ftestset$fil2 + ftestset$usr
model2 <- RMSE(preds, ftestset$rating)                          #calculate RSME for model2

###
###MODEL3 (movie effect version 2 + user effect + gender preferred by user effect)
#movie effect is already caculated in 4.3.2 : f2effect / fil2
f3trainset <- trainset %>% left_join(f2effect, by='movieId')    #movie effect is added

#usr
ueffect <- f3trainset %>% group_by(userId) %>%                  #ratings are grouped by userId
  summarize(usr = mean(rating-mu-fil2)) %>%                     
  mutate()
f3trainset <- f3trainset %>% left_join(ueffect, by='userId')    #user effect is added

#gu (gender preferred by user)
gueffect <- f3trainset %>% group_by(genres, userId) %>%         #ratings are grouped by genres and by userId
  summarize(gu = mean(rating-mu-fil2-usr)/10) %>%               
  mutate()     
#as user is already taken in account (ueffect), we consider gu only for 10% of its value 
f3trainset <- f3trainset %>% left_join(gueffect, by=c('genres', 'userId')) #gu effect is added 

#RMSE
ftestset <- testset %>% 
  left_join(f2effect, by='movieId') %>%
  left_join(ueffect, by='userId') %>%
  left_join(gueffect, by=c('genres', 'userId'))

ftestset[is.na(ftestset)] <- 0                                             #replace NA by 0 in dataframe ftestset

preds <- ftestset$mu + ftestset$fil2 + ftestset$usr + ftestset$gu
model3 <- RMSE(preds, ftestset$rating)                                     #calculate RSME for model3

###
###MODEL4 (movie effect version 2 + user effect + gender effect)
#movie effect is already caculated in 4.3.2 : f2effect / fil2
f4trainset <- trainset %>% left_join(f2effect, by='movieId')    #movie effect is added

#usr
ueffect <- f4trainset %>% group_by(userId) %>%                  #ratings are grouped by userId
  summarize(usr = mean(rating-mu-fil2)) %>%                    
  mutate()
f4trainset <- f4trainset %>% left_join(ueffect, by='userId')    #user effect is added

#g (gender)
geffect <- f4trainset %>% group_by(genres) %>%                  #ratings are grouped by genres
  summarize(g = mean(rating-mu-fil2-usr)) %>%  
  mutate()
f4trainset <- f4trainset %>% left_join(geffect, by='genres') #g effect is added 

#RMSE
ftestset <- testset %>% 
  left_join(f2effect, by='movieId') %>%
  left_join(ueffect, by='userId') %>%
  left_join(geffect, by='genres')

ftestset[is.na(ftestset)] <- 0                                             #replace NA by 0 in dataframe ftestset

preds <- ftestset$mu + ftestset$fil2 + ftestset$usr + ftestset$g
model4 <- RMSE(preds, ftestset$rating)                                     #calculate RSME for model4

model1
model2
model3
model4


# 5/RESULT SECTION ----------------------------------------------------
##MODEL3 gives the best performance (predictors : movie v2 + user + gender preferred by user)
##the model has to be calculated again, with the whole training dataset : edx

mu <- mean(edx$rating)

#calculate the movie effect 
feffect <- edx %>% group_by(movieId) %>% summarize(fil = mean(rating-mu)) %>% mutate()

#create a dataframe with number of ratings per movie
table <- edx %>% group_by(movieId) %>%
  summarize(nbr = n()) %>% mutate()

med <- median(table$nbr) #median

#if the number of ratings for one movie is less than the median, we apply a 'penalty' of 10% (x 0.9) on movie effect
table <- table %>% mutate(med)
penalty <- ifelse(table$nbr < table$med, 0.9, 1)
table <- table %>% mutate(penalty)

table <- table %>% left_join(feffect, by='movieId') #fil is added to calculate fil2
fil2 <- table$penalty * table$fil
table <- table %>% mutate(fil2)

f2effect <- table %>% select(movieId, fil2)
edxfinal <- edx %>% left_join(f2effect, by='movieId')                        #movie effect is added

#usr
ueffect <- edxfinal %>% group_by(userId) %>% 
  summarize(usr = mean(rating-mu-fil2)) %>% mutate()
edxfinal <- edxfinal %>% left_join(ueffect, by='userId')                     #user effect is added 

#gu (gender preferred by user)
gueffect <- edxfinal %>% group_by(genres, userId) %>%     
  summarize(gu = mean(rating-mu-fil2-usr)/10) %>% mutate()
#as the user effect is already taken in account in the model, we consider gu only for 10% of its value 
edxfinal <- edxfinal %>% left_join(gueffect, by=c('genres', 'userId'))       #gender by user effect is added

# rating average and the tree effects (movie effect v2, user effect, gender preferred by user effect) calculated with the training set (edx)
# are linked with relevant rows of the testset (validate)

# Because some couples (multi)gender items/user are not present in the test set, the effect gu (gender preferred by user) is sometimes NA.
# We replace NA by 0

load('validation.Rda')

val <- validation %>% 
  left_join(f2effect, by='movieId') %>%
  left_join(ueffect, by='userId') %>%
  left_join(gueffect, by=c('genres', 'userId'))

val[is.na(val)] <- 0 

res_final <- mu + val$fil2 + val$usr + val$gu
RMSE(res_final, validation$rating)                    #calculate RMSE on test set







