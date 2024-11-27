library(readr)
OA_11_7_yelp_academic_dataset_business_json <- read_csv("DSC/OA 11.7 - yelp_academic_dataset_business.json.csv")
View(OA_11_7_yelp_academic_dataset_business_json)

##User Data
library(readr)
user_data <- read_csv("DSC/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(user_data)

## Import ggplot
library(ggplot2)

## Print the names of the columns for the data
names(user_data)

## Find the r correlation for the data
## Correlation between cool votes and funny votes 
corr_score1 <- cor(user_data$cool_votes,user_data$funny_votes)
print(corr_score1)
## Correlation between cool votes and useful votes
corr_score2 <- cor(user_data$cool_votes,user_data$useful_votes)
print(corr_score2)
## Correlation between funny votes and useful votes
corr_score3 <- cor(user_data$funny_votes,user_data$useful_votes)
print(corr_score3)

## Linear regression of cool votes and useful votes
## plot the data
## create a linear model
linear_model <- lm(user_data$funny_votes~user_data$useful_votes)
print(linear_model)
## find the slope and intercepts using the coefficients 
coefs<- coef(linear_model)
lm_intercept<-coefs[1]
lm_slope<- coefs[2]
cat("Slope:",lm_slope,"Intercept:",lm_intercept)
## Plot the linear regression model and data
ggplot(user_data)+geom_point(aes(x=cool_votes,y=useful_votes))+
  geom_smooth(aes(x=cool_votes,y=useful_votes),method="lm",se=F)+
  labs(x="Cool Votes",y="Useful Votes")

## Writing Reviews
## make a linear model of the writing reviews to fans
linear_model <- lm(user_data$review_count~user_data$fans)
print(linear_model)
## find the slope and intercepts of the linear model
coefs<- coef(linear_model)
lm_intercept<-coefs[1]
lm_slope<- coefs[2]
cat("Slope:",lm_slope,"Intercept:",lm_intercept)
## graph the linear model and data 
ggplot(user_data)+geom_point(aes(x=review_count,y=fans))+
  geom_smooth(aes(x=review_count,y=fans),method="lm",se=F)+
  labs(x="Review Count",y="Fans")

## Use useful_votes for a better correlation
linear_model <- lm(user_data$useful_votes~user_data$fans)
print(linear_model)

## use k means for review count and fans
ggplot(user_data)+geom_point(aes(x=review_count, y=fans))
## make a table of the k means values
user_data_cluster <- kmeans(user_data[,3:4],3)
table(user_data_cluster$review_count,user_data$fans)

## use k means useful_votes and fans
ggplot(user_data)+geom_point(aes(x=review_count, y=fans))
## make a table of the k means values
user_data_cluster1 <- kmeans(user_data[,3:4],3)
table(user_data_cluster1$review_count,user_data$fans)



