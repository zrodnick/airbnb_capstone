require(dplyr)
require(ggplot2)
require(tidyr)
require(reshape2)
require(gtools)
require(mice)
require(dummies)
set.seed(12345)
#load data sets

sessions_df <- read.csv("sessions.csv", header=TRUE)
train_df_start <- read.csv("train_users_2.csv", header=TRUE)
summarystats_df <- read.csv("age_gender_bkts.csv")
countries_df <- read.csv("countries.csv")
test_df <- read.csv("test_users.csv")

##Training set wrangling

train_df1 <- train_df_start
colnames(train_df1)[1] <- "user_id"


#Fix date objects
train_df1$date_account_created <- as.Date(train_df1$date_account_created)
train_df1$date_first_booking <- NULL
train_df1$timestamp_first_active[train_df1$timestamp_first_active == ""] <- NA
train_df1$timestamp_first_active <- as.POSIXct(as.character(train_df1$timestamp_first_active), format="%Y%m%d%H%M%S")
train_df1$timestamp_first_active <- as.Date(train_df1$timestamp_first_active, format="%Y-%m-%d")
train_df1$timestamp_first_active[is.na(train_df1$timestamp_first_active)==TRUE] <- train_df1$date_account_created


#Clean up age
train_df1$age[train_df1$age > 98] <- NA
train_df1$age[train_df1$age < 15] <- NA
train_df1$age[is.na(train_df1$age)==TRUE] <- -1

train_df1$first_affiliate_tracked[train_df1$first_affiliate_tracked == ""] <- NA

train_df1$country_destination <- as.factor(train_df1$country_destination)


#Optional NA replacement

train_df2 <- train_df1
train_dfclean <- na.omit(train_df2)

age <- train_dfclean[, c("user_id", "age")]
train_dfclean$age <- NULL

#Feature Engineering

train_fe <- train_dfclean

train_fe <- dummy.data.frame(train_fe, names=c("gender", "signup_method", "language", "affiliate_channel", "affiliate_provider", "first_affiliate_tracked", "signup_app", "first_device_type", "first_browser"), omit.constants = TRUE, sep="_", fun=as.numeric, dummy.classes = "numeric")

train_fe <- separate(train_fe, date_account_created, into=c("year_account_created", "month_account_created", "day_account_created"), sep="-")

train_fe <- separate(train_fe, timestamp_first_active, into=c("year_first_active", "month_first_active", "day_first_active"), sep="-")

train_fe <- transform(train_fe, year_account_created = as.numeric(year_account_created), month_account_created=as.numeric(month_account_created), day_account_created=as.numeric(day_account_created), year_first_active=as.numeric(year_first_active), month_first_active=as.numeric(month_first_active), day_first_active=as.numeric(day_first_active))

#removing outcome variable, save for later

train_join <- train_fe


#sessions set wrangling

sessions_df <- group_by(sessions_df, user_id)
sessions_summary <- sessions_df %>% group_by(user_id) %>% summarise(sec_elapsed_avg = mean(secs_elapsed, na.rm=TRUE), sec_elapsed_total = sum(secs_elapsed, na.rm=TRUE))  

sessions_summary_pl <- sessions_df %>% group_by(user_id) %>% count(user_id)

sessions_sum2 <- select(sessions_df, user_id, device_type) %>% distinct(device_type)

colnames(sessions_summary_pl)[2] <- "total_actions"


sessions_summary <- merge(sessions_summary, sessions_summary_pl, by="user_id")
sessions_summary <- filter(sessions_summary, user_id != "")
sessions_summary[is.na(sessions_summary)==TRUE] <- 0


sessions_actions <- select(sessions_df, user_id, action)
sessions_actions2 <- melt(sessions_actions, id="user_id", na.rm=TRUE)
sessions_actions2 <- dcast(sessions_actions2, user_id ~ value + variable, length)
sessions_actions2 <- filter(sessions_actions2, user_id != "")
names(sessions_actions2)[2] <- "blank_action"

sessions_device <- select(sessions_df, user_id, device_type)
sessions_device2 <- sessions_device %>% group_by(user_id) %>% summarize(device_type = names(which.max(table(device_type))))
sessions_device2 <- filter(sessions_device2, user_id != "")
sessions_device3 <- melt(sessions_device2, id="user_id", na.rm=TRUE)
sessions_device3 <- dcast(sessions_device3, user_id ~ value + variable, length)

sessions_summary <- left_join(sessions_summary, sessions_device3, by="user_id")
sessions_final <- left_join(sessions_summary, sessions_actions2, by="user_id")

#Join sessions and train sets

train_full <- inner_join(train_join, sessions_final, by="user_id")

outcome.org <- train_full[, "country_destination"]
outcome <- outcome.org
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
outcome <- as.numeric(outcome)
outcome <- outcome - 1

train_boost <- train_full
train_boost$country_destination <- NULL

#other junk

actions <- as.data.frame(levels(sessions_df$action))
colnames(actions) <- "actions"


#Graphs!
ggplot(train_df1, aes(x=timestamp_first_active, fill=first_affiliate_tracked)) + geom_histogram(binwidth = 250)
ggplot(train_full2, aes(x=age))+ geom_histogram()


#Things still left to do:
#Remove ID column
#Find out what "labels" are. 
#Wrangle test set

test_df1 <- test_df
colnames(test_df1)[1] <- "user_id"


#Fix date objects
test_df1$date_account_created <- as.Date(test_df1$date_account_created)
test_df1$date_first_booking <- NULL
test_df1$timestamp_first_active[test_df1$timestamp_first_active == ""] <- NA
test_df1$timestamp_first_active <- as.POSIXct(as.character(test_df1$timestamp_first_active), format="%Y%m%d%H%M%S")
test_df1$timestamp_first_active <- as.Date(test_df1$timestamp_first_active, format="%Y-%m-%d")
test_df1$timestamp_first_active[is.na(test_df1$timestamp_first_active)==TRUE] <- test_df1$date_account_created


#Clean up age
test_df1$age[test_df1$age > 98] <- NA
test_df1$age[test_df1$age < 15] <- NA
test_df1$age[is.na(test_df1$age)==TRUE] <- -1

test_df1$first_affiliate_tracked[test_df1$first_affiliate_tracked == ""] <- NA




#Optional NA replacement

test_df2 <- test_df1
test_dfclean <- na.omit(test_df2)

test_age <- test_dfclean[, c("user_id", "age")]
test_dfclean$age <- NULL

#Feature Engineering

test_fe <- test_dfclean

test_fe <- dummy.data.frame(test_fe, names=c("gender", "signup_method", "language", "affiliate_channel", "affiliate_provider", "first_affiliate_tracked", "signup_app", "first_device_type", "first_browser"), omit.constants = TRUE, sep="_", fun=as.numeric, dummy.classes = "numeric")

test_fe <- separate(test_fe, date_account_created, into=c("year_account_created", "month_account_created", "day_account_created"), sep="-")

test_fe <- separate(test_fe, timestamp_first_active, into=c("year_first_active", "month_first_active", "day_first_active"), sep="-")

test_fe <- transform(test_fe, year_account_created = as.numeric(year_account_created), month_account_created=as.numeric(month_account_created), day_account_created=as.numeric(day_account_created), year_first_active=as.numeric(year_first_active), month_first_active=as.numeric(month_first_active), day_first_active=as.numeric(day_first_active))



test_join <- test_fe

test_full <- inner_join(test_join, sessions_final, by="user_id")

test_boost <- test_full
