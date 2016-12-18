require(dplyr)
require(ggplot2)
require(tidyr)
require(reshape2)
require(gtools)
require(mice)

#load data sets

sessions_df <- read.csv("sessions.csv", header=TRUE)
train_df_start <- read.csv("train_users_2.csv", header=TRUE)
summarystats_df <- read.csv("age_gender_bkts.csv")
countries_df <- read.csv("countries.csv")

##Training set wrangling

train_df1 <- train_df_start
colnames(train_df1)[1] <- "user_id"


#Fix date objects
train_df1$date_account_created <- as.Date(train_df1$date_account_created)
train_df1$date_first_booking <- NULL
train_df1$timestamp_first_active[train_df1$timestamp_first_active == ""] <- NA
train_df1$timestamp_first_active <- as.POSIXct(as.character(train_df1$timestamp_first_active), format="%Y%m%d%H%M%S")
train_df1$timestamp_first_active <- as.Date(train_df1$timestamp_first_active, format="%Y-%m-%d")
train_df1$timestamp_first_active[is.na(train_df1$timestamp_first_active)] <- train_df1$date_account_created


#Clean up age
train_df1$age[train_df1$age > 98] <- NA
train_df1$age[train_df1$age < 15] <- NA

# train_df1$age[is.na(train_df1$age)] <- (-1) Why?

train_df1$first_affiliate_tracked[train_df1$first_affiliate_tracked == ""] <- NA

train_df1$country_destination <- as.factor(train_df1$country_destination)


#Optional NA replacement

train_df2 <- train_df1
train_df2$age <- NULL
train_dfclean <- na.omit(train_df2)

train_fe <- train_dfclean



#sessions set wrangling
require(dummies)
sessions_df <- group_by(sessions_df, user_id)
sessions_summary <- sessions_df %>% group_by(user_id) %>% summarise(sec_elapsed_avg = mean(secs_elapsed, na.rm=TRUE), sec_elapsed_total = sum(secs_elapsed, na.rm=TRUE))  

sessions_summary_pl <- sessions_df %>% group_by(user_id) %>% count(user_id)

sessions_sum2 <- select(sessions_df, user_id, device_type) %>% distinct(device_type)

colnames(sessions_summary_pl)[2] <- "total_actions"


sessions_summary <- merge(sessions_summary, sessions_summary_pl, by="user_id")
sessions_summary <- filter(sessions_summary, user_id != "")


sessions_actions <- select(sessions_df, user_id, action)

sessions_actions2 <- dcast(sessions_actions, user_id ~ action, length)
sessions_actions2 <- filter(sessions_actions2, user_id != "")

sessions_device <- select(sessions_df, user_id, device_type)

sessions_device2 <- sessions_device %>% group_by(user_id) %>% summarize(device_type = names(which.max(table(device_type))))
sessions_device2 <- filter(sessions_device2, user_id != "")


sessions_summary <- left_join(sessions_summary, sessions_actions2)
names(sessions_summary)[names(sessions_summary) == "Var.2"] <- "blank"

#other junk

actions <- as.data.frame(levels(sessions_df$action))
colnames(actions) <- "actions"

simple_1 <- select(train_df1,gender,booked,country_destination)


train_df2 <- merge(train_df1, sessions_summary, by="user_id")

#Graphs!
ggplot(train_dfclean, aes(x=timestamp_first_active, fill=first_affiliate_tracked)) + geom_histogram(binwidth = 250)

