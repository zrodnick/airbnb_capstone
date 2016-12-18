require(dplyr)
require(ggplot2)
require(tidyr)
require(reshape2)

#load data sets

sessions_df <- read.csv("sessions.csv", header=TRUE)
train_df_start <- read.csv("train_users_2.csv", header=TRUE)
summarystats_df <- read.csv("age_gender_bkts.csv")
countries_df <- read.csv("countries.csv")

##Training set wrangling

train_df1 <- left_join(train_df_start, countries_df, by="country_destination")

#Fix date objects
train_df1$date_account_created <- as.Date(train_df1$date_account_created)
train_df1$date_first_booking <- NULL

train_df1$timestamp_first_active <- as.Date(train_df1$timestamp_first_active, format="%Y %m %d %h %m %s", origin="1970-01-01")



train_df1$country_destination <- as.factor(train_df1$country_destination)
train_df1$booked <- train_df1$country_destination != "NDF"
train_df1 <- train_df1[, c(1:15,23,16:22)]
train_df1$age <- NULL
colnames(train_df1)[1] <- "user_id"

#sessions set wrangling

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



#other junk

actions <- as.data.frame(levels(sessions_df$action))
colnames(actions) <- "actions"

simple_1 <- select(train_df1,gender,booked,country_destination)


train_df2 <- merge(train_df1, sessions_summary, by="user_id", all=TRUE)

