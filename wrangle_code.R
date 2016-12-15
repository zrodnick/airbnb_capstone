library(dplyr)
library(ggplot2)
library(tidyr)

sessions_df <- read.csv("sessions.csv", header=TRUE)
train_df_start <- read.csv("train_users_2.csv", header=TRUE)
summarystats_df <- read.csv("age_gender_bkts.csv")
countries_df <- read.csv("countries.csv")

train_df1 <- left_join(train_df_start, countries_df, by="country_destination")
train_df1$date_account_created <- as.Date(train_df1$date_account_created)
train_df1$date_first_booking<- as.Date(train_df1$date_first_booking)
train_df1$country_destination <- as.factor(train_df1$country_destination)
train_df1$booked <- train_df1$country_destination != "NDF"
train_df1 <- train_df1[, c(1:15,23,16:22)]

sessions_df <- group_by(sessions_df, user_id)

actions <- as.data.frame(levels(sessions_df$action))
colnames(actions) <- "actions"



simple_1 <- select(train_df1,gender,booked,country_destination)


