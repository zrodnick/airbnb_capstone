library(dplyr)
library(ggplot2)
library(tidyr)

sessions_df <- read.csv("sessions.csv", header=TRUE, na.strings = c("", " ", "NA"))
train_df_start <- read.csv("train_users_2.csv", header=TRUE, na.strings = c("", " ", "NA", "-unknown-", "NDF"))
summarystats_df <- read.csv("age_gender_bkts.csv")
countries_df <- read.csv("countries.csv")

train_df1 <- left_join(train_df_start, countries_df, by="country_destination")
train_df1$date_account_created <- as.Date(train_df1$date_account_created)
train_df1$date_first_booking<- as.Date(train_df1$date_first_booking)
