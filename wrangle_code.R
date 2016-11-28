library(dplyr)
library(ggplot2)
library(tidyr)

sessions_df <- read.csv("sessions.csv")
train_df_start <- read.csv("train_users_2.csv")
summarystats_df <- read.csv("age_gender_bkts.csv")
countries_df <- read.csv("countries.csv")

