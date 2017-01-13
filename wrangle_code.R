require(dplyr)
require(ggplot2)
require(tidyr)
require(reshape2)
require(gtools)
require(mice)
require(dummies)
require(e1071)
require(mice)
set.seed(12345)
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

train_age <- train_dfclean[, c("user_id", "age")]
#train_dfclean$age <- NULL

#Feature Engineering

train_fe <- train_dfclean

train_fe <- dummy.data.frame(train_fe, names=c("gender", "signup_method", "language", "affiliate_channel", "affiliate_provider", "first_affiliate_tracked", "signup_app", "first_device_type", "first_browser"), omit.constants = TRUE, sep="_", fun=as.numeric, dummy.classes = "numeric")

train_fe <- separate(train_fe, date_account_created, into=c("year_account_created", "month_account_created", "day_account_created"), sep="-")

train_fe <- separate(train_fe, timestamp_first_active, into=c("year_first_active", "month_first_active", "day_first_active"), sep="-")

train_fe <- transform(train_fe, year_account_created = as.numeric(year_account_created), month_account_created=as.numeric(month_account_created), day_account_created=as.numeric(day_account_created), year_first_active=as.numeric(year_first_active), month_first_active=as.numeric(month_first_active), day_first_active=as.numeric(day_first_active))

train_fe$destination_booked <- train_fe$country_destination != "NDF"

#Messing with age
train_mice <- train_fe[,-1]
train_mice$age[train_mice$age == -1] <- NA

mice_complete <- complete(mice(data=train_mice))

train_age$age_imputed <- mice_complete$age

#removing outcome variable, save for later

train_join <- train_fe


#sessions set wrangling

sessions_df <- group_by(sessions_df, user_id)

sessions_summary <- sessions_df %>% group_by(user_id) %>% summarise(secs_elapsed_avg = mean(secs_elapsed, na.rm=TRUE), secs_elapsed_total = sum(secs_elapsed, na.rm=TRUE), secs_elapsed_sd = sd(secs_elapsed, na.rm=TRUE), secs_elapsed_min= min(secs_elapsed, na.rm=TRUE), secs_elapsed_max = max(secs_elapsed, na.rm=TRUE), secs_elapsed_median = median(secs_elapsed, na.rm=TRUE), secs_elapsed_IQR = IQR(secs_elapsed, na.rm=TRUE))


#Insert skewness and kurtosis

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

train_large <- left_join(train_join, sessions_final, by="user_id")

train_boost1 <- train_full
outcome1.org <- train_boost1[, "destination_booked"]
outcome1 <- outcome1.org
num.class1 = length(levels(outcome1))
levels(outcome1) = 1:num.class1
outcome1 <- as.numeric(outcome1)

outcomeh.org <- train_boost1[, "country_destination"]
outcomeh <- outcomeh.org
num.classh = length(levels(outcomeh))
levels(outcomeh) = 1:num.classh
outcomeh <- as.numeric(outcomeh)
outcomeh <- outcomeh - 1
outcomeh_labels <- bind_cols(as.data.frame(outcomeh), as.data.frame(outcomeh.org))
outcomeh_labels$outcome <- as.factor(outcomeh_labels$outcomeh)
outcomeh_labels <- levels(outcomeh_labels$outcomeh.org)

train_boost1$country_destination <- NULL
train_boost1$destination_booked <- NULL

train_boost1.1 <- train_boost1
train_boost1.1$age[train_boost1.1$age == -1] <- train_age$age_imputed


train_boost2 <- train_full
train_boost2$country_destination[train_boost2$country_destination == "NDF"] <- NA 
train_boost2 <- na.omit(train_boost2)
outcome2.org <- train_boost2[, "country_destination"]
outcome2 <- outcome2.org
num.class2 = length(levels(outcome2))
levels(outcome2) = 1:num.class2
outcome2 <- as.numeric(outcome2)
outcome2 <- outcome2 - 1
outcome2_labels <- bind_cols(as.data.frame(outcome2), as.data.frame(outcome2.org))
outcome2_labels$outcome <- as.factor(outcome2_labels$outcome2)
outcome2_labels <- levels(outcome2_labels$outcome2.org)
train_boost2$country_destination <- NULL
train_boost2$destination_booked <- NULL

train_boost_large <- train_large
outcomel.org <- train_boost_large[, "country_destination"]
outcomel <- outcomel.org
num.classl = length(levels(outcomel))
levels(outcomel) = 1:num.classl
outcomel <- as.numeric(outcomel)
outcomel <- outcomel - 1
outcomel_labels <- bind_cols(as.data.frame(outcomel), as.data.frame(outcomel.org))
outcomel_labels$outcome <- as.factor(outcomel_labels$outcomel)
outcomel_labels <- levels(outcomel_labels$outcomel.org)
outcomelb.org <- train_boost_large[, "destination_booked"]
outcomelb <- outcomelb.org
num.classlb = length(levels(outcomelb))
levels(outcomelb) = 1:num.classlb
outcomelb <- as.numeric(outcomelb)
train_boost_large$country_destination <- NULL
train_boost_large$destination_booked <- NULL

#other junk

actions <- as.data.frame(levels(sessions_df$action))
colnames(actions) <- "actions"


#Graphs!
ggplot(train_df1, aes(x=timestamp_first_active, fill=first_affiliate_tracked)) + geom_histogram(binwidth = 250)
ggplot(train_full2, aes(x=age))+ geom_histogram()

