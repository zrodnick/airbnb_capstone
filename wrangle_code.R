require(dplyr)
require(ggplot2)
require(tidyr)
require(reshape2)
require(gtools)
require(mice)
require(dummies)
require(e1071)
require(mice)
require(scales)
set.seed(12345)
#load data sets

sessions_df <- read.csv("sessions.csv", header=TRUE)
train_df_start <- read.csv("train_users_2.csv", header=TRUE)
summarystats_df <- read.csv("age_gender_bkts.csv")
countries_df <- read.csv("countries.csv")


## Sessions Wrangling

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
train_df2[is.na(train_df2)] <- -1
train_dfclean <- train_df2
  
train_age <- train_dfclean[, c("user_id", "age")]
#train_dfclean$age <- NULL

#Feature Engineering

train_fe <- train_dfclean

train_fe <- dummy.data.frame(train_fe, names=c("gender", "signup_method", "signup_flow", "language", "affiliate_channel", "affiliate_provider", "first_affiliate_tracked", "signup_app", "first_device_type", "first_browser"), omit.constants = TRUE, sep="_", fun=as.numeric, dummy.classes = "numeric")

train_fe <- separate(train_fe, date_account_created, into=c("year_account_created", "month_account_created", "day_account_created"), sep="-")

train_fe <- separate(train_fe, timestamp_first_active, into=c("year_first_active", "month_first_active", "day_first_active"), sep="-")

train_fe <- transform(train_fe, year_account_created = as.numeric(year_account_created), month_account_created=as.numeric(month_account_created), day_account_created=as.numeric(day_account_created), year_first_active=as.numeric(year_first_active), month_first_active=as.numeric(month_first_active), day_first_active=as.numeric(day_first_active))

train_fe$destination_booked <- train_fe$country_destination != "NDF"

train_join <- train_fe

train_join[is.na(train_join)] <- -1

#Join sessions and train sets

train_full <- inner_join(train_join, sessions_final, by="user_id")

outcome_labels <- bind_cols(as.data.frame(outcome), as.data.frame(outcome.org))
outcome_labels$outcome <- as.factor(outcome_labels$outcome)
outcome_labels <- levels(outcome_labels$outcome.org)


train_boost1 <- train_full
outcome1.org <- train_boost1[, "destination_booked"]
outcome1 <- outcome1.org
num.class1 = length(levels(outcome1))
levels(outcome1) = 1:num.class1
outcome1 <- as.numeric(outcome1)

outcome.org <- train_boost1[, "country_destination"]
outcome <- outcome.org
num.class = length(levels(outcome))
levels(outcome) = 1:num.class
outcome <- as.numeric(outcome)
outcome <- outcome - 1
outcome_labels <- bind_cols(as.data.frame(outcome), as.data.frame(outcome.org))
outcome_labels$outcome <- as.factor(outcome_labels$outcome)
outcome_labels <- levels(outcome_labels$outcome.org)

train_boost1$country_destination <- NULL
train_boost1$destination_booked <- NULL



train_boost2 <- train_join
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




#other junk

actions <- as.data.frame(levels(sessions_df$action))
colnames(actions) <- "actions"


#Graphs!
ggplot(train_df1, aes(x=timestamp_first_active, fill=first_affiliate_tracked)) + geom_histogram(binwidth = 250)
ggplot(train_df1, aes(x=age))+ geom_histogram()

destination_summary <- as.data.frame(summary(train_df1$country_destination))
colnames(destination_summary) <- "Count"
destination_summary$Percentages <- percent(prop.table(destination_summary$Count))
destination_summary$Destination <- rownames(destination_summary)
destination_summary <- destination_summary[,c(3,1:2)]
destination_summary <- arrange(destination_summary, desc(Count))
write.csv(destination_summary, "destination_summary.csv", row.names=TRUE)

ggplot(train_df1, aes(x=date_account_created)) + geom_histogram(color="black", fill="blue", bins=42) + ggtitle("Accounts Created Over Time") + xlab("Date Account Created") + ylab("Frequency")

agebyyear <- as.data.frame(train_df_start$age)
colnames(agebyyear) <- "Age"
agebyyear$Year <- as.numeric(train_fe$year_account_created)
ggplot(agebyyear, aes(x=Age)) + geom_histogram(breaks=c(min(agebyyear$Age), seq(10,100,5), max(agebyyear$Age)), fill="red", col="black") 

ggplot(agebyyear, aes(x=Year, y=Age)) + geom_jitter(col="blue")
summary(agebyyear$Age)
summary(ageoutliers)

firstdevice <- as.data.frame(train_fe$year_account_created)
colnames(firstdevice) <- "Year"
firstdevice$First_Device <- train_df_start$first_device_type



firstdevice2010 <- filter(firstdevice, Year == 2010)
colnames(firstdevice2010) <- c("Year", "2010")
firstdevice2010table <- as.data.frame(table(firstdevice2010$"2010"))
firstdevice2010table$Freq <- prop.table(firstdevice2010table$Freq)
firstdevice2011 <- filter(firstdevice, Year == 2011)
colnames(firstdevice2011) <- c("Year", "2011")
firstdevice2011table <- as.data.frame(table(firstdevice2011$"2011"))
firstdevice2011table$Freq <- prop.table(firstdevice2011table$Freq)
firstdevice2012 <- filter(firstdevice, Year == 2012)
colnames(firstdevice2012) <- c("Year", "2012")
firstdevice2012table <- as.data.frame(table(firstdevice2012$"2012"))
firstdevice2012table$Freq <- prop.table(firstdevice2012table$Freq)
firstdevice2013 <- filter(firstdevice, Year == 2013)
colnames(firstdevice2013) <- c("Year", "2013")
firstdevice2013table <- as.data.frame(table(firstdevice2013$"2013"))
firstdevice2013table$Freq <- prop.table(firstdevice2013table$Freq)
firstdevice2014 <- filter(firstdevice, Year == 2014)
colnames(firstdevice2014) <- c("Year", "2014")
firstdevice2014table <- as.data.frame(table(firstdevice2014$"2014"))
firstdevice2014table$Freq <- prop.table(firstdevice2014table$Freq)

firstdevicefull <- full_join(firstdevice2010table, firstdevice2011table, by="Var1")
firstdevicefull <- full_join(firstdevicefull, firstdevice2012table, by="Var1")
firstdevicefull <- full_join(firstdevicefull, firstdevice2013table, by="Var1")
firstdevicefull <- full_join(firstdevicefull, firstdevice2014table, by="Var1")
colnames(firstdevicefull) <- c("Device", "2010", "2011", "2012", "2013", "2014")
firstdevicefull[,2:6] <- round(firstdevicefull[,2:6], 4)
write.csv(firstdevicefull, "firstdevicefull.csv")
