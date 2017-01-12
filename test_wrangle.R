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


test_df <- read.csv("test_users.csv")

test_df1 <- test_df
colnames(test_df1)[1] <- "user_id"

levels(test_df1$first_browser) <- c(levels(test_df1$first_browser), "-other-")
test_df1$first_browser[(test_df1$first_browser == "Arora") | (test_df1$first_browser =="Avant Browser") | (test_df1$first_browser =="Camino") | (test_df1$first_browser =="CometBird") | (test_df1$first_browser =="Comodo Dragon") | (test_df1$first_browser =="Conkeror") | (test_df1$first_browser =="CoolNovo") | (test_df1$first_browser =="Crazy Browser") | (test_df1$first_browser =="Epic") | (test_df1$first_browser =="Flock") | (test_df1$first_browser =="Google Earth") | (test_df1$first_browser =="Googlebot") | (test_df1$first_browser =="Ice Dragon") | (test_df1$first_browser =="IceWeasel") | (test_df1$first_browser =="Iron") | (test_df1$first_browser =="Kindle Browser") | (test_df1$first_browser =="Mozilla") | (test_df1$first_browser =="NetNewsWire") | (test_df1$first_browser =="OmniWeb") | (test_df1$first_browser =="Opera Mini") | (test_df1$first_browser =="Opera Mobile") | (test_df1$first_browser =="Outlook 2007") | (test_df1$first_browser =="Pale Moon") | (test_df1$first_browser =="Palm Pre web browser") | (test_df1$first_browser =="PS Vita browser") | (test_df1$first_browser =="SeaMonkey") | (test_df1$first_browser =="SlimBrowser") | (test_df1$first_browser =="Stainless") | (test_df1$first_browser =="TenFourFox") | (test_df1$first_browser =="TheWorld Browser") | (test_df1$first_browser =="wOSBrowser") | (test_df1$first_browser =="Yandex.Browser")] <- "-other-"

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
#test_dfclean$age <- NULL

#Feature Engineering

test_fe <- test_dfclean

test_fe <- dummy.data.frame(test_fe, names=c("gender", "signup_method", "language", "affiliate_channel", "affiliate_provider", "first_affiliate_tracked", "signup_app", "first_device_type", "first_browser"), omit.constants = TRUE, sep="_", fun=as.numeric, dummy.classes = "numeric")

test_fe <- separate(test_fe, date_account_created, into=c("year_account_created", "month_account_created", "day_account_created"), sep="-")

test_fe <- separate(test_fe, timestamp_first_active, into=c("year_first_active", "month_first_active", "day_first_active"), sep="-")

test_fe <- transform(test_fe, year_account_created = as.numeric(year_account_created), month_account_created=as.numeric(month_account_created), day_account_created=as.numeric(day_account_created), year_first_active=as.numeric(year_first_active), month_first_active=as.numeric(month_first_active), day_first_active=as.numeric(day_first_active))

#removing outcome variable, save for later

test_join <- test_fe

#Join sessions and test sets

test_full <- inner_join(test_join, sessions_final, by="user_id")

test_boost1 <- test_full
test_boost1$country_destination <- NULL
test_boost1$destination_booked <- NULL




