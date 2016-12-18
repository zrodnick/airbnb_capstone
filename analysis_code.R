logreg1 <- glm(booked~gender, simple_1, family="binomial")

library(nnet)
logreg2 <- multinom(country_destination~gender, data=simple_1)

logreg3 <- 

