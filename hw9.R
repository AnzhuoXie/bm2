library(tidyverse)
library(survival)
library(MASS)
library(KMsurv)

# Problem 2
time <- c(1, 2, 2, 4, 5, 6, 7, 8, 9, 10)
event <- c(1, 1, 1, 0, 0, 1, 0, 0, 0, 0)
survObj <- Surv(time, event==1)
km <- survfit(survObj~1, type="kaplan-meier")
summary(km)
plot(km)
# K-M, N-A, F-H
cbind(km$time,km$surv,cumsum(km$n.event/km$n.risk),
exp(-cumsum(km$n.event/km$n.risk)))

summary(km,time = c(5))







# Problem 3
data("tongue")
aneuploid = subset(tongue, type == 1)
diploid = subset(tongue, type == 2)
aneuploid$survObj = with(aneuploid, Surv(time, delta == 1))
diploid$survObj = with(diploid , Surv(time, delta == 1))
km_aneuploid = survfit(survObj ~ type, 
                       conf.type="log",
                       type = "kaplan-meier", 
                       data = aneuploid )
plot(km_aneuploid, 
     mark.time = T,
     main = "Aneuploid's Kaplan-Meier curve",
     ylab = "Probability for Survival", 
     xlab = "Time to Death in Week")

km_diploid = survfit(survObj~type, 
                     conf.type = "log",
                     type = "kaplan-meier", 
                     data = diploid)
plot(km_diploid , 
     mark.time = T,
     main="Diploid's Kaplan-Meier curve",
     ylab = "Probability for Survival", 
     xlab = "Time to Death in Week")

summary(km_aneuploid,time = c(52))
summary(km_diploid,time = c(52))