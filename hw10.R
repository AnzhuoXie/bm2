library(tidyverse)
library(survival)
library(KMsurv)

# problem a
data = data.frame(time = c(4,12,15,21,23,2,6,8,10,19),
                  death = c(1,0,1,0,1,1,0,0,1,1),
                  group = c(1,1,1,1,1,2,2,2,2,2))
fit = coxph(Surv(time,death)~factor(group), data=data)
summary(fit)

# problem b
data(kidtran)
fit2=coxph(Surv(time,delta)~strata(gender)+strata(race)+age,data=kidtran,ties='breslow')
summary(fit2)

Sfit.strata1=survfit(fit2,newdata=data.frame(gender=c(1),race=c(1,2),age=60))
plot(Sfit.strata1,xlab='Time to death (months)',ylab='survival propability', 
     col=c('black','blue'), main='Curve for Male',mark.time = TRUE)

Sfit.strata2=survfit(fit2,newdata=data.frame(gender=c(2),race=c(1,2),age=60))
plot(Sfit.strata2,xlab='Time to death (months)',ylab='survival propability', 
     col=c('black','blue'), main='Curve for Female',mark.time = TRUE)

# problem c
data(larynx)
datac = larynx %>% 
  mutate(Z1 = ifelse(stage == '2',1,0),
         Z2 = ifelse(stage == '3',1,0),
         Z3 = ifelse(stage == '4',1,0),
         Z4 = age)
fit3=coxph(Surv(time,delta)~Z1+Z2+Z3+Z4+Z1*Z4, data=datac,method='breslow')
summary(fit3)

exp(-7.382+0.112*50)


