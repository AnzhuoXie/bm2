rm(list = ls())

library(lme4)


data = read.csv('./HW7-politeness_data.csv')

# problem a
boxplot(frequency ~ attitude * gender, col = c("white","lightgray"), data = data)

# problem b
m1 = lmer(frequency ~ gender + attitude + (1|subject), REML = F, data = data)
summary(m1)
round(ranef(m1)$subject,2)
residuals(m1)

# problem c
m2 = lmer(frequency ~ gender + attitude + gender*attitude+ (1|subject), REML = F, data = data)
TS1 = deviance(m1) - deviance(m2)
p1 = 1-pchisq(TS1, 1)

# problem d
m3 = lmer(frequency ~ gender + attitude + (1|subject) + (1|scenario), data = data)
summary(m3)
ranef(m3)