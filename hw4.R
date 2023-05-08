library(dplyr)
library(MASS)
library(nnet)
data = c(65, 34, 54, 47, 100, 100, 130, 141, 76, 116, 111, 191, 67, 130, 48, 105, 62, 104)
data_table = array(data, c(2, 3, 3), 
                   dimnames = list(Contact = c("Low", "High"), 
                   Response = c("Low_satisfaction", "Median_satisfaction", "High_satisfaction"),
                   Area = c("Tower_block", "Apartment", "House"))
                   )

# Question 1
# Association between satisfaction and contact with other residents
cont = margin.table(data_table, margin = c(1, 2))
prop.table(cont, margin = 1)

## Association between the level of satisfaction and the type pf housing
hous = margin.table(data_table, margin = c(2, 3))
prop.table(hous, margin = 2)
chisq.test(hous)


# Question 2
fulldata = data.frame(low = as.vector(data_table[,1,]),
                      median = as.vector(data_table[,2,]),
                      high = as.vector(data_table[,3,]),
                      contact_level = rep(c(1, 2), 3),
                      housing = rep(c(2, 1, 3), each = 2))

model_2 = multinom(cbind(fulldata$low, fulldata$median, fulldata$high)~factor(contact_level)+factor(housing), data=fulldata)
summary(model_2)
exp(coef(model_2))
exp(confint(model_2))

# hypothesis about contact level
model_21 = multinom(cbind(fulldata$low, fulldata$median, fulldata$high) ~ factor(housing), data = fulldata)
TS1 = deviance(model_21) - deviance(model_2)
p1 = 1-pchisq(TS1, 2)

# hypothesis about house type
model_22 = multinom(cbind(fulldata$low, fulldata$median, fulldata$high) ~ factor(contact_level), data = fulldata)
TS2 = deviance(model_22) - deviance(model_2)
p2 = 1 - pchisq(TS2, 4)

## goodness of fit
pihat = predict(model_2, type = "probs")
m = rowSums(fulldata[,1:3])
res.pearson = (fulldata[,1:3] - pihat * m)/sqrt(pihat * m)
G.stat = sum(res.pearson ^ 2)
p3 = 1 - pchisq(G.stat, (6-4) * (3-1))


# Question 3
number = c(fulldata$low, fulldata$median, fulldata$high)
res = c(rep(c("L", "M", "H"), c(6, 6, 6)))
res = factor(res, levels = c("L", "M", "H"), ordered = T)

data_ordinary = data.frame(res = res, 
                           contact_level = rep(fulldata$contact_level, 3), 
                           housing = rep(fulldata$housing, 3), 
                           number = number)
model_31 = polr(res ~ factor(contact_level) + factor(housing), data_ordinary, weights = number, method = "logistic")
summary(model_31)
exp(coef(model_31))
exp(confint(model_31))

# hypothesis about contact level
model_32 = polr(res ~ factor(housing), data_ordinary, weights = number)
TS5 = deviance(model_32) - deviance(model_31)
p5 = 1 - pchisq(TS5, 1)

# hypothesis about housing type
model_33 = polr(res ~ factor(contact_level), data_ordinary, weights = number)
TS6 = deviance(model_33) - deviance(model_31)
p6 = 1 - pchisq(TS6, 2)

#Pearson Chi-Square residuals from proportional odds model
pihat = predict(model_31, type = "probs")
m = rowSums(fulldata[,1:3])
res.pearson = ((fulldata[,1:3]-pihat*m)/sqrt(pihat*m)) %>% round(.,2) %>% as.data.frame()

# Question 4
pihat = predict(model_31, fulldata, type = 'probs')
m = rowSums(fulldata[,1:3])
res.pearson = (fulldata[,1:3] - pihat * m) / sqrt(pihat * m)