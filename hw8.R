library(tidyverse)
library(readxl)
library(gee)
data = read_excel('./HW8-HEALTH.xlsx')

# (a)
data0 = subset(data, TIME == '1') %>% mutate(HEALTH = as.numeric(HEALTH == 'Good'))
glm.fit = glm(HEALTH ~ TXT, family = binomial, data = data0)
summary(glm.fit)

# (b)
data2 = subset(data, TIME > '1')
dim(data2)
data2$baseline = rep(subset(data, TIME == '1')$HEALTH, rep(3,80))
data2$nstat = as.numeric(data2$HEALTH == 'Good')

gee.fit = gee(nstate ~ baseline + TXT + TIME + as.factor(AGEGROUP), id = ID, data = data2, 
              family = 'binominal', corstr = 'unstructured', scale.fix = T, scale.value = 1)

summary(gee.fit)


# (c)
glmer.fit = glmer(nstate ~ baseline + TXT + TIME + as.factor(AGEGROUP) + (1|ID))
summary(glmer.fit)

