library(psych)

# problem 1
# (a)
age = rep(c(25, 35, 45, 55, 65, 75),2)
case = c(0, 5, 21, 34, 36, 8, 1, 4, 25, 42, 19, 5)
control = c(106, 164, 138, 139, 88, 31, 9, 26, 29, 27, 18, 0)
exposure = c(rep(0, 6), rep(1 , 6))
res = cbind(case, control)
prospective_model = glm(res ~ exposure + age, family=binomial(link='logit'))
summary(prospective_model)


# (b)
age = c(1,2,3,4,5,6)
age = factor(age)
indicator = dummy.code(age)
age1 = rep(indicator[,1], 2)
age2 = rep(indicator[,2], 2)
age3 = rep(indicator[,3], 2)
age4 = rep(indicator[,4], 2)
age5 = rep(indicator[,5], 2)
age6 = rep(indicator[,6], 2)
model_0 = glm(res ~ age1 + age2 + age3 + age4 + age5 + age6, family = binomial(link = 'logit'))
summary(model_0)
sum(residuals(model_0, type = 'deviance') ^ 2)
model_1 = glm(res ~ exposure + age1 + age2 + age3 + age4 + age5 + age6, family = binomial(link = 'logit'))
summary(model_1)
sum(residuals(model_1, type= 'deviance') ^ 2)


# problem 2
seed = c(rep(1, 11), rep(0, 10))
root = c(rep(1, 5), rep(0, 6), rep(1, 5), rep(0, 5))
germ_num = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 3, 22, 15, 32, 3)
total_num = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7)

# (a)
model_a = glm(cbind(germ_num, total_num - germ_num) ~ seed + root, family = binomial(link = 'logit'))
summary(model_a)
1 - pchisq(model_a$deviance, 21 - 3)
sum(residuals(model_a, type = 'pearson') ^ 2)

# (b)
person = sum(residuals(model_a, type = 'pearson') ^ 2)
person / (21 - 3)
model_a$deviance / (21 - 3)

summary(model_a, dispersion = (person / (21 - 3)))