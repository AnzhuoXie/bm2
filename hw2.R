library(ResourceSelection)

#problem 1

x = c(0,1,2,3,4)
death = c(2,8,15,23,27)
dataused = data.frame(death, x)
a = c(30,30,30,30,30)
survive = a - death

# a part
# logit
problem1_logit = glm(cbind(death, survive) ~ x, data = dataused, family = binomial(link = 'logit'))
summary(problem1_logit)
# confidential interval
confint.default(problem1_logit)
# deviance
sum(residuals(problem1_logit,type = 'deviance') ^ 2)
predict.glm(problem1_logit, newdata = data.frame(x = 0.01),type = 'r')


# probit
problem1_probit = glm(cbind(death, survive) ~ x, data = dataused, family = binomial(link = 'probit'))
summary(problem1_probit)
# confidential interval
confint.default(problem1_probit)
# deviance
sum(residuals(problem1_probit, type='deviance') ^ 2)
predict.glm(problem1_probit, newdata = data.frame(x = 0.01),type = 'r')


# complementary log-log 
problem1_cll = glm(cbind(death, survive) ~ x, data = dataused, family = binomial(link = 'cloglog'))
summary(problem1_cll)
# confidential interval
confint.default(problem1_cll) 
# deviance
sum(residuals(problem1_cll, type = 'deviance') ^ 2)
predict.glm(problem1_cll, newdata = data.frame(x = 0.01),type = 'r')


# b part
# logit
b0 = problem1_logit$coefficients[1]
b1 = problem1_logit$coefficients[2]
betacov = vcov(problem1_logit)
x0 = - b0 / b1
exp(x0)
sqrt(varx0)
varx0 = betacov[1,1] / (b1^2) + betacov[2,2]*(b0^2) / (b1^4) - 2 * betacov[1,2] * b0 / (b1^3)
# confidential interval
x0 + c(qnorm(0.05), -qnorm(0.05)) * sqrt(varx0)
# 90% CI for LD50
exp((x0 + c(qnorm(0.05), -qnorm(0.05)) * sqrt(varx0)))



# probit
b0 = problem1_probit$coefficients[1]
b1 = problem1_probit$coefficients[2]
betacov = vcov(problem1_probit)
x0 = - b0 / b1
exp(x0)
sqrt(varx0)
varx0 = betacov[1,1] / (b1^2) + betacov[2,2] * (b0^2) / (b1^4) - 2 * betacov[1,2] * b0 / (b1^3)
# confidential interval
x0 + c(qnorm(0.05), -qnorm(0.05)) * sqrt(varx0)
# 90% CI for LD50
exp((x0 + c(qnorm(0.05), -qnorm(0.05)) * sqrt(varx0)))



# complementary log-log 
c = log(-log(0.5))
b0 = problem1_cll$coefficients[1]
b1 = problem1_cll$coefficients[2]
betacov = vcov(problem1_cll)
x0 = (c - b0) / b1
exp(x0)
sqrt(varx0)
varx0 = betacov[1,1] / (b1^2) + betacov[2,2] * ((c - b0) ^ 2) / (b1 ^ 4) + 2 * betacov[1,2] * (c - b0) / (b1 ^ 3)
# confidential interval
x0 + c(qnorm(0.05), -qnorm(0.05)) * sqrt(varx0)
# 90% CI for LD50
exp((x0 + c(qnorm(0.05), -qnorm(0.05)) * sqrt(varx0)))


#--------------------------------------------------------------------------


# problem 2
amount = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
offer = c(4,6,10,12,39,36,22,14,10,12,8,9,3,1,5,2,1)
enrolls = c(0,2,4,2,12,14,10,7,5,5,3,5,2,0,4,2,1)
dataused = data.frame(amount, offer, enrolls)
glm_logit = glm(cbind(enrolls, offer - enrolls) ~ amount, data = dataused, family = binomial(link = 'logit'))

# part a
hl = hoslem.test(glm_logit$y, fitted(glm_logit), g = 10)

# part b
summary(glm_logit)
beta = glm_logit$coefficients[2]
exp(beta) 
exp(confint.default(glm_logit))

# part c
c = log(0.4 / (1 - 0.4))
beta0 = glm_logit$coefficients[1]
beta1 = glm_logit$coefficients[2]
betacov = vcov(glm_logit)
x0fit = (c - beta0) / beta1
varx0 = betacov[1,1] / (beta1 ^ 2) + betacov[2,2] * ((c - beta0) ^ 2) / (beta1 ^ 4) + 2 * betacov[1,2] * (c - beta0) / (beta1 ^ 3)
x0fit + c(qnorm(0.025), -qnorm(0.025)) * sqrt(varx0)