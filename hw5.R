library(pscl)

# problem 1
# a part
crab = read.table("./HW5-crab.txt", header = T)
M1 = glm(Sa ~ W, family = poisson(link = log), data = crab)
summary(M1)
Deviance1 = sum(residuals(M1, type = "deviance") ^ 2)
Pearson1 = sum(residuals(M1, type = "pearson") ^ 2)
Pearson1 / (173 - 2)

# b part
M2 = glm(Sa ~ W + Wt, family = poisson(link = log), data = crab)
summary(M2)
# Goodness of fit test
Deviance2 = sum(residuals(M2, type = "deviance") ^ 2)
Pearson2 = sum(residuals(M2, type = "pearson") ^ 2)
Pearson2 / (173 - 3)

1-pchisq(Deviance1 - Deviance2, 171-170)

# c part
summary(M2, dispersion = phi)
(Pearson2 / 3.16) / (173 - 3)


# Problem 2
# a part
parasite = read.table("./HW5-parasite.txt", header = T)
fit2 = glm(Intensity ~ factor(Area) + factor(Year) + Length, family=poisson(link = log), data = parasite)
summary(fit2)
exp(fit2$coefficients)

# b part
Deviance3 = sum(residuals(fit2, type = "deviance") ^ 2)
Pearson3 = sum(residuals(fit2, type = "pearson") ^ 2)
pchisq(deviance(fit2),df.residual(fit2), lower.tail = F)
Pearson3 / 1184

# c part
fit2c = zeroinfl(Intensity ~ factor(Area) + factor(Year) + Length, data = parasite)
summary(fit2c)
round(exp(fit2c$coefficients$count), 2)
round(exp(fit2c$coefficients$zero), 2)
