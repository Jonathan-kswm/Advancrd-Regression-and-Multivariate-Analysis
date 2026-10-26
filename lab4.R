load("Module_Data.Rdata")  

galaxy[1:10,]

# --- Linear Regression --------------------------------------------------------

modelVB <- lm(Vdisp ~ BlackHole, data = galaxy)

summary(modelVB)

# check the heuristics of the model
# Linearity
# Normality
# Constant Varience (homoscedasity)

par(mfrow = c(1,2))

betahat <- modelVB$coefficients

epsilon <- modelVB$residuals

plot(Vdisp ~ BlackHole, data = galaxy)

abline(betahat, col = "blue")

hist(epsilon, main = expression("Histofram of"~hat(epsilon)[i]), xlab = expression(hat(epsilon)[i]))

# Check homoscedasitcity - residuals vs covariates
# Check normality - Q-Q plot of residuals

par(mfrow = c(1,2))

plot(epsilon ~ galaxy$BlackHole)

abline(h=0, col="blue")

qqnorm(epsilon)


par(mfrow = c(1,1))

muhat <- predict(modelVB)

plot(epsilon ~ muhat)

abline(h = 0, col = "blue")

# --- Categorical covariates ---------------------------------------------------

modelVT <- lm(Vdisp ~ Type, data = galaxy)

summary(modelVT)

par(mfrow = c(2, 2))

plot(modelVT)

# --- Different linear regression models ---------------------------------------

newModel1 <- lm(Vdisp ~ BlackHole + Distance, data = galaxy)

plot(newModel1)

newModel2 <- lm(Vdisp ~ BlackHole + Distance -1, data = galaxy)

plot(newModel2)

newModel3 <- lm(Vdisp ~ I(log(Distance +1)), data = galaxy)

plot(newModel3)
