# lab 1

#-----setup-----
load("Module_Data.RData")

# example 1 Bivariate data and correlation

x1 <- c(3, 3.5, 4, 5, 6, 7, 10, 12, 19.5, 30)
x2 <- c(11, 11, 17, 13, 14, 24, 16, 3, 3, 0)

X <- cbind(x1, x2) # cbind combines two vectors to make one matrix

par(mar = c(4,4,1,1)) # this sets the margin width for future plots

plot(X)

cor(x1, x2) # correlation between the two

# create the pairs plot to look for data that we could analyse

pairs(galaxy[ , 2:6])

#-----Q-Q plots-----
# Q-Q plots (quantile quantile plots) is a way of checking if a sample
# comes from a normal distribution.
# see P1-basics.html for the mathematics of how they work
# see here for that in code

# estimated parameters
mu <- mean(galaxy$Vdisp)
sigma <- sd(galaxy$Vdisp)

# order statistics
order <- sort(galaxy$Vdisp)

# standadise order statistics
Z <- (order - mu)/sigma

# vectorise values
n <- length(galaxy$Vdisp)
r <- 1:n # this is creating our vector from 1 to n

# quantiles of N(0,1)
q <- qnorm(r/(n+1))

plot(q, Z)

abline(a =0, b = 1, col = "red")

# this was a shit way to do what R already has

qqnorm(Z)
abline(a =0, b = 1, col = "red")

# we are asking the question of how straight does a Q-Q plot need to be.

par(mfrow = c(3,2))

curve(dnorm, from=-3, to =3, main = "True Density Function")

# set up sampling from a standard normal distribution

x <- rnorm(mean = 0, sd = 1, n = 200)
qqnorm(x, main = "Normal Q-Q plot when n = 200")
abline(a = 0, b = 1, col = "red")

x <- rnorm(mean = 0, sd = 1, n = 30)
qqnorm(x, main = "Normal Q-Q plot when n = 30")
abline(a = 0, b = 1, col = "red")


x <- rnorm(mean = 0, sd = 1, n = 30)
qqnorm(x, main = "Normal Q-Q plot when n = 30 again")
abline(a = 0, b = 1, col = "red")


x <- rnorm(mean = 0, sd = 1, n = 10)
qqnorm(x, main = "Normal Q-Q plot when n = 10")
abline(a = 0, b = 1, col = "red")


x <- rnorm(mean = 0, sd = 1, n = 10)
qqnorm(x, main = "Normal Q-Q plot when n = 10 again")
abline(a = 0, b = 1, col = "red")

# data that is not normal
par(mfrow = c(2, 2))

curve(dexp, from = 0, to = 10, main = "True density")

x <- rexp(n = 200)
qqnorm(x, main = "Normal Q-Q plot when n = 200")
abline(a = 0, b = 1, col = "red")

x <- rexp(n = 30)
qqnorm(x, main = "Normal Q-Q plot when n = 30")
abline(a = 0, b = 1, col = "red")

x <- rexp(n = 10)
qqnorm(x, main = "Normal Q-Q plot when n = 10")
abline(a = 0, b = 1, col = "red")

#-----Historgrams-----

hist(galaxy$Vdisp, breaks = seq(from = 0, to = 480, by = 40))

hist(galaxy$Vdisp, breaks = seq(from = 0, to = 480, by = 80))


library("MASS")
library("faraway")
library("survival")
library("heplots")
library("HSAUR2")

data()

# These are datasets that you can look through
