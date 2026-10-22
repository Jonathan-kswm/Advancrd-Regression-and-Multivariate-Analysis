# Lab 2

# ---- Simulation ----
# simulation is used to check theoretical claims about data

set.seed(123) # can be any number but the same seed reproduces results

X1 <- rnorm(n = 100, mean =2, sd = 3)
X2 <- rnorm(n =100, mean = 8, sd = 1)

mean(X1 + X2)
var(X1 + X2)

# for more accurate results you can increase the sample size by bumping n

set.seed(111)

X1 <- rnorm(n = 10000, mean =2, sd =3)
X2 <- rnorm(n = 10000, mean =8, sd =1)

mean(X1 + X2)
var(X1 + X2)

# X1 + X2 is claimed to follow a normal distribution

par(mfrow = c(1, 2))

qqnorm(X1 + X2) # Q-Q plot so we are expecting this to be a straight line
abline(a =1, b = 1 , col = "red") # straight line to compare
# this may not be standadised that is why my gradient of the abline
# is wrong - think about what this should be

hist(X1 + X2, breaks = "FD", freq = F)
curve(dnorm(x, mean = 10, sd = sqrt(10)), col = "red", lwd=2, add = TRUE)

# ---- Multivariate Simulation ----

mu <- c(0,0)

Sigma <- c(5,4,4,5)

dim(Sigma) <- c(2,2) # this creates a 2x2 matrix from our vector

Sigma <- matrix(c(5, 4, 4, 5), nrow = 2, ncol = 2, byrow = TRUE)
# This also works

set.seed(456)

library("MASS")

Xsim <- mvrnorm(n =1, mu = mu, Sigma = Sigma) # this is 1 sample

Xsim <- mvrnorm(n =1000, mu = mu, Sigma = Sigma) # This is 1000 samples

# Each of the marginal distributions of a multivariate normal dist
# should be univariate normal.
# Check this using:

par(mfrow = c(1,2))

qqnorm(Xsim[,1])
abline(a = 0, b = 1, col = "red") # again think about the line choice here
qqnorm(Xsim[,2])


# ---- Mahalanobis distance ----

D <- function(X, mu, Sigma){
  sqrt(t(X - mu) %*% solve(Sigma) %*% (X - mu))
}

D(X = Xsim[1, ], mu = mu, Sigma = Sigma)

# sample size
n <- nrow(Xsim)

distances <- rep(NA, n)

for(i in 1:n){
  distances[i] <- D(X = Xsim[i,], mu = mu, Sigma = Sigma)
}

p <- ncol(Xsim)

q <- qchisq(0.95, df = p)

s <- ifelse(distances^2 < q, 1 , 0)

sum(s)/n

# visulisation of Mahalanobis distance
par(mar = c(4, 4, 1, 1))

q <- qchisq(0.5, df = p)

colour <- ifelse(distances^2 < q, "blue", "red3")

plot(Xsim, col = colour, pch = 16, cex = 0.6)

sum(colour == "blue")/n

# ---- Chi sq Q-Q plots ----

par(mfrow = c(1,1))

hist(distances^2,
     freq = FALSE,
     xlab = expression(d[i]^2),
     breaks = "FD"
)

p <- ncol(Xsim)

curve(dchisq(x, df =p), add = TRUE, col = "blue")

dsquared <- sort(distances^2)

n <- nrow(Xsim)

quantiles <- qchisq((1:n)/(n + 1), df = p)

plot(quantiles, dsquared, main = "Q-Q plot")