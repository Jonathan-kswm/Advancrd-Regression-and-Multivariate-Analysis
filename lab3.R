# lab 3

X <- matrix(c(6,9,10,6,8,3), nrow =3, byrow = TRUE)

Xbar <- colMeans(X)

S <- var(X)

Sinv <- solve(S)

# sample size

n <- nrow(X)

mu0 <- c(9,5)

Xbar <- matrix(Xbar, ncol = 1)
mu0 <-  matrix(mu0, ncol = 1)

# Calculate the T^2 test statistic

Tsq <- n * t(Xbar - mu0) %*% Sinv %*% (Xbar - mu0)

p <- ncol(X)

f0.9 <- (((n - 1) * (p) )/(n - p) ) * qf(0.9, p, n-p)

p <- ncol(X)
n <- nrow(X)

# Observed value of F:
F <- (n - p)*Tsq/((n - 1)*p)

# Probability of F exceeding that value:
pvalue <- 1 - pf(F, 2, 1)

# MANOVA

load("Module_Data.RData")

od <- oreodonts

names(od) <- c("BrainCase", "CheekTooth", "BullaLength",
               "BullaDepth", "species")

od$species <- factor(od$species)

colours <- c("black", "red", "goldenrod4", "green", "magenta3",  "blue", "orange")

pairs(od[, 1:4],
      col = colours[od$species],
      pch = 16,
      oma=c(3, 3, 3, 11))

legend("topright",
       legend = 1:7,
       col = colours,
       pch = 16,
       xpd = TRUE,
       title = "Species:")


# if you cant see the colours

colours <- c("black", "red", "goldenrod4", "green", "magenta3",  "blue", "orange")

pairs(od[, 1:4],
      col = colours[od$species],
      pch = (1:7)[od$species],
      oma=c(3, 3, 3, 11))

legend("topright",
       legend = 1:7,
       col = colours,
       pch = 1:7,
       xpd = TRUE,
       title = "Species:")

X <- od[ , 1:4]

means <- aggregate(x = X,
                   by = list(od$species),
                   FUN = mean)

SDs <- aggregate(x = X, by = list(od[, 5]), FUN = sd)

meantable <- round(as.matrix(means[, 2:5]))
SDtable   <- round(as.matrix(SDs[, 2:5]))

meantable[] <- paste(meantable, "(", SDtable, ")", sep = "")

noquote(meantable)

X <- as.matrix(od[ , 1:4])  # Matrix of our measurement data.

s <- od[ , 5]               # The species data.

m <- manova(X ~ s)          # Apply MANOVA.

summary(m, test = "Wilks")  # See the results.

i <- 1

speciesi <- od[od$species == i, ]

pairs(speciesi[ , 1:4])

par(mfrow = c(3, 2))

qqnorm(speciesi[ , 1])
qqnorm(speciesi[ , 2])
qqnorm(speciesi[ , 3])
qqnorm(speciesi[ , 4])

library("heplots")

cqplot(speciesi[ , 1:4])
