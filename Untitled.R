###### ACTSC445 A4 Q1 #######
install.packages("VGAM")
install.packages("copula")
library(VGAM)
library(MASS)
library(copula)


########################### part a ############################

### a2 ###
# Companies: Nintendo and Sony 
# Data of stock from November 29, 2010 to November 26, 2021
nintendo <- read.csv("NTDOY.csv")
sony <- read.csv("SONY.csv")
nintendoAdj <- ts(data = nintendo["Adj.Close"])
sonyAdj <- ts(data = sony["Adj.Close"])

# calculate daily log-returns, X will contain Nintendo, Y will contain Sony
n <- length(nintendoAdj)
X <- log(nintendoAdj[-1]/nintendoAdj[-n])
Y <- log(sonyAdj[-1]/sonyAdj[-n])

# mean, median, 1st, 3rd quartiles, std dev
X_mean <- mean(X)
X_median <- median(X)
X_1qt <- quantile(X, 0.25)
X_3qt <- quantile(X, 0.75)
X_std <- sd(X)

Y_mean <- mean(Y)
Y_median <- median(Y)
Y_1qt <- quantile(Y, 0.25)
Y_3qt <- quantile(Y, 0.75)
Y_std <- sd(Y)

### a3 ###

dates <- as.Date(nintendo$Date, origin = "2010-11-29")
dates <- dates[2:2769]
plot(dates, X, 
     main = "Daily log-returns of Nintendo from November 29, 2010, to November 26, 2021",  
     pch = 16, cex.main = 0.7, xlab = "Year")
plot(dates, Y, 
     main = "Daily log-returns of Sony from November 29, 2010, to November 26, 2021",  
     pch = 16, cex.main = 0.7, xlab = "Year")

### a4 ###
# Nintendo and Sony might behave in a similar way since both companies are in the
# industry of manufacturing video game consoles and distributing video games.
# Both companies sell a variety of video games in their respective distribution network 
# but there are also similar video games offered in both networks such as 
# Overwatch, Resident Evil, etc. which makes them both similar in terms of 
# video games they distributen.

########################### part a ############################



########################### part b ############################
plot(X, Y, 
     main = "Daily log-returns of Nintendo vs Sony 
     from November 29, 2010 to November 26, 2021",
     cex.main = 0.75, pch = 16)

# Based on the scatter plot, X and Y looks to have a positive correlation
# along with a fairly high correlation as there are a few outliers
########################### part b ############################



########################### part c ############################
# Pearson
pearson_phi <- cov(X,Y) / sqrt(var(X) * var(Y))

# Spearman
spear_rho <- cor.test(X, Y, method = "spearman", exact = FALSE)

# Kendall
kendall_tau <- kendall.tau(X, Y, exact = FALSE)
########################### part c ############################



########################### part d ############################

### d1 ###

d1_tau <- seq(0.8, 0.96, by = 0.02)

# calculate pearson corr. for each pair 
pear_corr <- c()
for (i in d1_tau) {
  eps_X = quantile(X, i)
  eps_Y = quantile(Y, i)
  X_new = X[X > eps_X]
  Y_new = Y[Y > eps_Y]
  
  pear_corr_i = cov(X_new,Y_new) / sqrt(var(X_new) * var(Y_new))
  pear_corr = c(pear_corr, pear_corr_i)
}

# plot of each pearson corr. with its respective tau value
# it looks that there is no clear pattern between the corr. coefficient
# vs tau value
plot(d1_tau, pear_corr, 
     main = "Pearson correlation coefficient with respective tau value",
     pch = 16, cex.main = 0.8, xlab = "tau", ylab = "Pearson coeff.")


### d2 ###

d2_tau <- seq(0.2, 0.04, by = -0.02)

# calculate pearson corr. for each pair 
pear_corr_d2 <- c()
for (i in d2_tau) {
  eps_X = quantile(X, i)
  eps_Y = quantile(Y, i)
  X_new = X[X <= eps_X]
  Y_new = Y[Y <= eps_Y]
  
  pear_corr_i_d2 = cov(X_new,Y_new) / sqrt(var(X_new) * var(Y_new))
  pear_corr_d2 = c(pear_corr_d2, pear_corr_i_d2)
}

# plot of each pearson corr. with its respective tau value
# it looks that the higher the tau value is the higher the pearson
# correlation coefficient is on average
plot(d2_tau, pear_corr_d2, 
     main = "Pearson correlation coefficient with respective tau value",
     pch = 16, cex.main = 0.8, xlab = "tau", ylab = "Pearson coeff.")

########################### part d ############################



########################### part e ############################

### e1 ###
Rx <- c()
Sy <- c()
# calculate each probability-transformed pairs
for (i in 1:length(X)) {
  Xi = X[i]
  Yi = Y[i]
  Ri = length(X[X <= Xi]) / (length(X) + 1)
  Si = length(Y[Y <= Yi]) / (length(X) + 1)
  
  Rx = c(Rx, Ri)
  Sy = c(Sy, Si)
}

# produce pair plot
# Based on the scatterplot, the data looks clustered but it looks the data 
# has a very low positive correlation
plot(Rx, Sy, 
     main = "Scatterplot of probability-transformed pairs",
     cex.main = 0.8, pch = 16, xlab = "Ri/(n+1)", ylab = "Si/(n+1)")


### e2 ###
F1 <- ecdf(X)(X)
F2 <- ecdf(Y)(Y)
e2_probs = c()
for (i in d1_tau) {
  e2_V = which(F2 > i)
  e2_prob = sum(F1[e2_V] > i)/length(e2_V)
  e2_probs = c(e2_probs, e2_prob)
}



# produce scatter plot for e2
# based on the plot, it looks that as u increases, the probability
# of P(U > u | V > v) will decrease
plot(d1_tau, e2_probs, 
     main = "Scatterplot of probability estimate for P(U > u | V > v)",
     cex.main = 0.8, pch = 16, xlab = "u", ylab = "probability")


### e3 ###
e3_probs = c()
for (i in d2_tau) {
  e3_V = which(F2 <= i)
  e3_prob = sum(F1[e3_V] <= i)/length(e3_V)
  e3_probs = c(e3_probs, e3_prob)
}

# produce scatter plot for e3
# based on the plot, it looks that as u increases, the probability
# of P(U <= u | V <= v) will increase
plot(d2_tau, e3_probs, 
     main = "Scatterplot of probability estimate for P(U <= u | V <= v)",
     cex.main = 0.8, pch = 16, xlab = "u", ylab = "probability")

########################### part e ############################



########################### part f ############################

### f1 ###
f1_mean <- c(X_mean, Y_mean)
f1_sigma <- matrix(c(var(X), cor(X,Y) * X_std * Y_std,
                     cor(X,Y) * X_std * Y_std, var(Y)),
                   nrow = 2, ncol = 2)


### f2 ###
# we will use the empirical method for estimating VaR_0.95
# generate 2000 samples of portfolio loss
f2_2000mvr <- mvrnorm(n = 2000, f1_mean, f1_sigma)
f2_payoff <- 10000*exp(f2_2000mvr[, 1]) + 10000*exp(f2_2000mvr[, 2])
f2_loss <- sort(-f2_payoff)
f2_var95 <- f2_loss[ceiling(0.95*2000)]

########################### part f ############################



########################### part g ############################

### g1 ###
g1_uniX <- dnorm(X, mean = X_mean, sd = X_std)
g1_uniY <- dnorm(Y, mean = Y_mean, sd = Y_std)


### g2 ###
g2_U <- pnorm(X)
g2_V <- pnorm(Y)
g2_copmatrix <- pobs(matrix(c(g2_U,g2_V), ncol=2))

# gumbel, frank, joe copulas will be tested
gumbel_cop <- fitCopula(gumbelCopula(2, dim = 2), g2_copmatrix, method = "ml")
frank_cop <- fitCopula(frankCopula(2, dim = 2), g2_copmatrix, method = "ml")
joe_cop <- fitCopula(joeCopula(2, dim = 2), g2_copmatrix, method = "ml")

# check BIC of each copula
# choose frank since it has lowest log likelihood
gumbel_BIC <- 2*log(length(X)) - 2*log(logLik(gumbel_cop))
frank_BIC <- 2*log(length(X)) - 2*log(logLik(frank_cop))
joe_BIC <- 2*log(length(X)) - 2*log(logLik(joe_cop))


### g3 ###
# generate 2000 samples using the frank copula
g3_sim <- rCopula(2000, frankCopula(2, dim = 2))
g3_payoff <- 10000*exp(g3_sim[, 1]) + 10000*exp(g3_sim[, 2])
g3_loss <- sort(-g3_payoff)
g3_var95 <- g3_loss[ceiling(0.95*2000)]

########################### part g ############################
