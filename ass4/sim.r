
## Load pwr package to easily calculate the statistical power
if (!require(pwr)) {install.packages("pwr")}
library(pwr)
## Disable scientific notation (1.05e10)
options(scipen = 999)


## Set number of simulations
nSims <- 100000 ## number of simulated experiments
M <- 106 ## Mean IQ score in the sample (will be compared with 100 in a one-sample t-test)
n <- 26 ## set sample size
SD <- 15 ## SD of the simulated data
## With a mean difference of 6, and SD of 15, and a sample size of 26, the test has 50% power)
p <- numeric(nSims)
## set up empty variable to store all simulated p-values
bars <- 20



## Run simulation
for (i in 1:nSims) { ## for each simulated experiment
  x <- rnorm(n = n, mean = M, sd = SD) ## Simulate data with specified mean,
  ## standard deviation, and sample size
  z <- t.test(x, mu = 100) ## perform the t-test against mu (set to value you want to test against)
  p[i] <- z$p.value ## get the p-value and store it
}


## Check power by summing significant p-values and dividing by number of simulations
sum(p < 0.05) / nSims # power
## Calculate power formally by power analysis
power <- pwr.t.test(
    d = (M - 100) / SD,
    n = n,
    sig.level = 0.01,
    type = "one.sample",
    alternative = "two.sided")$power

## Plot figure
png(file="P-valueDist001p.png",width=4000,height=3000, , units = "px", res = 500)
op <- par(mar = c(5, 7, 4, 4)) ## change white-space around graph
hist(p,
     breaks = bars,
     xlab = "P-values",
     ylab = "number of p-values\n",
     axes = FALSE,
     main = paste("P-value Distribution with",
                  round(power * 100, digits = 1),
                  "% Power"),
     col = "grey",
     xlim = c(0,1),
     ylim = c(0, 10000))

axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))

axis(
    side = 2,
    at = seq(0, nSims, nSims / 10),
    labels = seq(0, nSims,nSims / 10),
    las = 2)

## draw a line of the p-value distribution when the null hypothesis is true
abline(h = nSims / bars,
       col = "red",
       lty = 3)
dev.off ()
