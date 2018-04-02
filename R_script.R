# Course Project for Stat Inference

set.seed(123)
lambda <- 0.2
n  <- 40


sample_means = NULL
sample_variances= NULL
for (i in 1 : 1000) {
        sample_40 <- rexp(n,lambda)
        sample_means <- c(sample_means, mean(sample_40))
        sample_variances <- c(sample_variances, var(sample_40))
}

myMean <- mean(sample_means)
theory_Mean <- 1/lambda
myVar<- mean(sample_variances)
theory_Var <- (1/lambda)^2

hist(sample_means, breaks = 20, xlab = 'Mean', main = 'Histogram of Means')
abline(v = theory_Mean, col = 3, lwd = 3)
abline(v = myMean, col = 2, lwd = 3, lty = 2)
legend(6, 100, c('Theoretical Mean', 'Sample Mean'), lwd = c(3,3), lty = c(1,2), col = c(3,2))
curve(dnorm(x, mean=myMean, sd=sd(sample_means))*200, col="darkblue", lwd=2, add=TRUE)

hist(sample_variances, breaks = 14, xlab = 'Variance', main = 'Histogram of Variance')
abline(v = theory_Var, col = 3, lwd = 3)
abline(v = myVar, col = 2, lwd = 3, lty = 2)
legend(60, 200, c('Theoretical Variance', 'Sample Variance'), lwd = c(3,3), lty = c(1,2), col = c(3,2))
curve(dnorm(x, mean=myVar, sd=sd(sample_variances))*6000, col="darkblue", lwd=2, add=TRUE)

