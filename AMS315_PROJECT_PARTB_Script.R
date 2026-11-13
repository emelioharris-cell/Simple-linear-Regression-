getwd()
list.files()
library(dplyr)
Dataset_B <- read.csv("500234_partB.csv", header = TRUE)
str(Dataset_B)
head(Dataset_B)
summary(Dataset_B$x)
summary(Dataset_B$y)
sd(Dataset_B$x)
sd(Dataset_B$y)
length(Dataset_B$y)
length(Dataset_B$x)
hist(Dataset_B$x)
hist(Dataset_B$y)
plot(Dataset_B$x,Dataset_B$y, xlab = "IV", ylab = "DV", main = "Scatterplot: DV~IV")
M <- lm(y~x, data = Dataset_B)
summary(M)
abline(M, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='red')
cor(Dataset_B$x, Dataset_B$y)
cov(Dataset_B$x, Dataset_B$y)
CorCoe <- cor(Dataset_B$x, Dataset_B$y)
covxy <- cov(Dataset_B$x, Dataset_B$y)
summary(M)
intercept <- coef(M) [1]
slope <- coef(M) [2]
rsquared <- summary(M)$r.squared
pValue <- summary(M)$coefficients[2,4]
confint(M, level = 0.99)
plot(M$fitted.values, M$residuals, xlab= "Predicted  values", ylab = "Residuals", main = "Residual Plot")
abline(h=0)
M2 <- lm(log(y) ~x, data = Dataset_B)
M3 <- lm(y ~ log(x), data = Dataset_B)
M4 <- lm(log(y) ~ log(x), data = Dataset_B)
summary(M)$r.squared
summary(M2)$r.squared
summary(M3)$r.squared
summary(M4)$r.squared
plot(M2$fitted.values, M2$residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals Plot 2")
abline(h=0)
plot(M3$fitted.values, M3$residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals Plot 3")
abline(h=0)
plot(M4$fitted.values, M4$residuals, xlab = "Predicted Values", ylab = "Rediduals", main = "Residuals Plot 4")
abline(h=0)
Dataset_B$xbin <- round(Dataset_B$x, 1)
binned <- Dataset_B %>%
group_by(xbin) %>%
summarize(x = mean(x), y = mean(y), n=n())
binnedModel <- lm(y~x, data = binned)
summary(binnedModel)
plot(binnedModel$fitted.values, binnedModel$residuals)
abline(h=0)

