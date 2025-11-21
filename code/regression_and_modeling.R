# ===================
# pearson and spearman correlation
# ===================

# pearson
model1 <- lm(mpg ~ hp, data = mtcars)
r2 <- summary(model)$r.squared
summary(model1)

plot(mtcars$hp, mtcars$mpg,
     main = "Pearson Correlation (mpg vs hp)",
     xlab = "Horsepower (hp)",
     ylab = "Miles per gallon (mpg)",
     pch = 19)

abline(model1, lwd = 2)
text(250, 30, paste("R^2 =", round(r2, 3)))



# spearman
cor.test(mtcars$mpg, mtcars$hp, method = "spearman")

model_s <- lm(rank(mpg) ~ rank(hp), data = mtcars)
summary(model_s)


plot(rank(mtcars$hp), rank(mtcars$mpg),
     main = "Spearman Rank Correlation (mpg vs hp)",
     xlab = "Rank of hp",
     ylab = "Rank of mpg",
     pch = 19)

abline(lm(rank(mpg) ~ rank(hp), data = mtcars), lwd = 2)


# all
cor(mtcars, method = "pearson", use = "pairwise.complete.obs")

