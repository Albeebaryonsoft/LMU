
###################################################
### code chunk number 2: ExerciseCrossover.Rnw:54-55
###################################################
dat <- read.table("data/crossoverdata.dat", header = TRUE, sep = "\t")

###################################################
### code chunk number 3: ExerciseCrossover.Rnw:69-73
###################################################
PEFR.Sequence1.Period1 <- dat$PEFR[dat$Sequence == 1 & dat$Period == 1]
PEFR.Sequence1.Period2 <- dat$PEFR[dat$Sequence == 1 & dat$Period == 2]
PEFR.Sequence2.Period1 <- dat$PEFR[dat$Sequence == 2 & dat$Period == 1]
PEFR.Sequence2.Period2 <- dat$PEFR[dat$Sequence == 2 & dat$Period == 2]


###################################################
### code chunk number 4: ExerciseCrossover.Rnw:77-79
###################################################
d.bar <- 0.5 * (mean(PEFR.Sequence1.Period1) - mean(PEFR.Sequence1.Period2) - 
mean(PEFR.Sequence2.Period1) + mean(PEFR.Sequence2.Period2))


###################################################
### code chunk number 5: ExerciseCrossover.Rnw:83-88
###################################################
est <- by(dat[, "PEFR"], dat[, c("Period", "Sequence")], mean)
est

d.bar <- 0.5 * (est[1] - est[2] - est[3] + est[4])
d.bar


###################################################
### code chunk number 6: ExerciseCrossover.Rnw:92-101
###################################################
Z <- c(
mean(PEFR.Sequence1.Period1),
mean(PEFR.Sequence1.Period2),
mean(PEFR.Sequence2.Period1),
mean(PEFR.Sequence2.Period2))

interaction.plot(x.factor = c(1, 2, 1, 2), trace.factor = c(1, 1, 2, 2), response = Z,
ylab = "PEFR", xlab = "Period", legend = FALSE)
legend("topright", c("Med./Plac. ('AB')", "Plac./Med.('BA')"), lty = c(2, 1)) 


###################################################
### code chunk number 7: ExerciseCrossover.Rnw:113-115
###################################################
n.Sequence1 <- length(unique(dat$Subject[dat$Sequence == 1]))
n.Sequence2 <- length(unique(dat$Subject[dat$Sequence == 2]))


###################################################
### code chunk number 8: ExerciseCrossover.Rnw:119-123
###################################################
V <- ((n.Sequence1 - 1) * var(PEFR.Sequence1.Period1 - PEFR.Sequence1.Period2) + 
(n.Sequence2 - 1) * var(PEFR.Sequence2.Period2 - PEFR.Sequence2.Period1)) / (n.Sequence1 + n.Sequence2 - 2)

VAR <- V / 4 * (1 / n.Sequence1 + 1 / n.Sequence2)


###################################################
### code chunk number 9: ExerciseCrossover.Rnw:127-128
###################################################
2 * (1 - pt(d.bar / sqrt(VAR), n.Sequence1 + n.Sequence2 - 2))


###################################################
### code chunk number 10: ExerciseCrossover.Rnw:132-133
###################################################
d.bar + c(-1, 0, 1) * qt(0.975, n.Sequence1 + n.Sequence2 - 2) * sqrt(VAR)


###################################################
### code chunk number 11: ExerciseCrossover.Rnw:147-150
###################################################
library(nlme)
fit <- lme(PEFR ~ Drug + Period + Sequence, random = ~ 1 | Subject, data = dat)
summary(fit)


###################################################
### code chunk number 12: ExerciseCrossover.Rnw:154-155
###################################################
summary(fit)$sigma^2


###################################################
### code chunk number 13: ExerciseCrossover.Rnw:158-159
###################################################
2 * summary(fit)$sigma^2


###################################################
### code chunk number 14: ExerciseCrossover.Rnw:164-167
###################################################
2 * summary(fit)$sigma^2

V


###################################################
### code chunk number 15: ExerciseCrossover.Rnw:191-197
###################################################
alpha <- 0.05
beta <- 0.2
Delta <- 10
residualVariance <- 326
n <- 2 * residualVariance * (qnorm(1 - alpha/2) + qnorm(1 - beta))^2 / Delta^2
ceiling(n)


###################################################
### code chunk number 16: ExerciseCrossover.Rnw:210-215
###################################################
alpha <- 0.01
beta <- 0.1
standardizedEffect <- seq(0.5, 1.0, 0.001)
n <- 2 * 1 / standardizedEffect^2 * (qnorm(1 - alpha/2) + qnorm(1 - beta))^2
n <- ceiling(n)


###################################################
### code chunk number 17: ExerciseCrossover.Rnw:219-220
###################################################
plot(x = standardizedEffect, y = n, type = "l", xlab = "Standardized effect", ylab = "Sample size")


###################################################
### code chunk number 18: ExerciseCrossover.Rnw:224-226
###################################################
curve(ceiling(2 * 1 / x^2 * (qnorm(1 - alpha/2) + qnorm(1 - beta))^2), from = 0.5, to = 1.0,
xlab = "Standardized effect", ylab = "Sample size")


###################################################
### code chunk number 19: ExerciseCrossover.Rnw:252-253
###################################################
dat <- read.table("bioequivalencedata.dat", header = TRUE, sep = "\t")


###################################################
### code chunk number 20: ExerciseCrossover.Rnw:265-266
###################################################
dat$logAUC <- log(dat$AUC)


###################################################
### code chunk number 21: ExerciseCrossover.Rnw:270-272
###################################################
est <- by(dat[, "logAUC"], dat[, c("Period", "Sequence")], mean)
exp(est)


###################################################
### code chunk number 22: ExerciseCrossover.Rnw:276-278
###################################################
d.bar <- 0.5 * (est[1] - est[2] - est[3] + est[4])
exp(d.bar)


###################################################
### code chunk number 23: ExerciseCrossover.Rnw:282-294
###################################################
Z <- exp(
  c(
  est[1],
  est[2],
  est[3],
  est[4]
  )
)

interaction.plot(x.factor = c(1, 2, 1, 2), trace.factor = c(1, 1, 2, 2), response = Z,
ylab = "AUC", xlab = "Period", legend = FALSE)
legend("topleft", c("RT", "TR"), lty = c(2, 1))


###################################################
### code chunk number 24: ExerciseCrossover.Rnw:314-316
###################################################
n.Sequence1 <- length(unique(dat$ID[dat$Sequence == "TR"]))
n.Sequence2 <- length(unique(dat$ID[dat$Sequence == "RT"]))


###################################################
### code chunk number 25: ExerciseCrossover.Rnw:320-331
###################################################
logAUC.Sequence1.Period1 <- dat$logAUC[dat$Sequence == "TR" & dat$Period == 1]
logAUC.Sequence1.Period2 <- dat$logAUC[dat$Sequence == "TR" & dat$Period == 2]
logAUC.Sequence2.Period1 <- dat$logAUC[dat$Sequence == "RT" & dat$Period == 1]
logAUC.Sequence2.Period2 <- dat$logAUC[dat$Sequence == "RT" & dat$Period == 2]

V <- (
      (n.Sequence1 - 1) * var(logAUC.Sequence1.Period1 - logAUC.Sequence1.Period2) +
      (n.Sequence2 - 1) * var(logAUC.Sequence2.Period2 - logAUC.Sequence2.Period1)
      ) / (n.Sequence1 + n.Sequence2 - 2)

VAR <- V / 4 * (1 / n.Sequence1 + 1 / n.Sequence2)


###################################################
### code chunk number 26: ExerciseCrossover.Rnw:335-340
###################################################
T1 <- (d.bar - log(1.25)) / sqrt(VAR)
T1

T2 <- (d.bar - (-log(1.25))) / sqrt(VAR)
T2


###################################################
### code chunk number 27: ExerciseCrossover.Rnw:344-349
###################################################
criticalValue1 <- qt(0.05, n.Sequence1 + n.Sequence2 - 2)
criticalValue1

criticalValue2 <- qt(0.95, n.Sequence1 + n.Sequence2 - 2)
criticalValue2


###################################################
### code chunk number 28: ExerciseCrossover.Rnw:353-356
###################################################
T1 < criticalValue1

T2 > criticalValue2


###################################################
### code chunk number 29: ExerciseCrossover.Rnw:364-366
###################################################
ci <- d.bar + c(-1, 0, 1) * qt(0.95, n.Sequence1 + n.Sequence2 - 2) * sqrt(VAR)
ci


###################################################
### code chunk number 30: ExerciseCrossover.Rnw:370-371
###################################################
exp(ci)


