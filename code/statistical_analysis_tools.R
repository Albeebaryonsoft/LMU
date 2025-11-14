## ===============================
## 1) 常见分布：正态、二项、Poisson、t 分布
## ===============================

set.seed(123)

# ---- 正态分布：密度(d)、分位数(q)、累积(p)、随机数(r), 置信区间

x_norm <- seq(-3, 3, length.out = 7)
dnorm_vals <- dnorm(x_norm, mean = 0, sd = 1)   # density f(x)
pnorm_vals <- pnorm(x_norm, mean = 0, sd = 1)   # accumulative F(x)
qnorm_vals <- qnorm(c(0.025, 0.5, 0.975), mean = 0, sd = 1)  # 反向
qnorm_vals <- qnorm(0.025, mean = 0, sd = 1)  # 反向
rnorm_samp <- rnorm(5, mean = 0, sd = 1)        # random sample
x_norm; dnorm_vals; pnorm_vals; qnorm_vals; rnorm_samp


#norm参数可调版 计算置信区间
xbar <- 3.2; n <- 18; sigma <- 1.1; alpha <- 0.05
z <- qnorm(1 - alpha/2)
ci_mu_known_sigma <- c(xbar - z * sigma/sqrt(n), xbar + z * sigma/sqrt(n))
ci_mu_known_sigma

#norm参数可调版本 计算累计概率
x <- 165; mu <- 170; sigma <- sqrt(45)
pnorm(x, mu, sigma)  # 小于165的累计概率
1 - pnorm(x, mu, sigma) # 大于165 右侧



# ---- 二项分布：n 次独立试验，成功概率 p 例：n=10, p=0.3
k <- 0:10
dbinom_vals <- dbinom(k, size = 10, prob = 0.3)   # PMF P(X=k)
pbinom_vals <- pbinom(k, size = 10, prob = 0.3)   # CDF P(X<=k)
qbinom_vals <- qbinom(c(0.025, 0.5, 0.975), size = 10, prob = 0.3)
rbinom_samp <- rbinom(10, size = 10, prob = 0.3)
dbinom_vals; pbinom_vals; qbinom_vals; rbinom_samp


# ---- Poisson 分布：参数 lambda，置信区间
lambda <- 4
x_pois <- 0:12
dpois_vals <- dpois(x_pois, lambda)   # PMF
ppois_vals <- ppois(x_pois, lambda)   # CDF
qpois_vals <- qpois(c(0.025, 0.5, 0.975), lambda)
rpois_samp <- rpois(5, lambda)
dpois_vals; ppois_vals; qpois_vals; rpois_samp

#possion 参数可调版
k <- 12; Texp <- 5.0; alpha <- 0.05
lower <- if (k==0) 0 else 0.5 * qchisq(alpha/2, df = 2*k) / Texp
upper <- 0.5 * qchisq(1 - alpha/2, df = 2*(k + 1)) / Texp
c(lower, upper)                    
c(k/Texp, k/Texp)                  



# ---- t 分布：密度、累积、分位、随机 ------
df <- 10
x_t <- seq(-3, 3, length.out = 7)
dt_vals <- dt(x_t, df = df)            # 密度
pt_vals <- pt(x_t, df = df)            # 累积
qt_vals <- qt(c(0.025, 0.5, 0.975), df = df)  # 分位
rt_samp <- rt(5, df = df)
dt_vals; pt_vals; qt_vals; rt_samp


# ---- 单样本 t 检验：检验某总体均值是否为 mu0
x <- rnorm(20, mean = 5, sd = 2)
t_onesample <- t.test(x, mu = 5)   # H0: mean(x)=5
x; t_onesample    #结果包含CI

#one sample t-test+Ci 参数可调整版
xbar <- 3.2     # sample mean
s <- 1.1        # sample sd差
n <- 18        # smple size
mu0 <- 2.5         # H0: mean = mu0

t_stat <- (xbar - mu0) / (s / sqrt(n))
df <- n - 1
p_value <- 2 * (1 - pt(abs(t_stat), df))   # 双侧 p, pt累积分布
t_stat; df; p_value

alpha <- 0.02 # 参数可调
t_crit <- qt(1 - alpha/2, df)
ci_0.98 <- c(xbar - t_crit * s/sqrt(n), xbar + t_crit * s/sqrt(n))
ci_0.98 

#t-test参数可调版本 计算累计概率



# ---- 双样本 t 检验（独立样本）-------

x1 <- rnorm(25, mean = 10, sd = 3)    # it's a vector like x <- c(1, 2, 3, 4, 5)
x2 <- rnorm(22, mean = 11, sd = 3.5)
t_twosample_welch <- t.test(x1, x2, var.equal = FALSE,conf.level = 0.99) # Welch t 检验，通常默认的也是不相等
t_twosample_equalvar <- t.test(x1, x2, var.equal = TRUE,conf.level = 0.99)  # 传统等方差 t 检验
t_twosample_welch                
t_twosample_equalvar    #结果已包含CI

#two sample t-test+Ci 参数可调整版

xbar1 <- 9.67
s1 <- 2.1
n1 <- 25

xbar2 <- 11.70
s2 <- 2.6
n2 <- 22


# two variances are unequal
alpha <- 0.05   # 95% CI
dhat <- xbar1 - xbar2
SE <- sqrt(s1^2/n1 + s2^2/n2)

df <- (s1^2/n1 + s2^2/n2)^2 /
  ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))

tcrit <- qt(1 - alpha/2, df)
CI <- c(dhat - tcrit * SE,
        dhat + tcrit * SE)
p_value <- 2 * (1 - pt(abs(t_stat), df))
dhat;  df;  CI; p_value

#two variances are equal
sp2 <- ((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2)
sp <- sqrt(sp2)

SE_equal <- sp * sqrt(1/n1 + 1/n2)
df_equal <- n1 + n2 - 2
t_stat_equal <- dhat / SE_equal
p_value_equal <- 2 * (1 - pt(abs(t_stat_equal), df_equal))

tcrit_equal <- qt(1 - alpha/2, df_equal)
CI_equal <- c(dhat - tcrit_equal * SE_equal,
              dhat + tcrit_equal * SE_equal)

df_equal; t_stat_equal; p_value_equal; CI_equal


# ---- 配对样本 t 检验（前后测/成对设计）
before <- rnorm(18, mean = 120, sd = 10)
after  <- before - rnorm(18, mean = 5, sd = 6)   # 假设治疗后平均下降 5
t_paired <- t.test(before, after, paired = TRUE)
t_paired   #结果已经包含CI

# paired t 参数可调整版

dbar <- 2.4      
sd_d <- 1.7     
n <- 18
alpha <- 0.05

SE <- sd_d / sqrt(n)
df <- n - 1
t_stat <- dbar / SE
p_value <- 2*(1 - pt(abs(t_stat), df))

tcrit <- qt(1 - alpha/2, df)
CI <- c(dbar - tcrit * SE, dbar + tcrit * SE)

t_stat; p_value; CI



## ===============================
## 3) proportion的置信区间与检验
## ===============================

# ---- 单一比例 的 CI（近似/Wilson）与精确（Clopper-Pearson）
# 例：n=100 中成功 x=32
x_success <- 471; n_total <- 1000

# 近似（prop.test 使用 Wilson/正态近似，带连续性修正）
ci_prop_approx <- prop.test(x_success, n_total,conf.level = 0.95, correct = TRUE)
ci_prop_approx

# 精确区间（binom.test 使用 Clopper-Pearson）
ci_prop_exact <- binom.test(x_success, n_total, p = 0.5, conf.level = 0.95, alternative = "two.sided")
ci_prop_exact  # 检验总体p是不是0.2， 在p=0.3的情况下，观察到这个结果的概率

#输出结果已经包含CI



# ---- 双比例 1. test for equality of proportions
x1 <- 65; n1 <- 94
x2 <- 63; n2 <- 74
prop_2sample <- prop.test(c(x1, x2), c(n1, n2), conf.level = 0.95, correct = TRUE) # 比例差检验+CI
prop_2sample

prop.test(c(x1, x2), c(n1, n2), alternative = "less",  conf.level = 0.95,correct = TRUE) # H0: P1 >= P2 , H1: P1 < P2
prop.test(c(x1, x2), c(n1, n2), alternative = "greater",  conf.level = 0.95,correct = TRUE) # H0: P1 <= P2 , H1: P1 > P2

# ---- 双比例 2. test for independence (vs. association) in the 2*2 table : chi or fisher
# display 2*2 table
tab <- matrix(c(65, 29,  # n of success, n of failure
                63, 11),
              nrow = 2, byrow = TRUE)
colnames(tab) <- c("Success", "Failure")
rownames(tab) <- c("Group1", "Group2")
tab

# chi
chisq.test(tab, correct = FALSE)   # without Yates correction
chisq.test(tab)                    # with Yates correction (default for 2x2)

# fisher test :for small samples or exact p-value)
fisher.test(tab)

# ---- 双比例 配对 McNemar 配对比例 --------













## ===============================
## 4) F 分布 & 方差检验
## ===============================
# F 分布的基础函数
f_vals_d <- df(1:5, df1 = 10, df2 = 20)     # 密度
f_vals_p <- pf(1:5, df1 = 10, df2 = 20)     # 累积
f_vals_q <- qf(c(0.95, 0.975, 0.99), df1 = 10, df2 = 20)  # 上分位点
f_vals_d; f_vals_p; f_vals_q



# 双样本方差相等性检验（方差比检验），对应 F 分布
# H0: Var(x1) = Var(x2)
x1 <- rnorm(30, mean = 0, sd = 2)
x2 <- rnorm(25, mean = 0, sd = 3)
var_equal_test <- var.test(x1, x2)  # 基于 F = s1^2 / s2^2
var_equal_test


# 参数可调版
s1 <- 2      
s2 <- 3      
n1 <- 30     
n2 <- 25     
alpha <- 0.05   

F_stat <- (s1^2) / (s2^2)
df1 <- n1 - 1
df2 <- n2 - 1

p_value <- 2 * min(
  pf(F_stat, df1, df2),        # 左尾
  1 - pf(F_stat, df1, df2)     # 右尾
)

# 方差比 σ1²/σ2² 的置信区间---------------
F_lower <- F_stat / qf(1 - alpha/2, df1, df2)
F_upper <- F_stat / qf(alpha/2, df1, df2)
CI <- c(F_lower, F_upper)




## ===============================
## 5) 最大似然估计（MLE）示例
## ===============================

# ---- (a) Poisson(λ) 的 MLE：样本均值就是 λ̂
y <- rpois(100, lambda = 3.5)
lambda_hat_closed <- mean(y)
lambda_hat_closed

# 用 optim 做数值极大化（对数似然）
loglik_pois <- function(par, y) {
  lambda <- exp(par)         # 保证 λ>0，用对数参数化
  sum(dpois(y, lambda, log = TRUE))
}
fit_pois <- optim(par = log(mean(y)), fn = function(p) -loglik_pois(p, y),
                  method = "BFGS")
lambda_hat_optim <- exp(fit_pois$par)
c(closed_form = lambda_hat_closed, optim_MLE = lambda_hat_optim)

# ---- (b) 正态 N(μ, σ^2) 的 MLE
z <- rnorm(200, mean = 5, sd = 2)

# 解析解： μ̂ = 平均数； σ̂^2（MLE）= 平方和/ n （注意不是无偏的 s^2，用的是除以 n）
mu_hat <- mean(z)
sigma2_hat_mle <- mean( (z - mu_hat)^2 )
c(mu_hat = mu_hat, sigma2_hat_mle = sigma2_hat_mle, sd_hat_mle = sqrt(sigma2_hat_mle))

# 数值法（μ 不限，σ>0 用 log σ 参数化）
loglik_norm <- function(par, z) {
  mu <- par[1]
  log_sigma <- par[2]
  sigma <- exp(log_sigma)
  sum(dnorm(z, mean = mu, sd = sigma, log = TRUE))
}
fit_norm <- optim(par = c(mean(z), log(sd(z))),
                  fn = function(p) -loglik_norm(p, z),
                  method = "BFGS")
mu_hat_opt <- fit_norm$par[1]
sigma_hat_opt <- exp(fit_norm$par[2])
c(mu_hat_opt = mu_hat_opt, sigma_hat_opt = sigma_hat_opt)

# ---- (c) 二项分布 Bin(n, p) 的 MLE： p̂ = x / n
x <- 37; n <- 120
p_hat <- x / n
p_hat


