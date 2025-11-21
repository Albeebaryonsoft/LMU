## ===============================
## 1) 常见分布：正态、二项、Poisson、t 分布
## ===============================

set.seed(123)

# ---- 正态分布：密度(d)、分位数(q)、累积(p)、随机数(r), 置信区间

x_norm <- seq(-3, 3, length.out = 7)
dnorm_vals <- dnorm(x_norm, mean = 0, sd = 1)   # density f(x)
pnorm_vals <- pnorm(x_norm, mean = 0, sd = 1)   # accumulative F(x)
pnorm_vals <- pnorm(-4.9, mean = 0, sd = 1)   # accumulative F(x)
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

# ---- 单one sample 的 CI（近似/Wilson）与精确（Clopper-Pearson）
# 例：n=100 中成功 x=32
x_success <- 471; n_total <- 1000

# 近似（prop.test 使用 Wilson/正态近似，带连续性修正）
ci_prop_approx <- prop.test(x_success, n_total,conf.level = 0.95, correct = TRUE)
ci_prop_approx

# 精确区间（binom.test 使用 Clopper-Pearson）
ci_prop_exact <- binom.test(x_success, n_total, p = 0.5, conf.level = 0.95, alternative = "two.sided")
ci_prop_exact  # 检验总体p是不是0.2， 在p=0.3的情况下，观察到这个结果的概率

#输出结果已经包含CI



# ---- two sample proportion: z test 
x1 <- 53; n1 <- 60
x2 <- 34; n2 <- 50
prop_2sample <- prop.test(c(x1, x2), c(n1, n2), conf.level = 0.95, correct = TRUE) # 比例差检验+CI
prop_2sample

prop.test(c(x1, x2), c(n1, n2), alternative = "less",  conf.level = 0.95,correct = TRUE) # H0: P1 >= P2 , H1: P1 < P2
prop.test(c(x1, x2), c(n1, n2), alternative = "greater",  conf.level = 0.95,correct = TRUE) # H0: P1 <= P2 , H1: P1 > P2


# ---- two sample proportion: chi or fisher

tab <- matrix(c(53, 7,  # n of success, n of failure
                34, 16),
              nrow = 2, byrow = TRUE)
colnames(tab) <- c("Success", "Failure")
rownames(tab) <- c("Group1", "Group2")
tab

result_1 <- chisq.test(tab, correct = FALSE)   # without Yates correction
result_2 <- chisq.test(tab)                    # with Yates correction (default for 2x2)
result_1; result_2


# fisher test :for small samples or exact p-value)
fisher.test(tab)



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


## ===============================
## 6) Mantel–Haenszel (OR): analysis confounding and test if the association is still significant after adjustment
## ===============================

a1 <- 216; b1 <- 80;  c1 <- 144; d1 <- 120   # Stratum 1
a2 <- 8;   b2 <- 20;  c2 <- 32;  d2 <- 180   # Stratum 2
# a3 <- 再加一层也可以
tab_crude <- matrix(c(a1+a2, b1+b2,
                      c1+c2, d1+d2),
                    nrow = 2, byrow = TRUE)
tab_crude

a <- tab_crude[1,1]
b <- tab_crude[1,2]
c <- tab_crude[2,1]
d <- tab_crude[2,2]

crude_or <- (a*d)/(b*c)

tab_strata <- array(
  c(a1,b1,c1,d1,
    a2,b2,c2,d2),
  dim = c(2,2,2),
  dimnames = list(
    Exposure = c("E+","E-"),
    Disease  = c("D+","D-"),
    Strata   = c("S1","S2") #"S3"
  )
)
tab_strata
# tab_strata <- array(c(a1,b1,c1,d1,a2,b2,c2,d2,a3,b3,c3,d3),dim = c(2,2,3))


mh <- mantelhaen.test(tab_strata)
mh_or <- as.numeric(mh$estimate)

## -------- 3. 看是否混杂（>10% 变化）--------
pct_change <- (mh_or - crude_or)/crude_or * 100

list(
  crude_OR = crude_or,
  MH_adjusted_OR = mh_or,
  percent_change = pct_change,
  confounding = ifelse(abs(pct_change) > 10,
                       "Yes (evidence of confounding)",
                       "No (little evidence of confounding)")
  
)

mh

## ===============================
## 7) Mantel–Haenszel (RR): analysis confounding and test if the association is still significant after adjustment
## ===============================


a1 <- 52; b1 <- 135; c1 <- 53; d1 <- 235
a2 <- 88; b2 <- 85;  c2 <- 24; d2 <- 48

A <- a1 + a2
B <- b1 + b2
C <- c1 + c2
D <- d1 + d2

crude_rr <- (A / (A + B)) / (C / (C + D))

## 2) MH-adjusted RR（Rothman 公式）
N1 <- a1 + b1 + c1 + d1
N2 <- a2 + b2 + c2 + d2

num <- a1 * (c1 + d1) / N1 + a2 * (c2 + d2) / N2
den <- c1 * (a1 + b1) / N1 + c2 * (a2 + b2) / N2

mh_rr <- num / den

pct_change <- (mh_rr - crude_rr) / crude_rr * 100

confounding <- ifelse(abs(pct_change) >= 10,
                      "Yes, confounding present (≥10% change)",
                      "No, little confounding (<10% change)")

list(
  crude_RR = crude_rr,
  MH_RR = mh_rr,
  percent_change = pct_change,
  confounding = confounding
)


## ===============================
## 8) sign test - for one sample and paired sample
## ===============================

# 基本上就是二项分布
binom.test (x=2, n=11, p=0.5, conf.level =  0.95)  # P = 0.065 
binom.test (x=1, n=8, p=0.5, conf.level =  0.95)  


## ===============================
## 9) Wilcoxon signed rank test - for one sample and paired sample 
## ===============================

x <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770) # duplicated values, rank +0.5
wilcox.test(x, mu = 7725, exact = FALSE, correct = TRUE) # P = 0.0293 , 有参数后面的参数，因为有重复值

x1 <- c(65,72,68,90,77,43,52,80,75)
x2 <- c(84,78,68,89,91,44,66,89,93)

wilcox.test(x1, x2,
            paired = TRUE,
            exact = FALSE,   
            correct = TRUE)  # 1.5 < 3, p = 0.02471 < 0.05


# compare with student t  p = 0.01814
t.test(x, mu = 7725) 

# t-test 在满足其假设（特别是接近正态、无明显离群值）的情况下，利用最多的信息 → 统计效率最高 → 在同样样本量下最易检出差异 → power 最大。




## ===============================
## 10) Wilcoxon （ Mann- Whitney U test in R) - non-para test for two independent samples
## ===============================

#group1 <- c(6130,7050,7480,7480,7530,7580,7900,8080,8090,8110,8400,10150,10880)
#group2 <- c(8390,9190,9210,9680,9690,9970,11510,11850,12790)

group1 <- c(65,72,68,90,77,43,52,80,75)
group2 <- c(84,78,68,89,91,44,66,89,93)

wilcox.test(group1,group2, alternative = "two.sided", paired = FALSE, correct = FALSE)
# p=0.002372 < 0.05 , there is difference in average ranks in the two populations. 
# W = 13, is the number of scores in group with smaller sum of rank. 
# wilcox.test 近似正态分布， 使用了 continuity correction， 矫正过后p更大，更保守

# use coin package to run non-parameter test, have to follow the gramma
install.packages("coin")

x <- group1
y <- group2
g <- factor(c(rep("A", length(x)), rep("B", length(y))))
v <- c(x, y)

library(coin)
wilcox_test(v ~ g, distribution = "exact") 
# coin 使用 exact distribution 精确分布
# coin 计算真实的exact p-value, 能正确处理ties重复值，所以p更小，更精确


# 检验临界值25和88之外的面积是否为0.05
qwilcox(0.025, 9, 13, lower.tail = TRUE, log.p =  FALSE)  
qwilcox(0.025, 9, 13, lower.tail = FALSE, log.p =  FALSE)
pwilcox(29,13,9,lower.tail = TRUE, log.p = FALSE) + pwilcox(88,13,9,lower.tail = FALSE, log.p = FALSE)




