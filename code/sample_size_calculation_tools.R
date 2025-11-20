
if (!requireNamespace("pwr", quietly = TRUE)) install.packages("pwr")
library(pwr)

## =========================================================
## 样本量计算函数合集：t 检验 + 比例检验（6 大模块）
## 依赖：pwr 包
## =========================================================

library(pwr)

## -------------------------------------
## ① 单样本 t 检验（One-sample t-test）
## -------------------------------------
## 目的：检验单个均值是否与已知 μ0 不同
## 效应量：d = |μ - μ0| / SD

ss_t_one <- function(d, alpha = 0.05, power = 0.80,
                     alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  pwr.t.test(d = d,
             sig.level = alpha,
             power     = power,
             type      = "one.sample",
             alternative = alternative)
}

## 示例：
ss_t_one(d = 0.5, alpha = 0.05, power = 0.80)


## -------------------------------------
## ② 双样本 t 检验（Two-sample t-test）
## -------------------------------------
## 目的：比较两个独立组的均值
## 效应量：d = |μ1 − μ2| / SD_pooled

ss_t_two <- function(d, alpha = 0.05, power = 0.80,
                     alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  pwr.t.test(d = d,
             sig.level = alpha,
             power     = power,
             type      = "two.sample",
             alternative = alternative)
}

## 示例：
## 双侧：
ss_t_two(d = 0.4, alpha = 0.05, power = 0.80)
## 双侧：
ss_t_two(d = 0.25, alpha = 0.05, power = 0.80)

## 单侧：
ss_t_two(d = 0.5, alpha = 0.05, power = 0.80, alternative = "greater")

## -------------------------------------
## ③ 配对 t 检验（Paired t-test）
## -------------------------------------
## 目的：同一受试者前后对比
## 效应量：d = mean(diff) / SD(diff)

ss_t_paired <- function(d, alpha = 0.05, power = 0.80,
                        alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  pwr.t.test(d = d,
             sig.level = alpha,
             power     = power,
             type      = "paired",
             alternative = alternative)
}

## 示例：
ss_t_paired(d = 0.3, power = 0.90)


## ----------------------------------------------------
## ④ 单样本比例检验（One-sample proportion z-test）
## ----------------------------------------------------
## 目的：检验单个比例 p 是否与 p0 不同
## 使用正态近似公式（教材常用）

ss_prop_one <- function(p1, p0, alpha = 0.05, power = 0.80,
                        alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  
  z_alpha <- if (alternative == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  
  num <- ( z_alpha * sqrt(p0*(1-p0)) +
             z_beta  * sqrt(p1*(1-p1)) )^2
  den <- (p1 - p0)^2
  n   <- ceiling(num / den)
  
  structure(list(method      = "One-sample proportion (z approximation)",
                 p1          = p1,
                 p0          = p0,
                 alpha       = alpha,
                 power       = power,
                 alternative = alternative,
                 n           = n),
            class = "power.htest")
}

## 示例：
ss_prop_one(p1 = 0.15, p0 = 0.05)



## ----------------------------------------------------
## ⑤ 双样本比例检验（Two-sample proportion z-test，比老师给的z计算要更加好计算，手算的时候用）
## ----------------------------------------------------
## 目的：比较两个独立组比例 p1 vs p2

ss_prop_two <- function(p1, p2, alpha = 0.05, power = 0.80,
                        alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  
  p_bar  <- (p1 + p2) / 2
  z_alpha <- if (alternative == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  
  num <- ( z_alpha * sqrt(2 * p_bar * (1 - p_bar)) +
             z_beta  * sqrt(p1*(1-p1) + p2*(1-p2)) )^2
  den <- (p1 - p2)^2
  n   <- ceiling(num / den)   # 每组样本量
  
  structure(list(method      = "Two-sample proportion (z approximation)",
                 p1          = p1,
                 p2          = p2,
                 alpha       = alpha,
                 power       = power,
                 alternative = alternative,
                 n_per_group = n),
            class = "power.htest")
}

## 示例：
ss_prop_two(p1 = 0.15, p2 = 0.05)


## --------------------------------------------------------
## ⑥ 配对比例检验（McNemar Test, paired proportions）
## --------------------------------------------------------
## 目的：前后是否改变；或配对 case–control
## p10 = 从 1 变 0 的比例
## p01 = 从 0 变 1 的比例
## 只用“改变”的人：p10 + p01

ss_prop_mcnemar <- function(p10, p01, alpha = 0.05, power = 0.80,
                            alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  
  z_alpha <- if (alternative == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  
  num <- (z_alpha + z_beta)^2 * (p10 + p01)
  den <- (p10 - p01)^2
  n   <- ceiling(num / den)   # 配对数（pairs）
  
  structure(list(method      = "McNemar paired proportions (normal approx.)",
                 p10         = p10,
                 p01         = p01,
                 alpha       = alpha,
                 power       = power,
                 alternative = alternative,
                 n_pairs     = n),
            class = "power.htest")
}

## 示例：
ss_prop_mcnemar(p10 = 0.15, p01 = 0.05)

## =========================================================
## 使用方法：
## 1. source("sample_size_functions.R")
## 2. 直接调用上面的 ss_* 函数查看所需样本量
## =========================================================



# 老师没有讲配对样本t的样本量计算方法，是因为需要知道组内相关性 ρ（课堂无法提供）

###################################################
### alpha and beta, and sample size calculation ###
###################################################

install.packages("ggplot2")
library(ggplot2)

## 1) Bonferroni Correction: given alpha and sample size, calculate beta and draw distribution
d <- 0.5
n <- 100 # assume the sample size is 64,100, 200... power 会变大

SE <- sqrt(2/n)
NCP <- d / SE
k = 3 # k is number of multiple testing

alpha1  <- 0.05/k  # Bonferroni Correction
crit  <- qnorm(1 - alpha1/2)      

## x range
x <- seq(-4, 6, length.out = 2000)

## distributions
df <- data.frame(
  x = x,
  H0 = dnorm(x, mean = 0, sd = 1),
  H1 = dnorm(x, mean = NCP, sd = 1)
)

## Regions for shading
# Type I error region (alpha): H0 in rejection region
shade_alpha_right <- subset(df, x >= crit)
shade_alpha_left  <- subset(df, x <= -crit)

# Power region (1 - beta): H1 in rejection region
shade_power_right <- subset(df, x >= crit)
shade_power_left  <- subset(df, x <= -crit)

# Type II error region (beta): H1 in acceptance region
shade_beta <- subset(df, x > -crit & x < crit)

## Calculate alpha, beta, power
alpha <- (1 - pnorm(crit)) + pnorm(-crit)
beta  <- pnorm(crit, mean=NCP, sd=1) - pnorm(-crit, mean=NCP, sd=1)
power <- 1 - beta

alpha; beta; power

## ---------- Plot ----------
ggplot() +
  # H0
  geom_line(data=df, aes(x=x, y=H0), linewidth=1) +
  # H1
  geom_line(data=df, aes(x=x, y=H1), linewidth=1, color="red") +
  
  # Alpha (Type I): blue shading
  geom_area(data=shade_alpha_right, aes(x=x, y=H0),
            fill="blue", alpha=0.3) +
  geom_area(data=shade_alpha_left, aes(x=x, y=H0),
            fill="blue", alpha=0.3) +
  
  # Power (1 - beta): H1 in rejection region (red shading)
  geom_area(data=shade_power_right, aes(x=x, y=H1),
            fill="red", alpha=0.3) +

  # Beta: green shading
  geom_area(data=shade_beta, aes(x=x, y=H1),
            fill="green", alpha=0.3) +
  
  # Critical lines
  geom_vline(xintercept=c(-crit, crit), color="blue", linetype="dashed") +
  
  labs(
    x="Z",
    y="Density",
    title=paste0("H0: N(0,1) vs H1: N(", round(NCP,2), ",1)"),
    subtitle=paste0(
      "Alpha = ", round(alpha,3),
      "   |   Beta = ", round(beta,3),
      "   |   Power = ", round(power,3)
    )
  ) +
  theme_minimal(base_size = 14)




# 2）Bonferroni Correction: given cohen's d, corrected alpha, expected beta, calculate sample size

alpha_exp <- 0.05   # experiment-wide alpha
k         <- 1     # number of tests
alpha_i   <- alpha_exp / k   # Bonferroni 校正后的 per-test alpha

power     <- 0.80
beta      <- 1 - power

d         <- 0.5    # Cohen's d (真实效应大小，以 SD 为单位)

## 对应的 z 值
z_alpha <- qnorm(1 - alpha_i/2)  # z_{1 - α_i/2} for two-sided test
z_beta  <- qnorm(power)          # z_{1 - β}，因为 power = 1 - β

z_alpha
z_beta

## Z 近似公式计算每组样本量
n_per_group <- 2 * (z_alpha + z_beta)^2 / d^2
n_per_group
ceiling(n_per_group)  # 向上取整作为最终每组样本量


