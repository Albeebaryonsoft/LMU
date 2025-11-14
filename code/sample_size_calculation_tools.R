
if (!requireNamespace("pwr", quietly = TRUE)) install.packages("pwr")
library(pwr)

## ---- 1) t 检验：单样本 / 双样本 / 配对 ----
## 采用 Cohen's d 效应量
## d = |均值差| / SD ；配对 t 的 d 用配对差值的 SD

ss_t_one   <- function(d, alpha = 0.05, power = 0.80,
                       alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  pwr.t.test(d = d, sig.level = alpha, power = power,
             type = "one.sample", alternative = alternative)
}

ss_t_two   <- function(d, alpha = 0.05, power = 0.80,
                       alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  pwr.t.test(d = d, sig.level = alpha, power = power,
             type = "two.sample", alternative = alternative)
}

ss_t_paired <- function(d, alpha = 0.05, power = 0.80,
                        alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  pwr.t.test(d = d, sig.level = alpha, power = power,
             type = "paired", alternative = alternative)
}


## ---- 2) z 检验：单比例 / 双比例（独立）/ 配对比例（McNemar） ----
## A) 单样本比例：检验 H0: p = p0 vs H1: p ? p0
## 使用常见正态近似的样本量公式；可单侧/双侧

ss_prop_one <- function(p1, p0, alpha = 0.05, power = 0.80,
                        alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  z_alpha <- if (alternative == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  ## 近似公式（教科书常用；Wald-型）
  num <- ( z_alpha * sqrt(p0*(1-p0)) + z_beta * sqrt(p1*(1-p1)) )^2
  den <- (p1 - p0)^2
  n   <- ceiling(num / den)
  out <- list(method = "One-sample proportion (z, normal approx.)",
              alternative = alternative, alpha = alpha, power = power,
              p1 = p1, p0 = p0, n = n)
  class(out) <- "htest"; out
}

## B) 两独立样本比例：用 pwr 的 arcsin 变换（更稳健）
## h = 2*asin(sqrt(p1)) - 2*asin(sqrt(p2))

ss_prop_two <- function(p1, p2, alpha = 0.05, power = 0.80,
                        alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  h <- 2*asin(sqrt(p1)) - 2*asin(sqrt(p2))
  pwr.2p.test(h = h, sig.level = alpha, power = power,
              alternative = alternative)   ## 返回每组所需样本量 n
}

## C) 配对样本比例：McNemar（只取不一致格 b 与 c）
## 令 p10 = 正->负 的概率，p01 = 负->正 的概率（基于同一受试者前后对）
## 经典无连续性校正近似： n = ((z_{1-α*} + z_{1-β})^2 * (p01 + p10)) / (p10 - p01)^2
## 双侧用 α/2；单侧用 α

ss_prop_paired_mcnemar <- function(p10, p01, alpha = 0.05, power = 0.80,
                                   alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  z_alpha <- if (alternative == "two.sided") qnorm(1 - alpha/2) else qnorm(1 - alpha)
  z_beta  <- qnorm(power)
  num <- (z_alpha + z_beta)^2 * (p01 + p10)
  den <- (p10 - p01)^2
  n   <- ceiling(num / den)
  out <- list(method = "Paired proportions (McNemar, normal approx.)",
              alternative = alternative, alpha = alpha, power = power,
              p10 = p10, p01 = p01, n_pairs = n)
  class(out) <- "htest"; out
}

## ---- 3) 统一入口函数（可选） ----
## type: "t_one","t_two","t_paired","prop_one","prop_two","prop_paired"
ss_calc <- function(type, ..., alpha = 0.05, power = 0.80,
                    alternative = c("two.sided","less","greater")) {
  alternative <- match.arg(alternative)
  switch(type,
         t_one   = ss_t_one(..., alpha = alpha, power = power, alternative = alternative),
         t_two   = ss_t_two(..., alpha = alpha, power = power, alternative = alternative),
         t_paired= ss_t_paired(..., alpha = alpha, power = power, alternative = alternative),
         prop_one= ss_prop_one(..., alpha = alpha, power = power, alternative = alternative),
         prop_two= ss_prop_two(..., alpha = alpha, power = power, alternative = alternative),
         prop_paired = ss_prop_paired_mcnemar(..., alpha = alpha, power = power, alternative = alternative),
         stop("Unknown type")
  )
}


## t 检验
ss_t_one(d = 0.5, power = 0.8, alpha = 0.01)                       # 单样本 t
ss_t_one(d = 0.2)                                                  # 单样本 t

ss_t_two(d = 0.4, alternative="greater")# 双样本 t（右尾）
ss_t_two(d = 0.5)                       # 双样本 t（右尾）默认设定 替代假设：两个样本参数不相等

ss_t_paired(d = 0.3, power = 0.9)       # 配对 t

## 比例
ss_prop_one(p1 = 0.60, p0 = 0.50)                    # 单比例 z（双侧）

ss_prop_two(p1 = 0.70, p2 = 0.55, alternative="less") # 两比例 z（左尾），返回每组，所以总数要✖️2
ss_prop_two(p1 = 0.15, p2 = 0.05, power = 0.8, alpha = 0.05, alternative = "two.sided")    # 两比例 z，Cohen’s h，arcsine

power.prop.test(p1 = 0.15, p2 = 0.05, power = 0.80, sig.level = 0.05, alternative = "two.sided") # 两比例 Wald

ss_prop_paired_mcnemar(p10 = 0.15, p01 = 0.05)       # McNemar 配对比例


## 统一入口
ss_calc("t_two", d = 0.5, power = 0.9)
ss_calc("prop_one", p1 = 0.58, p0 = 0.50, alternative = "greater")


