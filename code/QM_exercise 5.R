
# QM excercise 5

# question 2, combined 5,7,8,9
# two variances are unequal
xbar1 <- 1.689
s1 <- sqrt(0.248)
n1 <- 27

xbar2 <- 2.307
s2 <- sqrt(0.511)
n2 <- 23

alpha <- 0.05   # 95% CI
dhat <- xbar1 - xbar2
SE <- sqrt(s1^2/n1 + s2^2/n2)

df <- (s1^2/n1 + s2^2/n2)^2 /
  ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))

tcrit <- qt(1 - alpha/2, df)
CI <- c(dhat - tcrit * SE,
        dhat + tcrit * SE)

p_value <- 2 * (1 - pt(abs(t_stat), df))

t_stat
p_value
CI

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

d_equal <- 2* (tcrit_equal * SE_equal)
d_unequal <- 2* (tcrit * SE)


d_equal;d_unequal

  
# 双样本方差相等性检验（方差比检验），对应 F 分布
# H0: Var(x1) = Var(x2)
# 参数可调版
s1 <- sqrt(0.248)      
s2 <- sqrt(0.511)     
n1 <- 27     
n2 <- 23     
alpha <- 0.05   

F_stat <- (s1^2) / (s2^2)
df1 <- n1 - 1
df2 <- n2 - 1

p_value <- 2 * min(
  pf(F_stat, df1, df2),        # 左尾
  1 - pf(F_stat, df1, df2)     # 右尾
)

p_value


# 方差比 σ1²/σ2² 的置信区间---------------
F_lower <- F_stat / qf(1 - alpha/2, df1, df2)
F_upper <- F_stat / qf(alpha/2, df1, df2)
CI <- c(F_lower, F_upper)
CI

# problem 3
# paired t 参数可调整版

dbar <- 0.199      
sd_d <- sqrt(0.0568)   
n <- 15
alpha <- 0.05

SE <- sd_d / sqrt(n)
df <- n - 1
t_stat <- dbar / SE
p_value <- 2*(1 - pt(abs(t_stat), df))

tcrit <- qt(1 - alpha/2, df)
CI <- c(dbar - tcrit * SE, dbar + tcrit * SE)

t_stat; p_value; CI






