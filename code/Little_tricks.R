install.packages("apisensr")
library(apisensr)
run_app()  # useful for bias evaluation


qt(1 - 0.05/2, df = 19) 
qnorm(0.05/2)
qnorm(1 - 0.05/2)
1 - pt(2.98, df = 19)

dpois(9,3) #概率质量函数 (PMF)：计算 P(X = x)
ppois(9,3) # cumulative 
qpois(0.95,3)

1-ppois(9,3)

# Maximise likehood
n <- 0:6              
p <- n / 6       
likelihood <- dbinom(7, size = 10, prob = p)  
data.frame(n, p, likelihood)  

# when n=4, p=4/6, largest likelihood





