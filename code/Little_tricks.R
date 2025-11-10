install.packages("apisensr")
library(apisensr)
run_app()  # useful for bias evaluation


qt(1 - 0.05/2, df = 19) 
qt(0.05/2, df = 93) 

qt(0.05/2, df = 166) 


1 - pt(2.99, df = 19)
pnorm(2.99)
dpois(2,21)
ppois(2,21)


2*pt((1242.2-1286)/(256.2/sqrt(94)), df = 93)

qt(0.05/2, df = 93)  # t=1.985
qnorm(0.05/2)

qnorm(1 - 0.05/2)
1 - pt(2.98, df = 19)

1 - pt(2.98, df = 19)

pt(7.72, df = 166)
qt(0.05/2, df = 166)  # t=1.985

dpois(9,3) #概率质量函数 (PMF)：计算 P(X = x)
ppois(9,3) # cumulative 
qpois(0.95,3)

1-ppois(9,3)

# Maximise likehood
n <- 0:6              
p <- n / 6       
likelihood <- dbinom(7, size = 10, prob = p)  
data.frame(n, p, likelihood)  



# two-sample t test
t.test()







