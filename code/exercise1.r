##############
# Exercise 1 #
##############

# 1 Simulate the data

# diseases
diabetes<- rbinom(10000,1,prob=0.1)
asthma<- rbinom(10000,1,prob=0.1)
cancer<- rbinom(10000,1,prob=0.1)
ms<- rbinom(10000,1,prob=0.1)
thyroid<- rbinom(10000,1,prob=0.1)
liver<- rbinom(10000,1,prob=0.1)
arthritis<- rbinom(10000,1,prob=0.1)

# candidate risk factors
pisces<- rbinom(10000,1,prob=0.05)
purple<- rbinom(10000,1,prob=0.05)
from0to1<- rbinom(10000,1,prob=0.05)
redhair<- rbinom(10000,1,prob=0.05)
firstnameC<- rbinom(10000,1,prob=0.05)
summer<- rbinom(10000,1,prob=0.05)

# 2 Perform the tests 
# diabetes
chisq.test(diabetes,pisces)$p.value
chisq.test(diabetes,purple)$p.value
chisq.test(diabetes,from0to1)$p.value
chisq.test(diabetes,redhair)$p.value
chisq.test(diabetes,firstnameC)$p.value
chisq.test(diabetes,summer)$p.value
# asthma
chisq.test(asthma,pisces)$p.value
chisq.test(asthma,purple)$p.value
chisq.test(asthma,from0to1)$p.value
chisq.test(asthma,redhair)$p.value
chisq.test(asthma,firstnameC)$p.value
chisq.test(asthma,summer)$p.value
# cancer
chisq.test(cancer,pisces)$p.value
chisq.test(cancer,purple)$p.value
chisq.test(cancer,from0to1)$p.value
chisq.test(cancer,redhair)$p.value
chisq.test(cancer,firstnameC)$p.value
chisq.test(cancer,summer)$p.value
# MS
chisq.test(ms,pisces)$p.value
chisq.test(ms,purple)$p.value
chisq.test(ms,from0to1)$p.value
chisq.test(ms,redhair)$p.value
chisq.test(ms,firstnameC)$p.value
chisq.test(ms,summer)$p.value
# thyroid D
chisq.test(thyroid,pisces)$p.value
chisq.test(thyroid,purple)$p.value
chisq.test(thyroid,from0to1)$p.value
chisq.test(thyroid,redhair)$p.value
chisq.test(thyroid,firstnameC)$p.value
chisq.test(thyroid,summer)$p.value
# liver
chisq.test(liver,pisces)$p.value
chisq.test(liver,purple)$p.value
chisq.test(liver,from0to1)$p.value
chisq.test(liver,redhair)$p.value
chisq.test(liver,firstnameC)$p.value
chisq.test(liver,summer)$p.value
# arthritis
chisq.test(arthritis,pisces)$p.value
chisq.test(arthritis,purple)$p.value
chisq.test(arthritis,from0to1)$p.value
chisq.test(arthritis,redhair)$p.value
chisq.test(arthritis,firstnameC)$p.value
chisq.test(arthritis,summer)$p.value