##############
# Exercise 3 #
##############


# 1
load("VOC.RData")
VOC[1:5,1:6]

# 2
y<-VOC[,1] # y=1 means "Gram positive", y=0 means "Gram negative"
X<-VOC[,-1]
pvalues<-numeric(199)

for (i in 1:199) 
 {
 pvalues[i]<-t.test(X[y==1,i],X[y==0,i])$p.value
 }

# more elegant solution
pvalues<-apply(X=X,MARGIN=2,FUN=function(x,y) return(t.test(x[y==1],x[y==0])$p.value),y=y)

# 3
my.pvalues_bonf<-pvalues*length(pvalues)
# Set the p-values larger than 1 to 1.
my.pvalues_bonf[my.pvalues_bonf>1]<-1


# 4
# Number of rejected null-hypotheses without adjustment
sum(pvalues<0.05)
# Number of rejected null-hypotheses with Bonferroni adjustment
sum(my.pvalues_bonf<0.05)

# 5
pvalues_bonf<-p.adjust(pvalues,method="bonferroni")
pvalues_holm<-p.adjust(pvalues,method="holm")
pvalues_bh<-p.adjust(pvalues,method="BH")

# Compare the 20 smallest p-values 
# with Bonferroni adjustment
sort(pvalues_bonf)[1:20]
# with Bonferroni adjustment "by hand"
sort(my.pvalues_bonf)[1:20]
# with Holm adjustment
sort(pvalues_holm)[1:20]
# with Benjamini-Hochberg
sort(pvalues_bh)[1:20]

# 6
sum(pvalues_holm<0.05)
sum(pvalues_bh<0.05)




