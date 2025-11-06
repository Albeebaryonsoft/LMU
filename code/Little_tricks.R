install.packages("apisensr")
library(apisensr)
run_app()  # useful for bias evaluation


qt(1 - 0.05/2, df = 26) 

# merge of datasets plot
dat<- mtcars
head(dat)
dat1 <- dat[,c(1:6)]
dat2 <- dat[,c(7:11)]
dat1 <- data.frame(ID = 1:32,dat1)
dat2 <- data.frame(id = 1:32, dat2)
dat_merged <- merge(dat1,dat2,by.x = "ID", by.y ="id")
dat_merged




