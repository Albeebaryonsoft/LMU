


# ==== dot plot ====
x <- -5:5        
y <- x**2       
plot(x,y,main ="Graphic 1")

x <- fra$BMI
y <- fra$cigsPerDay
plot(x,y,main ="Graphic 2")

# ==== histagram ====

hist(sleep$extra, main = "Graphic 3", col="Grey")

hist(sleep$extra, main = "Graphic 4", breaks = 20, col="2")

hist(fra$totChol,main = "fra - education", col = "pink",breaks = 10)


# ==== box plot ==== 

boxplot(sleep$extra,main="title")  # one

boxplot(extra~group,data=sleep, main="Boxplot", ylab= "extra", xlab="group") # two 

boxplot(ID~group,data=sleep, main="Boxplot", ylab= "extra", xlab="group")


fra <- read.csv("data/framingham.csv")

boxplot(BMI ~ male, data = fra,
        main = "Boxplot 1",
        ylab = "extra",
        xlab = "group")


boxplot(totChol ~ education, data = fra,
        main = "Boxplot 2",
        ylab = "totChol",
        xlab = "TenYearCHD") 

# ==== complex ggplot ANY PLOTS====

install.packages("ggplot2", type = "source")

library(ggplot2)

data(iris, package ="datasets")

ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width, color = Petal.Length, shape = Species)) + geom_point()
# 除了shape之外都是连续变量

fra$education <- as.factor(fra$education)
ggplot(fra, aes(x = age, y = heartRate, color = BMI, shape = education)) +
  geom_point(na.rm = TRUE, alpha = 0.8) +
  labs(x = "age", y = "heartrate", shape = "education", color = "BMI") +
  theme_minimal()


foo <- function(x) sin(x)+cos(x*0.5)

x <-seq(0, 20, len = 100)
y <- foo(x)
g<- ggplot(mapping = aes(x=x,y=y)) + geom_line()
g
g + geom_area()
g + geom_point()
g + geom_line() + geom_area()


library(cowplot)

g_point <- g + geom_point()
g_point_line <- g + geom_point() + geom_line()
g_point_line_color <- g+ geom_line(aes(color = y), linewidth = 2) + geom_point(color = "darkorange")
plot_grid(g, g_point, g_point_line, g_point_line_color, nrow =2, ncol =2, lables = "AUTO")
# combine

data <- mtcars
data$cyl <- as.factor(data$cyl)
ggplot(data, aes(cyl)) + geom_bar(color = "orange")

data$vs <- as.factor(data$vs)
ggplot(data, aes(cyl, fill =vs)) + geom_bar()
ggplot(data, aes(cyl, fill = vs)) + geom_bar(position = "dodge")
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram(bins = 20, alpha = 0.3, position ="identity") #what's position

ggplot(iris, aes(x=Species, y=Sepal.Width, fill = Species)) + geom_boxplot()
ggplot(iris, aes(x=Species, y=Sepal.Width, fill = Species)) + geom_violin()


# ===== exercise 2=========

x <- sample(1:10, 100, replace = TRUE) # 可以重复抽取 TRUE
x <- seq(0,10, length.out = 100)
y <- sin(x)
plot(x,y,main ="Graphic", pch=20)

ggplot(mapping = aes(x=x,y=y)) + geom_line(lty=4)  #不同数字，不同线型
ggplot(mapping = aes(x=x,y=y)) + geom_line(lty=3)

ggplot(mapping = aes(x=x,y=y)) + geom_line() 

# swap the axis
y <- seq(0,10, length.out = 100)
x <- sin(x)
plot(x,y,main ="Graphic 1", pch=20) 

g<- ggplot(mapping = aes(x=x,y=y)) + geom_line()

plot(x,y)
lines(x*4,lwd=2)



x <- c(1:10,1:20,1:30,1:40,1:50)
hist(x)

myline <- c(1:20,30:1)
myline
lines(myline, lwd=2)

df <- data.frame (x=c(rnorm(100),rexp(100)), group = rep(1:2,each = 100))
df # 创造两种不同来源的数据，一个正态分布一个逻辑回归

x

boxplot(x ~ group, data = df,
        main = "Boxplot")

boxplot(df$x, data = df,
        main = "Boxplot")

# rnorm, rexp, sample(), rep()


# excercise 3


# merge of datasets plot
dat<- mtcars
head(dat)
dat1 <- dat[,c(1:6)]
dat2 <- dat[,c(7:11)]
dat1 <- data.frame(ID = 1:32,dat1)
dat2 <- data.frame(id = 1:32, dat2)
dat_merged <- merge(dat1,dat2,by.x = "ID", by.y ="id")
dat_merged



dat <- Orange
tree_colors <- as.numeric(Orange$Tree)



palette_colors <- rainbow(length(unique(Orange$Tree)))
palette_colors

plot(Orange$age, Orange$circumference,
     col = palette_colors, pch= 16, title = "Tree", xlab = "age", ylab = "circumference", main = "Orange Tree Growth")

legend("topleft",legend = levels(Orange$Tree), col = palette_colors ,pch=16, title = "Tree")

library(ggplot2)

if(!requireNamespace("ggplot2", quietly == TRUE)) 
install.packages("ggplot2")
library(ggplot2)

class("circumference")
class("age")
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_point()
ggplot(Orange, aes(age, circumference, color = Tree)) + geom_line()




