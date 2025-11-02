
dpois(9,3) #概率质量函数 (PMF)：计算 P(X = x)
ppois(9,3) # cumulative 
qpois(0.95,3)

1-ppois(9,3)

dpois(5,3)

ppois(4,1)
1-ppois(4,1)


n <- 0:6              
p <- n / 6       
likelihood <- dbinom(7, size = 10, prob = p)  
data.frame(n, p, likelihood)  

# when n=4, p=4/6, largest likelihood

# ==== plot ====
x <- -5:5        
y <- x**2       
plot(x,y,main ="Graphic 1")

hist(sleep$extra, main = "Graphic 3", col="Grey")
hist(sleep$extra, main = "Graphic 4", breaks = 20, col="2")

boxplot(sleep$extra,main="title")
sleep

boxplot(extra~group,data=sleep, main="Boxplot", ylab= "extra", xlab="group")
boxplot(ID~group,data=sleep, main="Boxplot", ylab= "extra", xlab="group")



fra <- read.csv("data/framingham.csv")
fra
boxplot(age ~ male, data = fra,
        main = "Boxplot 1",
        ylab = "extra",
        xlab = "group")



boxplot(totChol ~ TenYearCHD, data = fra,
        main = "Boxplot 2",
        ylab = "totChol",
        xlab = "TenYearCHD") + 

install.packages("ggplot2")
library(ggplot2)

data(iris, package ="datasets")

iris
ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width, color = Petal.Length, shape = Species)) + geom_point()
fra

# 按心率区间分箱（可按需要改区间）
fra$heartRate_cat <- cut(
  fra$heartRate,
  breaks = c(-Inf, 60, 70, 80, 90, Inf),
  labels = c("≤60", "61–70", "71–80", "81–90", "≥91")
)

ggplot(fra, aes(x = BMI, y = cigsPerDay, color = age, shape = heartRate_cat)) +
  geom_point(na.rm = TRUE, alpha = 0.8) +
  labs(x = "BMI", y = "Cigarettes per Day", shape = "Heart Rate", color = "Age") +
  theme_minimal()

foo <- function(x) sin(x)+cos(x*0.5)

x <-seq(0, 20, len = 100)
y <- foo(x)
g<- ggplot(mapping = aes(x=x,y=y)) + geom_line()

g + geom_area()
g + geom_point()
g + geom_line() + geom_area()


library(cowplot)

g_point <- g + geom_point()
g_point_line <- g + geom_point() + geom_line()
g_point_line_color <- g+ geom_line(aes(color = y), linewidth = 2) + geom_point(color = "darkorange")
plot_grid(g, g_point, g_point_line, g_point_line_color, nrow =2, ncol =2, lables = "AUTO")


data <- mtcars
data$cyl <- as.factor(data$cyl)
ggplot(data, aes(cyl)) + geom_bar()

data$vs <- as.factor(data$vs)
ggplot(data, aes(cyl, fill =vs)) + geom_bar()
ggplot(data, aes(cyl, fill = vs)) + geom_bar(position = "dodge")
 # cyl只能计数

ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram(bins = 20, alpha = 0.3, position ="identity") #what's position

ggplot(iris, aes(x=Species, y=Sepal.Width, fill = Species)) + geom_boxplot()

ggplot(iris, aes(x=Species, y=Sepal.Width, fill = Species)) + geom_violin()


# ===== exercise 2=========
x <- sample(1:10, 100, replace = TRUE) # 可以重复抽取 TRUE
x <- seq(0,10, length.out = 100)
y <- sin(x)
plot(x,y,main ="Graphic", pch=20)

ggplot(mapping = aes(x=x,y=y)) + geom_line(lty=2)  ## lty 虚线
ggplot(mapping = aes(x=x,y=y)) + geom_line() 

# swap the axis
y <- seq(0,10, length.out = 100)
x <- sin(x)
plot(x,y,main ="Graphic 1", pch=20) 

g<- ggplot(mapping = aes(x=x,y=y)) + geom_line()

plot(x,y)
lines(x**2,lwd=2)



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








