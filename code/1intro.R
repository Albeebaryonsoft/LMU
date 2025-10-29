# ===== Course ==== #
1+2
3**2 # times
x <- -1
y <- -2
z <- 5
x*y

ls()
ls.str()

log(9,3) #= 2, 3 is the base

sum(c(TRUE,FALSE,TRUE))
sum(!c(TRUE,FALSE,TRUE)) # reverse


a <- c("a", "a.  d","v")
b <- c(1,2,3,4)
b+b
b*b
-b

a <- c(1,2,NA)
log(a)
a+1


1:3
4:-3
seq(4)
seq(-10,10, by = 2)
seq(-10,10,length.out = 10)

e <- c(12,353,685,24)
e[2]

selection <- c(1,4)
e[selection]
e[c(1,4)]
e[1:3]

1==2
1!=2
1 >= 2
!TRUE

1 == "2"
1 == "1"
c(T,F,T) == c(F,F,T)
c(T,F,T)  & c(F,F,T)
identical(c(T,F,T),c(F,F,T))

#filter
e[e < 100]
e[(e < 100) & (e >20)]
e[(e < 100) | (e >600)]

x<- 1:5
x[1] <-2
x

n <- append (e, c(777,888,999))
sort(n)
unique(n)
sum(n)

colors <- c("blue","red","blue","green")
colors
class(colors) # charactor

colors <- as.factor(colors)
colors #Levels: blue green red
levels(colors)
class(colors) # factor

x <- 1:4
(x_rbind <- cbind (x,x))
(x_cbind <- rbind (x,x))

dim(x_rbind)
dim(x_cbind)
nrow(x_rbind)
ncol(x_rbind)


(m <- matrix(1:6 ,nrow =3, ncol =2))
matrix(1:6 ,nrow =3, ncol =2, byrow = T)
array(1:12, dim = c(2,2,3)) # 3 dimentions

m[2,2]
m[nrow(m),ncol(m)]
m[,2]
m[2,]

l <- list(1, "2", 1:5, list(m))
l
l[4]

l <- list(hight = 1, gender =c("2"), phone = 1:5) #  not working task = list(m)
names(l)

l$gender
l$phone
l[2:3] <-NULL # replace element 2 and 3 with null
l

col <- as.factor(c("blue","red","green", "green","white","blue", "red"))
pri <- c(10,20,9,50,15,160,100) 
is_el <- c(F,F,F,T,F,T,T)

car_ds <- data.frame(color=col, price =pri,is_electric = is_el)
car_ds
str(car_ds)

car_ds[car_ds$color == "red", ] # pick the red 
car_ds[car_ds$price <= 50,"color"] 

car_ds[7,2] <-89
car_ds

# load data

data <-read.csv("data/framingham.csv")
head(data)
str(data)
data[data$currentSmoker == 1,]


# ==== exercise 1 =====


a <- 2
log(1000,base=10)
sin <- a
sin
sin(sin)
sin(1)^2 + cos(1)^2
b <- TRUE
b+1
sqrt(b)
b+b
c <- 1
c+1

v <- c(2, 4, 5, 6, 4, -1)
v+1
-v  #v not working
v[2]
v[-6]  #both works
v[1:5]
length(v[v=4]) # how many values of 4

v[v<0] <- -v[v<0] # Swap the sign of all negative values.
v

f1 <- sample(x=LETTERS, size =5) # random sampling
f1

sample(c("A", "B", "C"), 10, replace = TRUE, prob = c(0.6, 0.3, 0.1))
class(f1)

f2 <- factor(f1) #factor
f2

f3 <- c(sample(1:10, 5)) #vector
f3

L <- list(f1,f2,f3)
L
str(L)
L[3]
L[[3]]

df1 <- data.frame(student = f1,grade =f2, year =f3) # same

df1 <- data.frame(L)
colnames(df1) <- c("student","grade","year") #same

df1

df1[2,]
df1[,2]
df1[2:4,]

df1
write.csv(df1, "data/mydf.csv", row.names=FALSE) # seperated by ,
#write.csv2(df1,"data/mydf.csv", row.names=FALSE) # seperated by ;



df2 <- read.csv("mydf.csv")
identical(df1, df2) #false,strict comparison
all.equal(df1,df2) # true
str(df1)
str(df2)

