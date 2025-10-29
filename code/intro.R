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

# ==== exercise 1 =====

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
car_ds[8,2] <- 100
car_ds


