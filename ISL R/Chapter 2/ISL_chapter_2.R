loadhistory()
x<-c(1,2,3)
x
x = c(1,2,3)
y = c(4,5,6)
x+y
length(x)
ls()
x <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2, byrow=TRUE)
x
sqrt(x)
x^2
x=rnorm(50)
y=x+rnorm(50, mean=50, sd=.1)
cor(x,y)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
x=rnorm(100)
y=rnorm(100)
plot(x, y)
pdf("figure.1.pdf")
plot(x, y, xlab="this is the x-axis", ylab="this is the y-axis", main="Plot of X vs Y")
dev.off()
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
y=x
f=outer(x,y, function(x,y) cos(y)/(1+x^2))
jpeg("figure.2.jpg")
contour(x,y,f)
dev.off()
jpeg("figure.3.jpg")
plot.new()
contour(x, y, f, nlevels=45, add=T)
dev.off()
fa=(f-t(f))/2
jpeg("figure.4.jpg")
contour(x, y, fa, nlevels=15)
dev.off()
jpeg("figure.5.jpg")
image(x, y, fa)
dev.off()
jpeg("figure.6.jpg")
persp(x, y, fa)
dev.off()
jpeg("figure.7.jpg")
persp(x, y, fa, theta=30)
dev.off()
jpeg("figure.8.jpg")
persp(x, y, fa, theta=30, phi=20)
dev.off()
jpeg("figure.9.jpg")
persp(x, y, fa, theta=30, phi=70)
dev.off()
jpeg("figure.10.jpg")
persp(x, y, fa, theta=30, phi=40)
dev.off()

outMatrix <- matrix(data = 1:16, nrow = 4, ncol = 4, byrow = FALSE)
outMatrix
outMatrix[2,3]
outMatrix[c(1,3),c(2,4)]
outMatrix[1:3,2:4]
outMatrix[1:2,]
outMatrix[,1:2]
outMatrix[-c(1,3),]
dim(outMatrix)
setwd('C:/Users/j3kk/Machine-learning/')
Auto=read.csv(file = 'Auto.csv', header=T, na.strings="?")
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto = na.omit(Auto)
dim(Auto)
names(Auto)
jpeg("figure.11.jpg")
plot(Auto$cylinders, Auto$mpg)
dev.off()
attach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T),
plot(cylinders, mpg, col="red", varwidth=T, horizontal =T)
jpeg("figure.12.jpg")
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
dev.off()
hist(mpg)
hist(mpg, col=2)
jpeg("figure.13.jpg")
hist(mpg, col=2, breaks=15)
dev.off()

pairs(Auto)
pairs(∼mpg + displacement +horsepower + weight +acceleration , Auto)
plot(horsepower, mpg)
identify(horsepower, mpg, name)
summary(Auto)
summary(mpg)

savehistory()
q()

