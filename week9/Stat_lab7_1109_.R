# 107-1 Statistics Lab7: Estimating means with confidence


# Lab#1: t distribution with different df ---------------------------------

# t-distribution with df = 30
curve(dt(x, df=30), from = -3, to = 3, lwd = 4, ylab = "y")

# t-distribution with different df
curve(dt(x, df=2), from = -3, to = 3, lwd = 2, ylab = "y", add = T, col = "gray86")
curve(dt(x, df=5), from = -3, to = 3, lwd = 2, ylab = "y", add = T, col = "gray35")

ind = c(1,2,3,5,10,100)
for (i in ind) {
  curve(dt(x, df=i), from = -3, to = 3, lwd = 1.5, ylab = "y", add = T, col = "gray")
}

# standard normal distribution
curve(dnorm(x), from = -3, to = 3, lwd = 3, ylab = "y", add = T, col = "red")



# Lab#2: t-distribution functions ---------------------------------------
#qt-知道面積求t-score
qt(0.01, df = 10, lower.tail = FALSE)
qt(0.01, df = 200, lower.tail = FALSE)
#qnorm-知道面積求z-score
qnorm(0.01, lower.tail = FALSE)

#pt-知道t-score求面積
pt(2, df = 10, lower.tail = FALSE)
pt(2, df = 200, lower.tail = FALSE)
#normt-知道z-score求面積
pnorm(2, lower.tail = FALSE)


# Lab#3: Find the confidence interval -------------------------------------

x <- rnorm(20) #one sampling
cc <- t.test(x, alternative = "two.sided", conf.level=0.95) #t.test(): t檢定函數
cc
CI_Low <- cc$conf.int[1]; CI_Low
CI_High <- cc$conf.int[2]; CI_High



# 11.32 example p.246 -----------------------------------------------------

plot(c(0,3),type='n',xlim=c(6,9),xlab='Mean hour of sleep',ylab='',axes=F,
     main='Comparsion of the two confidence interval')

lines(c(7.1,8.22),c(2,2))
text(6.6,2,labels='Stat10')
lines(c(7.1,7.1),c(1.95,2.05))
lines(c(7.66,7.66),c(1.95,2.05))
lines(c(8.22,8.22),c(1.95,2.05))

lines(c(6.53,7.09),c(1,1))
text(6.3,1,labels='Stat13')
lines(c(6.53,6.53),c(0.95,1.05))
lines(c(6.81,6.81),c(0.95,1.05))
lines(c(7.09,7.09),c(0.95,1.05))

axis(1)

