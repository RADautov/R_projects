library(fitdistrplus)
library(MASS)
library("matlab")

drch <- rchisq(100,df = 10)
ysqr<-quadraticForm(10)

plot(density(rchisq(100,10)), main = "Ф-ии плотности хи-квдрат: 'истиная' и смоделированная")
lines(density(ysqr), col = 'red')

pkolmogorov1x <- function(x, n) {
  if (x <= 0) 
    return(0)
  if (x >= 1) 
    return(1)
  j <- seq.int(from = 0, to = floor(n * (1 - x)))
  1 - x * sum(exp(lchoose(n, j) + (n - j) * log(1 - x - j/n) + (j - 1) * log(x + j/n)))
}

kd<-c()
for (i in seq(0,1,0.05)) {
  kd<-c(pkolmogorov1x(i,20),kd)
}

plot(density(kd))
plot(density(yl), col = "red")
lines(density(rchisq(100,df = p))) 
plot(kd)
######
sy<-sort(yl)
sch<-sort(rchisq(100,df=p))
dn <- sch - yl

plot((dn))


kl<-c()
for(i in seq(0,1,0.01)) {
  kl<-c(kolmogor(i),kl)
}
plot(density(kl))

seq(0,1,0.01)#эмперическая функция распределения 
n <- length(yl)
x <- sort(yl); vals <- unique(x)
rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, 
                  method = "constant", yleft = 0, yright = 1, f = 0,
                  ties = "ordered")
plot(ecdf(yl))
yl <- quadraticForm(10)
plot(yl)
plot(pkolmogorov1x(ecdf(yl),length(yl)))
drch <- rchisq(40,df = 10)
yl<-quadraticForm(10)
#Dn - minMax статистика Колмогорова 
require(ggplot2)
sample1 <- yl # моя квадратичная форма
sample2 <- drch #rchisq
group <- c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
dat <- data.frame(KSD = c(sample1,sample2), group = group)
# create ECDF of data
cdf1 <- ecdf(sample1) 
cdf2 <- ecdf(sample2) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

# png(file = "c:/temp/ks.png", width = 1024, height = 768, type="cairo-png")
ggplot(dat, aes(x = KSD, group = group, color = group))+
  stat_ecdf(size=1) +
  theme_bw(base_size = 28) +
  theme(legend.position ="top") +
  xlab("Sample") +
  ylab("ECDF") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=8) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=8) +
  ggtitle("Dn: Y / Chi-square, p = 10 ") +
  theme(legend.title=element_blank())

library(mvtnorm)

# Some mean vector and a covariance matrix
mu <- colMeans(iris[1:50, -5])
cov <- cov(iris[1:50, -5])

# genrate n = 100 samples
sim_data <- rmvnorm(n = 100, mean = mu, sigma = cov)

# visualize in a pairs plot 
pairs(sim_data)

