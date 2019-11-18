t<-c()
tn<- c()
ind<-c()
 for (i in seq(2,22,1)) {
   
  for (j in seq(0,2,0.1)) {
    t <- dn(quadraticForm(i+j), rchisq(100,i))
  }
  tn<- c(tn, min(t))
  ind<-c(ind,which.min(t)/100 +i)
  t<-c()
 }

plot(ecdf(tn))
plot(ecdf(kd))
ks.test(tn,)
as.array(tn)
dn <- function(yl,drch) {
  #Dn - minMax статистика Колмогорова 
  require("ggplot2")
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
  return(abs(y1-y0))
}

