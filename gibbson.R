fx <- function(y1,y2,y3) {
  teta12 <- 1
  teta23 <- 2
  teta31 <- 3
  return(exp(-(y1+y2+y3+teta12*y1*y2+teta23*y2*y3+teta31*y3*y1)))
}
y1<-5
y2<-2
y3<-7
#условные распределения 
for (j in c(1:300)) {
fx1<-c()
for (i in seq(-2,2,0.01)) {
  fx1<-c(fx1,fx(i,y2,y3))
}
y1<-sampling(fx1)

fx2<-c()
for (i in seq(-2,2,0.01)) {
  fx2<-c(fx2,fx(y1,i,y3))
}
y2<-sampling(fx2)

fx3<-c()
for (i in seq(-2,2,0.01)) {
  fx3<-c(fx3,fx(y1,y2,i))
}
y3<-sampling(fx3)
}
sampling <- function(f) { #выборка с отклонением используется тогда, 
  hs<-hist(f)                             #когда форма f(x) делает семплирование напрямую сложным
  pyu<-10
  y<-1
  py<-1
  vy<-c()
  while(pyu>py) {
    y<-runif(1,0,10000)
    prov<-FALSE
    for (i in c(1:length(hs$breaks)-1)) {
      if((hs$breaks[i]<y)&&(y<=hs$breaks[i+1])){
        py<-hs$density[i]
        prov<-TRUE
      }
    }
    if(prov==FALSE) {
      py<-hs$density[length(hs$density)]
    }
    pyu<-runif(1)
  }
  return(y)
}
print(c(y1,y2,y3))