quadraticForm <- function(p){
  yl <- c()
  for (i in seq(1,p*p,1)) {
    y<-c()
    mu <- rnorm(p) #мат ожидание
    mcov <- matrix(data = rnorm(p*p,0,1), nrow = p, ncol = p, byrow = T) 
    mcov <- 1/(p-1) *t(mcov) %*% mcov #оценка ковариационной матрицы
    A<-t(chol(mcov))
    nu<-rnorm(p,0,1) #возможно она(с.в.) должна быть в (0,1]???
    x <- A %*% nu + mu #линейное преобразование вектора nu в 
    y <- t(x-mu) %*% ginv(mcov) %*% (x-mu) #квадратичная форма 
    yl<-c(yl,y)
  }
  return(yl)
}
plot(ecdf(quadraticForm(10)))
lines(ecdf(rchisq(100,10)), col = "red")

pkolmogorov1x <- function(x, n) {
  if (x <= 0) 
    return(0)
  if (x >= 1) 
    return(1)
  j <- seq.int(from = 0, to = floor(n * (1 - x)))
  1 - x * sum(exp(lchoose(n, j) + (n - j) * log(1 - x - j/n) + (j - 1) * log(x + j/n)))
}

kolmogor <- function(x) {
  eps<- 10^(-10)
  s  <- 0
  a <- 1
  n <- 0
  k <- 0
  da<- 100000
  ga<- 100000
  if(x <= 0) return(0)
  while((Mod(da)>= eps) ) {
    da<-a
    s <- s + a 
    n <- n + 1 
    a <- (-1)^n * exp((-2)*(n^2) * (x^2))
    da <- Mod(da)-Mod(a)
  }
  n <- -1
  a <- 1
  while((Mod(ga)>= eps) ) {
    ga <- a
    a <- (-1)^n * exp((-2)*(n^2) * (x^2))
    s <- s + a 
    ga <- Mod(ga)-Mod(a)
    n <- n - 1 
  }
  return(s)
}
  