#Lop: 20KDL1A
#Ten: Tran Huu Chi Cong
#MSSV: 20280009
#Tuan: KT
#-------------------------------------------------------------------
####### Bai 1 #######
mu <- 0
sigma <- 1
n <- 30
Y <- function(n){
  x <- rnorm(n, mu, sigma) 
  s <- 0
  for(i in 1:n)
    s=s+x[i]**2
  s
}
Z <- function(m) replicate(m,Y(n))

m <- 10000
hist(Z(m), freq =0 ,breaks=40)
curve(dchisq(x, df=30), from=0, to=8, col="blue", lty=1, lwd=2, add=TRUE)

####### Bai 2 #######
mu <- 0
sigma <- 1
Z <- function(m){
  x <- rnorm(1, mu, sigma) 
  y <- rchisq(m,df=m)
  z <- x/sqrt(y/m)
  z
}
Z(m)
mauZ <- function(n) replicate(n,Z(10))

n <- 10000
hist(mauZ(n), frep =0 ,breaks=40)
curve(dt(x, df=Inf), col="blue", lty=1, lwd=2, add=TRUE)
?dt

  


