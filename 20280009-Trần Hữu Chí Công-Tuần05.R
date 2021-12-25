#Lop: 20KDL1A
#Ten: Tran Huu Chi Cong
#MSSV: 20280009
#Tuan: 5
#-------------------------------------------------------------------
####### Bai 1 #######

mu <- 0
sigma <- 1
Y <- function(){
	x <- rnorm(2, mu, sigma) 
	sum((x[1])^2 + (x[2])^2)
}
mauY <- function(n) replicate(n, Y())
n = 100
hist(mauY(n), freq=0, breaks=40)
curve(dchisq(x, df=2), from=0, to=8, col="blue", lty=1, lwd=2, add=TRUE)

n = 1000
hist(mauY(n), freq=0, breaks=40)
curve(dchisq(x, df=2), from=0, to=8, col="blue", lty=1, lwd=2, add=TRUE)

n = 10000
hist(mauY(n), freq=0, breaks=40)
curve(dchisq(x, df=2), from=0, to=8, col="blue", lty=1, lwd=2, add=TRUE)
 