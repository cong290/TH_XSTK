#Lop: 20KDL1A
#Ten: Tran Huu Chi Cong
#MSSV: 20280009
#Tuan: 4
#-------------------------------------------------------------------
####### Bai 1 #######
barplot(dhyper(0:15,25,100-25,15), xlab="x = 0:15", ylab="P(X=x)", 
	  main="Ham xac suat cua phan phoi sieu boi")

####### Bai 2 #######
sum(dhyper(5:12, 25, 100-25, 15))
p <- phyper(12, 25, 100-25, 15) - phyper(4, 25, 100-25, 15)	
p

####### Bai 3 #######
#Cau a)
lamda = 0.6
curve(dexp(x, lamda), 0, 10)

#Cau b)
lamda = 0.3
curve(dexp(x, lamda), 0, 10, add=TRUE)

#Cau c)
pexp(10, 0.6) - pexp(0, 0.6)
pexp(10, 0.3) - pexp(0, 0.3)

####### Bai 4 #######
x = 0:8
barplot(dpois(x, 1), xlab="x = 0:8", ylab="P(X=x)", main="Ham xac suat cua bien X ~ P(1)")

####### Bai 5 #######
x = 0:10
curve(dchisq(x, 3), xlab="x = 0:10", ylab="fX(x)", main="Ham mat do xac suat c?a bien X")

####### Bai 6 #######
layout(matrix(c(1, 2), ncol = 1))
x = 0:50
plot(x, dbinom(x, 50, 0.08), ylab="fX(x)", main="Ham mat do cau bien X~B(50,0.08)",
     ylim = c(0, 0.25), type = "h")
plot(x, dpois(x, 4), ylab="fX(x)", main="Ham mat do cau bien X~P(4)",
     ylim = c(0, 0.25), type = "l")

####### Bai 7 #######
x = 0:50
plot(x, dbinom(x, 50, 0.4), type="h")
curve(dnorm(x, 20, 12), 0, 50, add = TRUE)

?sum

