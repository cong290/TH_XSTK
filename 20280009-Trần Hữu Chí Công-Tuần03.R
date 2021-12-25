#Lop: 20KDL1A
#Tên: Tran Huu Chi Cong
#MSSV: 20280009
#Tuan: 3
#----------------------------------------------------
########### Bài 1 ###############
a)
f <- function(p){
   0.07 * (p**(-0.93)) 
}
F <- function(p){
   integrate(f,lower = 0, upper = p)
}
F(0.2)

b)
integrate(f,0,1)	#dien tich duoi duong cong tu 0 den 1 cua ham mat do f

########### Bài 2 ###############
x <- sample(1:5, 100, TRUE,prob = c(0.1,0.2,0.4,0.2,0.1))
n <- 100
plot(1:5, table(x)/n, type="h", xlab = "x", ylab = "P(X = x)")
?plot