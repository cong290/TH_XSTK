#L?p: 20KDL1A
#Tên: Tr?n H?u Chí Công
#Ma s? sinh viên: 20280009
#Tu?n: 2
#----------------------------------------------------
########### Bài 1 ###############
print("Nhap so nguyen duong n tuy y:")
n <- scan()
x <- sample(seq(1:100), n, rep=TRUE)
x
# Ham tinh tong tich luy 
cum.sum <- function(x,n){
 s <- 0
 for(i in 1:n){
	s <- s + x[i]
 }
 return(s)
}

tong_tl <- cum.sum(x, n)
tong_tl

########### Bài 2 ###############
r <- c(seq(3,20))
r

# Ham xay dung dataframe tinh the tich hinh cau
spherical.volume <- function(r){
 V <- 4/3*pi*(r^2)
 data <- data.frame("radius"=r, "volume"=V)
 return (data)
}

data_spherical_volume <- spherical.volume(r)
data_spherical_volume

########### Bài 3 ###############
getwd()
setwd("F:/L?p trình R/Data cho cac bai thuc hanh/")
data_01 <- read.csv(file="data01.csv", sep=";", header=TRUE)
attach(data_01)

index <- numeric(length(Age))
index

for (i in 1:length(Age)) {
 if (Age[i] <= 60){
	index[i] <- 0
}else if (60 < Age[i] & Age[i] <= 70){
	index[i] <- 1
}else if (70 < Age[i] & Age[i] <= 80){
	index[i] <- 2
}else{
	index[i] <- 3
}
} 
index
    
########### Bài 4 ###############
a.
getwd()
data_fr <- read.csv(file="data11.csv", sep=";", header=TRUE)
attach(data_fr)
names(data_fr)

b.
cal.sample <- function(dat){
   x <- (a+b)/2
   m <- min(x)
   M <- max(x)
   mean <- sum(n*x)/sum(n)
   var <- sum(n*(x-mean)^2)/(sum(n)-1)
   display <- c(m, M, mean, var)
   display
}

cal.sample(data_fr)

########### Bài 5 ###############
print("Nhap so nguyen duong n tuy y:")
n <- scan()
x <- sample(seq(1:100), n, rep=TRUE)
x <- sort(x)

# Ham phan vi
phanvi <- function(X, p){
  i <- (p/100)*length(X)
  if (i %% 1 != 0){
      i <- trunc(i)
  }
  else{
      i <- (X[i] + X[i+1])/2
  }
  return (i)
}

phanvi(x,25)


















