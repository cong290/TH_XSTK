#Lop: 20KDL1A
#Ten: Tran Huu Chi Cong
#MSSV: 20280009
#Tuan: 7
#-------------------------------------------------------------------
rm(list=ls(all=T))
####### Bai 1 #######
#Tao 35 gia tri cua bien ngau nhien X ~ N(10,5^2)
n <- 35
alpha <- 0.05
sample_X <- rnorm(n, 10, 5)
trung_binh <- mean(sapmle_X)
do_lech_chuan <- sd(sapmle_X)
epsilon <- qnorm(1 - alpha / 2) * do_lech_chuan/sqrt(n)
cat(trung_binh - epsilon,"\n")
cat(trung_binh + epsilon)


####### Bai 2 #######

# a)
setwd("G:/Co_so_toan/Thuc Hanh R Console/Tai Lieu Chinh/Data R-7BaiTH")
data <- read.csv("data31.csv", header=T)
str(data)
# b)
ci.mean <- function(x, alpha) {
  trung_binh <- mean(x)
  do_lech_chuan <- sd(x)
  so_luong <- length(x)
  epsilon <- qnorm(1 - alpha/2)*do_lech_chuan / sqrt(so_luong)    # Dung sai
  cat(trung_binh - epsilon, trung_binh + epsilon)
}

profit <- data[,2]
ci.mean(profit, 1-0.95)

####### Bai 3 #######

# a)
data <- read.csv("data32.csv", header = T)
str(data)
time <- data[,1]
ci.mean(time, 1 - 0.95)

# b)
ci.prop <- function(f, n, alpha) {
  ty_le <- f/n;
  so_luong <- length(x)
  epsilon <- qnorm(1 - alpha/2)*sqrt(ty_le*(1 - ty_le)/so_luong)   # Dung sai
  cat(ty_le - epsilon, ty_le + epsilon)
}

num <- length(time[time > 5])
ci.prop(num, length(time), 1 - 0.9)
ci.prop(num, length(time), 1 - 0.95)
ci.prop(num, length(time), 1 - 0.99)

####### Bai 4 #######

# a)
# Chuyen bang tan so dang khoang thanh du lieu dang vecto
x <- c(1.3, 1.5, 1.7, 1.9, 2.1)
n <- c(6, 34, 31, 42, 12)
vec <- rep(x,n)
#KTC cho chieu cao trung binh
ci.mean(vec, 1 - 0.95)

# b)
# KTC cho ty le thanh nien dat suc khoe loai A
f = length(vec[vec>=1.7])
ktc.type_A = ci.prop(f,sum(n),0.05)

####### Bai 5 #######

ktc.tb <- function(mean, sigma, s, n, alpha) {
  if (sigma == T) {  # Da biet DLC cua tong the
    epsilon <- qnorm(1 - alpha/2)*s/sqrt(n)   # Dung sai
  } else {
    if (n >= 30) {
      epsilon <- qnorm(1 - alpha/2)*s/sqrt(n)
    } else {
      epsilon <- qt(1 - alpha/2, df = n - 1)*s/sqrt(n)
    }
  }
  cat(mean - epsilon, mean + epsilon)
}

####### Bai 6 #######

ktc.tb.mau <- function(x, sigma, alpha ) {
  n <- length(x)
  mean <- mean(x)
  if(sigma == F){  # Chua biet DLC tong the
    do_lech_chuan = sd(x)
  }
  ktc.tb(mean, adj, do_lech_chuan, n, alpha)
}

####### Bai 7 #######

x <- c(12.00, 12.05, 12.10, 12.15, 12.20, 12.25, 12.30, 12.35, 12.40)
n <- c(2,3,7,9,10,8,6,5,3)
vec <- rep(x,n)
alpha <- 1 - 0.95
ktc.tb.mau(vec, F, alpha)

