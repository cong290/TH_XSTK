#Lop: 20KDL1A
#Ten: Tran Huu Chi Cong
#MSSV: 20280009
#Tuan: 9
#-------------------------------------------------------------------
####### Bai 1 #######
setwd("F:/Lap_trinh_R/TH_XSTK/Data cho cac bai thuc hanh")
data <- read.csv("profit.csv", header = T)
str(data)
attach(data)

# a)
hist(profit)

# b)
dat_hang <- profit[profit>65]

ci.mean <- function(x, alpha) {
  trung_binh <- mean(x)
  do_lech_chuan <- sd(x)
  so_luong <- length(x)
  epsilon <- qnorm(1 - alpha/2)*do_lech_chuan / sqrt(so_luong)    # Dung sai
  cat(trung_binh - epsilon, trung_binh + epsilon)
}

ktc <- ci.mean(dat_hang, 1-0.99)

# c)
trung_binh <- mean(profit)
do_lech_chuan <- sd(profit)
T0 <- (trung_binh-60)/(do_lech_chuan/sqrt(length(profit)))
T0
t <- qt(1-0.01/2, length(profit)-1)
t

####### Bai 2 #######
x <- c(5,6,7,8,9,10)
n <- c(5,10,15,20,12,8)

ket_qua <- rep(x,n)

# a)
stem(ket_qua)

# b)
# Gia thuyet H0: mu = mu_0
# Doi thuyet H1: mu > mu_0
test.geq.oneside <- function(x, mu_0 , alpha){
  trung_binh <- mean(x)
  do_lech_chuan <- sd(x)
  so_luong <- length(x)
  T0 <- (trung_binh-mu_0)/(do_lech_chuan/sqrt(so_luong))
  p_giatri <- pt(T0, so_luong - 1, lower.tail = FALSE)
  if(p_giatri < alpha)
    print("Bac bo H0")
  else
    print("khong du co so bac bo H0")
  p_giatri
}

test.geq.oneside(ket_qua, 8, 0.05)

# c) 
# Gia thuyet H0: mu = mu_0
# Doi thuyet H1: mu < mu_0
test.leq.oneside <- function(x, mu_0 , alpha){
  trung_binh <- mean(x)
  do_lech_chuan <- sd(x)
  so_luong <- length(x)
  T0 <- (trung_binh-mu_0)/(do_lech_chuan/sqrt(so_luong))
  p_giatri <- pt(T0, n - 1)
  if(p_giatri < alpha)
    print("Bac bo H0")
  else
    print("khong du co so bac bo H0")
  p_giatri
}

test.leq.oneside(ket_qua, 8, 0.05)

####### Bai 3 #######
setwd("F:/Lap_trinh_R/TH_XSTK/Data cho cac bai thuc hanh")
teen_birth_rate <- read.table("teen-birth-rate-2002.txt",sep="\t", header=T)
teen_birth_rate <- data.frame(Black=data$Black,Hispanic=data$Hispanic,White=data$White)
str(teen_birth_rate)

X_bar <- apply(teen_birth_rate, 2, function(x) mean(x,na.rm=T))
S <- apply(teen_birth_rate, 2, function(x) sd(x,na.rm=T))
n <- apply(teen_birth_rate,2,function(x) length(x[is.na(x)==F]))

mu_0 <- mean(c(Black,Hispanic,White),na.rm=T)
z_value <- function(x,mu_0){
  trung_binh <- mean(x,na.rm=T)
  do_lech_chuan <- sd(x,na.rm=T)
  so_luong <- length(x[is.na(x)==F])
  (trung_binh-mu_0)*sqrt(so_luong)/do_lech_chuan
}

Z <- apply(teen_birth_rate, 2, function(x) z_value(x,mu_0))
p <- apply(teen_birth_rate, 2, function(x) 2*pt(abs(z_value(x,mu_0)), length(x[is.na(x)==F]) - 1, lower.tail = FALSE))

table <- round(data.frame(trung_binh,do_lech_chuan,n,Z,p_value),2)
table

####### Bai 4 #######

load('data04.rda')
n <- 80
x <- sum(survey) 	# So GV dong y
ty_le <- x/n 	    # Ty le mau

# a)
# Chon H0: p = 0.6 va H1:p > 0.6

# b)
p0 <- 0.6
Z0 <- (ty_le-p0)*sqrt(n)/sqrt(p0*(1-p0))
p_value <- 1 - pnorm(Z0)
if(p_value<0.05){
  print("Bac bo H0")
  print("Ket luan: Thuc hien thay doi thang diem")
} else {
  print("Khong du co so bac bo H0")
}
p_value


####### Bai 5 #######
n <- 100         # Tong so tre 
x <- 20          # So tre bi coi xuong
ty_le <- x/n     # Ty le tre bi coi xuong

p=0.15
Z0 <- Z0 <- (ty_le - p)*sqrt(n)/sqrt(p*(1-p))
p_value <- 2*min(pnorm(Z0),1-pnorm(Z0))
p_value
prop.test(20,100,p,conf.level=0.99)
if(p_value < 0.05)
{
  print("Bac bo H0")
  print("Ty le bat gap 1 tre em mac benh coi xuong la 15%")
}else{
  print("Khong du co so bac bo H0")
}

# a)
setwd("F:/Lap_trinh_R/TH_XSTK/Data cho cac bai thuc hanh")
times <- read.csv("times.csv",sep=",",header=T)
str(times)
attach(times)

tren_5_gio_KHTN <- KHTN[KHTN>5]

#####

# 1)
p_value <- 2*min(pnorm(1.96),1-pnorm(1.96))






