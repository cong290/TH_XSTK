#Lop: 20KDL1A
#Ten: Tran Huu Chi Cong
#MSSV: 20280009
#Tuan: KT lan 3
#-------------------------------------------------------------------
####### Bai 1 #######

X <- c(36,42,48,54,60,66,72)
n <- c(15,12,25,18,10,10,10)
du_lieu <- rep(X,n)

# a)
khoang_tin_cay <- function(x,alpha){
  trung_binh <- mean(x)
  do_lech_chuan <- sd(x)
  so_luong <- length(x)
  dung_sai <- qnorm(1-alpha/2)*do_lech_chuan/sqrt(so_luong)
  cat("Khoang tin cay cho trung binh: ",trung_binh - dung_sai," ",trung_binh + dung_sai)
}

alpha <- 1-0.96
kq <- khoang_tin_cay(du_lieu,alpha)

# b)
dat_tieu_chuan <- du_lieu[du_lieu>60]
ty_le <- length(dat_tieu_chuan)/length(du_lieu)
alpha <- 1-0.95
dung_sai <- qnorm(1-alpha/2)*sqrt(ty_le*(1-ty_le)/length(du_lieu))
cat("Khoang tin cay cho ty le: ",ty_le - dung_sai," ",ty_le + dung_sai)

####### Bai 2 #######

# a)
ci.tab.mean <- function(x,n,g){
  trung_binh <- mean(x)
  do_lech_chuan <- sd(x)
  so_luong <- sum(n)
  dung_sai <-   dung_sai <- qnorm(1-g/2)*do_lech_chuan/sqrt(so_luong)
  cat("Khoang tin cay cho trung binh: ",trung_binh - dung_sai," ",trung_binh + dung_sai)
}

x <- c(100,110,120,130,140,150,160)
n <- c(10,10,15,30,10,10,15)
mau <- rep(X,n)

# Ap dung
ci.tab.mean(x,n,1-0.96)

# b)

ci.tab.prop <- function(x,n,f,g){
  ty_le <- f/sum(n)
  dung_sai <- qnorm(1-g/2)*sqrt(ty_le*(1-ty_le)/sum(n))
  cat("Khoang tin cay cho ty le: ",ty_le - dung_sai," ",ty_le + dung_sai)
}

x <- c(100,110,120,130,140,150,160)
n <- c(10,10,15,30,10,10,15)
mau <- rep(X,n)

# Ap dung
f <- length(mau[mau>135])
ci.tab.prop(X,n,f,1-0.95)


