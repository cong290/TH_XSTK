

####### Bai 1 #######
setwd("F:/Lap_trinh_R/TH_XSTK/Data cho cac bai thuc hanh")

volume <- read.csv("volume.csv", header=TRUE)
attach(volume)
# a)
# H0: the tich sua 2 trung binh duoc 2 may dua vao la nhu nhau: mu_1 = mu_2
# H1: the tich sua 2 trung binh duoc 2 may dua vao la khac nhau: mu_1 # mu_2
test <- t.test(machine1, machine2, var.equal=FALSE)
# p_value = 0.1425 > 0.05,=> phan doan cua ky su tren la chua du co so

# b)
p_value <- test$p.value 
p_value

# c)
ktc <- test$conf.int
ktc

# d)
# H0: mu = mu_0
# H1: mu < mu_0
test.leq.oneside <- function(x, y, mu_0, sigma_1, sigma_2, alpha){
  n <- length(x)
  m <- length(y)
  mean_X <- mean(x)
  mean_Y <- mean(y)
  # Biet sigma_1 va sigma_2, thong ke Z~N(0,1)  
  Z <- (mean_X - mean_Y - mu_0)/sqrt(sigma_1^2/n+sigma_2^2/m)  
  p_value <- pnorm(Z)
  if(p_value < alpha)
    print("Bac bo H0")
  else
    print("Chua du co so bac bo H0")
  cat("p_value = ",p_value)
}

test.leq.oneside(machine1,machine2,0,0.002,0.0025,0.05)

# e)
# H0: mu = mu_0
# H1: mu > mu_0
test.geq.oneside <- function(x, y, mu_0, sigma_1, sigma_2, alpha){
  n <- length(x)
  m <- length(y)
  mean_X <- mean(x)
  mean_Y <- mean(y)
  # Biet sigma_1 va sigma_2, thong ke Z~N(0,1)  
  Z <- (mean_X - mean_Y - mu_0)/ sqrt(sigma_1^2/n + sigma_2^2/m)  
  p_value <- 1 - pnorm(Z)
  if(p_value < alpha)
    print("Bac bo H0")
  else
    print("Chua du co so bac bo H0")
  cat("p_value = ", p_value)
}

test.geq.oneside(machine1,machine2,0,0.002,0.0025,0.05)

####### Bai 2 #######
diameter <- read.csv("diameter.csv", header=TRUE)
names(diameter)
attach(diameter)

X <- diameter$extru.ma.1
Y <- diameter$extru.ma.2
# loai bo NA  
X_1 <- X[is.na(X)!=TRUE]
Y_1 <- Y[is.na(Y)!=TRUE]
length(X_1)
length(Y_1)
alpha <- 0.05
# a)
# H0: Co bang chung ung ho gia thiet hai may cho ra cac thanh thep voi duong kinh khac nhau
# H1: Khong co bang chung ung ho gia thiet hai may cho ra cac thanh thep voi duong kinh khac nhau
test <- t.test(X_1, Y_1, var.equal = TRUE, conf.level = 0.95)
test
# p_value < 2.2e-16 < 0.05 => Bac bo H0

# b)
p_value <- test$p.value
p_value

# c)
cat("Uoc luong su sai biet ve duong kinh trung binh cua cac thanh thep do hai may san xuat: ",test$conf.int)

# d)
# H0: mu = mu_0
# H1: mu < mu_0
test.leq.oneside <- function(x,y,mu_0,alpha){
  n <- length(x)
  m <- length(y)
  mu_X <- mean(x)
  mu_Y <- mean(y)
  S_p <- sqrt(((n-1)*sd(x)^2+(m-1)*sd(y)^2)/(n+m-2))
  T0 <- (mu_X - mu_Y - mu_0)/(S_p*sqrt(1/n+1/m))
  p_value <- pt(T0, n+m-2)
  if(p_value < alpha)
    print("Bac bo H0")
  else
    print("Chua du co so bac bo H0")
  cat("p_value = ", p_value)
}

# Ap dung
test.leq.oneside(X_1,Y_1,0,0.05)

# e)
# H0: mu = mu_0
# H1: mu > mu_0
test.geq.oneside <- function(x,y,mu_0,alpha){
  n <- length(x)
  m <- length(y)
  mu_X <- mean(x)
  mu_Y <- mean(y)
  S_p <- sqrt(((n-1)*sd(x)^2+(m-1)*sd(y)^2)/(n+m-2))
  T0 <- (mu_X - mu_Y - mu_0)/(S_p*sqrt(1/n+1/m))
  p_value <- 1-pt(T0, n+m-2)
  if(p_value < alpha)
    print("Bac bo H0")
  else
    print("Chua du co so bac bo H0")
  cat("p_value = ", p_value)
}

# Ap dung
test.geq.oneside(X_1,Y_1,0,0.05)

############# Bai 4 #############
getwd()
cholesterol <- read.csv("cholesterol.txt",sep=" ", header = TRUE)
cholesterol
attach(cholesterol)

data <- Before-After
data

mu_D <- mean(Before)-mean(After)
mu_D

# a)
# H0: che do an kieng va tap luyen co tac dung trong viec giam cholesterol trong mau
# H1: che do an kieng va tap luyen khong co tac dung trong viec giam cholesterol trong mau
alpha <- 0.05
test <- t.test(Before,After, paired=TRUE, var.equal = TRUE, data=data)
if(test$p.value < alpha){
  print("bac bo H0")
  test$p.value
}else{
  print("Khong du co so bac bo H0")
  test$p.value
}


