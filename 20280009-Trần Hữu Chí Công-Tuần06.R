####### Bai 3 #######
a.
sinh_vien <- seq(1,10)
cau_hoi_1 <- c(3,3,3,4,3,4,3,4,3,4)
cau_hoi_2 <- c(5,3,5,5,2,2,5,5,4,2)
cau_hoi_3 <- c(1,3,1,1,1,3,1,1,1,1)

data <- data.frame(sinh_vien, cau_hoi_1, cau_hoi_2, cau_hoi_3)
data

b.
table(cau_hoi_1)
table(cau_hoi_2)
table(cau_hoi_3)

c. 
par(mfrow = c(3, 1))
barplot(cau_hoi_1)
barplot(cau_hoi_2)
barplot(cau_hoi_3)

d.
par(mfrow = c(3, 1))
barplot(cau_hoi_1)
barplot(cau_hoi_2, horiz=T)
barplot(cau_hoi_3, horiz=T)

####### Bai 5 #######
getwd()
setwd("G:/Cơ sở toán/Thuc Hanh R Console/Tai Lieu Chinh/Data R-7BaiTH/")

diesel.engine = read.table('diesel_engine.dat',header=T)
diesel.time = read.csv('diesel_time.csv',header=T)
attach(diesel.engine);attach(diesel.time)
names(diesel.engine);names(diesel.time)
#Data frame diesel.engine
#Xac dinh so bien khuyet trong bien speed va thay doi gia tri
length(speed[speed=='NA'])
speed[is.na(speed)==T] = 1500

#Xac dinh so bien khuyet trong bien load va thay doi gia tri
length(load[load=='NA'])
load[is.na(load)==T]=20
speed;load

#Tinh trung binh, phuong sai, do lech chuan cua bien alcohol
alcohol
mean(alcohol);var(alcohol);sd(alcohol)

#Ghep hai dataframe diesel.engine va diesel.time thanh 1
diesel = data.frame(diesel.engine,diesel.time)
diesel

#Trich gia tri cua bien run ma co delay < 1.000
run[delay<1.000]

#Dem so dong co co timing = 30
length(run[timing==30])

#Ve bieu do boxplot cho cac bien speed, timing, delay
boxplot(speed,timing,delay) #Nhan xet do thi
#hoac
par(mfrow = c(2,2))
boxplot(speed)
boxplot(timing)
boxplot(delay)

#Ve bieu do phan tan cho cac cap (timing,speed) va (temp,press)
plot(timing,speed)
plot(temp,press)

#Chuyen bien load sang bien nhan to
load = factor(load)
load

#Chia bien delay thanh 4 doan deu nhau
delay
new.delay = cut(delay,breaks=4)
new.delay
#So gia tri trong tung khoang
tab = table(new.delay)
tab
barplot(tab[])

#Chia bien delay thanh cac doan: (0.283,0.7],(0.7,0.95],(0.95,1.2],(1.2,1.56]
cut.points = c(0.283,0.7,0.95,1.2,1.56)
new.delay1 = cut(delay,breaks=cut.points)
new.delay1
#So gia tri trong tung khoang
tab1 = table(new.delay1)
tab1
barplot(tab1[])

####### Bai 6 #######
year <- seq(1970,1979)
snow.cover <- c(6.5,12,14.9,10,10.7,7.9,21.9,12.5,14.5,9.2)
data <- data.frame(year, snow.cover)
data


hist(data)




