install.packages("UsingR")
library(UsingR)
central.park.cloud
table(central.park.cloud)
table(central.park.cloud)/length(central.park.cloud) 


barplot(table(central.park.cloud), ylab = "Frequency") 
barplot(table(central.park.cloud)/length(central.park.cloud), ylab = "Proportion")

sales <- c(45,44,46)
names(sales) <- c("John","Jack","Suzy")
barplot(sales, main = "Sales", ylab = "Pieces Sold")
barplot(sales, main = "Sales", ylab = "Pieces Sold", ylim = c(42,46), xpd = F)

dotchart(sales, xlab = "No. Sales")


x <- c(2,3,16,23,14,12,4,13,2,0,0,0,6,28,31,14,4,8,2,5)
stem(x, scale = 2)

sum(x)/length(x)
mean(x)

x.new = c(x,350)
mean(x.new)
median(x)
median(x.new)
which.max(table(x)) 

diff(range(x))
var(x)
sd(x)
IQR(x)
quantile(x, seq(0,1,by=.25))
summary(x)


attach(faithful)
hist(waiting) #defaults
hist(waiting, breaks = 10) #suggest 10 bins
hist(waiting, breaks = seq(43,108,length=10)) #use these breaks
hist(waiting, breaks = "Scott") #use "Scott" algorithm
hist(waiting,freq = FALSE)
lines(density(waiting))
detach(faithful)

attach(cfb)
summary(VEHIC)
hist(VEHIC, breaks="Scott", freq=F)
lines(density(VEHIC))
detach(cfb)

attach(alltime.movies)
boxplot(Gross,ylab="All-time gross sales")
detach(alltime.movies)




