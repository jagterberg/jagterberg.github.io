#########
# Assignment 1 Solutions

# 1.
> 1+2*(3+4)
[1] 15
> 4^3+3^(2+1)
[1] 91
> sqrt((4+3)*(2+1))
[1] 4.582576
> ((1+2)/(3+4))^2
[1] 0.1836735

# 2.
> x=c(2,5,4,10,8)
> x^2
[1]   4  25  16 100  64
> x-6
[1] -4 -1 -2  4  2
> (x-9)^2
[1] 49 16 25  1  1

# 3.
> price = c(15.9,21.4,19.9,21.9,20.0,16.5,17.9,17.5)
> range(price)
[1] 15.9 21.9
> mean(price)
[1] 18.875
> range(price)-mean(price)
[1] -2.975  3.025

# 4.
> bill = c(46,33,39,37,46,30,48,32,49,35,30,48)
> sum(bill)
[1] 473
> range(bill)
[1] 30 49
> sum(bill>40)
[1] 5
> sum(bill>40)/length(bill)
[1] 0.4166667

# 5.
# x+1= 2 4 6 8 10
# y*2= 4 6 10 14 22 26
# length(x) is clearly 5
# length(y) is clearly 6
# sum(x>5)= 2
# sum(x[x>5])= 7+9 = 16
# y[3]= 5
# y[-3]= 2 3 7 11 13
# y[x]= 2 5 11 NA NA
# y[y>=7]= 7 11 13

# 6.
# After downloading the file and placing into our working directory:
train = read.table('train.csv', sep=',')
length(train$V1)
1461
