##########
# Assignment 4 Solutions

# 1.
hello = function(x){
	print(paste('hello', x))
}

hello('kitty')

# 2.

ourhist = function(x){
	hist(x, breaks='Scott', freq=F)
	lines(density(x))
}

ourhist(rexp(100,3))

# 3.

problem_3 = function(x){
	for(i in 1:length(x)){
		if(x[i]<0) x[i]=x[i]-1
		else x[i] = x[i]+1
	}
	return(x)
}

y = problem_3(rnorm(100))
ourhist(y)

# 4.

problem_4 = function(x){
	x = ifelse(x<0, x-1, x+1)
	return(x)
}

y = problem_4(rnorm(100))
ourhist(y)

# 5.

classic_fib = function(n){
	if(n==1) return(0)
	else if (n==2) return(1)
	else return(classic_fib(n-1)+classic_fib(n-2))
}

# 6.

general_fib = function(n,a1=0,a2=1){
	if(n==1) return(a1)
	else if (n==2) return(a2)
	else return(general_fib(n-1)+general_fib(n-2))
}

# 7.

classic_ratios = c()
general_ratios = c()
for(i in 3:20){
	classic_ratios[i] = classic_fib(i)/classic_fib(i-1)
	general_ratios[i] = general_fib(i,3,4)/general_fib(i-1,3,4)
}

plot(1:20,general_ratios, pch = 1)
points(seq(1.5,20.5,1), classic_ratios, pch = 2)

# 8.

dbern = function(x,p){
	ifelse(x==0,1-p,ifelse(1,p,0))
}

pbern = function(x,p){
	ifelse(x < 0, 0, ifelse(x < 1, 1-p, 1))
}

rbern = function(n,p){
	u = runif(n)
	return(ifelse(u < 1-p, 0, 1))
}

# 9.

1-ppois(19,15)

# 10. 

cars = rpois(30,15)
ourhist(cars)
mean(cars)
sd(cars)

# 11.

month_means = c()

for(i in 1:12){
	month_means[i] = mean(rpois(30,15))
}

ourhist(month_means)

qqnorm(month_means)
qqline(month_means)

# 12.

year_means = c()

for(i in 1:20){
	year_means[i] = mean(rpois(365,15))
}

ourhist(year_means)

qqnorm(year_means)
qqline(year_means)



