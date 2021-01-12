library(UsingR)
attach(normtemp)

qqnorm(temperature); qqline(temperature)

test_normal_temp <- function(dat = temperature) {
  test.stat <- (mean(dat)-98.6)/(sd(dat)/sqrt(length(dat)))
  return(test.stat > 1.96 | test.stat < -1.96)
}

test_normal_temp()
t.test(x=temperature, mu = 98.6, alt="two.sided")

detach(normtemp)

p.hat = .8
test.stat = (p.hat-.75)/sqrt((.75*.25)/50)
test.stat > qnorm(.95)
prop.test(40, 50, .75, alt="greater") 




call.length<-c(2, 1, 3, 3, 3, 3, 1, 3, 16, 2, 2, 12, 20, 3, 1) 
t.obs = sum(call.length > 5); n = length(call.length)
n-t.obs
1-pbinom(11,n,.5) 


wilcox.test(salmon.rate, .005, alt= "greater")


mg300 = c(284, 279, 289, 292, 287, 295, 285, 279, 306, 298)
mg600 = c(298, 307, 297, 279, 291, 335, 299, 300, 306, 291)
plot(density(mg300))
lines(density(mg600), lty = 2)
t.test(mg300, mg600, alt = 'less', var.equal = T)


x_2001 = 50000*.117; x_2002 = 60000*.121
prop.test(c(x_2001, x_2002), c(50000,60000), alt = 'less')
pre_test = c(75,56,64,60,57,53,72,62,65,66)
post_test = c(88,74,83,68,58,50,67,64,74,60)
t.test(pre_test, post_test, alt = 'less' , var.equal=T)
t.test(pre_test, post_test, alt = 'less' , paired=T)


obs.counts = c(35,40,25); p.null = c(.35,.35,.30)
chisq.test(obs.counts, p.null)
table_pi2000 = table(pi2000)
chisq.test(table_pi2000) #use default p-vector
seatbelts = rbind(c(56,8),c(2,16))
chisq.test(seatbelts)

y <- rnorm(20); par(mfrow=c(1,2))
plot(density(y))
curve(dnorm(x),add=T, lty=2)
plot(ecdf(y))
curve(pnorm(x), add=T, lty=2); par(mfrow=c(1,1))

x <- rnorm(100, mean=5, sd=2)
ks.test(x, "pnorm", mean = 0, sd = 2) #wrong parameters
ks.test(x, "pnorm", mean = 5, sd = 2) #correct parameters
ks.test(x, "punif", min = 1, max = 9) #wrong proposal distribution

shapiro.test(stud.recs$sat.m)
shapiro.test(stud.recs$sat.v)


attach(babies)
mult_reg_no_factor = lm(
  wt ~ gestation + age + gestation*age,
  subset = gestation < 350 & age < 99
)

mult_reg_factor = lm(
  wt ~ gestation+age+gestation*age+factor(smoke),
  subset = gestation < 350 & age < 99 & smoke < 9
)

detach(babies)


