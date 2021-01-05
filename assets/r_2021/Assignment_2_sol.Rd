# Assignment 2 Solutions

########## 
# Problem 1.

library(UsingR)

# a.
attach(npdb)
table(state)
names(table(state))[table(state)==max(table(state))]

# b.
mean(amount)
median(amount)

detach(npdb)

##########
# Problem 2.

sum(pi2000 <= 3)/length(pi2000)

sum(pi2000 >= 5)/length(pi2000)

##########
# Problem 3.

attach(nym.2002)
hist(time, freq=F)
lines(density(time))
detach(nym.2002)

# The distribution is unimodal, skew-right.

##########
# Problem 4.

attach(reaction.time)
boxplot(time[as.character(control)=='T'],time[as.character(control)=='C'])

##########
# Problem 5.


x77 = data.frame(state.x77)
attach(x77)

# a.
ill_v_hsgrad = Illiteracy~HS.Grad
plot(ill_v_hsgrad)
abline(lm(ill_v_hsgrad))

# b.
le_v_mur = Life.Exp~Murder
plot(le_v_mur)
abline(lm(le_v_mur))

# c.
inc_v_ill = Income~Illiteracy
plot(inc_v_ill)
abline(lm(inc_v_ill))

detach(x77)


##########
# Problem 6.

attach(Cars93)
plot(MPG.city, Price, pch=as.numeric(Type))
detach(MPG.city)

