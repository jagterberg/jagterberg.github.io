# This is supplemental code for understanding the 
# difference between attach() and the dplyr functions
library(dplyr)
iris_dat <- iris

#suppose we want to take the man of the sepal.length.  
# the easiest is to do this:
mean(iris_dat$Sepal.Length)
mean(iris_dat[,"Sepal.Length"])

mean(Sepal.Length) #gives an error since it is not attached

#we need to tell R to look at iris_dat for the variable Sepal.Length:
attach(iris_dat)
mean(Sepal.Length)
detach(iris_dat)

mean(Sepal.Length) #gives an error again since we detached it

#can also do it with dplyr
iris_dat <- as_tibble(iris_dat)

#gives an error:
summarise(mean(Sepal.Length))


#need to tell R to look in iris_dat
#Piping operator '%>%' does this:
iris_dat %>% summarise(mean(Sepal.Length)) 




