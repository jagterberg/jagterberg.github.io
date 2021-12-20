iris <- iris
dhf <- density(iris$Sepal.Length)


x=1:2; y=letters[1:2]; z=1:3
data.frame(x,y)
data.frame(x,y,z) #error due to unequal lengths
list(x,y,z) #unnamed components
list(x.name=x,y.name=y,z.name=z) #named components
DF = data.frame(x,y)
DF$x #access x column of DF
DF[,'x'] #access column named x
DF$x[1] #access first entry in column x of DF
DF[1,2] # access entry in first row, second column

library(UsingR)
attach(babies)
not.these = (gestation==999) | (age==99) |(inc==98)
TMP = babies[!not.these,
             c('gestation','age', 'wt','inc')]
pairs(TMP)
detach(babies)

lst = list(x,y,z)
lst[[1]] #access first component x
lst.named = list(x.name=x,
                 y.name=y, z.name=z)
lst.named$x.name
names(lst.named)
lst.named[['x.name']]

install.packages("tidyverse")
library('tidyverse')

install.packages("nycflights13")
library(nycflights13)
flights #inspect data set
filter(flights, month==1,day==1)

arrange(flights, sched_dep_time, dep_delay)
arrange(flights, desc(sched_dep_time), dep_delay)

select(flights, year, month, day)
select(flights, year:day) 
select(flights,-(year:day))

select(flights, tail_num = tailnum)
rename(flights, tail_num = tailnum)
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance/air_time * 60)

summarise(flights,
          mean_dep_delay = mean(dep_delay, na.rm=T),
          sd_dep_delay = sd(dep_delay, na.rm=T))
by_tailnum = group_by(flights, tailnum)
summarise(by_tailnum,
          mean_dep_delay = mean(dep_delay, na.rm=T),
          sd_dep_delay = sd(dep_delay, na.rm=T))
sample_n(flights, 10) #sample 10 flights
sample_frac(flights,.01) #sample 1% of all flights

A1 = group_by(flights, year, month, day)
A2 = select(A1, arr_delay, dep_delay)
A3 = summarise(A2,
               arr = mean(arr_delay, na.rm = T),
               dep = mean(dep_delay, na.rm =T))
A4 = filter(A3, arr > 30 | dep > 30)

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = T),
    dep = mean(dep_delay, na.rm = T)) %>%
  filter(arr > 30 | dep > 30)

list_vs_sales = ggplot(txhousing, aes(x= listings,y= sales))
list_vs_sales + geom_point(col='red')

plot_list_vs_sales = list_vs_sales + geom_point(col='red')
plot_list_vs_sales + geom_smooth(method = 'lm')
plot_list_vs_sales + geom_smooth()

midwest_state = ggplot(midwest, aes(x=state))
midwest_state + geom_bar()
midwest_state + geom_bar(col="blue", fill = "yellow")


state_pops = midwest %>% group_by(state) %>%
  summarise(tot_pop=sum(poptotal))
ggplot(data=state_pops, aes(x=state,y=tot_pop))+
  geom_bar(stat= 'identity',col="blue", fill = "yellow")

ggplot(txhousing, aes(x=listings)) + geom_histogram()
ggplot(midwest, aes(state, area)) + geom_boxplot()
dat <- txhousing
dat$month <- as.factor(dat$month)
ggplot(dat, aes(listings,
                      sales,
                      lwd=month,col=month)) +
  geom_point() + #color already encoded in aes()
  guides() #add default legend (gradient for numerical data)

