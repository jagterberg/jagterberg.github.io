# Assignment 2 Solutions

library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)

##########
# Problem 1.

starwars1 = mutate(starwars, BMI = mass/(height/100)^2)
starwars2 = select(starwars1, name, BMI)
starwars3 = filter(starwars2, BMI>30)
starwars4 = arrange(starwars3, desc(BMI))

##########
# Problem 2.
starwars %>% mutate(BMI = mass/(height/100)^2) %>% select(name, BMI) %>% filter(BMI > 30) %>% arrange(desc(BMI))

# Jabba the Hutt is the character with the largest BMI.

##########
# Problem 3.

iris_tibble = as_tibble(iris)

iris_tibble %>% group_by(Species) %>% summarise(msl = mean(Sepal.Length), msw = mean(Sepal.Width))

##########
# Problem 4.

ggplot(iris_tibble, aes(Sepal.Width, Sepal.Length, col = Species)) + geom_point()

##########
# Problem 5.

iris_ratio = iris_tibble %>% mutate(sep_ratio = Sepal.Length/Sepal.Width)

ggplot(iris_ratio, aes(Species, sep_ratio)) + geom_boxplot()

##########
# Problem 6.

pbn = read.csv("Popular_Baby_Names.csv", sep=',')

pbn = as_tibble(pbn)
pbn1 = pbn %>% filter(Year.of.Birth == 2011) %>% group_by(Child.s.First.Name) %>% summarise(tot = sum(Count)) %>% filter(tot > 1000) %>% arrange(desc(tot))

ggplot(pbn1, aes(Child.s.First.Name, tot)) + geom_bar(stat='identity')


