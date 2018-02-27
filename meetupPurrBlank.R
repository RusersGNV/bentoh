install.packages(c("gapminder", "tidyverse"))

### Case 1
# Testing the idea: sample mean approaches population means
# as sample size (n) increases

# Fake population data, true mean = 0, true SD = 1
pop <- rnorm(10000)

# Now we want to sample n individual from the population
# and take the sample means. n = 1, 2, 3,...,1000.
n <- 1:1000

# Method 1: the for loop way
m1 <- rep(NA, length(n))
for (i in n) {
  x <- sample(pop, i)
  m1[i] <- mean(x)
}
plot(m1, type="l")

# Cool but what if I have for-loop-phobia?
# `purrr` allows us to avoid for loops, and has slightly cleaner code!
# Method 2: the `purrr` way
library(tidyverse)










### Case 1a
# What if I want to test both: if mean and SD converges to 
# the true mean and SD?
# We'll need df instead of vector right?
# Let's look at method 1
m1 <- data.frame(n = numeric(10000), mean = numeric(10000), sd = numeric(10000))
for (i in n) {
  x <- sample(pop, i)
  m1[i,] <- list(i, mean(x), sd(x))
}
plot(m1$n, m1$mean, type="l")
plot(m1$n, m1$sd, type="l")

# What about purrr?
# We can "map" a function and get a vector or a df output!
# (For free, if you know how to ask)







### Case 2
# The "map" function doesn't just take vector as input.
# In fact, "map" works extensively with list object.
library(gapminder)

# gapminder is a dataset with life expectancy, population
# and GDP per capita from 1952 - 2007 (by every five years)
# It's a good toy dataset
head(gapminder, 15)

# Let's say we have five models to predict life expectancy
f1 <- lifeExp ~ pop
f2 <- lifeExp ~ gdpPercap
f3 <- lifeExp ~ pop + gdpPercap
f4 <- lifeExp ~ pop + gdpPercap + year
f5 <- lifeExp ~ pop + gdpPercap + year + continent

m1 <- lm(f1, data = gapminder)
m2 <- lm(f2, data = gapminder)
m3 <- lm(f3, data = gapminder)
m4 <- lm(f4, data = gapminder)
m5 <- lm(f5, data = gapminder)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

AIC(m1, m2, m3, m4, m5)

# Quite tedious, and we ended up with lots of variables
# in our environment...
# Can purrr deal with this? Let's see if we can do it the 
# "vector way", i.e. build a vector of five formulas.





# Nope, vector doesn't work here because vectors in R is 
# only available for numeric, character and logical.
# So we have to use list.







### Case 2a
# One regression for each country








### Exercise
# Subset the gapminder data according to the following 
# regions, then find the mean life expectancy and mean
# GDP per capita for each region in each year
ANZ <- c("Australia", "New Zealand")
SEA <- c("Cambodia", "Indonesia", "Malaysia", "Myanmar", "Philippines", 
         "Singapore", "Thailand", "Vietnam")
SA <- c("Bangladesh", "India", "Nepal", "Pakistan", "Sri Lanka")
EA <- c("China", "Hong Kong, China", "Japan", "Korea, Dem. Rep.", "Korea, Rep.", 
        "Taiwan")