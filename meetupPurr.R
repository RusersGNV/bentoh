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
m2 <- map(n, ~ sample(pop, .x))
head(m2)

m2 <- map(n, ~ sample(pop, .x)) %>%
  map(~ mean(.x))
head(m2)

m2 <- map(n, ~ sample(pop, .x)) %>%
  map_dbl(~ mean(.x))
plot(m2, type="l")

###
# What if I want to test both if SD converges to the true SD?
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
m2 <- map(n, ~ sample(pop, .x)) %>%
  map_df(~ data.frame(mean = mean(.x), sd = sd(.x)),
         .id = "n")
plot(m2$n, m2$mean, type="l")
plot(m2$n, m2$sd, type="l")

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
formulas <- c()
formulas[1] <- f1

# Nope, vector doesn't work here because vectors in R is 
# only available for numeric, character and logical.
# So we have to use list.
formulas <- list(f1, f2, f3, f4, f5)
mod <- formulas %>%
  map( ~ lm(.x, data=gapminder))

mod %>%
  map(summary)

# Make a table of df and AIC
mod %>%
  map_df(~ data.frame(formula = format(formula(.x)),
                      df = df.residual(.x),
                      AIC = AIC(.x),
                      stringsAsFactors = F))

# Make a table of coefficients
mod %>%
  map(~ coef(.x)) %>%
  map_df(~ as.data.frame(t(.x)))

### Case 2a
# One regression for each country
# Here we make use of the function "split()" which split
# a big dataframe to a list of small dataframe given a 
# specific factor.
gap_bycty <- gapminder %>%
  split(.$country)
gap_bycty[[1]]
gap_bycty$Ghana

# Build a table of two columns, country and R square value
# by making use of the .id argument in map_df
mod_bycty <- gap_bycty %>%
  map(~ lm(lifeExp ~ gdpPercap, data=.x)) %>%
  map(summary) %>%
  map("r.squared") %>%
  map_df(~ data.frame(Rsquare = .x), .id="country")

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

# Answer
RegDat <- list(ANZ=ANZ, SEA=SEA, SA=SA, EA=EA) %>%
  map(~ filter(gapminder, country %in% .x)) %>%
  map(~ group_by(.x, year)) %>%
  map_df(~ summarise(.x, lifeExp = mean(lifeExp), gdpPercap = mean(gdpPercap)),
         .id = "region")
  
ggplot(RegDat, aes(x=year, y=lifeExp, col=region)) +
  geom_line()
