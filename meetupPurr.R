# Fake population data, true mean = 0, true SD = 1
pop <- rnorm(10000)

# Now we want to sample n individual from the population
# and take the sample means. n = 1, 2, 3,...,10000.
# This way we can test the theory of sample mean approaches
# population mean as sample size (n) increases.
n <- 1:10000

# Method 1: the for loop way
m1 <- rep(NA, length(n))
for (i in n) {
  x <- sample(pop, i)
  m1[i] <- mean(x)
}
plot(m1, type="l")

# Cool but what if I have for-loop-phobia?
# `purrr` allows us to avoid for loops, and has slightly cleaner code!
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

# What about the purrr way?
m2 <- map(n, ~ sample(pop, .x)) %>%
  map_df(~ data.frame(mean = mean(.x), sd = sd(.x)),
         .id = "n")
plot(m2$n, m2$mean, type="l")
plot(m2$n, m2$sd, type="l")

# So now we can "map" a function and get a vector or a df output!

# Let's make this more complicated and with list!
library(gapminder)

gapminder
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

# So tedious!
# Anyway for purrr to do this? Yes, of course
formulas <- c()
formulas[1] <- f1

# Nope vector doesn't work here because R doesn't have a vector object for formulas
# So we have to use list
formulas <- list(f1, f2, f3, f4, f5)
mod <- formulas %>%
  map( ~ lm(.x, data=gapminder))

mod %>%
  map(summary)

mod %>%
  map_df(~ data.frame(formula = format(formula(.x)),
                      df = df.residual(.x),
                      AIC = AIC(.x),
                      stringsAsFactors = F))

mod %>%
  map(~ coef(.x)) %>% map(~ t(as.matrix(.x))) %>%
  map_dfr(as.data.frame)
unique(gapminder$country)
