rm(list = ls(all=TRUE))

vec_1 <- c(1, 4, 6, 7)
str(vec_1)

vec_1[3] <- "medium" # let's replace the number 6 with the word "six"
str(vec_1)
class(vec_1)

countries <- c("ITA", "JAM", "JAM", "JAM", "USA", "USA", "CAD", "UK", "USA")

str(countries)

table(countries)

countries_fac <- as.factor(countries)

countries_fac

# sometimes there is no difference
table(countries_fac)

# but sometimes we get problems
as.numeric(countries)
as.numeric(countries_fac)


Protests <- c(1, 2, 4, 5, 44, 6, 10)
GDP <- c(31, 74, 3, 7, 100, 234, 41)
country_name <- c("Uganda", "Australia", "UK", "USA", "Fiji", "Nevis", "India")

dat_protest <- data.frame(country_name, Protests, GDP)
str(dat_protest)

dat_protest <- data.frame(Protests, GDP, country_name)
names(dat_protest)

dat_protest$Protests # OR
dat_protest[,"Protests"]

ifelse(dat_protest$Protests > 5, "big", "small")

fruits <- c("bananas", "apples", "grapes", "pears", "oranges")

for (i in fruits){
  cat("I like", i, "\n") # \n prints a new line
}

for (i in 2:3){
  cat("I like", fruits[i], "\n") # \n prints a new line
}

fruits

cat("I like", fruits[3])

x <- 100 # we start with our initial data point

for (i in 1:30){ # we set the loop to run 30 times
  x <- x/2 # we re-write x as a function of itself
}
