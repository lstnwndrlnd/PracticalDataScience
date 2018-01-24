#! install.packages(c("ggplot2", "knitr", "psych", "rmarkdown", "reshape2", "swirl"), dependencies=TRUE)

library(ggplot2)
library(knitr)
library(psych)
library(rmarkdown)
library(reshape2)
library(swirl)

#check if a particular package is missing
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

# assignment operators
x <- 4
y = 3
# super operator
# x <<- 4

# What is a namespace

bob = "bob"

class(x)
class(bob)

ls()

rm(bob)

ls()

cfb1 <- read.csv("data/cfb2017.csv")
cfb2 <- read.csv("https://raw.githubusercontent.com/JackStat/PracticalDataScience/master/data/cfb2017.csv")

# find dimensions of array
dim(cfb2)
# look at first few rows of file
head(cfb2)
#look at last few rows of file
tail(cfb2)
#look at 'Win' column
cfb1$Win

cfb2[1:5,"Win"]

class(cfb2$Win)
class(cfb2$Date)

sapply(cfb2, class)

(ss <- sapply(cfb2, class))
setwd("data")
setwd("../")

s+2
s-2
5*2
5^2
5**2
5/2
5%*%2
5%/%2
5%%2

seq(1,99,2)

odds <- seq(1,99, 2)
evens <- seq(2,100, 2)
all <- seq(1,100)
evens2 <- all[all%%2 == 0]

all(evens == evens2)
all(evens != evens2)
any(evens!=evens2)
any(evens==evens2)


hey = "hey momma"
string2 = "I'm a string"

#concatonate strings- "paste" them together
paste(hey, string2)

hey2 = c("hey momma", "hey Dadda")
string3 = "I'm a string"

#collapse is a space by default
st1 <- paste(hey2, string3, collapse = ",")
st2 <- paste(hey2, string3)

char2 <- paste("hey", "momma", "I'm", "a", "string")
char2

#search for string and return a boolean for each word in a string
grepl('momma', st2)
grepl('string', st2)

## any number
grepl('[0,9]', st2)

## any number (case sensitive)
grepl('[A-Za-z]', st2)

grepl("\\'", st2)

# search and replace
gsub("\\'", "SINGLEQUOTE", st2)

gsub("Hey (.*) I'm (.*)", "\\1", st2)

gsub("Hey (.*) I'm (.*)", "I am your \\1 and I am not a \\2", st2)

all

mod = rep(NA, length(all))



for (i in all){
  # print(i)
  if (i %% 2 == 0){
    mod[i] = 1
  }else{
    mod[i] = 0
  }
}
  
  
TRUE == TRUE
TRUE != TRUE
FALSE == TRUE
FALSE != TRUE

TRUE == TRUE | FALSE == FALSE
TRUE == TRUE | TRUE == FALSE

TRUE >FALSE
FALSE >= TRUE

class(mod)

is.numeric(mod)
is.character(mod)
is.data.frame(mod)

as.character(mod)

iris

dim(iris)
head(iris)
class(iris$Species)
str(iris)
levels(iris$Species)

as.numeric(iris$Species)
# A factor is really a number, but to us it looks like a character
as.character(iris$Species)

as.numeric(as.character(factor(c(1:5, "missing"))))
c(1,2,3,4) * c(TRUE, FALSE)

runif(100)
hist(runif(10000000))

N = 1250
plot(runif(N), runif(N))

rnorm(10000)
plot(density(rnorm(10000)))

samples = 10000
sample_size = 1000
sample_means = rep(NA, samples)

for(i in 1:samples){
  Values = rnorm(sample_size)
  sample_means[i] = mean(Values)
}

hist(sample_means)
plot(density(sample_means))

thelist <- list(
  iris = head(iris)
  , evens = evens
)

thelist[[1]]
thelist$evens

matrix()

as.matrix(1:10)
t(as.matrix(1:10))
mat1 <- matrix(1:10, ncol = 2)
mat2 <- matrix(1:10, nrow = 2)

a = mat1 %*% mat2



# install.packages("lme4")
library(lme4)

cfb1$Margin = with(cfb1, ScoreOff - ScoreDef)
ll <- lmer(Margin ~ (1|Team) + (1|Opponent), data = cfb1)

class(ranef(ll))
str(ranef(ll))

rr <- ranef(ll)
str(rr)

rr$Team
dim(rr$Team)

lmerRank <- data.frame(
  Team = rownames(rr$Team),
  OffenseScore = rr$Team[,1]
)

library(dplyr)

lmerRank %>%
  arrange(desc(OffenseScore)) %>%
  mutate(
    Rank = 1:n()
  ) %>%
  View


















