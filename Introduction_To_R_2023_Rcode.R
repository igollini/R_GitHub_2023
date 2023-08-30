#######
#  5  #
#######

3 + 4
?"+" # look up help for "+". You need quotes!
x <- 3 + 4 # store 3 + 4 into the object x
x # print the object R
y <- log(x) # store the natural log of x in the object y
log(x) -> y # same as before using -> instead of <-
y = log(x) # same as before using the = instead of <-
3 == 4 # == is the comparison operator for equal
3 != 4 # != is the comparison operator for not equal
?log # look up help for the function log
ls() # list of objects in the current workspace
rm(x) # remove the object x
3 != 4 # != is the comparison operator for not equal

#######
#  6  #
#######

M <- matrix(c(4, 2, 2, 3), 2, 2, byrow = FALSE) # create a matrix
M # print the matrix
solve(M) # finds the matrix inverse or a matrix equation solution
t(M) # matrix transpose
C <- chol(M) # Cholesky decomposition
t(C) %*% C # matrix multiplication
qr(M) # QR decomposition
eigen(M) # eigen decomposition
det(M) # Determinant
diag(M) # Find or set the matrix diagonal

#######
# 11  #
#######

data() # find out what standard data sets there are
plot(iris) # plot Fisher's iris data
head(iris, 4) # print the first 4 rows of the iris data
View(iris) # view the iris dataset on the viewer
summary(iris,  # summaries of the iris dataset up to the 2nd digit
        digits = 2) # you can split the command in two lines
sum(3, 4) # do the sum of 3 and 4
log(sum(1:10)) # Sum the numbers from 1 to 10 and then takes the natural log

#######
# 13  #
#######

x <- c(1, 3, 6)
y <- c("red", "yellow", "green")
z <- c(TRUE, FALSE)

#######
# 14  #
#######

str(iris)

#######
# 15  #
#######

standard_deviation <- function(x){
  if(!is.numeric(x)) { # check the input is numeric
    stop('This function needs a numeric input!\n',
         'Not an object of class: ', class(x)[1])
  }
  xbar <- mean(x) # sample mean
  N <- length(x) # sample size
  sqrt(sum((x - xbar)^2) / (N - 1)) # output
}

#######
# 16  #
#######

standard_deviation(iris)
standard_deviation(iris$Sepal.Length)
sd(iris$Sepal.Length)
apply(iris[,-5], 2, standard_deviation)


#######
# 30  #
#######

infant <- rio::import("infant/infant.xlsx", setclass = "tibble") # line not on the slides

library(dplyr)
infant <- filter(infant, smoke != 9 & age > 18)

#######
# 31  #
#######

select(infant, gestation, sex)
select(infant, pluralty:gestation, parity)
select(infant, -(id:outcome), -sex)

#######
# 32  #
#######

mutate(infant, 
       wt = ifelse(wt == 999, NA, wt),
       wt = wt * 0.4536)

#######
# 33  #
#######

infant <- infant |> 
    filter(smoke != 9 & age > 18) |>
    select(-(id:outcome), -sex) |>
    mutate(wt = ifelse(wt == 999, NA, wt),
           wt = wt * 0.4536)

#######
# 35  #
#######

stats <- summarise(infant,
                   `Mean mother's weight (kg)` = mean(wt, na.rm = TRUE),
                   `Sample size` = sum(!is.na(wt)),
                   `Total sample size` = n())
stats

#######
# 37  #
#######

infant <- infant |> filter(race != 10) |>
    mutate(`Ethnic group` = recode(race, `6` = "Latino", `7` = "Black",
                                   `8` = "Asian", `9` = "Mixed",
                                   .default = "White"))
res <- infant |> group_by(`Ethnic group`) |>
    summarise(`Mean weight (kg)` = mean(wt, na.rm = TRUE))
res

#######
# 38  #
#######

cut(c(0.12, 1.37, 0.4, 2.3), breaks = c(0, 1, 2, 3))

#######
# 39  #
#######

infant <- infant |>
    mutate(gestation = ifelse(gestation == 999, 
                              NA, gestation))

#######
# 40  #
#######

boxplot(infant$bwt)
with(infant, boxplot(bwt ~ `Ethnic group`))

#######
# 41  #
#######

hist(infant$bwt)
plot(density(infant$bwt))

#######
# 42  #
#######

plot(bwt ~ gestation, data = infant)
plot(infant$gestation, infant$bwt)

#######
# 44  #
#######

library(ggplot2)
ggplot(infant, aes(y = bwt * 28.35, x = `Ethnic group`)) +
    geom_boxplot() + 
    ylab("Birth Weight (g)")


#######
# 45  #
#######

infant <- mutate(infant, smoke = recode_factor(smoke, 
                                       `1` = "currently", `2`= "until pregnancy", 
                                       `3` = "used to", `0` = "never"))
p <- ggplot(infant, aes(y = bwt * 28.35, x = gestation, color = smoke)) +
    geom_point() + labs(x = "Gestation", y = "Birth Weight (g)", 
                        color = "Smoking status")
p


#######
# 46  #
#######

p + facet_wrap(~ smoke) + guides(color = "none")

##################################
### Version control - Slide 70 ###
##################################

usethis::git_sitrep() 

# do not run the following code if you are all set up on git/github.

# usethis::use_git_config(
#     user.name = "igollini", # Your username
#     user.email = "isabella.gollini@ucd.ie") # Your email

# usethis::create_github_token()

# gitcreds::gitcreds_set()

