
age <- 29

height = 1.7

age.height = age/height

# vectors
student1 = c(age,height)
student2 = c(78,1.9)

large_vector = c(student1,student2,"Nairobi","Kampala")

# list
large_vector2 = list(student1,student2,"Nairobi","Kampala")

# access vector items
large_vector2[[1]][1]
large_vector[1]

# data types
# Numeric data types
# Character
# boolean
# Date

# Coercing
typeof(large_vector)
typeof(large_vector2)
# Basic string operations
toupper("George")
tolower("George")
paste("My name is","George",sep = "_")
paste0("My name is","George")
sprintf("Working with str %s",3445345)
#Control structures
# comparison
2 == 3
"hello" != "hell"
23 < 56
if (age < 29) {
  print("Age is less than 29")
} else{
  print("Age is greater  than 29")
}
num = 12
if (num > 5) {
  print('Bigger than 5')
}
if (num > 10) {
  print('Bigger than 10')
}
myvar = ifelse(num > 5,('Bigger than 5'),
               ifelse(num > 6,('Bigger than 6'),('Bigger than 5')))

for (i in student1) {
 print(i)
}

x = 56
while (x <= 56 ) {
  print("ddsff")
  x = x+10
}

# functions

my_fun <- function(age){
  age_squrd = age * age
  return(age_squrd)
}
my_fun2 <- function() {
  x = 3+5
  return(x)
}
my_fun2()
my_fun3 <- function(age = 56) {
  agesqrd = age * age
  return(agesqrd)
}
my_fun3(age = 3)
my_fun4 <- function(age,gender,country = NULL) {
  out = paste("Your age is",age,"and you are a ",gender,
              ifelse(test = !is.null(country),yes = paste("You are from",country),no = "" ))
  return(out)
}
my_fun4(age = 34,gender = "Female")

age = c(23,45,67,12,78,90)
mean(age)
sd(age)
sqrt(age)
length(age)
sum(age)
summary(age)
# packages

# Data frame


