################CHAPTER 7#####################################################
library(tidyverse)
library(nycflights13)

#Coerce a df to a tibble
as_tibble(iris)

#Create new tibble from individual vectors
tibble(x = 1:5, y = 1, z = x ^ 2 + y)

#Odd column names
tb <- tibble(':)' = "smile", ' ' = "space", '2000' = "number")
tb

#Create a transposed tibble: data entry in code: column headings start with ~, entries separated by ,
tribble(~x, ~y, ~z,
        #--/--/----
        "a", 2, 3.6,
        "b", 1, 8.5)

############PRINTING
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace=TRUE)
)

nycflights13::flights %>%
  print(n=10, width=Inf)

###########SUBSETTING: use [[ ]] to extract single variable by name or position, use $ to extract by name with less typing
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

#extract by name
df$x
df[["x"]]

#extract by position
df[[1]]

#to use in a pipe use placeholder .
df %>% .$x
df %>% .[["x"]]


