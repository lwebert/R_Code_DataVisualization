##############CHAPTER 14#######################################################
library(magrittr)
library(pryr)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>%
  dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

foo_foo <- little_bunny()
foo_foo %>%
  hop(through = forest) %>%
  scoop(up = field_mouse) %>%
  bop(on = head)

#other tools: %T>% pipe, %$% pipe, %<>% transform operator
rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot %>%
  str()

mtcars %$%
  cor(disp, mpg)

mtcars <- mtcars %>%
  transform(cyl = cyl * 2)
mtcars %<>% transform(cyl = cyl * 2)

