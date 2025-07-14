############CHAPTER 2##########################################################
####################### calling functions ###
seq(1,10)
x <- "hello world"
y <- seq(1,10, length.out=5)
y
(y <- seq(1,10, length.out=5))


###########CHAPTER 3##########################################################
library(nycflights13)
library(tidyverse)

flights

#############################pick observations by their values with filter
#Filter Rows
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))

#Comparisons: >, >=, <, <=, !=, ==
near(sqrt(2) ^ 2, 2)
near(1/49*49,1)

#Logical Operators: "and" &, "or" |, "not" !
filter(flights, month == 11 | month ==12)
filter(flights, month %in% c(11,12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#Missing Values
is.na(x)
df <- tibble(x=c(1,NA,3))
filter(df, x>1)
filter(df, is.na(x) | x>1)

#############################reorder the rows with arrange
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

df <- tibble(x = c(5,2,NA))
arrange(df,x)
arrange(df, desc(x))

#############################pick variables by their names with select
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

select(flights, starts_with("yea"))
select(flights, ends_with("ime"))
select(flights, contains("time"))
select(flights, matches("(.)\\1"))
select(flights, num_range("x", 1:3))

#rename variables: new name = old name
rename(flights, tail_num = tailnum)

#move variables to start of df
select(flights, time_hour, air_time, everything())

#############################create new variables with functions of existing variables with mutate
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)

mutate(flights_sml, gain = arr_delay - dep_delay, speed = distance/air_time*60)
mutate(flights_sml, gain = arr_delay - dep_delay, hours = air_time/60, gain_per_hour = gain/hours)

#use transmute() to only keep new variables
transmute(flights, gain = arr_delay - dep_delay, hours = air_time/60, gain_per_hours = gain/hours)

#############################collapse many values down to a single summary with summarize
summarize(flights, delay = mean(dep_delay, na.rm=TRUE))

#use group_by() with any of those above arguments
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm=TRUE))

#combine multiple operations with the pipe

### Example: Relationship between average delay time & distance for each location

# 1. group flights by destination
by_dest <- group_by(flights, dest)
# 2. summarize - to compute distance, avg delay, and # of flights
delay <- summarize (by_dest, count=n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
# 3. filter to remove noisy points and Honolulu airport
delay <- filter(delay, count > 20, dest != "HNL")
# 4. plot it
ggplot(data = delay, mapping = aes(x=dist, y=delay)) +
  geom_point(aes(size=count), alpha = 1/3) +
  geom_smooth(se=FALSE)

#OR you can use the pipe %>% (read as "then")

delays <- flights %>%
  group_by(dest) %>%
  summarize(count=n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE)) %>%
  filter(count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x=dist, y=delay)) +
  geom_point(aes(size=count), alpha = 1/3) +
  geom_smooth(se=FALSE)

#na.rm=TRUE removes missing values, or filter "not (!) missing values (is.na) of a variable x"
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize (mean = mean(dep_delay))

#Counts with summarize(): Count n() or count of non-missing values sum(!is.na(x))
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay))
ggplot(data = delays, mapping = aes(x=delay)) +
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay, na.rm = TRUE), n=n())
ggplot(data = delays, mapping = aes(x=n, y=delay)) +
  geom_point(alpha = 1/10)

delays %>%
  filter(n>25) %>%
  ggplot(mapping = aes(x=n, y=delay)) +
  geom_point(alpha=1/10)


#Plot skill of batter (ba) against opportunities to hit (ab)
batting <- as_tibble(Lahman::Batting)

batters <- batting %>%
  group_by(playerID) %>%
  summarize(ba = sum(H, na.rm=TRUE) / sum(AB, na.rm=TRUE), ab = sum(AB, na.rm=TRUE))

batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x=ab, y=ba)) +
  geom_point() +
  geom_smooth(se=FALSE)


#Grouping of Multiple Variables
daily <- group_by(flights, year, month, day)
(per_day <- summarize(daily, flights = n()))
(per_month <- summarize(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))
