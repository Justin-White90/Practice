library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

#Examing relationships between price and variables of a diamond
ggplot(diamonds, aes(cut,price)) + geom_boxplot()
ggplot(diamonds, aes(color,price)) + geom_boxplot()
ggplot(diamonds, aes(clarity,price)) + geom_boxplot()


ggplot(diamonds, aes(carat,price)) + geom_hex(bins = 50)

#Filter the data to only include 2.5 carats or lower
# change the scales to log scales to better represent the relationship
diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

#Add the log2carat coloumn, add prediction of price in log scale
#Add new column with predicitions with regular price scale
grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat,20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2^lprice)

grid

ggplot(diamonds2, aes(carat,price)) +
  geom_hex(bins=50) +
  geom_line(data = grid, color = "red", size =1)

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat,lresid)) + 
  geom_hex(bins =50)

ggplot(diamonds2, aes(cut,lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color,lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity,lresid)) + geom_boxplot()

#add all variables to the model
mod_diamonds2 <- lm(lprice ~lcarat + color + cut + clarity, data = diamonds2)

grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamonds2) %>%
  add_predictions(mod_diamonds2)

ggplot(grid,aes(cut,pred))+
  geom_point()

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamonds2, "lresid2")

ggplot(diamonds2, aes(lcarat,lresid2)) + geom_hex(bins=50)

# Select the outliers for a summary look
diamonds2 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamonds2) %>%
  mutate(pred = round(2 ^pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)

#summarise and graph the flight data for number of flights leaving NYC
daily <- flights %>%
  mutate(date = make_date(year,month,day)) %>%
  group_by(date) %>%
  summarise(n = n())

ggplot(daily,aes(date,n)) + geom_line()

#Split the dates into each day of the week
daily <- daily %>%
  mutate(wday = wday(date,label = TRUE))

ggplot(daily,aes(wday,n))+ geom_boxplot()

#Create the model for each day of the week
mod <- lm(n ~ wday, data = daily)

# Record predictions for each day of the week
grid <- daily %>%
  data_grid(wday) %>%
  add_predictions(mod, "n")

#Graph predictions
ggplot(daily,aes(wday,n)) + 
  geom_boxplot() + 
  geom_point(data = grid, color = "red", size = 4)

daily <- daily %>%
  add_residuals(mod)

daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date,resid))+
  geom_ref_line(h=0) +
  geom_line()

ggplot(daily,aes(date,resid,colour = wday)) + geom_ref_line(h = 0) + geom_line()

daily %>%
  filter(resid < -100)

daily %>%
  ggplot(aes(date,resid)) + geom_ref_line(h=0) +
  geom_line(color = "grey50")+
  geom_smooth(se = FALSE, span = 0.2)

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101), 
      labels = c("spring","summer", "fall"))
}
daily <- daily %>%
  mutate(term = term(date))

daily %>%
  filter(wday=="Sat") %>%
  ggplot(aes(date,n,colour = term)) + 
  geom_point(alpha=1/3) +
  geom_line() + 
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>%
  ggplot(aes(wday,n, colour = term)) + geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

mod3 <- MASS::rlm(n~wday * term, data = daily)

daily %>%
  add_residuals(mod3, "resid") %>%
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 2, colour = "white") + 
  geom_line()

