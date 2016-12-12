library("dplyr")
library("ggplot2")
diamonds <- diamonds

slice(diamonds, c(1,4,5))

filter(diamonds, carat > 0.3, color == 'J')
subset(diamonds, carat > 0.3 & color == "J")
arrange(diamonds, desc(price))
arrange(diamonds, -price)
mutate(diamonds, sqrt_price = price**2)
nrow(diamonds) - !(nrow(diamonds)%%2)
1 + !(344 %% 2)

slice(diamonds, seq(1, nrow(diamonds) - !(nrow(diamonds) %% 2), by = 2))
      

my_df <- select(mtcars, mpg, am, vs, hp) %>% 
  filter(mpg > 14, hp > 100) %>% 
  arrange(-mpg) %>% 
  slice(1:10) %>% 
  rename(`Miles per gallon` = mpg, `Gross horsepower` = hp)

# Mutate each

df <- as_data_frame(matrix(rnorm(40), ncol = 5))
df %>% mutate_each(funs(. ** 2))
all_to_factor <- function(x){
  mutate_each(x, funs(as.factor(.)))
  
  
}

center <- function(x) (x - min(x))/(max(x) - min(x)) + 1
center(rnorm(10))
df$lol <- 'lol'

df[,sapply(df, is.numeric)] <-  mutate_each(df[,sapply(df, is.numeric)], funs(log2(center(.))))
