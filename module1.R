# Apply functions

df <- data.frame(rnorm(26), letters, runif(26), LETTERS)

unname(unlist(sapply(df,  function(x) { if (is.numeric(x)) { median(x)}})))

apply(df, 2, median)

sapply(df,  function(x) { if (is.numeric(x)) { median(x)}},USE.NAMES = F)
is.numeric(df[,1])
median(df[,3])

# get neg vals

test_data <- as.data.frame(list(V1 = c(NA, -0.5, -0.7, -8), V2 = c(-0.3, NA, -2, -1.2), V3 = c(1, 2, 3, NA)))

test.data.not.list <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), V2 = c(NA, -10.2, -10.1, -9.3, -12.2), V3 = c(NA, NA, -9.3, -10.9, -9.8)))

get_negative_values <- function(df){
  x1 <- apply(df, 2, function(x) x[!is.na(x) & x < 0])
  x2 <- x1[sapply(x1, length)>0]
  x3 <- lapply(x2, length)
  if (all(x3 == x3[[1]])) {
    return(do.call(cbind, x2))
  } else {
    return(x2)
  }
}

x <- get_negative_values(test_data)
str(get_negative_values(test_data))

y <- get_negative_values(test.data.not.list)


get_negative_values <- function(test_data){  
  # Вариант решения из ответов
  negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
  return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))
}


# Замени na на mean
test_data <- as.data.frame(list(V1 = c(NA, NA, NA, NA, 13, 12, 9, 10, 8, 9, 11, 11, 10, 12, 9), V2 = c(NA, 12, 8, NA, 11, 11, 9, 8, 8, 10, 10, 11, 10, 10, 10), V3 = c(NA, 5, NA, 13, 12, 11, 11, 14, 8, 12, 8, 8, 10, 10, 8), V4 = c(10, 10, 10, 10, 13, 10, 11, 7, 12, 10, 7, 10, 13, 10, 9)))

na_rm <- function(df){
  d <- apply(df, 2, function(x){
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
  })
  return(d)
}

na_rm(test_data)


# Positive sums
positive_sum <-  function(test_data){
  retlist <- lapply(test_data, function(x){
    y <- x[!is.na(x) & x > 0]
    return(ifelse(length(y) > 0, sum(y), 0))
  })
  return(retlist)
  
}

d <- data.frame(X1 = c(-1, -2, 0), X2 = c(10, 4, NA), X3 = c(-4, NA, NA))

positive_sum(d)


# Filter by names
DATA <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
noms = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
grepl("HPS5", rownames(DATA))

DATA[sapply(noms, function(x) which(grepl(x, DATA[,1]))), ]

my_names <- function(df, names){
  df[sapply(names, function(x) which(grepl(x, df[,1]))), ]
}


# Find outliers




# Lm for normal data

smart_lm <- function(df){
  numeric <- df[,sapply(df, is.numeric)]
  norm <- numeric[,sapply(numeric, function(x){
    shapiro.test(x)$p.value > 0.05
  })]
  if (length(norm) < 2) return("There are no normal variables in the data")
  fit <- lm(as.formula(paste(names(norm[1]), "~", paste(names(norm[2:length(norm)]),
                                                        collapse = "+"))), norm)
  return(fit$coefficients)
  
}

x <- smart_lm(swiss)
glm(swiss[,1] ~ swiss[,4] + swiss[,3], family="binomial", swiss)
fla <- as.formula(paste(names(swiss[1]), "~", paste(names(swiss[2:4]), collapse = "+")))
names(swiss[1])
swiss
lm(Fertility ~ Agriculture, family = "binomial", swiss)
summary(x)
nonshapdf <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")
smart_lm(nonshapdf)
smart_lm(data.frame(x = 1:100, y = 1:100, z = 1:100))

# Выборочный t-тест

one_sample_t <- function(df, trumean){
  df_num <- df[,sapply(df, is.numeric)]
  lapply(df_num, function(x) unlist(t.test(x, mu = trumean)[1:3]))
}


one_sample_t(iris[, 1:4], 4)
str(t.test(iris[, 1], mu = 4))

# get one value from lists

normality_tests <- lapply(iris[, 1:4], shapiro.test)

lapply(normality_tests, function(x) x$p.value)
get_p_value = function(test_data){    
  sapply(test_data, '[', 2)}

sapply(normality_tests, '[', 2)

###############################
#                             #
#      DPLYR                  #
#                             #
###############################
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

# функция перевода заданных колонок в факторы

to_factors <- function(df, factors){
  df %>% mutate_at(factors, funs(as.factor(as.numeric(. > mean(.)))))
}

to_factors(mtcars[1:4], factors = c(1, 3))

# High price

high_price <- diamonds %>% 
  select(color, price) %>% 
  group_by(color) %>% 
  arrange(-price) %>% 
  slice(1:10)

#########################
#                       #
#     DATA.TABLE        #
#                       #
#########################

