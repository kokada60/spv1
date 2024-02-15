library(tidyverse)
library(tidymodels)

w <- c(168, 428, 296, 392, 80, 56, 352, 444, 168, 200, 4, 52, 20, 228, 72)
cs <- c(272, 300, 311, 365, 167, 149, 366, 310, 192, 229, 88, 118, 62, 319, 193)

length(w) 
length(cs)

suesLot <- data.frame(WeeksOnJob =w, Carsold = cs)
suesLot2 <- data.frame(WeeksOnJob = w, Carsold = cs, WeeksOnJobSq = w ^ 2)
suesLot2 

m1 <- lm(formula = Carsold ~ WeeksOnJob, data=suesLot) 
m1
m2 <- lm(formula=Carsold ~ WeeksOnJobSq + WeeksOnJob, data=suesLot2)
m2
summary(m2)
summary(m1)$residual


f1 <- function(p, x) {
  x ^ p
}

N <- 100000000
xc <- runif(N, 0, 1)
xc
yc <- runif(N, 0, 3)
yc

chk <- yc <= f1(4, xc) 
( sum(chk) / N ) * ( 3 * 1 ) 


A <- pi * ( r ^ 2 ) 

x^2 + y^2 = 1 
y^2 = 1 - x^2 
y = sqrt( 1 - x^2 )
x <- runif(N, 0, 1)


f2 <- function(x) {
  abs(sqrt(1 - (x ^ 2)))
}

y <- runif(N, 0, 1)

shotTheCircle <- ( f2(x) >= y )
shotTheCircle %>% mean() * 4
pi
N

# How about semi-circle?? 
# X will be limited to range of -1 to 1. 
# Y will be limited to range of 0 to 1. Not -1 to 1. 
# So in this case it might be necessary to sample full range of values for both X and Y.
# The preliminary solution is still be area covered by f(x)... PI will be calculated from hit ratio. 
x <- runif(N, -1, 1) 
y <- runif(N, 0, 1) 

# f(X) is the semi-circle, bounded at the bottom by x-axis ( y = 0 ). 
shotTheCircle <- ( f2(x) >= y ) %>% mean()
2 * shotTheCircle * 2

# shotTheCircle ~ ( Semi-Circle Area ) / ( Rectangle Area ),  with Rectangle Area = 2 * 1. So the Semi-Circle Area = shotTheCircle * 2 
# 2*shotTheCircle ~ 1.570852 
# So pi approximation ~ 2 * shotTheCircle * 2 ~ 3.141704
pi


# Multivariate integration approximation... 
# Instead of trapping dots inside a curve, the dots will be trapped within a surface. Hypercurve to Hypersurface. Remember partial derivative? Mutivariate integration? 
# Function in this case will be  z = (x^2) * y 
# x and y will be bounded within 0:1 range. 
# Within that constraint, x will also be within 0:1 range. 


f4 <- function(x, y) {
  (x^2) * y
}
x <- runif(N, 0, 1) 
y <- runif(N, 0, 1) 
z <- runif(N, 0, 1) 
# x and y distribution will be sampled using runif because of the assumption that all points will have equal probability of being selected for both x and y, that selection will be
# completely random under equal probability. 
# z points will be simulated under the same premise, that all points are equally possible. 

shotThePolygon <- ( f4(x, y) >= z )
shotThePolygon %>% mean() 

shotThePolygon ~ polygonVolume / CubeVolume with CubeVolume = 1. 
shotThePolygon ~ polygonVolume ~ 0.1667

  # Try developing two folded sampling solution, ie. taking the mean of the routine above repeated m-times, with each iteration taking N- runif samples of x and y. 
  # The m-iterations could be "reduced" to calculate the cumulative sum and taking the mean at the end. Probably not cummulative mean, because why add extra calculation 
  # at each iteration? But if it instant view is desired at each iteration, perhaps this is an option to be considered. 
f44 <- function(N, f) {
     
}

randomise <- function(f) f(runif(1e3)) 
randomise(mean)
randomise(sum)


library(repurrrsive)
library(listviewer)
jsonedit(got_chars)


f1 <- function(got) {
  got$books %>% unlist() %>% paste0(sep=" ")
}

got_chars %>% map(f1)
a4 <- map(got_chars, f1) 

a <- vector("list", 20)
a2 %>% is.list()
a2 <- c(1:10)
f1(got_chars[[1]])
seq_along(c(1:10))


got_chars %>% length() 
a4 %>% length()

map_int(got_chars, length)
got_chars %>% length()
got_chars[[1]] %>% length()

got_chars %>% map_int(function(.x) { .x$books %>% length() })

got_chars %>% map_lgl( ~ .$alive)
got_chars %>% map(~ list(name=.$name, id=names(.)))
imap(got_chars, ~ list(name = .$name, id=.y) %>% unlist()) 

map_lgl(got_chars[[1]], is.numeric)
map_lgl(got_chars[[1]], is.numeric) %>% map(got_chars[[1]], `[`, .)
map(got_chars[[1]], `[`)
map(mtcars, `[`)
cross(got_chars[[1]]$book, got_chars[[1]]$aliases) %>% 
select_if(got_chars[[1]], is.numeric)

a <- map_dbl(mtcars, ~ length(unique(.x)))
map_chr(got_chars, "books")
got_chars %>% is.list()

x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

map_chr(x, list("y", 1))

g1 <- list(books=got_chars[[1]]$books, aliases=got_chars[[1]]$aliases, id=got_chars[[1]]$id)
g2 <- list(books=got_chars[[2]]$books, aliases=got_chars[[2]]$aliases, id=got_chars[[2]]$id)
g3 <- list(books=got_chars[[3]]$books, aliases=got_chars[[3]]$aliases, id=got_chars[[3]]$id)
gs <- list(g1, g2, g3) 
map(gs, "books") %>% unlist()

g1 %>% is.list()

map(got_chars, "books") 

list(1:5, c(1:10, NA))
sum(1:10)

map_lgl(mtcars, is.numeric)
map_dbl(mtcars, sd)

mtcars2 <- mtcars
mtcars2 <- mutate(mtcars, cc=rep("cc", mtcars2 %>% nrow()), cc2 = rep("cc2", nrow(mtcars)))
                  
mtcars2 %>% select_if(is.numeric) %>% map_dbl(sd)

mtcars %>% select_if(is.numeric)
iris %>% str()
iris %>% select_if(is.factor) %>% map( ~ nlevels(.)) -> cc 

x <- list(
  list(1, c(3, 9)), 
  list(c(3, 6), 7, c(4, 7, 6))
)
x
triple <- function(x) x * 3
map(x, map, .f=triple) 
map(x, triple)

x
map(x, ~ map(.x, triple))
map(x, function(.x, f) { map(.x, f) }, triple)


trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(7, 10)))
trials %>% map("p.value") %>% unlist() %>% data.frame(pv = .) %>% ggplot(aes(x=pv)) + geom_histogram(bins=100)
trials %>% map("p.value") %>% 

  formulas <- list(
    mpg ~ disp, 
    mpg ~ I(1 / disp), 
    mpg ~ disp + wt, 
    mpg ~ I(1 / disp) + wt
  )

formulas %>% map(~ lm(formula=., data=mtcars))

bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}
bootstraps <- map(1:10, ~ bootstrap(mtcars)) 

by_cyl <- mtcars %>% split(.$cyl)
by_cyl %>% jsonedit()
a <- imap(by_cyl, function(.x, .y) { .y = list(.x) }) %>% as_tibble()
a2 <- pivot_longer(a, cols=names(a), names_to="cyl", values_to="split")
a2 <- a2 %>% mutate(model = map(split, function(.x) { lm(.x$mpg ~ .x$wt, data = .x)}))
jsonedit(a2) 

a2 %>% mutate(coefs = map(a2$model, ~ coefficients(.x)))
a2 %>% mutate(ag = map(a2$model, augment))

mtcars %>% summarise(across(where(is.numeric), mean))
library(dplyr)
mtcars %>% as_tibble() %>% mutate(across(where(is.double), round))

mtcars %>% mutate(across(where(is.double), round))
mt <- mtcars %>% rownames_to_column(var="ModelName") %>%  as_tibble()

ac_items <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')

ac_items %>% group_by(category) %>% summarise(across(.cols=c("sell_value", "buy_value"), .fn=mean, na.rm=TRUE))
ac_items %>% mutate(across(.cols=c("sell_value", "buy_value"), .fn=function(.x) {replace_na(.x, 0)+200}))

af <- ac_items %>% filter(category=="Fish") %>% select(buy_value) 
max(af)
af2 <- rbind(af, data.frame(buy_value = 10.20))

max(af, na.rm=T)

max(.x, na.rm=TRUE)



max(ac_items$buy_value)

# Inf in all NA vector... 
max.Vector <- function(x) {
  if(!all(is.na(x))) { max(x, na.rm=TRUE) }
  else { 0 }
}
z0 <- rep(0, nrow(af))
z0
flatten(af) %>% unlist()


# consider the divide-by-zero case in .fn 
prop_Calc <- function(x, xmx) {
  if_else(is_empty(xmx) | xmx==0, list(rep(0, nrow(x))), unlist(x / xmx))  
}
ac_items %>% group_by(category) %>% 
  mutate(across(c(sell_value, buy_value), ~ prop_Calc(.x, max.Vector(.x)), .names="{col}_proportion")) %>% 
  select(category, ends_with("_proportion"))

ac_items %>% group_by(category) %>% 
  mutate(across(c(sell_value, buy_value), list(prop=~ .x/max.Vector(.x), max=~ max.Vector(.x)), .names="{.col}_{.fn}")) %>% 
  select(category, sell_value, buy_value, ends_with(c("_max", "_prop"))) %>% View()
         
ac_items %>% group_by(category) %>% 
  mutate(across(contains("_value"), list(prop=~.x / max.Vector(.x), max=~max.Vector(.x)), .names="{.col}_{.fn}")) %>% 
  select(category, ends_with(c("_value", "_max", "_prop"))) %>% View()


1:4 %>% 
  reduce(.f = `+`, .init=100)

numbers <- seq(1, 100) 
sum(numbers) 
numbers %>% reduce(.f = `+`)
a <- function(a, b) {
  a + b
}

numbers %>% reduce(a)
numbers %>% accumulate(a) 

text <- "The quick brown fox jumps over the lazy dog."
text %>% str_to_lower() %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("\\s\\s+", "\\s") %>% 
  str_split("\\s", simplify = FALSE) %>% 
  unlist() %>% 
  map(~ data.frame(key=., value=1)) %>% 
  reduce(rbind)

list.files(path=".//data", pattern=".xlsx") %>% 
  map(~ paste(".//data//", ., sep="")) %>%
  map(read_xlsx) %>% reduce(rbind)


mtcars %>% colnames()

#init = "mpg ~ cyl" 
#accumulate by adding one column at a time. 
i <- setdiff(colnames(mtcars), c("mpg", "cyl"))

models <- accumulate(i, paste, sep=" + ", .init="mpg ~ cyl") %>%
  set_names(1:length(.)) %>%
  enframe(., name="model", value="spec") %>% 
  mutate(fittedmodel = map(spec, lm, data=mtcars))

rs_of_models <- models$fittedmodel %>% 
    map( summary) %>% 
    map_dbl("adj.r.squared") %>% 
    enframe(value="Adj-RSqr") 

bind_cols(rs_of_models, models)

a <- c(1, 2, 3, 4, 5, 6) 
keep(a, `==`, 5)

head_while(a, is.numeric)
head_while(a, ~ . > 4)


starwars %>% View()


map_if_df(starwars, is.numeric, function(.x){.x*2}) 

starwars


a <- "round"
b <- list(10.999)
do.call(a, b)




