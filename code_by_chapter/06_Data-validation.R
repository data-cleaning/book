## ----message=FALSE-------------------------------------------------------
library(validate)
data(retailers)

## ----message=FALSE-------------------------------------------------------
cf <- check_that(retailers, turnover > 0, staff.costs/staff < 50)

## ------------------------------------------------------------------------
summary(cf)

## ------------------------------------------------------------------------
cf <-  check_that(retailers
    , to = turnover > 0
    , sc = staff.costs/staff < 50
    , cd = if ( staff > 0 ) staff.costs > 0
    , mn = mean(profit,na.rm=TRUE) > 0
  )  

## ------------------------------------------------------------------------
summary(cf)

## ----fig.height=4--------------------------------------------------------
barplot(cf[1:3],main="Retailers")

## ------------------------------------------------------------------------
v <- validator(
    turnover > 0
  , staff.costs / staff < 50
  , total.costs >= 0
  , staff >= 0
  , turnover + other.rev == total.rev
)
v

## ------------------------------------------------------------------------
cf <- confront(retailers, v)
cf

## ------------------------------------------------------------------------
head(values(cf))

## ------------------------------------------------------------------------
retailers[3,c("staff","staff.costs")]

## ----eval=FALSE----------------------------------------------------------
## v <- validator( x > 0, mean(y))

## ------------------------------------------------------------------------
v <- validator(
    x > 0
  , y > 0
  , x + y == z
  , u + v == w
  , mean(u) > mean(v))

## ------------------------------------------------------------------------
summary(v)

## ------------------------------------------------------------------------
v[c(1,3)]

## ------------------------------------------------------------------------
v[[1]]

## ------------------------------------------------------------------------
label(v)[1] <- "x positivity"

## ------------------------------------------------------------------------
variables(v)

## ----eval=FALSE----------------------------------------------------------
## variables(v, as="list")
## variables(v, as="matrix")

## ------------------------------------------------------------------------
w <- v

## ------------------------------------------------------------------------
names(v)
names(w)

## ------------------------------------------------------------------------
names(v)[1] <- "foo"
names(w)

## ------------------------------------------------------------------------
w <- v[TRUE]

## ------------------------------------------------------------------------
retailers %>% 
  check_that(turnover >= 0, staff >= 0) %>%
  summary()

## ------------------------------------------------------------------------
v <- validator(turnover >= 0, staff >= 0)
retailers %>% confront(v) %>% summary()

## ------------------------------------------------------------------------
v <- validator(employees >= 0)
cf <- confront(retailers, v)
cf

## ----eval=FALSE----------------------------------------------------------
## confront(retailers, v, raise="error")

## ----eval=FALSE----------------------------------------------------------
## confront(retailers, v, raise="all")

## ------------------------------------------------------------------------
v <- validator(x+y==1)
d <- data.frame(x=0.5,y=0.50001)
summary(confront(d,v))
summary(confront(d,v,lin.eq.eps=0.01))

## ------------------------------------------------------------------------
voptions(v, raise="all")

## ------------------------------------------------------------------------
voptions(raise="all")

## ------------------------------------------------------------------------
# query the global option setting (no argument prints all options)
voptions("raise")
# query settings for a specific object
voptions(v,"raise")

## ------------------------------------------------------------------------
validate::reset()

## ------------------------------------------------------------------------
validate::reset(v)

## ----eval=FALSE----------------------------------------------------------
## v <- validator(.file="rules.txt")

## ----eval=FALSE----------------------------------------------------------
## v <- validator(.file="yamlrules.yaml")

## ----eval=FALSE----------------------------------------------------------
## export_yaml(v, file="myfile.yaml")

## ------------------------------------------------------------------------
str <- as_yaml(v)

## ------------------------------------------------------------------------
class(x) == "numeric"
class(x) %in% c("numeric", "complex")

## ------------------------------------------------------------------------
check_that(iris, nrow(.) >= 10) %>% summary()

## ------------------------------------------------------------------------
data("retailers")
check_that(retailers, sum(is.na(.))/prod(dim(.)) < 0.2) %>% summary() 

## ----eval=FALSE----------------------------------------------------------
## gender %in% c('male','female')

## ----eval=FALSE----------------------------------------------------------
## tolower(gender) %in% c('male', 'female')

## ----eval=FALSE----------------------------------------------------------
## stringdist::ain(gender, c('male','female'), maxDist=2) == TRUE

## ------------------------------------------------------------------------
# create a data frame
d <- data.frame( gender = c('female','female','male','unknown'))
# create a validator object
v <- validator(
  gender_codes := c('female', 'male') # store a list of valid codes
 , gender %in% gender_codes           # reuse the list.
)
# confront and summarize
confront(d,v) %>% summary()

## ------------------------------------------------------------------------
v <- validator(
   turnover + other.rev == total.rev
  , other.rev <= turnover
)

## ------------------------------------------------------------------------
cf <- confront(retailers,v)
summary(cf)['expression']

## ----eval=FALSE----------------------------------------------------------
## if ( gender == "male" ) pregnant == FALSE

## ----eval=FALSE----------------------------------------------------------
## !(gender == "male") | pregnant == FALSE

## ----eval=FALSE----------------------------------------------------------
## v <- validator(if(gender == "male") pregnant == FALSE)
## d <- data.frame(gender = "female", pregnant = FALSE)
## summary(confront(d,v))

## ------------------------------------------------------------------------
v <- validator(test = if ( x + y == 10 ) z > 0)
d <- data.frame(x = 4, y = 5, z = -1)

## ------------------------------------------------------------------------
values(confront(d,v))

## ------------------------------------------------------------------------
values(confront(d,v,lin.eq.eps=2))

## ------------------------------------------------------------------------
v <- validator(cov(height,weight) > 0)
values(confront(women,v))

## ------------------------------------------------------------------------
v <- validator( height < median(height) + 1.5*IQR(height))
cf <- confront(women,v)
head(values(cf),3)

## ------------------------------------------------------------------------
v <- validator(city + street ~ zipcode)

## ------------------------------------------------------------------------
d <- data.frame(
  street = rep("Spui",4)
  , city   = c("The Hague", "The Hague","Amsterdam", "The Hague")
  , zipcode= c(2511,2513,2511,2511)
)
cbind(d, fd_value=values(confront(d,v))[,1])

## ----eval=FALSE----------------------------------------------------------
## all(street + city ~ zipcode)

## ------------------------------------------------------------------------
dutch_women <- data.frame(heightCM = 176.2)
v <- validator(
 inch := 1/2.54
 , us_mean := mean(height)
 , upplim = us_mean < 1.1 * ref$heightCM/inch
 , lowlim = us_mean > 0.9 * ref$heightCM/inch
)
summary(
  confront(dat = women,x=v, ref = dutch_women)
)[1:5]

## ------------------------------------------------------------------------
v <- validator(
 inch := 1/2.54
 , us_mean := mean(height)
   # we use 'dw' in stead of 'ref' now.
 , upplim = us_mean < 1.1 * dw$heightCM/inch 
 , lowlim = us_mean > 0.9 * dw$heightCM/inch
)
 # the reference data must be named correspondingly when calling 'confront'
cf <-  confront(women, v, ref=list(dw = dutch_women))

## ------------------------------------------------------------------------
# a reference environment in which we may store reference data sets or variables
refdat <- new.env()
# the name of the reference data is 'dw' in the reference environment
refdat$dw <- dutch_women
cf <- confront(women,v, ref=refdat)

## ------------------------------------------------------------------------
v <- validator(weight == ref$weight)
cf <- confront(women,v, ref=women)

## ----eval=FALSE----------------------------------------------------------
## m := mean(x, na.rm=TRUE)
## y < m
## z < m

## ----eval=FALSE----------------------------------------------------------
## y < mean(x, na.rm=TRUE)
## z < mean(x, na.rm=TRUE)

## ----eval=FALSE----------------------------------------------------------
## var_group(a,b) > 0

## ----eval=FALSE----------------------------------------------------------
## a > 0
## b > 0

## ----eval=FALSE----------------------------------------------------------
## mygroup := var_group(a,b)
## mygroup > 0

## ----eval=FALSE----------------------------------------------------------
## var_group(a,b) > var_group(c,d)

## ----eval=FALSE----------------------------------------------------------
## a > c
## a > d
## b > c
## b > d

## ------------------------------------------------------------------------
v <- validator(
  other.rev > 0
 , turnover > 0
 , total.rev > 0
 , staff.costs > 0
 , total.costs > 0
 , turnover + other.rev == total.rev
)

## ------------------------------------------------------------------------
retailers$id <- paste0("r-",1:nrow(retailers))
cf <- confront(retailers,v,key='id')
# rule-wise aggregation
aggregate(cf)

## ------------------------------------------------------------------------
head(aggregate(cf, by='record'),n=3)

## ------------------------------------------------------------------------
# by default aggregation is over rules:
sort(cf)

## ------------------------------------------------------------------------
head(sort(cf,by='record'), n=3)

## ------------------------------------------------------------------------
head( values(cf), n=3 )

## ------------------------------------------------------------------------
v <- validator(
  height > 0         # record-wise rule
  , sum(weight) > 0  # sum-rule
)
cf <- confront(women[1:3,],v)

## ------------------------------------------------------------------------
values(cf)

## ------------------------------------------------------------------------
values(cf["V2"])

