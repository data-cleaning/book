## ----include=FALSE-------------------------------------------------------
library(dplyr)
library(magrittr)

## ------------------------------------------------------------------------
library(dplyr)
library(magrittr)
data(iris)
iris %>% 
  filter(Species=="setosa") %>%
  select(Sepal.Width) %>%
  head(3)

## ----eval=FALSE----------------------------------------------------------
## # library calls
## library(validate)
## 
## # Read data
## data(retailers, package="validate")
## 
## # Read the rules
## rules <- validator(.file="rules.txt")
## 
## ## Start cleaning!
## 
## # implement data cleaning operations
## 
## ## Done?
## 
## # write data
## write.csv(retailers, file="retailers_clean.csv",row.names=FALSE)

## ----eval=FALSE----------------------------------------------------------
## # library calls
## library(validate)
## library(dcmodify)
## library(deductive)
## 
## # Read data
## data(retailers, package="validate")
## 
## # Read validation rules
## rules <- validator(.file="rules.txt")
## 
## ## Start cleaning!
## 
## # Read modifying rules
## mods <- modifier(.file="modify.txt")
## 
## # apply modifiers
## retailers <- modify(retailers, mods)
## 
## # correct typographic errors
## retailers <- correct_typos(retailers, rules)
## 
## ## Done?
## 
## # write data
## write.csv(retailers, file="retailers_clean.csv",row.names=FALSE)

## ----eval=FALSE----------------------------------------------------------
## # library calls
## library(validate)
## library(dcmodify)
## library(deductive)
## library(errorlocate)
## 
## # Read data
## data(retailers, package="validate")
## 
## # Read validation rules
## rules <- validator(.file="rules.txt")
## 
## ## Start cleaning!
## 
## # Read modifying rules
## mods <- modifier(.file="modify.txt")
## 
## # apply modifiers
## retailers <- modify(retailers, mods, sequential=TRUE)
## 
## # correct typographic errors
## retailers <- correct_typos(retailers, rules)
## 
## # remove sufficient fields to fix the data
## retailers <- replace_errors(retailers, rules)
## 
## ## Done?
## 
## # write data
## write.csv(retailers, file="retailers_clean.csv",row.names=FALSE)

## ----echo=FALSE----------------------------------------------------------
dat <- read.csv("R/retailers_located.csv")
rules <-validator(.file="R/rules.txt")
plot(dat[variables(rules)],las=1)

## ----eval=FALSE----------------------------------------------------------
## # library calls
## library(validate)
## library(dcmodify)
## library(deductive)
## library(errorlocate)
## library(simputation)
## 
## # Read data
## data(retailers, package="validate")
## 
## # Read validation rules
## rules <- validator(.file="rules.txt")
## 
## ## Start cleaning!
## 
## # Read modifying rules
## mods <- modifier(.file="modify.txt")
## 
## # apply modifiers
## retailers <- modify(retailers, mods, sequential=TRUE)
## 
## # correct typographic errors
## retailers <- correct_typos(retailers, rules)
## 
## # remove sufficient fields to fix the data
## retailers <- replace_errors(retailers, rules)
## 
## # deductive imputation
## retailers <- impute_lr(retailers, rules)
## 
## # impute by robust regression on staff + total.rev
## retailers <- impute_rlm(retailers
##   , turnover + other.rev + staff.costs +
##     total.costs + profit ~ staff + total.rev)
## 
## # impute by robust regression on total.rev
## retailers <- impute_rlm(retailers
##   , turnover + other.rev + staff.costs +
##     total.costs + profit ~ total.rev)
## 
## # impute using the missForest algorithm
## retailers <- impute_mf(retailers, . ~ .)
## 
## ## Done?
## 
## # write data
## write.csv(retailers, file="retailers_clean.csv",row.names=FALSE)

## ----eval=FALSE----------------------------------------------------------
## # library calls
## library(validate)
## library(dcmodify)
## library(deductive)
## library(errorlocate)
## library(simputation)
## library(rspa)
## 
## # Read data
## data(retailers, package="validate")
## 
## # Read validation rules
## rules <- validator(.file="rules.txt")
## 
## ## Start cleaning!
## 
## # Read modifying rules
## mods <- modifier(.file="modify.txt")
## 
## # apply modifiers
## retailers <- modify(retailers, mods, sequential=TRUE)
## 
## # correct typographic errors
## retailers <- correct_typos(retailers, rules)
## 
## # remove sufficient fields to fix the data
## retailers <- replace_errors(retailers, rules)
## 
## # deductive imputation
## retailers <- impute_lr(retailers, rules)
## 
## # record missing values for match_restrictions
## miss <- is.na(retailers)
## 
## # impute by robust regression on staff + total.rev
## retailers <- impute_rlm(retailers
##   , turnover + other.rev + staff.costs +
##     total.costs + profit ~ staff + total.rev)
## 
## # impute by robust regression on total.rev
## retailers <- impute_rlm(retailers
##   , turnover + other.rev + staff.costs +
##     total.costs + profit ~ total.rev)
## 
## # impute using the missForest algorithm
## retailers <- impute_mf(retailers, . ~ .)
## 
## # adjust imputed values to match restrictions
## retailers <- match_restrictions(retailers, rules, adjust=miss)
## 
## ## Done!
## 
## # write data
## write.csv(retailers, file="retailers_clean.csv",row.names=FALSE)

## ------------------------------------------------------------------------
library(validate)
dat <- read.csv("R/retailers_clean.csv")
rules <- validator(.file="R/rules.txt")
voptions(rules, lin.eq.eps=0.01, lin.ineq.eps=0.01)

confront(dat, rules)
sum(is.na(dat))

## ------------------------------------------------------------------------
data(retailers, package="validate")
library(simputation)
library(daff)

retailers2 <- impute_lm(retailers, turnover ~ total.rev)

# compute diff ('retailers' is the reference set)
d <- diff_data(retailers, retailers2)
# get the diff as a data.frame
d$get_data()

## ------------------------------------------------------------------------
retailers3 <- dplyr::rename(retailers2, income = turnover)
d1 <- diff_data(retailers, retailers3)
head(d1$get_data())

## ----warning=FALSE-------------------------------------------------------
retailers3_reconstructed <- patch_data(retailers, d)
all.equal(retailers3_reconstructed, retailers3)

## ------------------------------------------------------------------------
iris1 <- iris
iris1[1:3,1] <- NA
iris1[2:4,2] <- iris1[2:4,2]*2

library(validate)
cells(start = iris, step1 = iris1)

## ------------------------------------------------------------------------
iris2 <- iris1
iris2[1:3, 3] <- -(1:3)
cells(start = iris, step1 = iris1, step2=iris2)

## ------------------------------------------------------------------------
v <- validator(
  Sepal.Length >= 0
 , Petal.Length >= 0
)
compare(v, start=iris, step1=iris1, step2=iris2)

## ------------------------------------------------------------------------
library(validate)
library(simputation)
library(lumberjack)

data(retailers)

# we add a unique row-identifier
retailers$id <- seq_len(nrow(retailers))

# create a logging object.
logger <- cellwise$new(key="id")

out <- retailers %>>% 
  start_log(logger) %>>%
  impute_lm(staff ~ turnover) %>>%
  impute_median(staff ~ size) %>>%
  dump_log(file="mylog.csv", stop=TRUE)

## ------------------------------------------------------------------------
log <- read.csv("mylog.csv")
head(log)

## ------------------------------------------------------------------------
table(log$expression)

## ------------------------------------------------------------------------
# re-read data for this example
data(retailers)
retailers$id <- seq_len(nrow(retailers))

logger <- cellwise$new(key="id")
retailers <- start_log(retailers, logger)
retailers <- retailers %>>% impute_lm(staff ~ turnover)
retailers <- retailers %>>% impute_median(staff ~ size)
dump_log(retailers,file="mylog.csv", stop=TRUE)

## ----eval=FALSE----------------------------------------------------------
## library(validate)
## library(dcmodify)
## library(deductive)
## library(errorlocate)
## library(simputation)
## library(rspa)
## library(lumberjack)
## 
## # Read data
## data(retailers, package="validate")
## 
## # Read validation rules
## rules <- validator(.file="rules.txt")
## voptions(rules, lin.eq.eps=0.01,lin.ineq.eps=0.01)
## # Initialize logger.
## logger <- lbj_rules(rules)
## 
## # tag 'retailers' for logging
## retailers <- start_log(retailers, logger)
## 
## ## Start cleaning
## 
## # Read modifying rules
## mods <- modifier(.file="modify.txt")
## 
## # apply modifiers
## retailers <- retailers %>>%
##   modify(mods, sequential=TRUE) %>>%
##   correct_typos(rules) %>>%
##   replace_errors(rules) %>>%
##   impute_lr(rules) %>>%
##   tag_missing() %>>%
##   impute_rlm(turnover + other.rev + staff.costs +
##       total.costs + profit ~ staff + total.rev) %>>%
##   impute_rlm(turnover + other.rev + staff.costs +
##       total.costs + profit ~ total.rev) %>>%
##   impute_mf(. ~ .) %>>%
##   match_restrictions(rules) %>>%
##   dump_log(file="cleaninglog.csv",stop=TRUE)
## 
## ## Done!
## 
## # write data
## write.csv(retailers, file="retailers_clean.csv", row.names=FALSE)

## ------------------------------------------------------------------------
logdata <- read.csv("R/cleaninglog.csv")

