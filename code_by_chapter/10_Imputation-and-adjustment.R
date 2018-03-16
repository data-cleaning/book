## ----echo=FALSE, results=FALSE-------------------------------------------
suppressPackageStartupMessages(library(VIM))

## ----eval=TRUE, fig,keep='none',fig.show='hide'--------------------------
data("retailers",package="validate")
VIM::aggr(retailers[3:9], sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)

## ----eval=FALSE----------------------------------------------------------
## VIM::pbox(retailers[3:9], pos=1)

## ------------------------------------------------------------------------
t.test(log(staff) ~ is.na(other.rev), data=retailers)

## ----eval=FALSE----------------------------------------------------------
## dat <- log10(abs(retailers[c(3,5)]))
## VIM::marginplot(dat, las=1, pch=16)

## ------------------------------------------------------------------------
library(simputation)
library(magrittr) # for convenience
data(retailers, package="validate")
retl <- retailers[c(1,3:6,10)]
head(retl, n=3)

## ------------------------------------------------------------------------
impute_lm(retl, turnover + other.rev + total.rev ~ 1) %>% head(3)

## ------------------------------------------------------------------------
impute_lm(retl, turnover + other.rev + total.rev ~ 1 | size) %>% head(3)

## ------------------------------------------------------------------------
impute_lm(retl, turnover + other.rev + total.rev ~ size) %>% head(3)

## ------------------------------------------------------------------------
impute_lm(retl, turnover + other.rev + total.rev ~ staff - 1
  , weight=1/retl$staff) %>% head(3)

## ------------------------------------------------------------------------
impute_lm(retl, turnover + other.rev + total.rev ~ staff + vat) %>% head(3)

## ------------------------------------------------------------------------
# make results reproducible
set.seed(1)
# add normal residual
impute_lm(retl
    , turnover + other.rev + total.rev ~ staff
    , add_residual = "normal") %>% head(3)
# add observed residual
impute_lm(retl
    , turnover + other.rev + total.rev ~ staff
    , add_residual = "observed") %>% head(3)

## ------------------------------------------------------------------------
impute_rlm(retl, turnover + other.rev + total.rev ~ staff) %>% head(3)

## ------------------------------------------------------------------------
impute_rlm(retl, turnover + other.rev + total.rev ~ staff
  , method="MM") %>% head(3)

## ------------------------------------------------------------------------
impute_en(retl, turnover + other.rev + total.rev ~ staff + size 
    , s=0.005, alpha=0.5) %>% head(3)

## ------------------------------------------------------------------------
impute_cart(retl, staff ~ .) %>% head(3)

## ------------------------------------------------------------------------
impute_rf(retl, staff ~ .) %>% head(3)

## ------------------------------------------------------------------------
impute_rf(retl, staff ~ . - vat) %>% head(3) 

## ------------------------------------------------------------------------
impute_mf(retl, staff ~ .) %>% head(3)

## ------------------------------------------------------------------------
set.seed(1) # make reproducible
# random hot deck imputation (multivariate; complete cases are donor)
impute_rhd(retl, turnover + other.rev + total.rev ~ size) %>% head(3)

## ------------------------------------------------------------------------
# random hot deck imputation (univariate)
impute_rhd(retl, turnover + other.rev + total.rev ~ size
  , pool="univariate") %>% head(3)

## ------------------------------------------------------------------------
impute_shd(retl, turnover + other.rev + total.rev ~ staff) %>% head(3)

## ------------------------------------------------------------------------
impute_knn(retl, turnover + other.rev + total.rev ~ ., k=1) %>% head(3)

## ------------------------------------------------------------------------
impute_pmm(retl, turnover + other.rev + total.rev ~ staff) %>% head(3)

## ------------------------------------------------------------------------
impute_pmm(retl, turnover + other.rev + total.rev ~ staff
  , predictor=impute_rlm, method="MM") %>% head(3)

## ------------------------------------------------------------------------
impute_const(retl, other.rev ~ 0) %>% head(3)

## ------------------------------------------------------------------------
impute_proxy(retl, total.rev ~ vat) %>% head(3)

## ------------------------------------------------------------------------
impute_proxy(retl
  , turnover ~ mean(turnover/total.rev,na.rm=TRUE) * total.rev) %>% 
  head(3)

## ------------------------------------------------------------------------
impute_proxy(retl, turnover ~ mean(turnover, na.rm=TRUE) | size) %>% head(3)

## ----cache=TRUE----------------------------------------------------------
impute_em(retl, ~ .- size) %>% head(3)

## ------------------------------------------------------------------------
data(retailers, package="validate")
dat <- retailers[c("staff","turnover")]

## ------------------------------------------------------------------------
colSums(is.na(dat))/nrow(dat)

## ------------------------------------------------------------------------
out <- Amelia::amelia(dat, m=20, logs=c("staff","turnover"), p2s=0)

## ----eval=FALSE----------------------------------------------------------
## plot(out)

## ----eval=FALSE----------------------------------------------------------
## overimpute(out, var="staff")

## ------------------------------------------------------------------------
mod <- Zelig::zelig(turnover ~ staff, model="ls", data=out, cite=FALSE)

## ------------------------------------------------------------------------
# summary of model based on original data
summary( lm(turnover ~ staff, data=dat) )

# summary of model based on multiple imputed data
summary(mod)

## ------------------------------------------------------------------------
# create a 2-variable dataset
data(retailers, package="validate")
dat <- retailers[c("staff","turnover")]

## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages({
library(mice)
})

## ------------------------------------------------------------------------
library(mice)
out <- mice::mice(dat, m=20, printFlag=FALSE)

## ------------------------------------------------------------------------
out$imp$staff[, 1:6]

## ------------------------------------------------------------------------
fits <- with(out, lm(staff ~ turnover))

## ------------------------------------------------------------------------
est <- pool(fits)
summary(est)

## ------------------------------------------------------------------------
range(dat$staff,na.rm=TRUE)
apply(out$imp$staff, 1 , range)

## ------------------------------------------------------------------------
imp_15 <- complete(out, 15)
head(imp_15, 3)

## ------------------------------------------------------------------------
M <- 20
# Complete the original data, 20 times
completed <- lapply(1:20, function(i) complete(out, i))
# pass the data frames to 'Zelig::mi'
completed_mi <- do.call(Zelig::mi, completed)
# use the created object as data for 'zelig'
Zelig::zelig(staff ~ turnover, data=completed_mi, model='ls', cite=FALSE)

## ------------------------------------------------------------------------
library(validate)
library(simputation)
library(errorlocate)
library(rspa)
data(retailers)

## ------------------------------------------------------------------------
v <- validator(
  staff >= 0
  , turnover >= 0
  , other.rev >= 0
  , turnover + other.rev == total.rev
  , total.rev - total.costs == profit
  , total.costs >= 0
)

## ------------------------------------------------------------------------
d1 <- replace_errors(retailers,v)
miss <- is.na(d1)

## ------------------------------------------------------------------------
d2 <- impute_mf(d1, . ~ .)
sum(is.na(d2))

## ------------------------------------------------------------------------
d3 <- match_restrictions(d2, v, adjust=is.na(d1) )

## ------------------------------------------------------------------------
# set sensitivity to violations of linear (in)equalities to 
# less than 1e-2
voptions(x=v, lin.eq.eps=1e-2, lin.ineq.eps=1e-2)

compare(v, start=retailers,locate=d1, imputed=d2, adjusted=d3)

