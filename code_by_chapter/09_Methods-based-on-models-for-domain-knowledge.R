## ------------------------------------------------------------------------
library(dcmodify)
dat <- data.frame(x = c(5,17), y = c("a","b"),stringsAsFactors=FALSE)
modify_so(dat, if( x > 6 ) y <- "c")

## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(magrittr))

## ------------------------------------------------------------------------
library(magrittr)
dat %>% modify_so(
    if (x > 6) y <- "c"
  , if (y == "c") x <- 11 )

## ------------------------------------------------------------------------
mod <- modifier(
  if ( x > 6 ) y <- "c"
  , if ( y == "c" ) x <- 11
)
modify(dat, mod)

## ------------------------------------------------------------------------
mod <- modifier(.file="modifiers.txt")

## ------------------------------------------------------------------------
variables(mod)

## ----eval=FALSE----------------------------------------------------------
## # multiple assignment
## if ( x > 0 ){
##   x <- 0
##   y <- 2 * y
## }
## 
## # nested conditions
## if ( x > 0 ){
##   x <- 0
##   if ( y < 10 ){
##     y <- 2*y
##   }
## }
## 
## # reassignment
## x <- y - 7

## ----eval=FALSE----------------------------------------------------------
## modifier(
##   x_limit := 10*median(x)
##   , if ( x > x_limit ) x <- NA
## )

## ----eval=FALSE----------------------------------------------------------
## modifier( if ( x > 10*median(x) ) x <- NA )

## ------------------------------------------------------------------------
dat <- data.frame(x=c(NA, 0), y=c(0,0))
m <- modifier( if ( x == 0 ) y <- 1 )
modify(dat, m)

## ------------------------------------------------------------------------
modify(dat, m, na.condition=TRUE)

## ------------------------------------------------------------------------
dat <- data.frame(x = 0, y = 0)
m <- modifier(
  if ( x == 0 ) x <- 1
  , if ( x == 0 ) y <- 1
)

## ------------------------------------------------------------------------
modify(dat, m)
modify(dat, m, sequential=FALSE)

## ------------------------------------------------------------------------
voptions(na.condition=TRUE)

## ------------------------------------------------------------------------
voptions(m, sequential=FALSE)

## ------------------------------------------------------------------------
library(validate)
library(deductive)
dat <- data.frame(x1 = 123, x2 = 192, x3 = 252)
v <- validate::validator(x1 + x2 == x3)
deductive::correct_typos(dat, v)

## ------------------------------------------------------------------------
dat <- data.frame(x1 = -123, x2 = 129, x3 = 252)
deductive::correct_typos(dat,v)

## ------------------------------------------------------------------------
dat <- data.frame(x1 = -123, x2 = 129, x3 = 252)
v <- validate::validator(x1 + x2 == x3, x1 < 0)
deductive::correct_typos(dat, v)

## ----eval=FALSE----------------------------------------------------------
## data(retailers)

## ----eval=FALSE----------------------------------------------------------
## compare(v, old=retailers, new=tcor)

## ----eval=TRUE-----------------------------------------------------------
library(validate) 
library(deductive)
v <- validate::validator( 
    x1 + x2      == x3 
  , x4 + x5 + x6 == x1 
  , x7 + x8      == x2 
  , x9 + x10     == x3) 
dat <- data.frame( 
    x1 = 100, x2=NA_real_, x3=NA_real_, x4 = 15, x5 = NA_real_ 
  , x6 = NA_real_, x7 = 25, x8 = 35, x9 = NA_real_, x10 = 5) 
dat
impute_lr(dat,v) 

## ----eval=FALSE----------------------------------------------------------
## cells(old = retailers, new = imp)

