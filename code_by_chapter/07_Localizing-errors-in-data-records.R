## ----eval=FALSE----------------------------------------------------------
## rules <- validator(
##   employees >= 0,                             # numerical
##   profit == turnover - cost,                  # numerical
##   sector   %in% c("nonprofit", "industry"),   # categorical
##   bankrupt %in% c(TRUE, FALSE),               # categorical
##   if (bankrupt == TRUE) sector == "industry", # categorical
##   if ( sector    == "nonprofit"
##      | bankrupt  == TRUE
##      ) profit    <= 0,     # conditional
##   if (turnover > 0) employees >= 1            # conditional
## )

## ------------------------------------------------------------------------
library(errorlocate)
rules <- validator(
  age >= 0
)
raw_data <- data.frame(age = -1, married = TRUE)
le <- locate_errors(raw_data, rules)
values(le)

## ------------------------------------------------------------------------
library(errorlocate)
rules <- validator(
  age >= 0,
  if (age <= 16) married == FALSE
)
raw_data <- data.frame(age = -1, married = TRUE)
le <- locate_errors(raw_data, rules)
values(le)

## ------------------------------------------------------------------------
replace_errors(raw_data, rules)

## ------------------------------------------------------------------------
library(errorlocate)
v <- validator(
  age >= 0,
  if (married==TRUE) age >= 16
)
raw_data <- data.frame( age     = 4
                      , married = TRUE)
le <- locate_errors(raw_data, v)

values(le)

## ------------------------------------------------------------------------
raw_data <- data.frame(age = 4, married = TRUE)
weight <- c(age = 2, married = 1) # assuming we have a registration of marriages
le <- locate_errors(raw_data, v, weight=weight)
values(le)

