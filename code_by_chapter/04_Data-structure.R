## ----eval=FALSE----------------------------------------------------------
## library(DBI)
## con <- dbConnect(RSQLite::SQLite(), "test.db") # create an empty db
## dbWriteTable(con, "mtcars", mtcars) # copy a table to the database
## 
## dbListFields(con, "mtcars")
## dbReadTable(con, "mtcars") # retrieve data from the data base
## 
## # You can fetch all results:
## res <- dbSendQuery(con, "SELECT * FROM mtcars WHERE cyl = 4")
## dbFetch(res)
## dbDisconnect(con)

## ----eval=FALSE----------------------------------------------------------
## library(dplyr)
## 
## # project and rename columns
## my_iris <- select( iris
##                  , sepal_width = Sepal.Width
##                  , species = Species
##                  )
## 
## # select rows
## my_iris <- filter(my_iris, sepal_width > 3)
## 
## # group by
## my_iris <- group_by(my_iris, species)
## 
## summarize( my_iris, count = n()
##          , mean=mean(sepal_width)
##          )  # count total

## ----eval=FALSE----------------------------------------------------------
## library(dplyr)
## 
## iris %>%   # data set
##   select( sepal_width = Sepal.Width # project and rename columns
##         , species = Species) %>%
##   filter( sepal_width > 3) %>%      # select rows
##   group_by(species) %>%             # group by
##   summarize( count = n()            # calculate aggregates
##            , mean=mean(sepal_width)
##            )  # count total

## ------------------------------------------------------------------------
A <- matrix(1:6, nrow=3, dimnames=list(NULL, c("a", "b")))
as.data.frame(A)
as.matrix(as.data.frame(A))

## ------------------------------------------------------------------------
dat <- data.frame(num=1:2, text=c("a", "b"))
M <- as.matrix(dat)
M
as.data.frame(M)

## ----message=FALSE-------------------------------------------------------
str(Nile)
start(Nile)
end(Nile)
frequency(Nile) #annual data so frequency == 1

## ------------------------------------------------------------------------
data <- rnorm(3*4) # 3 years
(quarter <- ts(data = data, start = 2012, frequency = 4))

## ----echo=FALSE, fig.cap="UK lung deaths", fig.height=6, fig.lp="fig:ldeath"----
par(mfrow=c(2,1))
plot(ldeaths, las=1, ylab="", xlab="") #, main="UK lung deaths")
monthplot(ldeaths, las=1, ylab="")#, main="UK lung deaths p/month")

## ------------------------------------------------------------------------
data <- matrix(rnorm(2*4), ncol=2, dimnames=list(NULL, c("a","b")))
(quarter <- ts(data = data, start = 2012, frequency = 4))

## ----echo=FALSE, fig.cap="directed graph", message=FALSE-----------------
library(igraph)
g <- graph(c(1,2, 1,3, 3,4))
V(g)$label <- c("a","b","c","d")
plot(g, vertex.color="white", edge.color="black", layout=layout.reingold.tilford)

## ----message=FALSE-------------------------------------------------------
library(jsonlite)
fromJSON("http://api.census.gov/data/2000/sf1/tags.json")
fromJSON("http://api.worldbank.org/countries/bra?format=json")[[2]]

## ----include=FALSE-------------------------------------------------------
library(tidyr)
library(dplyr)

## ------------------------------------------------------------------------
library(tidyr)
library(dplyr)

finance_messy <- data.frame(
  name = c("Alice", "Bob", "Carla"),
  tax_2015 = c(40, 10, 2),
  income_2015 = c(41, 90, 100)
)

finance_messy

## ------------------------------------------------------------------------
finance_messy %>% 
  gather(variable, amount, -name)

## ------------------------------------------------------------------------
finance_tidy <- 
  finance_messy %>% 
  gather(variable, amount, -name) %>% 
  separate(variable, into = c("variable", "year"))

## ------------------------------------------------------------------------
finance_tidy %>% 
  spread(variable, amount)

