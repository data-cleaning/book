## ------------------------------------------------------------------------
x <- c("\uf6"        # small latin 'o' with diaereses        
     , "\u6f\u0308") # small latin 'o' followed by combining diaereses
x 

## ------------------------------------------------------------------------
iconv(x,toRaw=TRUE)

## ------------------------------------------------------------------------
y <- stringi::stri_trans_nfc(x)
iconv(y,toRaw=TRUE)

## ------------------------------------------------------------------------
x <- "\ufb00" # Latin small ligature ff
x
nchar(x)
nchar(stringi::stri_trans_nfc(x))
nchar(stringi::stri_trans_nfkc(x))

## ------------------------------------------------------------------------
grepl("f", stringi::stri_trans_nfc(x))
grepl("f", stringi::stri_trans_nfkc(x))

## ------------------------------------------------------------------------
chartr("eiou","aaaa", c("hello world","Olá Mundo"))

## ------------------------------------------------------------------------
toupper(c("hello world","Olá Mundo"))
tolower(c("hello world","Olá Mundo"))

## ------------------------------------------------------------------------
stringi::stri_trans_tolower(c("hello world","Olá Mundo"))
stringi::stri_trans_toupper(c("hello world","Olá Mundo"))
stringi::stri_trans_totitle(c("hello world","Olá Mundo"))

## ------------------------------------------------------------------------
iconv(c("hello world","Olá Mundo"), to="ASCII//TRANSLIT")
stringi::stri_trans_general(c("hello world","Olá Mundo"), id="latin-ascii")

## ------------------------------------------------------------------------
str <- c("a","xacc","xdcc","xbccccxyac")
grepl("(a|b)c*",str)

## ------------------------------------------------------------------------
sub("(a|b)c*", "H", str)

## ------------------------------------------------------------------------
gsub("(a|b)c*", "H", str)

## ------------------------------------------------------------------------
x <- c("aa","baa")
grepl("a(a|b)", x)
grepl("^a(a|b)", x)

## ------------------------------------------------------------------------
c("\uf6","\\uf6")

## ------------------------------------------------------------------------
# detect a.b, not a<wildcard>b
grepl("a\\.b", c("a.b", "aHb"))

## ------------------------------------------------------------------------
# recognize "a\b" and not "ab"
grepl("a\\\\b", c("a\\b","ab"))

## ------------------------------------------------------------------------
grepl("a[\\]b", c("a\\b","ab"))

## ------------------------------------------------------------------------
x <- c("a.b", "aHb", "a.b.*")
grepl("a.b.*", x)
grepl("a.b.*", x, fixed=TRUE)

## ------------------------------------------------------------------------
s <- "The <em>hello world</em> programme"
gsub(pattern="<.*>", replacement="", x=s)

## ------------------------------------------------------------------------
gsub(pattern="<.*?>", replacement="", x=s)

## ------------------------------------------------------------------------
 s <- "the <em>hello world</em> programme"
 gsub("<em>(.*?)</em>", "<b>\\1</b>", s)

## ------------------------------------------------------------------------
grepl("<(.*?)>.*?</\\1>", c("<li> hello","<em>hello world</em>"))

## ------------------------------------------------------------------------
grepl("<(?P<tag>.*?)>.*?</(?P=tag)>"
  , c("<li> hello","<em>hello world</em>")
  , perl=TRUE)

## ------------------------------------------------------------------------
rex::rex( "a" %or% "b", zero_or_more("c"))

## ------------------------------------------------------------------------
# look for . or ,:
r <- rex::rex("." %or% ",")
# the double backslash necessary in R (\\.) is not printed.
r
# however, using str, we see that it is actually there
str(r)

## ------------------------------------------------------------------------
r <- rex::rex(blank
 , one_or_more(digit)
 , maybe( "." %or% ",", one_or_more(digit))
 , maybe( one_of("e","E"), one_or_more(digit))
 , blank
)

## ------------------------------------------------------------------------
nr <- rex::rex(one_or_more(digit))
Ee <- rex::rex(one_of("E","e"))
r <- rex::rex(blank, nr, maybe("." %or% "," , nr), maybe(Ee,nr), blank)

## ------------------------------------------------------------------------
qdapRegex::rm_number("Sometimes 12,5 is denoted 12.5 but we may round it to 13")

## ------------------------------------------------------------------------
x <- "'Is that an intra-word dash?', she asked"
tm::removePunctuation(x)
tm::removePunctuation(x, preserve_intra_word_dashes=TRUE)

## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(stringr))

## ------------------------------------------------------------------------
library(stringr)
str_trim("  hello world ", side='both')

## ------------------------------------------------------------------------
pat <- c("[[:digit:]]+","[^[:digit:]]+")
str_replace("Hello 12 34", pattern = pat, replacement = "")

## ------------------------------------------------------------------------
pat <- "[[:blank:]]{2,}" # two or more blank characters
rep <- " "               # replace with single space
str_replace_all("Hello   many    spaces", pattern = pat, replace = " ")

## ------------------------------------------------------------------------
x <- c("a.b", "aHb", "a.b.*")
stringr::str_detect(x, "a.b.*")
stringr::str_detect(x, stringr::fixed("a.b.*"))

## ------------------------------------------------------------------------
x <- c("a.b", "aHb", "A.B.*")
str_detect(x, fixed("a.b.*"))
str_detect(x, fixed("a.b.*", ignore_case=TRUE))

## ------------------------------------------------------------------------
i <- c("I", "\u0130", "i")
i
str_detect(i, fixed("i",ignore_case=TRUE))
str_detect(i, coll("i", ignore_case=TRUE))
str_detect(i, coll("i", ignore_case=TRUE, locale = "tr"))

## ------------------------------------------------------------------------
str_to_lower(i)
str_to_lower(i, locale="tr")

## ------------------------------------------------------------------------
gsubfn::gsubfn("[0-9]+", function(x) 2*as.numeric(x), c("(1,2)","x-23-y"))

## ------------------------------------------------------------------------
x <- c("date: 11-11-2011", "Either 10-01-2001 or 07-03-2012")
stringr::str_extract(x,"[0-9]{2}-[0-9]{2}-[0-9]{2}")
stringr::str_extract_all(x,"[0-9]{2}-[0-9]{2}-[0-9]{2}")

## ------------------------------------------------------------------------
stringr::str_extract_all(x,"[0-9]{2}-[0-9]{2}-[0-9]{2}", simplify=TRUE)

## ------------------------------------------------------------------------
iban <- "NLABNA987654321"
substr(start=1, stop=2,iban)

## ------------------------------------------------------------------------
x <- c("10-10 2010","12.12-2012")
stringr::str_split(x,"[-\\. ]")

## ------------------------------------------------------------------------
# standard categories
codes <- c("male", "female")
# vector of 'raw' input data
gender <- c("M", "male ", "Female", "fem.","female")

## ------------------------------------------------------------------------
# lookup of gender in the 'codes' lookup table
match(x = gender, table = codes)

## ------------------------------------------------------------------------
# remove trailing whitespace, cast to lowercase
gender_clean <- stringi::stri_replace_all(gender,"", regex="[[:blank:]]+$")
gender_clean <- stringi::stri_trans_tolower(gender_clean)
gender_clean
match(x = gender_clean, table = codes)

## ------------------------------------------------------------------------
stringdist::amatch(x = gender, table = codes, maxDist = 3)
stringdist::amatch(x = gender_clean, table = codes, maxDist = 3)

## ------------------------------------------------------------------------
data.frame(
  gender         = gender
  , gender_clean = gender_clean
  , match        = codes[match(gender, codes)]
  , clean_match  = codes[match(gender_clean, codes)]
  , amatch       = codes[stringdist::amatch(gender,codes,maxDist=3)]
  , clean_amatch = codes[stringdist::amatch(gender_clean,codes,maxDist=3)]
)

## ------------------------------------------------------------------------
# some 'raw' data to be clustered.
subject <- c("mathematics", "physics", 
     "astrophysics", "math", "numerical math")
# compute distance matrix, adding strings as row/column names
M <-stringdist::stringdistmatrix(subject,subject, useNames=TRUE)
# Convert to 'dist' object and cluster hierarchically 
h <- hclust(as.dist(M), method='single')

## ------------------------------------------------------------------------
adist("fu","foo")
adist("fu","foo", cost=c(i=0.1, d=1, s=1))

## ------------------------------------------------------------------------
adist(c("hello","world"),c("olá mundo"))

## ------------------------------------------------------------------------
# a pattern to search for
re <- "abc"
# two strings to search in
x <- c("FOOabcBAR","FOOaXcBAR")
# grep finds only one match (in the first element)
grep(pattern=re, x=x)
# agrep finds both
agrep(pattern=re, x=x, max.distance=1)

## ------------------------------------------------------------------------
library(stringdist)
stringdist("hello",c("hallo","wereld"))

## ------------------------------------------------------------------------
stringdistmatrix("hello",c("hallo","wereld"), useNames="strings")

## ------------------------------------------------------------------------
match("hello",c("hallo", "wereld"), nomatch=0)
amatch("hello",c("hallo", "wereld"), nomatch=0, maxDist=1)

## ------------------------------------------------------------------------
"hello" %in% c("hallo", "wereld")
ain("hello",c("hallo", "wereld"), maxDist=1)

## ------------------------------------------------------------------------
stringdist("hello",c("hallo","wereld"),method="hamming")
amatch("hello",c("hallo", "wereld"), nomatch=0,method="hamming", maxDist=1)

## ------------------------------------------------------------------------
stringdist("hello","hallo", method="osa", weight=c(1,1,0.1,1))

## ------------------------------------------------------------------------
# Compute the Jaro distance
stringdist("marhta", "martha", method="jw")
# Compute the Jaro-Winkler distance with p=0.1
stringdist("marhta", "martha", method="jw", p=0.1) 

## ------------------------------------------------------------------------
# raw data
x <- c("Stan Marhs","Kyle Brovlofski")
# lookup table
sp_char <- c("Kenny McCormick","Kyle Broflovski","Stan Marsh","Eric Cartman")
# find matches
amatch(x=x, table=sp_char, method="jw", p=0.1, maxDist=0.2)

## ------------------------------------------------------------------------
amatch(x, table=sp_char, method="qgram", q=2, maxDist=5)
stringdistmatrix(x, sp_char, method="qgram", q=2, useNames="strings")

## ------------------------------------------------------------------------
phonetic(sp_char)

## ------------------------------------------------------------------------
stringdistmatrix(x, sp_char, method="soundex", useNames="strings")

## ------------------------------------------------------------------------
stringdist::stringsim("hello",c("hallo", "olá"), method="lcs")

## ------------------------------------------------------------------------
sp_char <- c("Kenny McCormick","Kyle Broflovski","Stan Marsh","Eric Cartman")
library(textcat)
textcat_xdist(sp_char)

## ------------------------------------------------------------------------
library(kernlab)
sk <- stringdot()

## ------------------------------------------------------------------------
1 - sk("kyle brovlofski", "kyle broflovski")

## ------------------------------------------------------------------------
sk3 <- stringdot(length=3,lambda=0.5)
1 - sk3("kyle brovlofski", "kyle broflovski")

## ------------------------------------------------------------------------
stringsim("Farnsworth, Hubert J.", "Hubert J. Farnsworth", method="cosine", q=2)
stringsim("Farnsworth, Hubert J.", "Hubert J. Farnsworth", method="jw", p=0.1)

## ------------------------------------------------------------------------
stringdist(enc2utf8("Motörhead"), "Motorhead", method="lv")
stringdist(enc2utf8("Motörhead"), "Motorhead", method="lv", useBytes=TRUE)

