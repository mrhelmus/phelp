library(ape)
library(picante)
library(tseries)
library(mFilter)
library(data.table)
############################################################################################
#### utility functions
#### 1. A bunch of text capitalization manipulators
###  2. Extracting long-term trends of the time series
###########################################################################################

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
#############

capply<-function(str, ff)
{
 sapply(lapply(strsplit(str, NULL), ff), paste, collapse="")
}
#############

cap <- function(char) {

# change lower letters to upper, others leave unchanged
if (any(ind <-letters==char))
{
LETTERS[ind]} else {char}
}
############

capitalize <- function(str) { # vector of words
 ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),cap),collapse="")
 capply(str,ff)
}
############

lower <- function(char) {
 # change upper letters to lower, others leave unchanged
 if (any(ind <- LETTERS==char)) letters[ind]    else char
}
#############

lowerize <- function(str) {
 ff <- function(x) paste(lapply(unlist(strsplit(x, NULL)),lower),collapse="")
 capply(str,ff)
}
###########

CapLeading <- function(str) {ff <- function(x) {r <- x ; r[1]<-cap(x[1]); r}; capply(str,ff)}
########

trend.coef<-function(x)
{
  y<-as.numeric(scale(x$trend))
  y<-y[!is.na(y)]
  if(length(y)>3){
  xx<-1:length(y)
  fit<-(lm(y~xx))
  if(0.05>summary(fit)$coefficients[2,4]) {coef(fit)[2]} else {0}
  } else {0}
}
########

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
########

replace.multi <- function(data, oldvalue, newvalue) {
  # convert any factors to characters
  if (is.factor(data))     data     <- as.character(data)
  if (is.factor(oldvalue)) oldvalue <- as.character(oldvalue)
  if (is.factor(newvalue)) newvalue <- as.character(newvalue)
  # create the return vector
  newvec <- data
  # put recoded values into the correct position in the return vector
  for (i in unique(oldvalue)) newvec[data == i] <- newvalue[oldvalue == i]
  newvec
}
#######
require(Biostrings)
sting.compare<-function(x,vec){
  n<-length(vec)
  ds<-rep(NA,n)
  for(i in 1:length(vec)){
  if(!is.na(vec[i])){ds[i]<-stringDist(c(x,vec[i]))}
  }
  return(ds)
}


