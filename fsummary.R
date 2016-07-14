fsummary <- function(data){
library(psych)
data2<-cbind(data$age,data$size)
colnames(data2)<-c("age","size")


initials=describe(data2, na.rm = FALSE, interp=FALSE,skew = FALSE, ranges = FALSE,trim=FALSE,
         type=3,check=FALSE,fast=TRUE)

initials<-initials[,-1]
initials
}