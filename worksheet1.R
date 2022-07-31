#Q1
#===
fact=function(n){
  x=1
  for(i in 1:n)
    x=x*i
  return(x)
}

fact(8)
factorial(8)
#==========================================

#Q2
#===
expo=function(n){
  (1+1/n)^n
}

n=readline('Enter your integer : ')
n=as.integer(n)

paste('your limit is : ',expo(n))

#============================================

#Q3
#===
seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
View(seat)
seat[seat$Roll==221453,2]

#=============================================

#Q4
#===

setwd('D:/IITK docs/SEM 1/Data Science Lab 1')
data=read.csv('seating.csv')
View(data)
