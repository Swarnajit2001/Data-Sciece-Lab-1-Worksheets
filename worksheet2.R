#Q1
#====
seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
sum(floor(seat$Roll / 10^4) == 22) # number of students having roll numbers starting with 22

#==========================================

#Q2
#===

cricket <- read.csv("https://dvats.github.io/assets/course/mth208/battingbowling.csv")
View(cricket)

#1
#---
attach(cricket)
AR = cricket[Bowling < 40 & Batting > 25,] # ARs are those whose batting avg is > 25 and bowling avg is < 40
View(AR)

#2
#---
AR_Team = AR$Team #team of the all rounders
names(table(AR_Team)[table(AR_Team) == max(table(AR_Team))]) #country name with most ARs

#3
#---
names(table(AR_Team)[table(AR_Team) == min(table(AR_Team))]) #country name with least ARs
#=========================================

#Q3
#===

x = 0:10 
y = x

plot(x, y,
     main = 'Y=X Plot',
     xlab = 'x',
     ylab = 'y',
     type = 'l') #plotting x=y

#========================================

#Q4
#===

n = 1:1000

f_n = (1+1/n)^n

plot(n,f_n,
     pch = 19,
     ylab = 'f(n)',
     main = 'Plot of n vs f(n) = (1+1/n)^n') #plotting n vs f(n)  

abline(h=exp(1),col='red',lwd=2) #adding a red line at the value 'e'
