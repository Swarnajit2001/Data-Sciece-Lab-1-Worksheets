#Q1

x = seq(1, by = 2, length.out = 1000); x

#Q2

fibo = c(1, 1)

for(i in 3 : 1000){
  fibo[i] = fibo[i - 1] + fibo[i - 2]
}

#Q3

die = function(){
  outcome = sample(1 : 6, 1)
  if(outcome %% 2 == 0){
    return(1)
  }
  else
    return(0)
}

mean(replicate(1000, die()))

#Q4

toss = function(coin){
  outcome = sample(c('H','T'), coin, T)
  if(table(outcome)[1] < 8)
    return('lose')
  else
    return('win')
}

table(replicate(1000,toss(15)))

#Q5

matrix(1, nrow = 5, ncol = 5)
   
#Q6

m = matrix(0, nrow = 5, ncol = 5)

for(i in 1:5){
  m[i, i] = i
}
m

#Q7

m = matrix(sample(1:6, 10*10, T),nrow = 10)
m

#Q8

mat = function(n,rho){
  m = matrix(0, nrow = n, ncol = n)
  for(i in 1:n){
    m[i,i] = 1
    for(j in 1:n){
      if(i != j)
        m[i, j] = rho
    }
  }
  return(m)
}

mat(5, 2)

#Q9

mat = function(n, rho){
  m = matrix(0, nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n)
      m[i,j] = rho^(abs(i - j))
  }
  return(m)
}

mat(5,4)

#Q10
m = matrix(1:12,nrow=3)
mat = function(m){
  d = dim(m)
  l = seq(1, d[2], 2)
  n = matrix(0, nrow = d[1], ncol = length(l))
  for (i in 1 : length(l)){
    n[, i] = m[, l[i]]
  }
  return(n)
}

mat(matrix(1:12,nrow=3))

#Q11

array(1, c(10,4,6,5))

#-------------------------------------------

#Q5

HP = function(){
  packet = 1
  toy = sample(1 : 7, 1, prob = c(.25, .20, .20, .15, .10, .05, .05))
  while (length(unique(toy)) < 7){
    toy = c(toy, sample(1 : 7, 1, prob = c(.25, .20, .20, .15, .10, .05, .05)))
    packet = packet + 1 
  }
  return(packet)
}

mean(replicate(1000, HP()))

# Q6

med = function(){
  day = 0
  half = 0
  while(half != 1){
    half = sample(0 : 1, 1, prob = c(1-(day / 100), day / 100))
    day = day + 1
  }
  return(day)
}

mean(replicate(1000, med()))

#Q7 

MontyHall = function(door){
  m = c('Car', rep('Goat', door - 1))
  original = sample(m, door)
  choice = sample(1 : door, 1)
  output = ifelse(original[choice] == 'Car', 0, 1)
  return(output)
}

mean(replicate(1000, MontyHall(3)))
  
#----------------------------------------

#assignment 1

tennis = function(p){
  set = 0
  i = 1
  while (length(set) < 5 & max(table(set)) < 3) {
    set [i] = sample(0 : 1, 1, prob = c(1-p, p))
    i = i + 1
    
  }
  return(length(set))
}

mean(replicate(1000, tennis(0.7)))

#------------------------------------------

#W3

#Q4

blow = function(age){
  attempt = 0
  candle = 0
  while (candle < age) {
    candle = candle + sample(0 : (age - candle), 1)
    attempt = attempt +1
  }
  return(attempt)
}


mean(replicate(1000, blow(25)))

#W4

#Q1
library(imager)

dog = load.image('dog.jpeg')
plot(dog)
green = sqrt((dog[,,1,1])^2 + (dog[,,1,2]-1)^2 + dog[,,1,3]^2)
points(which(green == min(green), arr.ind = T),pch = 19, cex = 1, col = 3)

red = sqrt((dog[,,1,1]-1)^2 + (dog[,,1,2])^2 + dog[,,1,3]^2)
points(which(red == min(red), arr.ind = T),pch = 19, cex = 1, col = 2)

blue = sqrt((dog[,,1,1])^2 + (dog[,,1,2])^2 + (dog[,,1,3] - 1)^2)   
points(which(blue == min(blue), arr.ind = T),pch = 19, cex = 1, col = 4)  
  
#Q3
M = 'col1.png'
clr = function(M){
  pic = load.image(M)[,,1,]
  plot(load.image(M))
  distance = c(mean((pic[,,1]-1)^2 + pic[,,2]^2 + pic[,,3]^2),
               mean((pic[,,1])^2 + (pic[,,2]-1)^2 + pic[,,3]^2),
               mean((pic[,,1])^2 + pic[,,2]^2 + (pic[,,3]-1)^2))
  
  ifelse(which(distance == min(distance)) == 1, 'red', ifelse(which(distance == min(distance)) == 2, 'green', 'blue'))
}
  
clr('col3.png')

#Q4

snow = function(M, threshold){
  pic = load.image(M)[,,1,]
  plot(load.image(M))
  distance = mean((pic[,,1]-1)^2+(pic[,,3]-1)^2+(pic[,,3]-1)^2)
  ifelse(distance < threshold, 'lot of snow', 'not that much snow')
}

snow('land1.jpeg',0.3)

#---------------------------------

#W5

#Q1

rot180 = function(M){
  pic = load.image(M)[,,1,]
  d = dim(pic)
  rot.pic = array(0, d)
  for(i in 1 : d[1]){
    for(j in 1 : d[2]){
      rot.pic[i,j,] = pic[d[1] - i +1, d[2] - j + 1, ]
    }
  }
  plot(as.cimg(rot.pic))
}

rot180('dog.jpeg')


#Q2

clock90 = function(M){
  pic = load.image(M)[,,1,]
  d = dim(pic)
  rot.pic = array(0, d[c(2,1,3)])
  for(i in 1 : d[2]){
    rot.pic[d[2]-i+1,,] = pic[,i,]
  }
  plot(as.cimg(rot.pic))
}

clock90('dog.jpeg')

#Q3

anticlock90 = function(M){
  pic = load.image(M)[,,1,]
  d = dim(pic)
  rot.pic = array(0, d[c(2,1,3)])
  for(i in 1 : d[1])
    rot.pic[,d[1]-i+1,] = pic[i,,]
  plot(as.cimg(rot.pic))
}

anticlock90('dog.jpeg')

#Q4,5

M = 'dog.jpeg'
d1=300
compress = function(M, d1){
  pic = load.image(M)[1:600,1:600,1,]
  d = dim(pic)[1]
  cr = d/d1
  comp.pic = array(0, dim = c(d1,d1,3))
  for(i in 1 : d1){
    for(j in 1 : d1){
      for(k in 1:3){
        comp.pic[i,j,k] = mean(pic[(cr*i) : (cr*i - cr +1), (cr*j) : (cr*j - cr +1),k])
      }
    }
  }
  plot(as.cimg(comp.pic))
}

compress('dog.jpeg',60)


















