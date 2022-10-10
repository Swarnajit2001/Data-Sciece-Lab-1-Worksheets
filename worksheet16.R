library(profvis)
library(rbenchmark)

profvis({
  attempts <- function(age)
  {
    count <- 0
    remain <- age # age no. of candles remain in the beginning
    while(remain > 0)
    {
      count <- count + 1
      # randomly choose any number between 1 and remain
      blow_out <- sample(1:remain, size = 1)
      remain <- remain - blow_out
    }
    return(count)
  }
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }
})


profvis({
  attempts <- function(age)
  {
    count <- 0
    remain <- age # age no. of candles remain in the beginning
    while(remain > 0)
    {
      count <- count + 1
      # randomly choose any number between 1 and remain
      blow_out <- sample(1:remain, size = 1)
      remain <- remain - blow_out
    }
    return(count)
  }
  att_vec <- numeric(length = 1e3)
  replicate(1e3, attempts(25))
})

benchmark({
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }},
  replicate(1e3, attempts(25)), replications = 100)


benchmark({
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4)
  {
    att_vec[i] <- attempts(25)
  }},
  replicate(1e4, attempts(25)), replications = 20)


benchmark({
  att_vec <- numeric(length = 1e3)
  for(i in 1:1e3)
  {
    att_vec[i] <- attempts(25)
  }},
  replicate(1e3, attempts(25)), replications = 100)


benchmark({
  att_vec <- numeric(length = 1e4)
  for(i in 1:1e4)
  {
    att_vec[i] <- attempts(25)
  }},
  {
    att_vec <- NULL
    for(i in 1:1e4)
    {
      att_vec <- c(att_vec, attempts(25))
    }
  },
  replicate(1e4, attempts(25)), 
  replications = 25)



#Q5
library(imager)
#initial one
profvis({
  rotate = function(M){
    col.mat = as.array(load.image(M)[,,1,])
    d = dim(col.mat)
    col.rot.mat = array(0,dim = d)
    for(i in 1 : d[1]){
      for(j in 1 : d[2]){
        for(k in 1 : 3){
          col.rot.mat[i, j, k] = col.mat[(d[1] - i + 1), (d[2] - j + 1), k]
        }
      }
    }
    rot.img = as.cimg(col.rot.mat)
    plot(rot.img)
  }
  
  rotate('dog.jpeg')})


#modified one
profvis({
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
})


#pro-modification

dog = load.image('dog.jpeg')
d = dim(dog)
rot = dog[d[1] : 1, d[2] : 1,]
plot(as.cimg(rot))


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

rotate = function(M){
  pic = load.image(M)[,,1,]
  col.mat = as.array(pic)
  d = dim(col.mat)
  col.rot.mat = array(0,dim = d)
  for(i in 1 : d[1]){
    for(j in 1 : d[2]){
      for(k in 1 : 3){
        col.rot.mat[i, j, k] = col.mat[(d[1] - i + 1), (d[2] - j + 1), k]
      }
    }
  }
  rot.img = as.cimg(col.rot.mat)
  plot(rot.img)
}


#Q5 2nd part
## comparison
benchmark({
  rot180('dog.jpeg')
},
{
  rotate('dog.jpeg')},
{
  
  dog = load.image('dog.jpeg')
  d = dim(dog)
  rot = dog[d[1] : 1, d[2] : 1,]
  plot(as.cimg(rot))
},
replications = 25)



