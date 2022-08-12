#Q1
#===
library(imager)
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

rotate('dog.jpeg')

#====================================

#Q3
#===
anticlock90 = function(M){
  col.mat = as.array(load.image(M)[,,1,])
  d = dim(col.mat)
  col.rot.mat = array(0,dim = d[c(2,1,3)])
  for(i in 1 : d[1]){
      for(k in 1 : 3){
        col.rot.mat[,(d[1] - i + 1) , k] = col.mat[i, , k]
      }
    }
  ac90 = as.cimg(col.rot.mat)
  plot(ac90)
}

anticlock90('dog.jpeg')

#===================================

#Q2
#===

clock90 = function(M){
  col.mat = as.array(load.image(M)[,,1,])
  d = dim(col.mat)
  col.rot.mat = array(0,dim = d[c(2,1,3)])
  for(i in 1 : d[2]){
    for(k in 1 : 3){
      col.rot.mat[(d[2] - i + 1) ,, k] = col.mat[,i , k]
    }
  }
  c90 = as.cimg(col.rot.mat)
  plot(c90)
}

clock90('dog.jpeg')
#===================================

#Q4
#===
col.mat = as.array(load.image('dog.jpeg')[,,1,])
dim(col.mat)
cropped.dog = col.mat[1 : 600, 1 : 600,]
plot(as.cimg(cropped.dog))

compress = function(M,d1){
  col.mat = M
  d = dim(col.mat)
  comp.mat = array(0, dim = c(d1,d1,3))
  i = 1
  j = 1
  for(k in 1:3){
    while(i < (d1-1) & j < (d1-1)){
      comp.mat[i,j,k] = mean(matrix(c(col.mat[i,j,k],col.mat[i,j + 1,k],col.mat[i+1,j,k],col.mat[i+1,j+1,k]),nrow = 2))
      i = i+2
      j = j+2
    }
  }  
  plot(as.cimg(comp.mat))
}

compress(cropped.dog,300)
