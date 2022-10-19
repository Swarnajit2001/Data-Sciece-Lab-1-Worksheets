#Q1

data1 = read.csv('auto-mpg.csv')
cor(data1$mpg, data1$acceleration)

#Q2

plot(data1$acceleration, data1$mpg, pch = 19,
     main = 'Plot of accelaration vs mpg')
abline(lm(data1$mpg ~ data1$acceleration), col = 2, lwd = 2)

#Q3

ggplot(data1, aes(acceleration, mpg))+
  geom_point(aes(col = as.factor(cylinders)))+
  labs(col = 'cylinders')+
  geom_smooth(data = data1[data1$cylinders == 4,],
              method = 'lm',
              formula = y ~ x,
              aes(col = '4'))+
  geom_smooth(data = data1[data1$cylinders == 6,],
              method = 'lm',
              formula = y ~ x,
              aes(col ='6'))+
  geom_smooth(data = data1[data1$cylinders == 8,],
              method = 'lm',
              formula = y ~ x,
              aes(col = '8'))+
  geom_smooth(method = 'lm',
              formula = y ~ x,
              aes(col = 'overall'))

#Q5

iris %>% head()

ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point(aes(col = Species))+
  labs(col = 'Species')+
  geom_smooth(data = iris[iris$Species == 'setosa',],
              method = 'lm',
              formula = y ~ x,
              aes(col = 'setosa'),
              se = F)+
  geom_smooth(data = iris[iris$Species =='virginica',],
              method = 'lm',
              formula = y ~ x,
              aes(col = 'virginica'),
              se = F)+
  geom_smooth(data = iris[iris$Species == 'versicolor',],
              method = 'lm',
              formula = y ~ x,
              aes(col = 'versicolor'),
              se = F)+
  geom_smooth(method = 'lm',
              formula = y ~ x,
              aes(col = 'overall'),
              se = F)

#Q6

data2 = read.csv('fire-dat.csv')

cor(data2)
