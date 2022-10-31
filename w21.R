
data = read.csv('LowRepeated.csv', header = F, skip = 4)
data_1 = data[-1,]

type = data[1,2:9]
names(type) = NULL

data_final = data.frame(data_1[,1], stack(data_1[,-1]))
data_final[,1] = as.numeric(data_final[,1])
data_final[,2] = as.numeric(data_final[,2])
data_final[,3] = as.factor(data_final[,3])
colnames(data_final) = c('wave', 'reflectance', 'type')
library(ggplot2)

ggplot(data_final,aes(wave,reflectance))+
  geom_line(aes())
