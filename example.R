setwd('/Users/zhangxiang/GitHub/多元正态分布的均值向量检验')
source('main.R')
library(readr)
example = read_csv('example.csv')
X = unlist(example)
names(X) = NULL
X = matrix(X,ncol=4)
mean.test(X,mu=c(22.75,32.75,51.5,61.5),method='unknown')
