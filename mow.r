PimaData <- read.csv(file="./pima_dataset.csv", header=TRUE, sep=",")
set.seed(1500)
repeat{
  sample <- sample.int(n = nrow(PimaData), size = floor(.8*nrow(PimaData)))
  pd.train <- PimaData[sample, ]
  pd.test  <- PimaData[-sample, ]
  ratioPima<- sum(PimaData$Class)/(nrow(PimaData)-sum(PimaData$Class))
  ratioTrain<- sum(pd.train$Class)/(nrow(pd.train)-sum(pd.train$Class))
  ratioTest<- sum(pd.test$Class)/(nrow(pd.test)-sum(pd.test$Class))
  
  if((abs(ratioPima-ratioTrain)<0.02) & (abs(ratioPima-ratioTest)<0.02)){
    break
  }
}
test_data<-pd.test
train_data<-pd.train
g_columns <- ncol(PimaData) - 1
g_maxIter <- 20
g_popSize <- 20
g_runNumber <- 20
g_elitism <- 0.1
g_crossoverChance <- 0.8
g_mutationChance <- 0.1
g_tree_levels <- 5

library(GA)
source("./functions.r")


print("Test attribute tree")
ga1 <- test_attribute_tree(g_maxIter, g_popSize, g_runNumber, g_elitism, g_crossoverChance, g_mutationChance, g_tree_levels)
print(ga1)
print(summary(ga1))
#print("Test attribute threshold tree")
#ga2 <- test_attr_thresh_tree(g_maxIter, g_popSize, g_runNumber, g_elitism, g_crossoverChance, g_mutationChance, g_tree_levels)
#summary(ga2)
