#Load the dataset
train <- read.csv("c:/python/train.csv")
table(is.na(train))

#Visulaization:

set.seed(80)
data <- sample(as.integer(row.names(train[train$label == 1,])), 100)
par(mfrow = c(10,10), mar = c(0.1,0.1,0.1,0.1))
for (k in data){
  row <- NULL
  for (n in 2:785)
    row[n-1] <- train[k,n]
  
  matrix1 <- matrix(row,28,28,byrow=FALSE)
  matrix2 <- matrix(rep(0,784),28,28)
  
  for (i in 1:28)
    for (j in 1:28)
      matrix2[i,28-j+1] <- matrix1[i,j]
  if (k==16573)
    image(matrix2, axes=FALSE, col=heat.colors(12))
  else
    image(matrix2, axes=FALSE, col=topo.colors(12))
}
rm(i,n,j,k,row,matrix1,matrix2)

#Splitting the train and test sets
set.seed(25)
intest <- sample(1:42000,10000)
xtrain <- train[-intest,-1]
ytrain <- train[-intest,1]
xtest <- train[intest,-1]
ytest <- train[intest,1]

table(ytrain)
table(ytest)

#Decreasing size of train set due to large data in trainset
#To build a new train set I am going to: 1) use K-Mean clustering algorithm; 
#2) 500 class centers (10 digits * 50 clusters) are becoming observations in train set. 
#I chose euclidean distance as distance metric and simple average of all observations in clusters as cluster centers, 
#using rule of thumb too - great rule for controversial situations.

raw_train_set <- train[-intest,]
final_train_set <- NULL

for (i in 0:9)
{
  digit <- raw_train_set[raw_train_set$label==i,-1]
  set.seed(222)
  cluster <- kmeans(x = digit,centers = 50, iter.max = 20)
  new_data <- cbind(rep(i,50),cluster$centers)
  final_train_set<- rbind(final_train_set,new_data)
}
final_train_set<-as.data.frame(final_train_set)

xtrain <- final_train_set[,-1]
ytrain <- final_train_set[,1]
table (ytrain)

rm(digit,cluster,new_data,i,raw_train_set,final_train_set)
rm(X_train,Y_train)

#visualization the new train data set
data <- sample(1:500,100)
par(mfrow=c(10,10),mar=c(0.1,0.1,0.1,0.1))

for (k in data)
{
  row <- NULL
  for (n in 1:784)
    row[n] <- xtrain[k,n]
  
  matrix1 <- matrix(row,28,28,byrow=FALSE)
  matrix2 <- matrix(rep(0,784),28,28)
  
  for (i in 1:28)
    for (j in 1:28)
      matrix2[i,28-j+1] <- matrix1[i,j]
  
  image(matrix2, axes=FALSE, col=topo.colors(12))
}

rm(i,n,j,k,row,matrix1,matrix2)

#Predictions:

distance <- NULL
xtrain <- as.matrix(xtrain)
xtest <- as.matrix(xtest)
m <- dim(xtrain)[1]
n <- dim(xtest)[1]
for (j in 1:n)
{
  dist <- rowSums((xtrain - t(replicate(m, xtest[j,])))^2)
  distances<-cbind(distances,dist)
}
distances <- (t(distances))

#Predicting
rows<-NULL
for (i in 1:n)
  rows <- c(rows, which.min(distances[i,])[[1]])

predictions <- data.frame(predicted = ytrain[rows], actual = ytest, cluster = rows)
rm(dist,j,m,n,i,rows)

#Model Valuation

accuracy <- function (pred,actual)
{
  conf_matrix <- table(predictions = pred, actual = actual)
  sum <- 0
  for (j in 1:10)
    for (i in 1:10)
      if (j == i)
        sum<-sum+conf_matrix[i,j]
  return(list(conf_matrix,sum/length(pred)))
}

accuracy(predictions$predicted,predictions$actual)


#Visulaization
par(mfcol=c(14,14),mar=c(0.1,0.1,0.1,0.1))

data <- 101:198
for (k in data)
{
  row <- NULL
  for (n in 1:784)
    row[n] <- X_test[k,n]
  
  matrix1 <- matrix(row,28,28,byrow=FALSE)
  matrix2 <- matrix(rep(0,784),28,28)
  
  for (i in 1:28)
    for (j in 1:28)
      matrix2[i,28-j+1] <- matrix1[i,j]
  
  image(matrix2, axes=FALSE, col=topo.colors(12))
}

data <- predictions$cluster[101:198]
for (k in data)
{
  row <- NULL
  for (n in 1:784)
    row[n] <- X_train[k,n]
  
  matrix1 <- matrix(row,28,28,byrow=FALSE)
  matrix2 <- matrix(rep(0,784),28,28)
  
  for (i in 1:28)
    for (j in 1:28)
      matrix2[i,28-j+1] <- matrix1[i,j]
  
  image(matrix2, axes=FALSE, col=heat.colors(12))
}

rm(i,n,j,k,row,matrix1,matrix2)

