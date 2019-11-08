data("iris")
X <- iris[,1:2]
Y <- iris[,5]

#Step1 
result <- kmeans(X,X[1:3,],iter.max=6,algorithm="Lloyd") # iteration causes change until max.iter is 6
result$centers
result$cluster
plot(X,col=result$cluster)

#step2
numClusters <- 3
result <- kmeans(X,numClusters,nstart=1000,iter.max=1,algorithm="Lloyd")
result$centers
result$cluster
plot(X,col=result$cluster)

#step3
result = kmeans(X,numClusters)
plot(X,col=result$cluster)