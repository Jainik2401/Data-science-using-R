setwd("/home/faculty/Documents/Krupa/Subjects/DataScience-I/data")
protein <- read.table("protein.txt", sep="\t", header=TRUE)
summary(protein)

vars.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center") #returns the mean values of all the columns
pscale <- attr(pmatrix, "scaled:scale") # returns the standard deviations


d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D") # Clustering
plot(pfit, labels=protein$Country) # Dendrogram: a tree that represents the nested clusters
rect.hclust(pfit, k=5)

# Extracting cluster - Start
groups <- cutree(pfit, k=5)
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))

    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

print_clusters(groups, 5)
# Extracting cluster - End

# Visualizing clusters - Start
#install.packages("ggplot2")
library(ggplot2)
princ <- prcomp(pmatrix) #Calculate the principal component 
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:nComp]
project.plus <- cbind(as.data.frame(project),
                      cluster=as.factor(groups),
                      country=protein$Country)

ggplot(project.plus, aes(x=PC1, y=PC2)) +                	
  geom_point(aes(shape=cluster)) +
  geom_text(aes(label=country),
            hjust=0, vjust=1)
# Visualizing clusters - End

# Cluster Boot - Start
#install.packages("fpc")
library(fpc)                                  	
kbest.p<-5                                                   	
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI, 	
                            method="ward.D", k=kbest.p)

summary(cboot.hclust$result) 
cboot.hclust$result$clusterlist
groups<-cboot.hclust$result$partition
print_clusters(groups, kbest.p)
cboot.hclust$bootbrd
# Cluster Boot - End

## k - means algorithm

pclusters <- kmeans(pmatrix, kbest.p, nstart=100, iter.max=10)
summary(pclusters)
print(pclusters$centers)

groups <- pclusters$cluster
print_clusters(groups, kbest.p)

#clusterboot with k-means
kbest.p<-5
cboot.test<-clusterboot(pmatrix, clustermethod=kmeansCBI, krange=5)
cboot<-clusterboot(pmatrix, clustermethod = kmeansCBI, runs=10, iter.max=1000,krange=5)
#cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI, runs=20 #cross cluster,iter.max=150 #intra cluster, krange=kbest.p, seed=15555)
groups <- cboot$result$partition
print_clusters(cboot$result$partition, kbest.p)



