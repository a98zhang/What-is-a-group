library(factoextra)
library(cluster)
library(NbClust)
# Import the file titled, "GroupRating_DataMatrix_N250.csv"
# It contains 30 groups with 19 traits as our initial input matrix.
groups<-read.csv('C:/Users/ashle/Job.Intern.Lab/CDL/GroupPaper/Group_Rating_Study_N481_scored.csv')
#groups<-read.csv(file.choose())
head(groups)  #head() have a brief look at the data



# The first column is removed, as it was simply the names of the groups
df<-groups[-1]
head(df)

allLabels <- c("fem", "stud","chris","dem","repub","rich",
               "poor","midc","workc","asn","wht","blck",
               "hisp","arab","gay","wom","men","pol","mot","samst",
               "spor","fam","hsfd","coup","cowk","chur","hobb","neigh",
               "vol","bsns")
allLabels_full <- c("Feminists", "Students","Christians","Democrats","Republican","Rich",
                    "Poor","Midclass","Workclass","Asian","White","Black",
                    "Hispanic","Arab","Gay","Women","Men","Police","Mothers","SameState",
                    "Sports","Family","HighSchoolFriends","Couple","Coworkers","Church","Hobby","Neighbor",
                    "Volunteer","Business")


# Standardize the data sets
# The features are normalized by computing the z-score for each column: (x - mean)/std
df <- scale(df)
rownames(df)<- allLabels_full


all_models <- readRDS("C:/Users/ashle/R/study1/June22/k_cluster_models.rds")



library("factoextra")
#Plot the dataset
fviz_pca_ind(princomp(df), title = "PCA - Group Categories Data", 
             habillage = all_models[[3]]$cluster,  palette = "jco",
             geom = "text", ggtheme = theme_classic(),
             legend = "bottom",addEllipses = T)


# dimensions to represent
library(multicon)
horn(df,sims = 100)
library(paran)
paran(df, graph = TRUE)
paran(df, graph = TRUE, col=c("black","red","blue"),
      lty=c(1,1,1))
fviz_eig(princomp(df), choice = "eigenvalue", geom ="line")
library(psych)
fa.parallel(df,fm='pa',fa="fa",n.iter=100,show.legend=FALSE)


# determine optimal number of clusters
library(NbClust)
nb <- NbClust(data = df, distance = "euclidean", min.nc = 2, 
              max.nc = 10,method = "kmeans", index="all")
fviz_nbclust(nb)
km.res <- kmeans(df, 3, nstart = 25 )
km.res$cluster
fviz_cluster(km.res, data = df_reduced, geom = "point",
             stand = FALSE, frame.type = "norm")
# plot the clustering result
clusplot(df,km.res$cluster,color=TRUE,shade=TRUE,labels=2, 
         lines=0,main="PCA - Group Categories Data")
# plot the chosen model
# https://stat.ethz.ch/R-manual/R-patched/library/cluster/html/clusplot.default.html
clusplot(df,all_models[[3]]$cluster,color=TRUE,shade=TRUE,labels=2,
         col.txt= c("#133c70", "#133c70", "#133c70"),
         col.clus= c("#00AFBB", "#E7B800", "#FC4E07"),
         lines=0,main="PCA - Group Categories Data")







# dimension reduced ver.
df.pca <- princomp(df)
df_reduced <- df.pca$scores[1:30, 1:2]
clusplot(df_reduced,all_models[[3]]$cluster,color=TRUE,shade=TRUE,labels=2, 
         lines=0,main="PCA - Group Categories Data")
library(NbClust)
nb_reduced <- NbClust(data = df_reduced, distance = "euclidean", min.nc = 2, 
        max.nc = 10,method = "kmeans", index="all")
fviz_nbclust(nb_reduced)
km.res_reduced <- kmeans(df_reduced, 3, nstart = 25 )
km.res_reduced$cluster
fviz_cluster(km.res_reduced, data = df_reduced, geom = "point",
             stand = FALSE, frame.type = "norm")
clusplot(df_reduced,km.res_reduced$cluster,color=TRUE,shade=TRUE,labels=2, 
         lines=0,main="PCA - Group Categories Data")










# Random data generated from the social categories data set from Ryan Lei's 
# Qualtric survey.
random_df <- apply(df, 2, 
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
random_df <- scale(random_df)
rownames(random_df)<- allLabel

# Plot the random df
fviz_pca_ind(princomp(random_df), title = "PCA - Random Group Categories Data", 
             geom = "text", ggtheme = theme_classic())

random.kmeans <- kmeans(random_df,centers=4)
clusplot(random_df,random.kmeans$cluster, color=TRUE,shade=TRUE,labels=2, 
     lines=0,main="PCA - Random Group Categories Data")









# Compute Hopkins statistic for group categories dataset.
res <- get_clust_tendency(df, n = nrow(df)-1, graph = TRUE)
res$hopkins_stat

#Visualize dissimilarity matrices for data and random data
fviz_dist(dist(df), show_labels = FALSE)+
  labs(title =" Group data")

fviz_dist(dist(random_df), show_labels = FALSE)+
  labs(title = "Random data")

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

new.df <- df[,-7]
new.df <- new.df[,-23]
df <- new.df
# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)


# Hierarchical clustering
hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean", 
                 hc_method = "ward.D2", graph = FALSE)
# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

# K-means clustering
km.res <- eclust(df.special, "kmeans", k = 3, nstart = 25, graph = FALSE)


fviz_silhouette(km.res, palette = "jco", 
                ggtheme = theme_classic())

# Silhouette information
silinfo <- km.res$silinfo
names(silinfo)
# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)
# Average silhouette width of each cluster
silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
silinfo$avg.width
# The size of each clusters
km.res$size



order(silinfo$widths$sil_width)

silinfo$widths[19,]

nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")

######### 30 indices to find optimal number of clusters #########
library(factoextra)
library(NbClust)

NbClust(data = df, distance = "euclidean", min.nc = 2, 
        max.nc = 10,method = "kmeans", index="all")



######### 0318 dimension reduction >>> k-means clustering ############
# check correlation between original features (high)
library(ggcorrplot)
cormat <- round(cor(df), 2)
ggcorrplot(cormat, hc.order = TRUE, type = "lower", outline.color = "white")

# pca
pca <- prcomp(df)
summary(pca)
scores <- as.data.frame(pca$x)
head(scores[1:2])


# plot the pca
g.pca <- prcomp(df)
eig <- get_eig(g.pca)
# TODO export the eigenvalues into excel file book
var <- get_pca_var(g.pca)

fviz_pca_var(g.pca, col.var="coord") +
  scale_color_distiller(palette = "YlOrRd",direction = 1) + theme_minimal()
fviz_pca_contrib(g.pca, choice = "var", axes = 3)

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

t1 <- var$cor[,1:2]
v1 <- t1[,1]
v2 <- t1[,2]
d1 <- data.frame(v1,v2)
colnames(d1) <- c("Dimension 1", "Dimension 2")


# check valid number of principal components
pr_var <-  pca$sdev ^ 2
pve <- pr_var / sum(pr_var)
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", ylim =c(0,1), type = 'b')
rot_loading <- varimax(pca$rotation[, 1:2])
rot_loading

# k-means clustering on original high dimensions
# yet plot the clustering results (the cluster membership) using PCA results
scores <- as.data.frame(pca$x)
head(scores[1:2])
clustering <- kmeans(df, centers = 3)
cluster_groups <- clustering$cluster
plot(PC1~PC2, data=scores, 
     main= "Existing TB cases per 100K distribution",
     cex = .1, lty = "solid", col=cluster_groups)
text(PC1~PC2, data=scores, 
     labels=rownames(df),
     cex=.8, col=cluster_groups)

# what about cluster on the principal components
df_pca <- data.frame(pca$x[,1:2])
curr_model<- kmeans(df_pca, 3, iter.max = 10000)
clusplot(df_pca, curr_model$cluster,color=TRUE,shade=TRUE,labels=2,
         col.txt= c("#133c70", "#133c70", "#133c70"),
         col.clus= c("#00AFBB", "#E7B800", "#FC4E07"),
         lines=0,main="PCA - Group Categories Data")


