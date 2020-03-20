library(factoextra)
library(cluster)
library(NbClust)
# Import the file titled, "GroupRating_DataMatrix_N250.csv"
# It contains 30 groups with 19 traits as our initial input matrix.
groups<-read.csv(file.choose())
head(groups)  #head() have a brief look at the data

# The first column is removed, as it was simply the names of the groups
df<-groups[-1]
head(df)

allLabels <- c("fem", "stud","chris","dem","repub","rich",
               "poor","midc","workc","asn","wht","blck",
               "hisp","arab","gay","wom","men","pol","mot","samst",
               "spor","fam","hsfd","coup","cowk","chur","hobb","neigh",
               "vol","bsns")

# Random data generated from the social categories data set from Ryan Lei's 
# Qualtric survey.
random_df <- apply(df, 2, 
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)

# Standardize the data sets
# The features are normalized by computing the z-score for each column: (x - mean)/std
df <- scale(df)
random_df <- scale(random_df)

library("factoextra")
# Plot faithful data set
rownames(df)<- allLabels
rownames(random_df)<- allLabels

#Plot the dataset
fviz_pca_ind(princomp(df), title = "PCA - Group Categories Data", 
             habillage = all_models[[2]]$cluster,  palette = "jco",
             geom = "text", ggtheme = theme_classic(),
             legend = "bottom",addEllipses = T)

clusplot(df,all_models[[2]]$cluster,color=TRUE,shade=TRUE,labels=2, 
         lines=0,main="PCA - Group Categories Data")

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
        max.nc = 10,method = "kmeans")

