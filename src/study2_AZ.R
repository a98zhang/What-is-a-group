# Load libraries
library(factoextra)
library(cluster)
library(NbClust)
library(ggplot2)
library(paran)

# Import the file titled, "Adult Groups Study 2_N386.csv"
groups<-read.csv('C:/Users/ashle/Job.Intern.Lab/CDL/GroupPaper/Adult_Groups_Study_2_N386.csv')
# It contains 40 groups with 19 traits as our initial input matrix.
#groups<-read.csv(file.choose())

# The first column is removed, as it was simply the names of the groups
df<-groups[-1]

allLabels <- c("fam","sup","rom","fri","gang","orc","team","band","polish",
               "ameri","women","blacks","jews","doc","fli","comp","proj",
               "jury","play","studcom","env","studexam","waiter","uni","dinner",
               "room","bus","bank","movie","neigh","students","studcla","retir",
               "classic","athl","plum","teach","factory","laboru","pol")
allLabels_full <- c("Family","SupportGroup", "Romantic","Friends","Gang","Orchestra","SportsTeam",
               "RockBand","Polish","American", "Women","Blacks","Jews","Doctors","FlightCrew",
               "CompanyCommittee","ProjectWorkers","Jury","Cast","CampusCommittee",
               "EnvironmentalOrg","StudyingForExam","Waiters","UniSocialClub",
               "DinnerTogether","Roommates","BusStop","BankLine","MovieAudience",
               "Neighbor","SameUniversity","SameClass","RetireHome","ClassicalMusic",
               "AthleteContest","Plumbers","Teachers","SameFactory","LaborUnion","PoliticalParty")

# Standardize the data sets
# The features are normalized by computing the z-score for each column: (x - mean)/std
df <- scale(df)
row.names(df) <- allLabels_full

all_models <- readRDS("C:/Users/ashle/R/study2/June8/k_cluster_models.rds")
all_regs <- readRDS("C:/Users/ashle/R/study2/June8/log_reg_models.rds")





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


# number of clusters to use
library(NbClust)
nb <- NbClust(data = df, distance = "euclidean", min.nc = 2, 
              max.nc = 10,method = "kmeans", index="all")
fviz_nbclust(nb)



#Plot the dataset
fviz_pca_ind(princomp(df), title = "PCA - Group Categories Data", 
             habillage = all_models[[3]]$cluster,  palette = "jco",
             geom = "text", ggtheme = theme_classic(),
             legend = "bottom",addEllipses = T)

#"The CLUSPLOT display is then the bivariate plot of the objects 
#relative to the rst two principal components"
# - Displaying a clustering with CLUSPLOT (Pison, Struyf, & Rousseeuw, 1999)
clusplot(df,all_models[[2]]$cluster,color=TRUE,shade=TRUE,labels=2, 
         lines=0,main="PCA - Group Categories Data")
clusplot(df,all_models[[3]]$cluster,color=TRUE,shade=TRUE,labels=2,
         col.txt= c("#133c70", "#133c70", "#133c70"),
         col.clus= c("#00AFBB", "#E7B800", "#FC4E07"),
         lines=0,main="PCA - Group Categories Data")




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






# Random data generated from the social categories data set from Ryan Lei's 
# Qualtric survey.
random_df <- apply(df, 2, 
                   function(x){runif(length(x), min=0, max=10)})  # a matrix
random_df <- as.data.frame(random_df)
random_df <- scale(random_df)
row.names(random_df)<- allLabels

# Plot the random df
fviz_pca_ind(princomp(random_df), title = "PCA - Random Group Categories Data", 
             geom = "text", ggtheme = theme_classic())

random.kmeans <- kmeans(random_df,centers=4)
clusplot(random_df,random.kmeans$cluster, color=TRUE,shade=TRUE,labels=2, 
         lines=0,main="PCA - Random Group Categories Data")







#new.df <- df[,-7]
#new.df <- new.df[,-23]
#df <- new.df
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
km.res <- eclust(df, "kmeans", k = 3, nstart = 25, graph = TRUE)






#######
set.seed(123)
km.res <- kmeans(df, 3, nstart = 25 )
km.res$cluster

#fviz_cluster takes k-means results and the original data as arguments
#observations are represented by points, 
#using principal components if the number of variables is greater than 2.
fviz_cluster(km.res, data = df, geom = "point",
             stand = FALSE, frame.type = "norm")
fviz_cluster(km.res, data = df, ellipse.type="t", palette = "Set2",
             repel =TRUE, show.clust.cent=FALSE, main= "                                      Study 2 k-means Clustering Result",
             ggtheme=theme_minimal())
km.res <- eclust(df, "kmeans", k = 3, nstart = 25, graph = FALSE)
fviz_pca_ind(princomp(random_df), title = "PCA - Random Group Categories Data", 
             geom = "text", ggtheme = theme_classic())
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

