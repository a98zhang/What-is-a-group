#################################### preparation ###############################
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(factoextra)
library(cluster)
library(NbClust)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(FactoMineR)
library(clustertend)
library (fpc)
library (clValid)


groups<-read.csv('C:/Users/ashle/Job.Intern.Lab/CDL/GroupPaper/Group_Rating_Study_N481_scored.csv')
df<-groups[-1]
allLabels_full <- c("Feminists", "Students","Christians","Democrats","Republican","Rich",
                    "Poor","Midclass","Workclass","Asian","White","Black",
                    "Hispanic","Arab","Gay","Women","Men","Police","Mothers","SameState",
                    "Sports","Family","HighSchoolFriends","Couple","Coworkers","Church","Hobby","Neighbor",
                    "Volunteer","Business")
df <- scale(df)
rownames(df)<- allLabels_full 
all_models <- readRDS("C:/Users/ashle/R/study1/June22/k_cluster_models.rds")
curr_model <- all_models[[3]]

# compute PCA
g.pca <- prcomp(df)

################################### PCA plotting ###############################

# PCA results - eigenvalues
eig <- get_eig(g.pca)
p <- fviz_eig(g.pca, choice = "eigenvalue", ncp = 5,
              addlabels=TRUE, hjust = -0.3, width = 0.5,
              barfill="white", barcolor ="darkblue",
              linecolor ="red") + 
  theme_minimal()
print(p)

# PCA results - variables
var <- get_pca_var(g.pca)

  # correlation circle ($coord) - variable correlation plot
  fviz_pca_var(g.pca, col.var="coord", repel = TRUE) +
    scale_color_distiller(palette = "YlOrRd",direction = 1) + theme_minimal()
  
  # quality of representation of variables ($cos2)
  corrplot(var$cos2, is.corr=FALSE)
  fviz_cos2(g.pca, choice = "var", axes = 1:2)
  fviz_pca_var(g.pca, col.var="cos2", repel = TRUE) +
    scale_color_distiller(palette = "YlOrRd",direction = 1) + theme_minimal()
  
  # contributions 
  corrplot(var$contrib, is.corr=FALSE)
  fviz_contrib(g.pca, choice = "var", axes = 1)
  fviz_contrib(g.pca, choice = "var", axes = 2)
  fviz_contrib(g.pca, choice = "var", axes = 1:2)
  fviz_pca_var(g.pca, col.var="contrib", repel = TRUE) +
    scale_color_distiller(palette = "YlOrRd",direction = 1) + theme_minimal()

  # classifying variables 
  set.seed(123)
  res.km <- kmeans(var$coord, centers = 2, nstart = 25)
  grp <- as.factor(res.km$cluster)
  # Color variables by groups
  fviz_pca_var(g.pca, col.var = grp, 
               palette = c("#0073C2FF", "#EFC000FF"),
               legend.title = "Cluster")
  
  # dimension description
  res.desc <- dimdesc(PCA(df), axes = c(1,2), proba = 0.05)
  res.desc$Dim.1
  
# PCA results - individuals    
fviz_pca_ind(g.pca,
             col.ind = "cos2", # Color by the quality of representation
             pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

  # colored by clusters
  res.ind <- fviz_pca_ind(g.pca, title = "PCA - Group Categories Data", 
               habillage = curr_model$cluster,  
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               geom = c("text", "point"), ggtheme = theme_classic(),
               legend = "bottom",addEllipses = T, mean.point=FALSE)
  ggpubr::ggpar(res.ind,
                title = "Principal Component Analysis",
                subtitle = "Study 1 data set",
                caption = "Source: factoextra",
                xlab = "PC1", ylab = "PC2",
                legend.title = "Species", legend.position = "top",
  )

################################ Clustering Plot ###############################

# use clusplot to plot the clustering results 
clusplot(df,curr_model$cluster,color=TRUE,shade=TRUE,labels=2,
         stand = TRUE,
         #span = FALSE,
         #xlim = c(7,-7), 
         #ylim = c(7,-7), 
         cex.txt = 0.75,plotchar = TRUE,
         col.p = "#7a49aa",col.txt = "#663695",
         col.clus = c("#62c3f7","#F68AA6","#FB8620"),
         lines=0,main="PCA - Group Categories Data")


#######################E#### K-means + PCA plotting ############################
  
# use ggbiplot to draw correlation circle on the clustering plot
ggbiplot(g.pca,ellipse=T,groups=as.factor(curr_model$cluster),
         scale=0, ellipse.prob = 0.8,
         #var.axes = F,
         row.scale = 1, var.scale = 1,
         labels=rownames(df)
)+
  scale_color_manual(name="Classifier", values = c("#F68AA6","#6Eb232","#62c3f7"))+
  theme_minimal()

#################################### table #####################################

library(data.table)
library(dplyr)
library(formattable)
library(tidyr)

#Set a few color variables to make our table more visually appealing

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"  

t1 <- var$cor[,1:2]
v1 <- t1[,1]
v2 <- t1[,2]
d1 <- data.frame(v1,v2)
colnames(d1) <- c("Dimension 1", "Dimension 2")

#################################### 3D Plot ###################################

library(rgl)
library(pca3d)

groupings <- as.factor(curr_model$cluster)
plot3d(g.pca$scores[,1:3])
td <- pca3d(pc,group=groupings,show.ellipses=TRUE,
            #fancy = T, 
            show.labels = T, show.shadows = T,
            palette = c("powderblue","rosybrown1","sandybrown"),
            labels.col = c("skyblue4","palevioletred","tan4"),
            show.axe.titles = T, bg = "white", axes.color = "grey",
            #biplot = T, biplot.vars = 3,
            ellipse.ci=0.75, show.plane=F)
makeMoviePCA(td)

snapshotPCA3d(file=("pca3dPlot.png"))

#################################### Output ####################################

pca.plot <- fviz_pca_var(g.pca, col.var="contrib", repel = TRUE) +
  scale_color_distiller(palette = "YlOrRd",direction = 1) + theme_minimal()

comb.plot <- ggbiplot(g.pca,ellipse=T,groups=as.factor(curr_model$cluster),
                      scale=0, ellipse.prob = 0.8,
                      #var.axes = F,
                      row.scale = 1, var.scale = 1,
                      labels=rownames(df)
)+
  scale_color_manual(name="Classifier", values = c("#F68AA6","#6Eb232","#62c3f7"))+
  theme_minimal()

pdf("Study 1 - PCA.pdf") 
print(pca.plot)
print(comb.plot)
dev.off() 
