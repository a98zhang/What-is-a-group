# ---------------------------------------------------------------------------- #
# I. Preparatory: packages

# load packages: for PCA and Cluster plotting 

library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggthemes)
library(factoextra)
library(cluster)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(FactoMineR)
library(clustertend)

# ---------------------------------------------------------------------------- #
# II. Study 1 

# 0. import data and models for study 1
groups <- read.csv('data/group_study1.csv')

labels <- groups[, 1]            # first column (group name) as row names 
data <- scale(as.matrix(groups[-1]))
rownames(data) <- labels 

curr_model <- readRDS("out/kmean_study1.rds")

# 1. conduct PCA
prin <- princomp(data)    # use princomp to stay consistent with clusplot
prin_var <-  prin$sdev ^ 2
pve <- prin_var / sum(prin_var)


# 2. plot clustering results on principal components
pdf("out/clusplot_study1.pdf") 
clusplot(data, curr_model$cluster, color=TRUE, shade=TRUE, labels=2,
         stand = TRUE,
         #span = FALSE,
         xlim = c(-2.5, 2.5), 
         ylim = c(-2.5, 2.5), 
         xlab = paste("Principal Component 1 (", format(pve[[1]]*100, digits=2, nsmall=2), "%)"), 
         ylab = paste("Principal Component 2 (", format(pve[[2]]*100, digits=2, nsmall=2), "%)"),
         cex.txt = 0.8, plotchar = TRUE,
         col.p = "#464646",col.txt = "#464646",
         col.clus = c("#008fd5","#77AB43","#ff950e"),
         lines=0, main="PCA - Group Categories Data")
dev.off()

# 3. inspect principal components
pdf('out/PCA_study1.pdf')
ggbiplot(prin, ellipse=F, groups=as.factor(curr_model$cluster),
         scale=0, ellipse.prob = 0.8,
         #var.axes = F,
         row.scale = 1, var.scale = 1,
         labels=rownames(data)
)+
  scale_color_manual(name="Classifier", values = c("#77AB43","#008fd5","#ff950e"))+
  theme_minimal()
dev.off()

var <- get_pca_var(prin)
pca <- data.frame(var$cos2)
write.csv(pca, 'out/PCA_study1.csv', row.names = TRUE)

# ---------------------------------------------------------------------------- #
# III. Study 2

# 0. import data and models for study 2
groups <- read.csv('data/group_study2.csv')

labels <- groups[, 1]            # first column (group name) as row names 
data <- scale(as.matrix(groups[-1]))
rownames(data) <- labels 

curr_model <- readRDS("out/kmean_study2.rds")


# 1. conduct PCA
prin <- princomp(data)    # use princomp to stay consistent with clusplot
prin_var <-  prin$sdev ^ 2
pve <- prin_var / sum(prin_var)


# 2. plot clustering results on principal components
pdf("out/clusplot_study2.pdf") 
clusplot(data, curr_model$cluster, color=TRUE, shade=TRUE, labels=2,
         stand = TRUE,
         #span = FALSE,
         xlim = c(-3, 2), 
         ylim = c(-3, 2), 
         xlab = paste("Principal Component 1 (", format(pve[[1]]*100, digits=2, nsmall=2), "%)"), 
         ylab = paste("Principal Component 2 (", format(pve[[2]]*100, digits=2, nsmall=2), "%)"),
         cex.txt = 0.8, plotchar = TRUE,
         col.p = "#464646",col.txt = "#464646",
         col.clus = c("#008fd5","#77AB43","#ff950e"),
         lines=0, main="PCA - Group Categories Data")
dev.off()

# 3. inspect principal components
pdf('out/PCA_study2.pdf')
ggbiplot(prin, ellipse=F, groups=as.factor(curr_model$cluster),
         scale=0, ellipse.prob = 0.8,
         #var.axes = F,
         row.scale = 1, var.scale = 1,
         labels=rownames(data)
)+
  scale_color_manual(name="Classifier", values = c("#77AB43","#008fd5","#ff950e"))+
  theme_minimal()
dev.off()

var <- get_pca_var(prin)
pca <- data.frame(var$cos2)
write.csv(pca, 'out/PCA_study2.csv', row.names = TRUE)


# ---------------------------------------------------------------------------- #
# IV. Study 1 - dimension reduced

# 0. import data and models for study 1
groups <- read.csv('data/group_study1.csv')

labels <- groups[, 1]            # first column (group name) as row names 
data <- scale(as.matrix(groups[-1]))
rownames(data) <- labels 

curr_model <- readRDS("out/kmean_pc_study1.rds")

# 1. conduct PCA
prin <- princomp(data)    # use princomp to stay consistent with clusplot
prin_var <-  prin$sdev ^ 2
pve <- prin_var / sum(prin_var)


# 2. plot clustering results on principal components
pdf("out/clusplot_pc_study1.pdf") 
clusplot(data, curr_model$cluster, color=TRUE, shade=TRUE, labels=2,
         stand = TRUE,
         #span = FALSE,
         xlim = c(-2.5, 2.5), 
         ylim = c(-2.5, 2.5), 
         xlab = paste("Principal Component 1 (", format(pve[[1]]*100, digits=2, nsmall=2), "%)"), 
         ylab = paste("Principal Component 2 (", format(pve[[2]]*100, digits=2, nsmall=2), "%)"),
         cex.txt = 0.8, plotchar = TRUE,
         col.p = "#464646",col.txt = "#464646",
         col.clus = c("#008fd5","#77AB43","#ff950e"),
         lines=0, main="PCA - Group Categories Data")
dev.off()

# 3. inspect principal components
pdf('out/PCA_pc_study1.pdf')
ggbiplot(prin, ellipse=F, groups=as.factor(curr_model$cluster),
         scale=0, ellipse.prob = 0.8,
         #var.axes = F,
         row.scale = 1, var.scale = 1,
         labels=rownames(data)
)+
  scale_color_manual(name="Classifier", values = c("#77AB43","#008fd5","#ff950e"))+
  theme_minimal()
dev.off()

var <- get_pca_var(prin)
pca <- data.frame(var$cos2)
write.csv(pca, 'out/PCA_study1.csv', row.names = TRUE)


# ---------------------------------------------------------------------------- #
# III. Study 2 - dimension reduced

# 0. import data and models for study 2
groups <- read.csv('data/group_study2.csv')

labels <- groups[, 1]            # first column (group name) as row names 
data <- scale(as.matrix(groups[-1]))
rownames(data) <- labels 

curr_model <- readRDS("out/kmean_pc_study2.rds")


# 1. conduct PCA
prin <- princomp(data)    # use princomp to stay consistent with clusplot
prin_var <-  prin$sdev ^ 2
pve <- prin_var / sum(prin_var)


# 2. plot clustering results on principal components
pdf("out/clusplot_pc_study2.pdf") 
clusplot(data, curr_model$cluster, color=TRUE, shade=TRUE, labels=2,
         stand = TRUE,
         #span = FALSE,
         xlim = c(-3, 2), 
         ylim = c(-3, 2), 
         xlab = paste("Principal Component 1 (", format(pve[[1]]*100, digits=2, nsmall=2), "%)"), 
         ylab = paste("Principal Component 2 (", format(pve[[2]]*100, digits=2, nsmall=2), "%)"),
         cex.txt = 0.8, plotchar = TRUE,
         col.p = "#464646",col.txt = "#464646",
         col.clus = c("#008fd5","#77AB43","#ff950e"),
         lines=0, main="PCA - Group Categories Data")
dev.off()

# 3. inspect principal components
pdf('out/PCA_pc_study2.pdf')
ggbiplot(prin, ellipse=F, groups=as.factor(curr_model$cluster),
         scale=0, ellipse.prob = 0.8,
         #var.axes = F,
         row.scale = 1, var.scale = 1,
         labels=rownames(data)
)+
  scale_color_manual(name="Classifier", values = c("#77AB43","#008fd5","#ff950e"))+
  theme_minimal()
dev.off()

var <- get_pca_var(prin)
pca <- data.frame(var$cos2)
write.csv(pca, 'out/PCA_study2.csv', row.names = TRUE)