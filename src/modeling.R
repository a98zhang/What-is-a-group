# ---------------------------------------------------------------------------- #
# I. Preparatory: packages and helper functions

# load packages: for data outputing, wrangling, LASSO, PCA plotting, Clustering

library(openxlsx)
library(readxl)
library(writexl)
library(dplyr)
library(multicon)
library(glmnet)
library(selectiveInference)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggthemes)
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

# helper fn: Convert a vector of strings into one-dimensional string

labelsToString <- function(mylabels) {
  full_string <- ''
  for(i in 1:length(mylabels)){
    if(i == 1) {
      full_string <- paste0(mylabels[i])
    }else{
      full_string <- paste0(full_string, ', ', mylabels[i])
    }
  }
  return(full_string)
}


# helper fn: simulate k-means clustering

simulateClusterings <- function(data, models_per_k=100, k_range=3,
                                cluster_min=3, saveModels=F, savePlots=F) {
  
  # data is the matrix of n x d input vectors (n groups and d traits per group)
  # models_per_k is the number of models to construct per k.
  # k_range is a vector with the values of k - numbers of clusters.
  # cluster_min is the min # of groups contained within each cluster in a model.
  # used to make sure cross-validated logistic regression not produce errors
  # working_directory is the directory used to hold graphs & data generated
  # If saveModels is set to T, then the models will be saved as RDS files.
  
  # 0. preparatory
    
    set.seed(10010)
  
  # 1. construct a variable number of k-means clustering models at given k's.
  
    all_models <- list()
    
    # for a specified range of k...
    for (i in 1:length(k_range)) {
      
      k <- k_range[i]
      
      # simulate a given amount of models 
      for (j in 1:models_per_k) {
        
        # construct a k-means model and store
        model_count <- (i-1) * models_per_k + j
        all_models[[model_count]] <- kmeans(data, k, iter.max = 10000) 
  
      }
      
    }
    
  # 2. remove models that have less than 'cluster_min' examples per cluster.
    
    i <- length(all_models)
    
    while (i > 0) {
      
      model_counts <- table(all_models[[i]]$cluster)
      # remove as long as there is one of the clusters has size <= cluster_min
      if (TRUE %in% (model_counts <= cluster_min)) {
        all_models <- all_models[-i]
      }
      
      i <- i-1
      
    }
    print(paste0('Number of cluster models in set: ', length(all_models)))
    

  
  # 3. generate plottings of the k-means models using PCA cluster plot and save
    
  if (savePlots== T) {
    for (i in 1:length(all_models)) {
      
      png(paste0('kmeansPlot',i,'.png'))
      
      myplot <- clusplot(data, all_models[[i]]$cluster,
                         color=TRUE, shade=TRUE, labels=2, lines=0)
      
      dev.off()  # needed when creating .png. the plot in this iteration is over. 
    }
  }  

    
  
  # 4. save and return the models
  
  if (saveModels == T) {
    saveRDS(all_models,"out/k_cluster_models.rds")
  }

  return(all_models)
}


# helper fn: find most common solution

findMostCommonClustering <- function(data, labels, all_models) {
  
  # 0. create an empty list for all clustering solutions
  solutions <- list()
  
  # 1. iterate through all clustering models to extract clustering solutions 
  
  for (i in 1:length(all_models)) {
    
    solution <- NULL
    
    for (j in 1:nc) {
      curr_groups <- labels[all_models[[i]]$cluster == j]
      solution <- cbind(solution, labelsToString(curr_groups))
    }
    
    solutions[[i]] <- labelsToString(sort(solution))

  }
  
  # 2. extract unique solutions and aggregate identical solutions
  
  unique_solutions <- unique(solutions)
  print(paste0('In total, ', length(unique_solutions), ' unique clustering solutions'))
  class_solutions <- list()
  
  for (i in 1:length(unique_solutions)) {
    class_solutions[[i]] <- which(solutions == unique_solutions[[i]])
  }
  
  print(paste0('Count the frequency of each unique solution: ', lengths(class_solutions)))
  
  # 3. extract the most common solution and return the model
  
  chosen_solutions <- class_solutions[[which.max(lengths(class_solutions))]]
  chosen_kmeans <- all_models[chosen_solutions]
  
  return(chosen_kmeans[[1]])
  
}

# helper fn: simulate LASSO logistic regression

simulateLogReg <- function(data, kmean_model, saveBetas=NULL){
  
  # 0. create empty data frame for the beta coefficients & set seed
  
  set.seed(42405)
  
  all_coefs <- data.frame(row.names=colnames(data))
  all_coefs[, 1:3] <- 0
  #sigtest <- list()
  
  # for each cluster of the model
  for (j in 1:length(kmean_model$size)){
    
    # 1. create the outcome of the logistic regression model
    
    y <- (kmean_model$cluster == j)*1
    y <- as.factor(y)
    
    # 2. for each model perform 100 times simulation of LASSO regression 
    
    lambdas = NULL
    for (i in 1:100){
      cvfit <- cv.glmnet(data, y, family = "binomial", type.measure = "class")
      errors <- data.frame(cvfit$lambda, cvfit$cvm)
      lambdas <- rbind(lambdas, errors)
    }
    
    # 3. take mean cross-validation error for each lambda and select the optimal
    
    lambdas <- aggregate(lambdas[, 2], list(lambdas$cvfit.lambda), mean)
    bestindex = which(lambdas[2]==min(lambdas[2]))[1]
    bestlambda = lambdas[bestindex,1]
    
    # 4. fit LASSO with the fixed lambda
    
    fit <- glmnet(data, y, family = "binomial", type.measure = "class", lambda=bestlambda)
    all_coefs[, j] <- fit$beta[, 1]
    
    # 5. run significance testing for the beta coefficients
    #beta_hat <- coef(fit, x = data, y = y, s = bestlambda/length(y), exact=TRUE)
    #sigtest[[j]] <- fixedLassoInf(x = data, y = as.numeric(y), beta_hat, bestlambda, family="binomial")
   
    # 6. set the column name
    
    curr_groups <- groups[kmean_model$cluster == j,][1]
    colnames(all_coefs)[j] <- labelsToString(curr_groups[,1])
  }
  
  # 4. save in csv and return the coefficients
  if (!is.null(saveBetas)) {
    write.csv(all_coefs, saveBetas, row.names = TRUE)
  }
  
  return (sigtest)
  
}


# ---------------------------------------------------------------------------- #
# II. Study 1 

# 0. import data and models for study 1

groups <- read.csv('data/group_study1.csv')

labels <- groups[, 1]            # first column (group name) as row names 
data <- scale(as.matrix(groups[-1]))
rownames(data) <- labels 

# 1. determine the number of principal components 
png('out/horn_study1.png')
horn(data, sims = 100)
dev.off()

# 2. determine the number of clusters 
nb <- NbClust(data = data, distance = "euclidean", min.nc = 2, 
              max.nc = 8, method = "kmeans")
fviz_nbclust(nb, barfill="#0084d1", barcolor="#0084d1") + 
  ggtitle("Bar Graph for NbClust Solution") + 
  theme(plot.title = element_text(hjust=0.5))
ggsave("out/nbclust_study1.png", width = 5, height = 5)


nc <- 3 # nbclust suggesting 3 

# 3. simulate k-means cluster
all_models <- simulateClusterings(data, models_per_k=100, k_range=nc,
                                  cluster_min=3, saveModels=F, savePlots=F)


# 4. find the most common solutions 
curr_kmean <- findMostCommonClustering(data, labels, all_models)
saveRDS(curr_kmean,"out/kmean_study1.rds")

# 5. simulate LASSO logistic regressions modeling the cluster membership
betas <- simulateLogReg(data, kmean_model=curr_kmean, 
                        saveBetas='out/LASSO_study1.csv'
                        )



# ---------------------------------------------------------------------------- #
# III. Study 2

# 0. import data and models for study 2
rm(list = setdiff(ls(), lsf.str()))

groups <- read.csv('data/group_study2.csv')

labels <- groups[, 1]            # first column (group name) as row names 
data <- scale(as.matrix(groups[-1]))
rownames(data) <- labels 


# 1. determine the number of principal components 
png('out/horn_study1.png')
horn(data, sims = 100)
dev.off()

# 2. determine the number of clusters 
nb <- NbClust(data = data, distance = "euclidean", min.nc = 2, 
              max.nc = 8, method = "kmeans")
fviz_nbclust(nb, barfill="#0084d1", barcolor="#0084d1") + 
  ggtitle("Bar Graph for NbClust Solution") + 
  theme(plot.title = element_text(hjust=0.5))
ggsave("out/nbclust_study1.png", width = 5, height = 5)


nc <- 3 # nbclust suggesting 3 

# 3. simulate k-means cluster
all_models <- simulateClusterings(data, models_per_k=100, k_range=nc,
                                  cluster_min=3, saveModels=F, savePlots=F)


# 4. find the most common solutions 
curr_kmean <- findMostCommonClustering(data, labels, all_models)
saveRDS(curr_kmean,"out/kmean_study2.rds")

# 5. simulate LASSO logistic regressions modeling the cluster membership
betas <- simulateLogReg(data, kmean_model=curr_kmean, 
                        saveBetas='out/LASSO_study2.csv'
                        )


