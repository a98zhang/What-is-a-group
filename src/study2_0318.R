# ---------------------------------------------------------------------------- #
# 0. Must Load: packages, original datasets, helper functions

#install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)
library(openxlsx)
library(readxl)
library(writexl)
library(dplyr)

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

# function that creates all the models and graphs (100 simulations)

constructLogRegModelsFromClusters <- function(data, models_per_k=10,k_range=3,
                                              cluster_min=3,working_directory="~/r_files/newDataGraphs",
                                              saveModels=F, doLogReg=T,special=F){
  
  # The steps involved are as follows:
  
  # 1. Construct a variable number of k-means clustering models at k = 3.
  
  # This counter will count what model we are currently at 
  model_count <- 1
  
  # All the models will be stored in a list.
  all_models <- list()
  
  # For a specified range of k...
  for(k in k_range){
    
    for(i in 1:models_per_k){
      # Construct a k-means model
      curr_model<-kmeans(data,k, iter.max = 10000) 
      
      # Store the current model in our all_models list at index model_count
      all_models[[model_count]] <- curr_model
      
      model_count <- model_count + 1
    }
    
  }
  
  # Go through and remove models that have less than 'cluster_min' examples per cluster.
  
  i = length(all_models)
  while(i>0) {
    model_counts <- table(all_models[[i]]$cluster)
    #print(model_counts)
    # at least one is false. 
    if(TRUE %in% (model_counts <= cluster_min)){
      all_models <- all_models[-i]
    }
    i <- i-1
  }
  print(paste0('Number of cluster models in set: ',length(all_models)))
  
  # Set the directory to save into as the working_directory specified in the function arguments.
  dir.create(working_directory)
  setwd(working_directory) 
  
  # 2. Plot the k-means models using PCA cluster plot. Save all the graphs in the specified
  # working_directory.
  for(i in 1:length(all_models)){
    png(paste0('kmeansPlot',i,'.png')) # don't overwrite 
    if(special){
      rownames(data) <- groups.special$X
    }else{
      # rownames(data) <- groups$group
      rownames(data) <- allLabels
    }
    
    myplot <- clusplot(data,all_models[[i]]$cluster,color=TRUE,shade=TRUE,labels=2, lines=0)
    dev.off()  # needed when creating .png. the plot in this iteration is over. 
  }
  
  # 3. For each cluster model, form logistic regression classifiers to distingiush 
  # each individual cluster in the model.
  
  # If doing logistic regression is set to true...
  if(doLogReg == T){
    # This can fail if cluster_min is too low.
    all_log_models <- list()
    for(i in 1:length(all_models)){
      # each cluster solution. 
      curr_kmean <- all_models[[i]]
      curr_log_models <- list()
      print(paste0('K-means Model ',i))
      # log model for each cluster within the cluster solution. 
      # that discriminate the incluster and out-cluster relationship using a binomial 
      for(j in 1:length(curr_kmean$size)){
        print(paste0('Log Model ',j))
        y <- (curr_kmean$cluster == j)*1
        y <- as.factor(y)   # gmlt labels of classes ()
        table(y)

        cvfit <- cv.glmnet(data, y, family = "binomial", type.measure = "class")
        
        # Save the new log reg cvfit into current log reg models' list for this k-mean model.
        curr_log_models[[j]] <- cvfit
      }
      all_log_models[[i]] <- curr_log_models
      
    }
    
    # Store all beta coefficients for each model in a list
    all_coef_list <- list()
    
    # 4. Find the most and least predictive weights for each cluster in the model
    for(i in 1:length(all_models)){
      curr_kmean <- all_models[[i]]
      curr_log_reg <- all_log_models[[i]]
      print(paste0("This is cluster model ", i))  
      
      # Data frame to store the beta coefficients for the model
      all_coefs <- data.frame(row.names=colnames(data))
      all_coefs[,1:length(curr_log_reg)] <- 0
      for(j in 1:length(curr_log_reg)){
        # Save the model with smallest lambda
        small.lambda.index <- which(curr_log_reg[[j]]$lambda == curr_log_reg[[j]]$lambda.min)
        #Get that model's weights and print them.
        small.lambda.betas <- curr_log_reg[[j]]$glmnet.fit$beta[, small.lambda.index]
        print(paste0("This is cluster ",j, " in model ",i))
        mylabels <- groups[curr_kmean$cluster == j,]$X
        print(mylabels)
        print(small.lambda.betas[order(small.lambda.betas)])
        
        #Set the j-th column to current cluster's beta coefficients.
        all_coefs[,j] <- small.lambda.betas
        
        # Set the column name to the groups in the cluster
        colnames(all_coefs)[j] <- labelsToString(mylabels)
      }
      # Add current data frame for the clusters to all_coef_list
      all_coef_list[[i]] <- all_coefs
      print('')
    }
  }
  
  # If saveModels is T (it is F or false by default)...
  if(saveModels == T){
    # Save the current clustering models in an rds titled k_cluster_models.rds 
    # in the working_directory
    saveRDS(all_models,"k_cluster_models.rds")
    
    if(doLogReg == T){
      # Save the logistic regression models in an rds titled log_reg_models.rds
      # in the working_directory
      saveRDS(all_log_models, "log_reg_models.rds")
    }
    
  }
  
  model_list <- list(all_models)
  if(doLogReg == T){
    model_list[[2]] <- all_log_models
    
    # Save all the data-frames into an excel file
    wb = createWorkbook()
    
    for(i in 1:length(all_coef_list)){
      temp <- paste0('Model ', i)
      addWorksheet(wb=wb, sheetName = temp )
      #sheet = createSheet(wb, paste0('Model ', i))
      writeData(wb = wb, sheet = temp, x = all_coef_list[[i]], rowNames = TRUE)
      #addDataFrame(all_coef_list[[i]],sheet=sheet,row.names=TRUE)
    }
    
    saveWorkbook(wb, "all_models_and_clusters_workbook.xlsx")
  }
  # Return the list of k-clustering models and list of logistic regression models
  # in a giant list.
  return(model_list)
}


# import data and models for study 2
groups<-read.csv('C:/Users/ashle/Job.Intern.Lab/CDL/GroupPaper/Adult_Groups_Study_2_N386.csv')
data<-as.matrix(groups[-1])
data <- scale(data)
allLabels <- c("fam","sup","rom","fri","gang","orc","team","band","polish",
               "ameri","women","blacks","jews","doc","fli","comp","proj",
               "jury","play","studcom","env","studexam","waiter","uni","dinner",
               "room","bus","bank","movie","neigh","students","studcla","retir",
               "classic","athl","plum","teach","factory","laboru","pol")
rownames(data)<- allLabels


# ---------------------------------------------------------------------------- #
# 1. Perform dimension reduction and determine optimal hyperparameters

# determine optimal number of principal components to maintain
library(multicon)
horn(data,sims = 100)

# perform dimension reduction
pca <- prcomp(data)
data <- pca$x[,1:2]

# determine optimal number of clusters
library(NbClust)
nb <- NbClust(data = data, distance = "euclidean", min.nc = 2, 
              max.nc = 10,method = "kmeans", index="all")
fviz_nbclust(nb)


# Tweak the variables in this function to find the results you would like in cluster analysis.
model_list.pc <- constructLogRegModelsFromClusters(data,saveModels = T,models_per_k = 100,cluster_min=1,
                                                   working_directory="C:/Users/ashle/R/study2/March18",
                                                   k_range=3, doLogReg=T,special=F)


# ---------------------------------------------------------------------------- #
# 2. Label clusters in regression models

# inspecting model from previously generated models (1-100)
reg_models <- readRDS("C:/Users/ashle/R/study2/March18/log_reg_models.rds")
all_kmeans <- readRDS("C:/Users/ashle/R/study2/March18/k_cluster_models.rds")
reg_models_labeled <- list()
all_coef_list <- list()

for (i in 1:length(reg_models)) {

  curr_model <- reg_models[[i]]
  curr_kmean <- all_kmeans[[i]]
  
  curr_model_labeled <- list()
  all_coefs <- data.frame(row.names=colnames(data))
  all_coefs[, 1:length(curr_model)] <- 0

  for (j in 1:length(curr_model)) {
    
    cvfit <- curr_model[[j]]
    
    # inspecting coefficients results 
    coef <- coef(cvfit, s="lambda.min")
    
    # matching model to groups
    curr_groups <- groups[curr_kmean$cluster == j,][1]
    labelString <- labelsToString(curr_groups[,1])
    
    all_coefs[,j] <- as.matrix(coef)[-1]
    colnames(all_coefs)[j] <- labelString
  }
  
  all_coef_list[[i]] <- all_coefs
}


# save data
saveRDS(all_coef_list, "C:/Users/ashle/R/study2/March18/reg_models_labeled.rds")

# re-creating all_models_and_clusters_workbook.xlsx with groups labeling
wb = createWorkbook()
for(i in 1:length(all_coef_list)){
  temp <- paste0('Model ', i)
  addWorksheet(wb=wb, sheetName=temp)
  writeData(wb=wb, sheet=temp, x=all_coef_list[[i]], rowNames=TRUE)
}
saveWorkbook(wb, "C:/Users/ashle/R/study2/March18/all_models_and_clusters_workbook_study1_labeled.xlsx")

# ---------------------------------------------------------------------------- #
# 3. Find solution that emerged most consistently


# load in all labeled models and find the most frequently appeared solution
reg_models_labeled <- readRDS("C:/Users/ashle/R/study2/March18/reg_models_labeled.rds")

solutions <- list()

for (i in 1:length(reg_models_labeled)) {
  curr_model_labeled <- reg_models_labeled[[i]]
  curr_model_labeled <- curr_model_labeled[order(colnames(curr_model_labeled))]
  solutions[[i]] <- labelsToString(colnames(curr_model_labeled))
}

unique_solutions <- unique(solutions)

class_solutions <- list()
for (i in 1:length(unique_solutions)) {
  class_solutions[[i]] = which(solutions == unique_solutions[[i]])
}

chosen_solutions = class_solutions[[which.max(lengths(class_solutions))]]
reg_models_chosen <- reg_models_labeled[chosen_solutions]

length(class_solutions)
lengths(class_solutions)

# save data
saveRDS(reg_models_chosen, "C:/Users/ashle/R/study2/March18/reg_models_solution.rds")

