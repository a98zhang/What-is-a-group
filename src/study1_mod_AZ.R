library(cluster)
library(glmnet)   # lasso regression
library(multicon)
#library(xlsx)
library(openxlsx)
library(readxl)
library(writexl)

################### Helper Function ###################

# Convert a vector of strings into one-dimensional string
labelsToString <- function(mylabels){
  full_string <- ''
  for(i in 1:length(mylabels)){
    if(i == 1){
      full_string <- paste0(mylabels[i])
    }else{
      full_string <- paste0(full_string, ', ', mylabels[i])
    }
  }
  return(full_string)
}

################### Feature Extraction ###################

# Import the file titled, "GroupRating_DataMatrix_N250.csv"
# It contains 30 groups with 19 traits as our initial input matrix.
groups<-read.csv(file.choose())
head(groups)

## This is a holdout group that removes poor and hsfriends (lowest silhouette scores)
groups.special <- groups
groups.special <- groups.special[-25,]
groups.special <- groups.special[-22,]
groups.special <- groups.special[-7,]
groups.special <- groups.special[-6,]
groups.special <- groups.special[-5,]
groups.special$X
groups$X
df.special <- scale(groups.special[-1])

# The features are normalized by computing the z-score for each column: (x - mean)/std
# The first column is removed, as it was simply the names of the groups
df<-scale(groups[-1])
head(df)

allLabels <- c("fem", "stud","chris","dem","repub","rich",
               "poor","midc","workc","asn","wht","blck",
               "hisp","arab","gay","wom","men","pol","mot","samst",
               "spor","fam","hsfd","coup","cowk","chur","hobb","neigh",
               "vol","bsns")

# perform dimension reduction
pca <- prcomp(df)
df.pc <- pca$x[,1:2]

################### Logistic Regression Weight Extraction ###################

# This function will be used to find the best weights for each cluster by using logistic regression.

# Logistic Regression will assign weights to each feature in a sample. We can use these weights
# to find how important those features were for classification, and in concordance, for a given cluster.

constructLogRegModelsFromClusters <- function(data, models_per_k=10,k_range=3,
                                              cluster_min=3,working_directory="~/r_files/newDataGraphs",
                                              saveModels=F, doLogReg=T,special=F){
  
      # data is the matrix of n x d input vectors.
      # In our case, there are n groups and d traits per group.
  
      # models_per_k is the number of models to construct per k.
      # start from random different points - centroid starting points. 
  
      # k_range is a vector with the values of k you would like to try to build clusters for.
  
      # cluster_min is the minimum amount of groups contained within each cluster in a given model.
      # cluster_min is used to make sure cross-validated logistic regression will not produce errors
      # really important for logistic regression, in order to make it have meaningful result.
      # analog. if only have a few, very hard to generalize to other groups. 
      # if possible, structure the logistic regression of every single person's idea of the social groups.
      # but now z-score out of the constraints. 
  
      # working_directory is the directory that will be used to hold the graphs and data generated
      # from the models. The default is your home directory + "/r_files/graphs".
      # For example, if the user paul is on a mac, the directory would be "paul/r_files/graphs"
  
      # If saveModels is set to T, then the models themselves will be saved as RDS files.
  
      # The steps involved are as follows:
  
      # 1. Construct a variable number of k-means clustering models at k = 4 and k = 5.

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
            # supervised: class - what the label is telling us. 
            # assumption that these clusters we found are true. 
            # predictive analytical. 
        
            nfold <- 3 # deal with cross-validation. 
            # supervised learning: logistic
            # thousand of training example. fit the example to the classifiers. binary(0-1) or multi-nomial.
            # is it a dog or it is not a dog
            # which model is the most performs.
            # 80 - 20. hold-out. see how accurate 
            # kfold-cross-validation
            # do the 80-20, 90-10. of that 90%, split into 10 folds. validate the 10th. hold-out. 
            # ipok. then train on what you just held-out. 
            # fold back in. versus test set. fold set. 
            # training set - test set 
            # assign folds evenly using the modulus operator
            fold0 <- sample.int(sum(y==0)) %% nfold
            fold1 <- sample.int(sum(y==1)) %% nfold
            foldid <- numeric(length(y))
            foldid[y==0] <- fold0
            foldid[y==1] <- fold1
            foldid <- foldid + 1
            
            
            # cv.glmnet performs cross-validation and 
            # returns a list with all ingredients of the cross-validation
            cvfit <- cv.glmnet(data, y, family = "binomial", type.measure = "class")
        
            # Save the new log reg cvfit into current log reg models' list for this k-mean model.
            curr_log_models[[j]] <- cvfit
          }
          # Add the log reg models' list for this i k-means model into the all_log_models list.
          # log reg model (binary classifiers here) versus k-means model. 
          # instead of looking at the class membership, look at in-cluster 3-means clustering. 
          # beta weight associated with each log reg model.
          # A - B+C; B - A+C; C - A+B
          # binary: small training example.
          # in group and out of group. 
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

# Tweak the variables in this function to find the results you would like in cluster analysis.
model_list <- constructLogRegModelsFromClusters(df,saveModels = T,models_per_k = 100,cluster_min=1,
                                                working_directory="C:/Users/ashle/R/study1/June22",
                                                k_range=3, doLogReg=T,special=F)

# Tweak the variables in this function to find the results you would like in cluster analysis.
model_list.pc <- constructLogRegModelsFromClusters(df.pc,saveModels = T,models_per_k = 100,cluster_min=1,
                                                working_directory="C:/Users/ashle/R/study1/March17",
                                                k_range=3, doLogReg=T,special=T)
all_models <- model_list[[1]]
#Go through all the models and find models with same variance
all_residuals <- c()
# things you can do 
for(i in 1:length(all_models)){
  residualSquares <- all_models[[i]]$betweenss/all_models[[i]]$totss
  # for eac
  all_residuals <- c(all_residuals,residualSquares)
}
(table(all_residuals)/100)*100
# This function is used to find the optimal number of groups to minimize the sum of squares
# within groups (aka find the optimal number of groups k)
  wssplot <- function(groups, nc=15, seed=1234){
    # wss is the number of rows - 1 (participants) times the sum of the variance
    # of each column. For df, each column sum is 1, and there are 30-1 rows, so it becomes
    # wss <- 29*19 = 551
    wss <- (nrow(groups)-1)*sum(apply(groups,2,var))
    
    # Go from 2 to 15 clusters, find the within group sum of squares
    for (i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(groups, centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")}

wssplot(df)
# Use this code to predict your logistic regression model on the original dataset.
# if the prediction is unsuccessful at even predicting on itself (predict != y),
# then the model is extremely poor at predicting.
# predict(cvfit, newx = data[1:30,], s = "lambda.min", type = "class")

################### 3-D PCA Plotting ################### 
  
  # Do PCA on the data set using correlation matrix.
  pc <- princomp(df, cor=TRUE, scores=TRUE)
  myloadings <- loadings(pc)
  loadings.df <- data.frame(myloadings[1:19,1:19])
  
  # Save all the data-frames into an excel file
  wb = createWorkbook()

  sheet = createSheet(wb, paste0('Loadings'))
  addDataFrame(loadings.df,sheet=sheet,row.names=TRUE)
  
  saveWorkbook(wb, "data_loadings.xlsx")
  
  #Show 3D graphs
  
  library(rgl)
  library(pca3d)
  

  # Use the k-means models we created to produce 3-D PCA plots.
  all_models <- model_list[[1]]
  for(i in 1:length(all_models)){
    curr_model <- all_models[[i]]
    groupings <- as.factor(curr_model$cluster)
  
    plot3d(pc$scores[,1:3])
    pca3d(pc,group=groupings,show.ellipses=TRUE,
        ellipse.ci=0.75, show.plane=FALSE)

    snapshotPCA3d(file=paste0("pca3dPlot",i,'.png'))
  }
  
  # Varimax Rotated Principal Components
  # retaining 5 components 
  library(psych)
  fit <- principal(df, nfactors=2, rotate="varimax")
  fit # print results
  
  fitObliq <- principal(df, nfactors=2, rotate="oblimin")
  fitObliq # print results
  
  library(multicon)
  
  horn(df,sims = 100)
  
  qu# Euclidean distance function
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  all_models
  
  stable <- list()
  for(i in 1:length(all_models)){
    
  # Okay so we want to extract the centroids first in a given clustering
  myClustering <- all_models[[i]]
  myClustering$centers
  
  table(myClustering$cluster)
  
  clustering.stabilities <- c()
  
  # For each group for a given cluster model, 
  # we want the distance from the centroid - avg distance from all other centroids 
  #start with first cluster
  for(j in 1:nrow(df)){
    currentGroup <- df[j,]
    myAssignedCluster <- myClustering$cluster[j]
    
    distFromCentroid <- euc.dist(myClustering$centers[myAssignedCluster,],currentGroup)
    allOtherDistances <- c()
    for(k in 1:length(table(myClustering$cluster))){
      if (k != myAssignedCluster){
        otherCentroid <- euc.dist(currentGroup,myClustering$centers[k,])
        allOtherDistances <- c(allOtherDistances,otherCentroid)
      }
    }
    stability <- mean(allOtherDistances) - distFromCentroid
    clustering.stabilities <- c(clustering.stabilities,stability)
    
  }
  names(clustering.stabilities) <- groups$group
  newName <- paste0("Cluster",i)
  stable[[i]] <- clustering.stabilities
  }
  
  stable.df <- as.data.frame(stable[[1]])
  for(i in 2:length(stable)){
    nextName <- paste0('stable',i)
    stable.df[nextName] <- stable[[i]]
  }
  sort(rowMeans(stable.df))
  
  # Save all the data-frame into an excel file
  wb = createWorkbook()

  sheet = createSheet(wb, paste0('Cluster Stability'))
  addDataFrame(as.data.frame(sort(rowMeans(stable.df))),sheet=sheet,row.names=TRUE)
  
  saveWorkbook(wb, "cluster_stability.xlsx")
  ################### Classification with Holdout ################### 
  
  dfNoBio <- df[,-10]
  dfNoStab <- df[,-12]
  holdout_model_list <- constructLogRegModelsFromClusters(dfNoStab,saveModels = T,models_per_k = 10,cluster_min=3,
                                                  working_directory="~/r_files/bioAndStabHoldout",
                                                  k_range=4:5)
  
  
  ################### Get all models between_SS / total_SS ###################
  
  residualSS <- c()
  all_models <- model_list[[1]]
  for(i in 1:length(all_models)){
    residualSS <- c(residualSS,all_models[[i]]$betweenss/all_models[[i]]$totss)
  }
  
  # Get the clusterings of the 4 distinct clusters formed
  myClusterings <- all_models[[1]]$cluster
  myClusterings <- cbind(myClusterings, all_models[[19]]$cluster)
  table(myClusterings[,4])
  myClusterings <-  myClusterings[,-4]
  similarity <- data.frame(myClusterings)
  colnames(similarity) <- c('cluster1','cluster2','cluster3', 'cluster4')
  
  similarity$cluster3
  


  
########### Generate Clusters Silhouette Plot ############
  
library (vegan)
  
dis = vegdist(df, method="euclidean")
res = pam(df,3)
quartz()
sil = silhouette(res$cluster,dis)  

rownames(sil) <- groups$X
plot(sil, main = "Clusters Silhouette plot")

rownames(df) <- groups$X

clusplot(df,res$cluster,color=TRUE,shade=TRUE,labels=2, lines=0)

######### Generate Average Silhouette Width Plot ##############

library(purrr)

sil_width <- map_dbl(2:10,  function(k){
  model <- pam(df, k)
  model$silinfo$avg.width
})

sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

library("ggthemes")
library(ggplot2)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10) +
  ggtitle("Average Silhouette Width vs. Number of Clusters", subtitle = NULL) +
  xlab("Number of Clusters") + ylab("Average Silhouette Width")+
  geom_vline(xintercept=3, linetype="dotted") + 
  theme_minimal()


################################################################