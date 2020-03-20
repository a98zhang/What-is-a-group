# ---------------------------------------------------------------------------- #
# 0. Must Load: packages, original datasets, helper functions

#install.packages("glmnet", repos = "http://cran.us.r-project.org")
library(glmnet)
library(openxlsx)
library(readxl)
library(writexl)
library(dplyr)
library(selectiveInference)

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

# import data and models for study 1
groups<-read.csv('C:/Users/ashle/Job.Intern.Lab/CDL/GroupPaper/Group_Rating_Study_N481_scored.csv')
data<-as.matrix(groups[-1])
data <- scale(data)
allLabels <- c("fem", "stud","chris","dem","repub","rich",
               "poor","midc","workc","asn","wht","blck",
               "hisp","arab","gay","wom","men","pol","mot","samst",
               "spor","fam","hsfd","coup","cowk","chur","hobb","neigh",
               "vol","bsns")
rownames(data)<- allLabels

# ---------------------------------------------------------------------------- #
# 1. Label clusters in regression models

# inspecting model from previously generated models (1-100)
reg_models <- readRDS("C:/Users/ashle/R/study1/June22/log_reg_models.rds")
all_kmeans <- readRDS("C:/Users/ashle/R/study1/June22/k_cluster_models.rds")
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

    # labeled <- list(cluster=j, active_coef=coef@x[-1], groups=curr_groups, label=labelString)
    # curr_model_labeled[[j]] <- labeled
    # reg_models_labeled[[i]] <- curr_model_labeled
    
    all_coefs[,j] <- as.matrix(coef)[-1]
    colnames(all_coefs)[j] <- labelString
  }
  
  all_coef_list[[i]] <- all_coefs
}

# capture labeling results in .txt file
# capture.output(reg_models_labeled, file="C:/Users/ashle/R/study1/June22/log.txt")

# save data
saveRDS(all_coef_list, "C:/Users/ashle/R/study1/June22/reg_models_labeled.rds")

# re-creating all_models_and_clusters_workbook.xlsx with groups labeling
wb = createWorkbook()
for(i in 1:length(all_coef_list)){
  temp <- paste0('Model ', i)
  addWorksheet(wb=wb, sheetName=temp)
  writeData(wb=wb, sheet=temp, x=all_coef_list[[i]], rowNames=TRUE)
}
saveWorkbook(wb, "C:/Users/ashle/R/study1/June22/all_models_and_clusters_workbook_study1_labeled.xlsx")

# ---------------------------------------------------------------------------- #
# 2. Find solution that emerged most consistently


# load in all labeled models and find the most frequently appeared solution
reg_models_labeled <- readRDS("C:/Users/ashle/R/study1/June22/reg_models_labeled.rds")

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

# save data
saveRDS(reg_models_chosen, "C:/Users/ashle/R/study1/June22/reg_models_solution.rds")

# ---------------------------------------------------------------------------- #
# 3. Average the coefficients across iterations


# load in solution models and aggregate the coefficients
reg_models_chosen <- readRDS("C:/Users/ashle/R/study1/June22/reg_models_solution.rds")
reg_models_binded <- bind_rows(reg_models_chosen)
reg_models_binded$attribute <- rep(rownames(reg_models_chosen[[1]]), times = length(reg_models_chosen))
reg_models_binded %>%
  group_by(attribute) %>%
  summarise_each(funs(mean)) -> reg_models_mean

# save data
wb = createWorkbook()
addWorksheet(wb=wb, "summarise")
writeData(wb=wb, "summarise", reg_models_mean, rowNames=TRUE)
saveWorkbook(wb, "C:/Users/ashle/R/study1/June22/all_models_and_clusters_workbook_study1_summarised.xlsx")


# ---------------------------------------------------------------------------- #
# 4. Attempt predict using original data as test data
  
# generating a new model using the adopted k-means solution
all_kmeans <- readRDS("C:/Users/ashle/R/study1/June22/k_cluster_models.rds")
curr_kmean <- all_kmeans[[3]]
  
  # train logistic regression model for each cluster
  curr_logs <- list()
  predictions <- list()
  for (i in 1:length(curr_kmean$size)){
    y <- (curr_kmean$cluster == i)*1
    y <- as.factor(y)
    cvfit <- cv.glmnet(data, y, family="binomial", type.measure="class")
    curr_logs[[i]] <- cvfit
    newX = as.matrix(data)
    predictions[[i]] <- predict(cvfit, newx=newX, type="class", s="lambda.min")
  }
  
  # compare
  for (i in 1:length(curr_kmean$size)){
    y <- (curr_kmean$cluster == i)*1
    predicted_y <- predictions[[i]]
    if (!all(y==predicted_y)){
      print('---------')
      for (j in 1:length(y)){
        if (y[[j]] == 1){
          print(allLabels[[j]])
        }
        if (y[[j]]!= predicted_y[[j]]){
          print(paste0('erro in group: ', allLabels[[j]]))
          print(paste0('the actual: ', y[[j]]))
          print(paste0('the predicted:', predicted_y[[j]]))
        }
      }
    }
  }


  
# ---------------------------------------------------------------------------- #
# 5. Averaging to solve variability in cv-glmnet (added 03/18/2020)

#### instead of using the generated, we just here simulate 100 times (newly)
curr_kmean <- all_kmeans[[3]]

all_coefs <- data.frame(row.names=colnames(data))
all_coefs[, 1:3] <- 0
# for each cluster apply a binomial logistic regression model
for (j in 1:length(curr_kmean$size)){
  y <- (curr_kmean$cluster == j)*1
  y <- as.factor(y)
  
  # for each model perform 100 times simulation
  lambdas = NULL
  for (i in 1:100){
    cvfit <- cv.glmnet(data, y, family = "binomial", type.measure = "class")
    errors <- data.frame(cvfit$lambda, cvfit$cvm)
    lambdas <- rbind(lambdas, errors)
  }
  
  # take mean cvm for each lambda
  lambdas <- aggregate(lambdas[, 2], list(lambdas$cvfit.lambda), mean)
  
  # select the best one
  bestindex = which(lambdas[2]==min(lambdas[2]))[1]
  bestlambda = lambdas[bestindex,1]
  
  # and now run glmnet once more with it
  fit <- glmnet(data, y, family = "binomial", type.measure = "class", lambda=bestlambda)
  all_coefs[, j] <- fit$beta[, 1]
  
  # set the column name
  curr_groups <- groups[curr_kmean$cluster == j,][1]
  colnames(all_coefs)[j] <- labelsToString(curr_groups[,1])
}

# save in csv
write.csv(all_coefs,"C:/Users/ashle/R/study1/June22/simulated.csv", row.names = TRUE)