
require(dplyr)
library(grid)
library(png)


update.packages(ask = FALSE, checkBuilt = TRUE)

#####glbal variable
coeff_tst_sel=0.0



###############################################################################################################################
#Function for showing face images
showFace <- function(x){
  y <- t(apply(matrix(as.numeric(x), nrow=64, byrow=T), 2, rev)) 
  
  png("" width=400, height=400)
  image(y,col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")
  dev.off()
  
    
}



###############################################################################################################################
#Function for calculating the Euclidean distance between two vectors
eucled_diff <- function(x){
  sqrt(((x-coeff_tst_sel) %*% t(x-coeff_tst_sel)))
}


###############################################################################################################################
#loading data


mainMethod <- function (dataFrameInput){
  
  dataX <-  data.frame(dataFrameInput)
  
  str(dataX, list.len = 5)
  
  #Displaying first 40 face images
  par(mfrow=c(4, 10))
  par(mar=c(0.05, 0.05, 0.05, 0.05))
  # for (i in 1:40) {
  #   showFace(dataX[i, ])
  # }
  
  #labeling from 1 to 40 for 40 persons in the data
  dataY<- select(mutate(data.frame(rep(seq(1:40),each=10)), index = row_number()), 2, label = 1) 
  str(dataY)
  
  #splitting training and test data
  set.seed(1234)
  trainSample <- arrange(sample_n(group_by(dataY, label),8), index)
  testSample <-  setdiff(dataY, trainSample)
  
  train_dataMatrx <-`rownames<-`(data.matrix(filter(dataX, row_number() %in% trainSample[, "index", drop=TRUE])), trainSample[, "label", drop=TRUE])
  str(train_dataMatrx, list.len = 5)
  
  test_DataMatrx <-`rownames<-`(data.matrix(filter(dataX, row_number() %in% testSample[, "index", drop=TRUE])), testSample[, "label", drop=TRUE])
  str(test_DataMatrx)
  
  #Average face
  avgFace <- colMeans(train_dataMatrx)
  par(mfrow=c(1,1))
  showFace(avgFace)
  
  dataMatrxCenter <- scale(train_dataMatrx, center = TRUE, scale = FALSE)
  
  #PCA computations
  covMat <- t(dataMatrxCenter) %*% dataMatrxCenter / nrow(train_dataMatrx-1) #Covariance matrix
  eigenn <- eigen(covMat)
  eigenVec <- eigenn$vectors #Eigen vector
  eigenVal <- eigenn$values  #Eigen Value
  str(eigenVal)
  
  #SVD computation for Eigen faces result which is better than using PCA
  svd <- svd(dataMatrxCenter)
  eigenVec2 <- svd$v 
  str(eigenVec2)
  eigenVal2 <- svd$d^2/(ncol(dataMatrxCenter)-1) 
  str(eigenVal2)
  
  #selection of eigen vectors/faces
  variance_Proportion <- eigenVal2/sum(eigenVal2) 
  var_Prop_Cumulative <- cumsum(eigenVal2)/sum(eigenVal2) 
  
  graphics.off()
  par(mfrow=c(1, 1))
  plot(seq_along(var_Prop_Cumulative),var_Prop_Cumulative*100, xlab = "Eigenvalues", ylab = "Percentage of cumulative variance %", 
       main = "Percentage of cumulative variance in total variance")
  
  sel_var <- min(which(var_Prop_Cumulative > 0.95)) 
  sel_vec <-  eigenVec[, 1:sel_var] #selected eigen vectors
  str(sel_vec)
  
  par(mfrow=c(4,10))
  par(mar=c(0.05, 0.05, 0.05, 0.05))
  for (i in 1:40) {
    showFace(sel_vec[, i])
  }
  
  #Coefficient for the training and test faces
  coeff_tr_face <- `rownames<-`(dataMatrxCenter %*% sel_vec, rownames(train_dataMatrx)) 
  str(coeff_tr_face)
  
  coeff_tst_face<- t(apply(test_DataMatrx, 1, function(x) x-avgFace)) %*% sel_vec 
  
  #Reconstructing faces with coefficient and eigenvector
  par(mfrow=c(1,2))
  par(mar=c(0.05, 0.05, 0.05, 0.05))
  showFace((train_dataMatrx[1, ]))
  showFace((coeff_tr_face[1, ] %*% t(sel_vec) + avgFace))
  
  
  #Face recognition calculation for test faces
  results <-`colnames<-`(data.frame(matrix(NA, nrow = 80, ncol = 3)), c("Image labels", "Classified labels",
                                                                        "Correctly classified (1) / Incorrectly classified(0)"))
  
  for (i in 1:nrow(coeff_tst_face)) { 
    coeff_tst_sel_yy <- coeff_tst_face[i, , drop=FALSE]
    
    assign("coeff_tst_sel", coeff_tst_sel_yy, envir = .GlobalEnv)
    # print (coeff_tst_sel)
    
    
    diff_coeff <- apply(coeff_tr_face, 1, eucled_diff)
    results[i, 1]  <- rownames(coeff_tst_face)[i]
    results[i, 2] <- rownames(coeff_tr_face)[which(min(diff_coeff)==diff_coeff)]
  }
  
  results[, 3] <- ifelse(results[, 2] == results[, 1], 1, 0)
  results[1:40, ]
  (shareCor <- sum(results[, 3])/nrow(results))
}
