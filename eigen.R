
library(png)
require(dplyr)
library(grid)

update.packages(ask = FALSE, checkBuilt = TRUE)

coeffecientSel=0.0


#This function generates and saves faces
showFace <- function(x, fileName){
  y <- t(apply(matrix(as.numeric(x), nrow=64, byrow=T), 2, rev)) 
  
  pathTemp <- paste("temp\\", fileName, ".png")
  
  # pathTemp2 <- paste(pathTemp, ".png")
  
  png(pathTemp, width=600, height=600)
  image(y,col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")
  dev.off()
  
}

# This is for Euclidean distance calculator
diffCalc <- function(x){
  sqrt(((x-coeffecientSel) %*% t(x-coeffecientSel)))
}

# This is the main method of the whole program
mainMethod <- function (df, splitPoint, ratioOfOffset){

  dataX <- data.frame(df)
  
  str(dataX, list.len = 5)
  
  #storing the first 40 faces
  par(mfrow=c(4, 10))
  par(mar=c(0.05, 0.05, 0.05, 0.05))

  for (i in 1:40) {
    newName <- paste("data-", i) 
    showFace(dataX[i, ], newName)
  }
  
  
  #labeling from 1 to 40 for 40 persons in the data
  dataY<- select(mutate(data.frame(rep(seq(1:40),each=10)), index = rowNumberr()), 2, label = 1) 
  str(dataY)
  
  #splitPointting training and test data
  set.seed(1234)
  sp <- splitPoint/10
  
  trainSample <- arrange(sample_n(group_by(dataY, label),sp), index)
  testSample <-  setdiff(dataY, trainSample)
  
  trainMat <-`rownames<-`(data.matrix(filter(dataX, rowNumberr() %in% trainSample[, "index", drop=TRUE])), trainSample[, "label", drop=TRUE])
  str(trainMat, list.len = 5)
  
  dataMatTest <-`rownames<-`(data.matrix(filter(dataX, rowNumberr() %in% testSample[, "index", drop=TRUE])), testSample[, "label", drop=TRUE])
  str(dataMatTest)
  
  #Average face
  avgFace <- colMeans(trainMat)
  par(mfrow=c(1,1))
  showFace(avgFace, "AverageFace")
  
  dataMatrxCenter <- scale(trainMat, center = TRUE, scale = FALSE)
  
  #PCA computations
  covMat <- t(dataMatrxCenter) %*% dataMatrxCenter / nrow(trainMat-1) #Covariance matrix
  eigenn <- eigen(covMat)
  eigenVec <- eigenn$vectors #Eigen vector
  eigenVal <- eigenn$values  #Eigen Value
  str(eigenVal)
  
  #SVD computation for Eigen faces result which is better than using PCA
  svd <- svd(dataMatrxCenter)
  eigenVec2 <- svd$v 
  str(eigenVec2)
  eigValueTwo <- svd$d^2/(ncol(dataMatrxCenter)-1) 
  str(eigValueTwo)
  
  #selection of eigen vectors/faces
  varProp <- eigValueTwo/sum(eigValueTwo) 
  varPropCum <- cumsum(eigValueTwo)/sum(eigValueTwo) 
  
  graphics.off()
  par(mfrow=c(1, 1))
  
  png("temp\\plot.png", width=600, height=600)
  
  plot(seq_along(varPropCum),varPropCum*100, xlab = "Eigenvalues", ylab = "Percentage of cumulative variance %", 
       main = "Percentage of cumulative variance in total variance")
  dev.off()
  
  sVar <- min(which(varPropCum > ratioOfOffset)) 
  sVec <-  eigenVec[, 1:sVar] #selected eigen vectors
  str(sVec)
  
  par(mfrow=c(4,10))
  par(mar=c(0.05, 0.05, 0.05, 0.05))
  
  
  for (i in 1:40) {
    newName <- paste("sVec-",i) 
    showFace(sVec[, i], newName)
  }
  
  
  #Coefficient for the training and test faces
  coeffefficientTrFace <- `rownames<-`(dataMatrxCenter %*% sVec, rownames(trainMat)) 
  str(coeffefficientTrFace)
  
  coeffefficientTestFace<- t(apply(dataMatTest, 1, function(x) x-avgFace)) %*% sVec 
  
  #Reconstructing faces with coefficient and eigenvector
  par(mfrow=c(1,2))
  par(mar=c(0.05, 0.05, 0.05, 0.05))
  showFace((trainMat[1, ]), "Train")
  showFace((coeffefficientTrFace[1, ] %*% t(sVec) + avgFace), "LastOne")
  
  bla <- ((400/100) * (100 - splitPoint))
  
  #Face recognition calculation for test faces
  results <-`colnames<-`(data.frame(matrix(NA, nrow = bla, ncol = 3)), c("Image labels", "Classified labels", "Correctly classified (1) / Incorrectly classified(0)"))
  
  for (i in 1:nrow(coeffefficientTestFace)) { 
    coeffecientSelYYY <- coeffefficientTestFace[i, , drop=FALSE]
    
    assign("coeffecientSel", coeffecientSelYYY, envir = .GlobalEnv)
 
    coefficientDifference <- apply(coeffefficientTrFace, 1, diffCalc)
    results[i, 1]  <- rownames(coeffefficientTestFace)[i]
    results[i, 2] <- rownames(coeffefficientTrFace)[which(min(coefficientDifference)==coefficientDifference)]
  }
  
  results[, 3] <- ifelse(results[, 2] == results[, 1], 1, 0)
  results[1:40, ]
  shareCor <- sum(results[, 3])/nrow(results)
  
  write.csv(results, file = "temp\\results.csv")
  
  accrcy <- paste("Accuracy: ", toString(shareCor * 100), "%") 
  
  return (accrcy)
}
