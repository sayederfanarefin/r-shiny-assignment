
library(png)
require(dplyr)
library(grid)

update.packages(ask = FALSE, checkBuilt = TRUE)

coeffecientSel=0.0

showFace <- function(x, fileName){
  y <- t(apply(matrix(as.numeric(x), nrow=64, byrow=T), 2, rev)) 
  
  pathTemp <- paste("temp\\", fileName, ".png")
  
  png(pathTemp, width=600, height=600)
  image(y,col=grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")
  dev.off()
  
}

diffCalc <- function(x){
  sqrt(((x-coeffecientSel) %*% t(x-coeffecientSel)))
}

mainMethod <- function (df, splitPoint, ratioOfOffset){

  dataX <- data.frame(df)
  
  str(dataX, list.len = 5)
  
  par(mfrow=c(4, 10))
  par(mar=c(0.05, 0.05, 0.05, 0.05))

  for (i in 1:40) {
    newName <- paste("data-", i) 
    showFace(dataX[i, ], newName)
  }
  
  dataY<- select(mutate(data.frame(rep(seq(1:40),each=10)), index = rowNumberr()), 2, label = 1) 
  str(dataY)
  
  set.seed(1234)
  sp <- splitPoint/10
  
  trainSample <- arrange(sample_n(group_by(dataY, label),sp), index)
  testSample <-  setdiff(dataY, trainSample)
  
  trainMat <-`rownames<-`(data.matrix(filter(dataX, rowNumberr() %in% trainSample[, "index", drop=TRUE])), trainSample[, "label", drop=TRUE])
  str(trainMat, list.len = 5)
  
  dataMatTest <-`rownames<-`(data.matrix(filter(dataX, rowNumberr() %in% testSample[, "index", drop=TRUE])), testSample[, "label", drop=TRUE])
  str(dataMatTest)
  
  avgFace <- colMeans(trainMat)
  par(mfrow=c(1,1))
  showFace(avgFace, "AverageFace")
  
  dataMatrxCenter <- scale(trainMat, center = TRUE, scale = FALSE)
  
  covMat <- t(dataMatrxCenter) %*% dataMatrxCenter / nrow(trainMat-1)
  eigenn <- eigen(covMat)
  eigenVec <- eigenn$vectors 
  eigenVal <- eigenn$values  
  str(eigenVal)
  
  svd <- svd(dataMatrxCenter)
  eigenVec2 <- svd$v 
  str(eigenVec2)
  eigValueTwo <- svd$d^2/(ncol(dataMatrxCenter)-1) 
  str(eigValueTwo)
  
  varProp <- eigValueTwo/sum(eigValueTwo) 
  varPropCum <- cumsum(eigValueTwo)/sum(eigValueTwo) 
  
  graphics.off()
  par(mfrow=c(1, 1))
  
  png("temp\\plot.png", width=600, height=600)
  
  plot(seq_along(varPropCum),varPropCum*100, xlab = "Eigenvalues", ylab = "Percentage of cumulative variance %", 
       main = "Percentage of cumulative variance in total variance")
  dev.off()
  
  sVar <- min(which(varPropCum > ratioOfOffset)) 
  sVec <-  eigenVec[, 1:sVar] 
  str(sVec)
  
  par(mfrow=c(4,10))
  par(mar=c(0.05, 0.05, 0.05, 0.05))
  
  
  for (i in 1:40) {
    newName <- paste("sVec-",i) 
    showFace(sVec[, i], newName)
  }
  
  coeffefficientTrFace <- `rownames<-`(dataMatrxCenter %*% sVec, rownames(trainMat)) 
  str(coeffefficientTrFace)
  
  coeffefficientTestFace<- t(apply(dataMatTest, 1, function(x) x-avgFace)) %*% sVec 
  
  par(mfrow=c(1,2))
  par(mar=c(0.05, 0.05, 0.05, 0.05))
  showFace((trainMat[1, ]), "Train")
  showFace((coeffefficientTrFace[1, ] %*% t(sVec) + avgFace), "LastOne")
  
  variab <- ((400/100) * (100 - splitPoint))
  
  results <-`colnames<-`(data.frame(matrix(NA, nrow = variab, ncol = 3)), c("Image labels", "Classified labels", "Correctly classified (1) / Incorrectly classified(0)"))
  
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
