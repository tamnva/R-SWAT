#-------------------------------------------------------------------------------
# Multivariate linear regression for sensitivity analysis (LHS for parameter generation)
#-------------------------------------------------------------------------------
linearRegression <- function(objFunction, parameterValue, objCriteria){
  
  parameterValue <- as.data.frame(parameterValue)
  
  header <- strsplit(colnames(objFunction), split = "_")
  temp <- c()
  for (i in 1:length(header)){
    temp <- c(temp, header[[i]][1])
  }
  
  temp <- which(temp %in% objCriteria)
  
  if (length(temp) > 1) {
    parameterValue$V1 <- rowSums(objFunction[,temp])
  } else {
    parameterValue$V1 <- objFunction[,temp]
  }
  
  temp <- summary(lm(V1 ~ ., parameterValue))
  
  tValue <- as.numeric(temp$coefficients[,3])
  tValue <- tValue[2:length(tValue)]
  
  pValue <- as.numeric(temp$coefficients[,4])
  pvalue <- pValue[2:length(pValue)]
  
  result <- matrix(c(tValue, pvalue), byrow = FALSE, ncol = 2)
  colnames(result) <-  c('t-stat', 'p-value')
  
  rowName <- c()    
  for (i in 1:nrow(result)){
    rowName <- c(rowName, paste('Parameter_', i, sep = ''))
  }
  
  rownames(result) <- rowName
  
  return(result)
  
}

#-------------------------------------------------------------------------------	
# Parameter generation by Latin Hypercube Sampling (LHS)
#-------------------------------------------------------------------------------	
lhsRange <- function(nIter, paramRange){
  
  nParam <- nrow(paramRange)
  paramSampling <- randomLHS(nIter, nParam)
  
  for (i in 1:nParam){
    paramSampling[,i] <- paramRange[i,1] +  paramSampling[,i] * 
      (paramRange[i,2] - paramRange[i,1])
  }
  
  paramSampling <- cbind(c(1:nrow(paramSampling)), paramSampling)
  return(paramSampling)
}