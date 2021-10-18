# GLUE approach

#-------------------------------------------------------------------------------
# 1.Draw a sample of points specified prior distribution (e.g., uniform distr.)
#-------------------------------------------------------------------------------
runifSampling <- function(nsample, xmin, xmax){

  # Random generator within [0,1] (matrix format)
  nparam <- length(xmin)
  
  for (i in 1:nsample){
    if (i == 1){
      output <- matrix(runif(nparam), nrow = 1)
    } else {
      output <- rbind(output, runif(nparam))
    }
  }
  
  # Parameter range conversion 
  for (i in 1:nparam){
    output[,i] <- xmin[i] + (xmax[i] - xmin[i]) * output[,i]
  }
  
  # Add number of simulation in the first column
  output <- cbind(c(1:nrow(output)), output)
  
  # Remove row name and column name
  colnames(output) <- NULL
  rownames(output) <- NULL
  
  return(output)
 
}

#-------------------------------------------------------------------------------
# 2.Compute the likelihood values for each parameter sample (Run model -> get e.g., NSE)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# 3. Normalize the likelihood values of the behavioral solutions (sum = 1)
#-------------------------------------------------------------------------------
normLik <- function(lik, threshold){
  
  output <- list()
  # Get index of behavioral simulations
  output$idx <- which(lik >= threshold)
  output$normLik <- lik[idx]/sum(lik[idx])
  
  return(output)
  
}

#-------------------------------------------------------------------------------
# 4. 95% GLUE prediction uncertainty, x is only behavioral simulation 
# Please see the original code in this link
# http://www.uncertain-future.org.uk/wp-content/uploads/2016/06/R-GLUE.zip
#-------------------------------------------------------------------------------
ecdf.pred <- function(perc,x,w){

  #perc = percentile
  #x = values
  #w = weights - sum to 1
  
  # trim x so only have values with positive weights
  x <- x[w > 0]
  w <- w[w > 0]
  
  # form the empirical cdf
  sort.x <- sort(x,index=TRUE)
  ecdf <- cumsum(w[sort.x$ix])

  # calculate the percentiles
  out <- rep(NA,length(perc))
  for(ii in 1:length(perc)){
    jj <- which.min(abs(ecdf - perc[ii]))
    flag <- TRUE
    while(flag == TRUE){
      if(perc[ii] <= 0.5){
        if(ecdf[jj] > perc[ii]){
          if (jj == 1) {
            flag <- FALSE
          } else {
            jj = jj - 1
          }         
        }else{
          flag <- FALSE
        }
      }else{
        if(ecdf[jj] < perc[ii]){
          if (jj == length(ecdf)){
            flag = FALSE
          } else {
            jj <- jj + 1
          }
        }else{
          flag <- FALSE
        }
      }
    }
    out[ii] <- sort.x$x[jj]
  }
  
  return(out)
}


