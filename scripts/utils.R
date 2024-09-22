# Create a function to detect similar formular (x + y and y + x)
is_same <- function(x, y){
  all_vect <- list()
  
  if (length(x) != length(y)) {
    error_mess <- paste("Impossible to compare size of", length(x), "for x and", length(y), "for y")
    stop(error_mess)
  }
  
  for (i in 1:length(x)) {
    
    spl_x <- strsplit(x[i], ""); spl_y <- strsplit(y[i], "")
    spl_x <- spl_x[[1]]; spl_y <- spl_y[[1]]
    if (length(spl_x) != length(spl_y)) {
      warning("Number of character in x and y differs")
    }
    suppressWarnings({
      is_same <- sort(spl_x) == sort(spl_y)
    })
    
    all_vect[[i]] <- all(is_same)
  }
  return(unlist(all_vect))
}



make_table <- function(models){
  
  # this function extracts the model data for a single model (row)
  extract_model_data <- function(model){
    c(summary(model)$ds$key,
      model$ddf$ds$aux$ddfobj$scale$formula,
      model$ddf$criterion,
      ddf.gof(model$ddf, qq=FALSE)$dsgof$CvM$p,
      summary(model)$ds$average.p,
      summary(model)$ds$average.p.se
    )
  }
  
  # applying that to all the models then putting it into a data.frame
  res <- as.data.frame(t(as.data.frame(lapply(models, extract_model_data))),
                       stringsAsFactors=FALSE)
  
  # making sure the correct columns are numeric
  res[,3] <- as.numeric(res[,3])
  res[,4] <- as.numeric(res[,4])
  res[,5] <- as.numeric(res[,5])
  res[,6] <- as.numeric(res[,6])
  
  # giving the columns names
  colnames(res) <- c("Key function", "Formula", "AIC", "Cramer-von Mises p-value",
                     "P_a", "se(P_a)")
  
  # creating a new column for the AIC difference to the best model
  res[["DeltaAIC"]] <- res$AIC - min(res$AIC, na.rm=TRUE)
  # ordering the model by AIC score
  res <- res[order(res$AIC),]
  
  # returning the data.frame
  return(res)
}