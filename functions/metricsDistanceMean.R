# Marc Edwards - BEACONs
# Marc 7 2017
# function for adding dissimilarity euclidean distance and  mean values to shapefiles

# Euclidean distance formula for distance between vectors x and y is d(x,y) = sqrt(sum(xi-yi)^2).
# Euclidean distance is the square root of the sum of squared differences between corresponding elements of the two vectors
# In our case the dissimilarity values form the x vector and the y vector is point zero in a multidimensional plot.

dissimilarity_distance_mean <- function(netPath, colNames, calcDistance = TRUE, calcMean = TRUE){
  
  # netPath - shapefile to add distnance and mean to
  # colNames - columns in the shapefile to summarise
  # calcDistance - logical - add euclidean distance column
  # calcMean - logical - add mean_DS column
  
  # CHECKS ################
  # Check netPath exists
  if(file.exists(netPath)){
    shp <- shapefile(netPath)
  } else{
    stop(paste0("File does not exist: "), netPath)
  }
  
  # convert colName string to vector
  cols <- gsub(" ","",unlist(strsplit(colNames,","))) # converts string from GUI into vector. Must be separated by commas, spaces removed if present
  
  # Check colNames exist in shp
  if(!all(cols %in% names(shp))){
    stop(paste0("Columns: ", cols[!cols %in% names(shp)], ", not in file: ", netPath))
  }
  
  # CALCULATIONS ##########
  # Distance
  if(calcDistance == TRUE){
    shp@data$distance <- 0
    
    absSums <- rep(0, nrow(shp@data))
    for(col in cols){
      x <- shp@data[[col]]
      absSums <- absSums + (abs(x - 0)**2)
    }
    shp@data$distance <- round(absSums ** 0.5, 4)
  }
  
  if(calcMean == TRUE){
    shp@data$mean_DS <- 0
    m <- rep(0, nrow(shp@data))
    for(col in cols){
      x <- shp@data[[col]]
      m <- m + x
    }
    shp@data$mean_DS <- round(m / length(cols), 4)
  }
  
  # remove any extra SP_ID columns
  for(x in colnames(shp@data)){ 
    if(grepl("SP_ID",x)==TRUE)
      shp@data[[x]] <- NULL
  }
  
  # SAVE
  #######################################
  writePolyShape(shp, netPath)
  # doesn't save prj file because we are overwriting the original netShp so original prj file will be preserved.
}