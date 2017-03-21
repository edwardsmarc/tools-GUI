# Marc Edwards - BEACONS
# March 9 2017
# function to sum column values

metricsColumnSum <- function(netPath, colNames, newCol){
  
  library(raster, lib.loc = "../packages/")
  
  ### CHECKS ########
  
  # check netShp exists and is shp
  if(!grepl(".shp$", netPath)){
    stop(paste0("File should end in '.shp': ", netPath))
  }
  if(file.exists(netPath)){
    netShp <- shapefile(netPath)
  } else{
    stop(paste0("File does not exist: ", netPath))
  }
  
  # convert colName string to vector
  cols <- gsub(" ","",unlist(strsplit(colNames,","))) # converts string from GUI into vector. Must be separated by commas, spaces removed if present
  
  # Check colNames exist in shp
  if(!all(cols %in% names(netShp))){
    stop(paste0("Columns: ", cols[!cols %in% names(netShp)], ", not in file: ", netPath))
  }
  
  sumV <- rep(0,nrow(netShp@data))
  for(col in cols){
    sumV <- sumV + netShp@data[[col]]
  }
  netShp@data[[newCol]] <- sumV
  
  # remove any extra SP_ID columns
  for(x in colnames(netShp@data)){ 
    if(grepl("SP_ID",x)==TRUE)
      netShp@data[[x]] <- NULL
  }
  
  #save shapefile
  writePolyShape(netShp, netPath)
}