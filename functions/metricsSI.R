# Marc Edwards - BEACONs
# Mar 13 2017
# Function to calculate shape index and add column of values to network shp file
# adapted from metricsNetSI.R used in NWB
# dissolves polygons with common 'networks' values together, then applies SI formula relative to circle

metricsSI <- function(netPath, newCol){
  
  library(raster, lib.loc = "../packages/")
  library(rgeos, lib.loc = "../packages/")
  
  ### CHECKS ########
  # check shp exists and has networks column
  if(file.exists(netPath)){
    netShp <- shapefile(netPath)
  } else{
    stop(paste0("File does not exist: ", netPath))
  }
  
  ### RUN CALCS ########
  netList <- unique(netShp@data$networks)
  netShp@data[[newCol]] <- 0
  counter <- 1
  n <- length(netList)
  for (net in netList){
    if(counter %% 1000 == TRUE){print(paste0(counter, " of ", n))}
    netPolys <- netShp[netShp@data$networks==net,] # subset by rows in net
    netPolys@data$JoinID <- 1 # add join ID
    
    # fix topology
    if (!gIsValid(netPolys)) {
      #netPolys <- buffer(netPolys, 0, dissolve=F)
      netDiss <- gBuffer(netPolys, byid=F, width=0) # this dissolves into single feature so no need to run unionSpatialPolygons if using gBuffer method
    } else{
      netDiss <- unionSpatialPolygons(netPolys, netPolys[["JoinID"]]) # dissolve
    }
    
    netShp@data[[newCol]][netShp@data$networks==net] <- round(gLength(netDiss,byid=F)/(2*sqrt(pi*gArea(netDiss,byid=F))),1) # calculate SI
    
    counter<- counter + 1
  }
  
  ### SAVE ######
  # remove any extra SP_ID columns
  for(x in colnames(netShp@data)){ 
    if(grepl("SP_ID",x)==TRUE)
      netShp@data[[x]] <- NULL
  }
  
  #save shapefile
  writePolyShape(netShp, netPath)
}