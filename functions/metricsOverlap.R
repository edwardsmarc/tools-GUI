# Marc Edwards - BEACONs
# Mar 13 2017
# function to add overlap percentage column to networks shapefile
# Overlap is calculated between each network and the provided reference area
# adapted from the NWB function metricsNetOverlap()

metricsOverlap <- function(netPath, refPath, newCol){
  
  library(raster, lib.loc = "../packages/")
  library(rgeos, lib.loc = "../packages/")
  
  ### CHECKS ########
  # check shp exists and has networks column
  if(file.exists(netPath)){
    netShp <- shapefile(netPath)
  } else{
    stop(paste0("File does not exist: ", netPath))
  }
  
  # check rewf shp exists and has networks column
  if(file.exists(refPath)){
    refShp <- shapefile(refPath)
  } else{
    stop(paste0("File does not exist: ", refPath))
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
    
    # calculate proportion overlap as area of intersection / total network area
    netArea <- gArea(netDiss)
    overlap <- gIntersection(netDiss, refShp)
    if(is.null(overlap)){
      overlapArea <- 0
    } else{
      overlapArea <- gArea(overlap)
    }
    overlap_prop <- overlapArea / netArea
    
    netShp@data[[newCol]][netShp@data$networks==net] <- round(overlap_prop,3) 
    
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