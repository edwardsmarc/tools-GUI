# Marc Edwards - BEACONS
# Feb 7 2017
# New function based on evalTargets_NWB.R to generate target tables based on MDR representation. Proportion of each class in the ecoregion is multiplied by the MDR value to get target.
# based on evalTargets_NWB, simplified by removing protected area values from tables. Also removes range and type columns that showed raster value descriptors. Users should now refer to their raster value class breaks in a separate table.
  
gen_target_table <- function(mdrVal, ecoRast, ecoBnd, outFile) {
  
  library(raster)

  # Checks 
  ############################
  # ecoRast exists
  if(file.exists(ecoRast)){
    ecoR <- raster(ecoRast)
  } else{
    stop(paste0("File does not exist: ", ecoRast))
  }
  
  # ecoBnd exists
  if(file.exists(ecoBnd)){
    bndR <- raster(ecoBnd)
  } else{
    stop(paste0("File does not exist: ", ecoBnd))
  }
  
  # ecoRast and ecoBnd have same cell size
  if(!res(ecoR)[1] == res(bndR)[1]){
    stop("Raster files have different cell sizes!")
  }
  
  # MDR value provided - make sure numeric
  if(nchar(mdrVal) > 0){
    mdr <- as.numeric(mdrVal)
  }
  
  # Calculate ecoregion area using eco boundary
  bndR[bndR == 0] <- NA # If raster has zero values filling the extent, convert them to NA 
  cellsize <- res(bndR)[1]
  cellarea <- cellsize * cellsize / 1000000
  bnd.area <- sum(table(values(bndR))) * cellarea
  
  # get all non-NA values from reclassified ecoregion grid
  ecoR[ecoR == 0] <- NA # convert zeros to NA
  v.ecoR <- values(ecoR)
  v.ecoR <- v.ecoR[!is.na(v.ecoR)]
  
  # convert raster values to data frame
  nc <- data.frame(table(v.ecoR))
  names(nc) <- c("class","bnd_km2")
  nc$class <- as.numeric(levels(nc$class))
  nc$bnd_km2[is.na(nc$bnd_km2)] <- 0
  nc$bnd_km2 <- round(nc$bnd_km2 * cellarea, 3)
  
  # calculate ecoregion targets
  nc$bnd_prop <- round(nc$bnd_km2/bnd.area,4) # bnd_prop
  nc$target_km2 <- round(nc$bnd_prop*mdr,3) # target_km2 (MDR-based target)
  nc <- nc[,c("class","target_km2","bnd_km2","bnd_prop")]
  
  # create summary table and write results
  write.csv(nc,outFile,row.names=F,quote=T)
  
}