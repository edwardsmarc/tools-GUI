# Marc Edwards - BEACONs
# Feb 15 2017
# Function to evaluate protected area shapefiles based on area and intactness targets

# Input shapefile is a polygon or set of polygons. These are intersected with the catchments and any catchments with intactness below the threshold value are removed. The resulting catchments are dissolved into contiguous polygons which are then used to calculate area, size ratio, and minimum intactness.

# Minimum intactness is calculated by intersecting the catchments with each polygon being evaluated, catchments with areas below x km2 are removed (this removes tiny sliver polygons around the edge) and the minimum intactness of the resulting catchments is extracted. 

# rm(list=ls())
# inputShp <- "../../../BEACONs/tools_GUI_case_study/input/PA_High_test.shp"
# catchmentsShp <- "../../../BEACONs/tools_GUI_case_study/input/PA_High_test_catchments_prj3.shp"
# intactnessCol <- "intact"
# areaTarget <- "4225"
# intactnessThreshold <- "0.8"
# outFile <- "../../../BEACONs/tools_GUI_case_study/input/PA_High_test_evaluated.shp"
# prjFile <- "../../../../Dropbox (BEACONs)/NWB/May2016/scripts/functions/nwb.prj"

evaluate_pa_shp <- function(inputShp, catchmentsShp, intactnessCol, areaTarget, intactnessThreshold, outFile, prjFile){
  
  # inputShp - PA shapefile. This can be a file with seperate features for each polygon to be evaluated or a single feature contianing multiple polygons.
  # catchmentsShp - catchments shapefile, covering all inputShp polygons
  # intactnessCol - intactness column in catchmentsShp - ranging between 0 - 1
  # areaTarget - area target PAs need to achieve - e.g. Minimum Dynamic Reserve value
  # intactnessThreshold - minimum intacntess of catchments permitted in benchmark - e.g. 0.8
  
  library(raster, lib.loc = "../packages/")
  library(maptools, lib.loc = "../packages/")
  library(sp, lib.loc = "../packages/")
  library(rgeos, lib.loc = "../packages/")
  
  # CHECKS
  ###################################
  
  # does inputShp exist - open
  if(file.exists(inputShp)){
    inShp <- shapefile(inputShp)
  } else{
    stop(paste0("File does not exist: ", inputShp))
  }
  
  # does catchments exists and have intactness column - open
  if(file.exists(catchmentsShp)){
    print("Loading catchments...")
    catchShp <- shapefile(catchmentsShp)
    if(!intactnessCol %in% names(catchShp)){
      stop(paste0("Column: '", intactnessCol, "' not present in file: ", catchmentsShp))
    }
  } else{
    stop(paste0("File does not exist: ", catchmentsShp))
  }
  
  # convert areaTarget and intacntessThreshold to numeric
  areaTarget <- as.numeric(areaTarget)
  intactnessThreshold <- as.numeric(intactnessThreshold)/100
  
  # Make output dir if it doesn't exist.
  # outFile_split <- strsplit(outFile, "/")[[1]]
  # outFile_dir <- paste0(outFile_split[1:(length(outFile_split)-1)], collapse="/")
  # if(!dir.exists(outFile_dir)){
  #   dir.create(outFile_dir)
  # }
  
  # Check if outFile has .shp.
  if(!grepl(".shp", outFile)){
    stop("outFile is not a '.shp'. Add '.shp' to outFile path")
  }
  
  
  # POLYGON PROCESSING
  ################################################
  
  # Fix topology if its not valid
  if (!gIsValid(inShp)) {
    inShp <- buffer(inShp, 0, dissolve=F)
  }
  
  # Disaggregate polygons - this is the same as exploding or multipartToSinglepart in ArcGIS
  print("Disaggregate...")
  inShp_dis <- sp::disaggregate(inShp)
  inShp_dis@data$dis_ID <- 1:nrow(inShp_dis) # add unique id for each row
  
  # intersect catchments and PAs
  print("Intersect with catchments...")
  inShp_int <- raster::intersect(catchShp, inShp_dis)
  # Fix topology if its not valid
  if (!gIsValid(inShp_int)) {
    inShp_int <- buffer(inShp_int, 0, dissolve=F)
  }
  
  # remove catchments where intacntess is less than threshold
  inShp_int_i <- inShp_int[inShp_int@data[[intactnessCol]] >= intactnessThreshold,]
  
  # Dissolve back together using PA id column
  print("Dissolve...")
  inShp_dslv <- raster::aggregate(inShp_int_i, by = "dis_ID")
  # Fix topology if its not valid
  if (!gIsValid(inShp_dslv)) {
    inShp_dslv <- buffer(inShp_dslv, 0, dissolve=F)
  }
  
  # Disaggregate again to get final patches
  print("Disaggregate again...")
  inShp_dis2 <- sp::disaggregate(inShp_dslv)
  inShp_dis2@data$dis_ID2 <- 1:nrow(inShp_dis2) # add unique id for each row
  # Fix topology if its not valid
  if (!gIsValid(inShp_dis2)) {
    inShp_dis2 <- buffer(inShp_dis2, 0, dissolve=F)
  }
  
  # delete sliver polygons
  # for(i in 1:length(inShp_dis2@polygons)){
  #   jj <- length(inShp_dis2@polygons[[i]]@Polygons)
  #   dList <- NULL
  #   for (j in 1:jj) { # makes list of polygons with area < 1
  #     if (inShp_dis2@polygons[[i]]@Polygons[[j]]@area < 1) {
  #       dList <- c(dList, j)
  #     }
  #   }
  #   if (length(dList) > 0) { # delete dList polygons
  #     counter <- 0
  #     for (d in dList) {
  #       inShp_dis2@polygons[[i]]@Polygons[[d-counter]] <- NULL
  #       counter = counter + 1
  #     }
  #   }
  # }
  
  shp <- inShp_dis2
  
  
  # ADDING COLUMNS
  ######################################################
  
  print("Calculate area, area target...")
  # Calculate area in km2
  shp@data$Area_km2 <- round(gArea(shp, byid = TRUE) / 1000000, 3)
  
  # filter out polygons smaller than 1km2
  shp <- shp[shp@data$Area_km2 >= 1,]
  
  # Add area target
  shp@data$areatarget <- areaTarget
  
  #####
  # calculate minimum intactness
  # intersect catchments and polygons, remove slivers, select full catchments of intersecting cathments, convert to centroids, intersect centrods that fall in polygons, calc min intact.
    # just intersecting leaves tiny catchments fragments that only just overlap polygon. Would have to set area limit for slivers to include. Instead, select catchments to include based on their centroid overlap. Can't use centroid overlap of full catchments dataset because some catchments that were filtered out based on intacntness threshold will get added back in (i.e. their centroids don't fall within the catchment polygon).
  
  print("Calculate minimum intacntess...")
  
  shp_int1 <- raster::intersect(catchShp, shp)
  shp_int1@data$Area_int <- gArea(shp_int1, byid = TRUE) / 1000000 # remove slivers <0.01km2
  shp_int1 <- shp_int1[shp_int1@data$Area_int >= 0.01,] # remove slivers <0.01km2
  catch_int <- catchShp[catchShp@data$CATCHNUM %in% shp_int1@data$CATCHNUM,] # select catchments that intersect
  cc1 <- gCentroid(catch_int, byid = TRUE) # make all catchments centroids
  cc1DF <- SpatialPointsDataFrame(cc1, catch_int@data) # join catchments data frame to centroids
  shp_int2 <- raster::intersect(cc1DF, shp)
  
  # calculate minimum intacntess catchment in each dis_ID2 group
  shp@data$min_intact <- 0
  for(gp in shp@data$dis_ID2){
    shp@data$min_intact[shp@data$dis_ID2 == gp] <- min(shp_int2@data[[intactnessCol]][shp_int2@data$dis_ID2 == gp])
  }
  #####
  
  # calculate size ratio - Area of polygon / areaTarget
  shp@data$sizeratio <- round(shp@data$Area_km2 / shp@data$areatarget,3)
  
  # SAVE
  ##################################
  
  print("Saving...")
  writePolyShape(shp, outFile)
  file.copy(prjFile, paste0(strsplit(outFile,".shp")[[1]], ".prj"), overwrite = TRUE)
}