# Marc Edwards - BEACONs
# Feb 8 2017
# criteria2catch - function to tabulate catchments shp with raster criteria values. Replicating addCriteria2Catch.py script that uses ArcGIS TabulateArea function 

criteria2catch <- function(catchmentsShp, rasterPath, cellsize, disaggregate.factor=1, criteria, chunksize, outFile="", prjFile){

# catchmentsShp - catchments shp that will have criteria values added to it. Must have CATCHNUM column.

# rasterPath - file path tp ascii raster with cell size in metres. Raster should display integer values relating to classes of data. Either categorical classes or ranges of continuous data.

# cellsize - cells size in metres for the criteria raster map.

# disaggregate.factor - factor to reduce cell size by when summing values in each catchment. Depending on cell size and the size of catchments, a simple tabulation of cell values inside each catchment may not be very accurate because cells could have large overlap with the catchments, i.e. the cells may not line up well with the catchments boundary. Having smaller cells gives more accurate values because cells will follow catchment boundaries more closely. Cells are selected within polygons if their centroid is inside the polygon. Disaggregate.factor applies a reduction of cell size based on the cell size of the input raster. If '1' is supplied (default) then the original cells are used to sum criteria values. If 10 is used, each cell is split into 10 in both the x and y directions. So a 250m resolution would change to a 25m resolution. This increases processing time so a balance neesd to be met between accuracy and processing. In testing using a 250m resolution raster and a factor of 10, it took ~24 hours to process 36,936 catchments (NWB ecoregion 15). This would need to be repeated for each criteria map.

# criteria - name of the criteris that will be added to the catchments columns. 

# chunksize - To speed up the processing and reduce memory requirements, the catchments are split into chunks and run one at a time. With a raster cell size of 250m and a factor disaggregate.factor of 10, a chunksize of 50 gave good processing time.

# outFile (optional) - location to save the updated catchments shapefile. If left blank, the new columns will be added to the catchmentsShp. Note that adding an outFile in the GUI that is the same as the catchmentsShp will not add the new columns. If new columns are to be added to the catchmentsShp then outFile must be left blank. 
  
#prjFile = path to the .prj file to be used.

  
  library(raster, lib.loc = "../packages/")
  library(maptools, lib.loc = "../packages/")
  
  # Assign outfile
  if(nchar(outFile) == 0){
    outFile <- catchmentsShp
  }
  
  # read shp
  if(file.exists(catchmentsShp)){
    print(paste0("loading shapefile: ", catchmentsShp))
    catchShp <- shapefile(catchmentsShp)
    if(!"CATCHNUM" %in% names(catchShp)){
      warning("No CATCHNUM column in catchShp!")
    }
  } else{
    stop(paste0("file does not exist: ", catchmentsShp))
  }

  # read raster
  if(file.exists(rasterPath)){
    ras <- raster(rasterPath)
    print(paste0("loading Raster: ", rasterPath))
    print(paste0("Raster resolution: ", paste0(res(ras),collapse=" x ")))
  } else{
    stop(paste0("file does not exist: ", rasterPath))
  }
  
  # Checks ##################
  # catchnum
  if(!"CATCHNUM" %in% names(catchShp)){
    stop("No CATCHNUM column in catchments shapefile")
  }
  # raster is integer
  if(!grepl("INT",dataType(ras))){
    stop("Raster should be of type integer. Current raster is of type: ", dataType(ras), ". See R raster package documentation for function dataType().")
  }
  # convert to numeric
  cellsize <- as.numeric(cellsize)
  disaggregate.factor <- as.numeric(disaggregate.factor)
  chunksize <- as.numeric(chunksize)
  
  cellarea <- (cellsize / disaggregate.factor) * (cellsize / disaggregate.factor)
  
  
  # disaggregate raster using disaggregate.factor  ##################
  rasD <- disaggregate(ras, disaggregate.factor, method = '')
  
  # get list of values in the raster and add columns to catchments
  for(val in unique(rasD)){
    #catchShp[[paste0(toupper(criteria), val)]] <- 0 # add columns of 0
    catchShp[[paste0(criteria, val)]] <- 0 # add columns of 0
  }
  
  # split catchments into blocks of chunksize and run each individually ##############################
  counter <- 1
  blocks <- ceiling(length(catchShp@data$CATCHNUM) / chunksize)
  for(block in 1:blocks){
    
    print(paste0("Processing chunk ", block, " of ", blocks))
    
    if(counter == 1){
      range <- counter:(counter*chunksize)
    } else if(counter > 1 & counter < max(blocks)){
      range <- (((counter-1) * chunksize) + 1):(counter * chunksize)
    } else if(counter == max(blocks)){
      range <- (((counter-1) * chunksize) + 1):length(catchShp@data$CATCHNUM)
    }
  
    catchShp2 <- catchShp[range,]

    counter <- counter + 1
    
    # extract vales
    ext <- extract(rasD, catchShp2)
    names(ext) <- catchShp2$CATCHNUM
    
    # get list of values
    vals <- unique(unlist(ext))
    vals <- vals[!is.na(vals)] # all values found in catchments - na's removed
    
    # sum values and add to catchments
    for(c in names(ext)){
        cvals <- ext[[c]] # raster values in catchment c
        cvals <- cvals[!is.na(cvals)]
      for(v in vals){
        area_m <- length(cvals[cvals==v]) * cellarea # calc area for raster value 'v' in catchment 'c'
        #catchShp[[paste0(toupper(criteria), v)]][catchShp@data$CATCHNUM == c] <- area_m # add to shp
        catchShp[[paste0(criteria, v)]][catchShp@data$CATCHNUM == c] <- area_m # add to shp
      }
    }
  }
  ##########################################################################################
  # remove any extra SP_ID columns
  for(x in colnames(catchShp@data)){ 
    if(grepl("SP_ID",x)==TRUE)
      catchShp@data[[x]] <- NULL
  }
  
  #save shapefile
  writePolyShape(catchShp, outFile)
  file.copy(prjFile, paste0(strsplit(outFile,".shp")[[1]], ".prj"), overwrite = TRUE)
}