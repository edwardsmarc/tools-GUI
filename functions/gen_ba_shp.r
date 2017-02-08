# Marc Edwards - BEACONs
# Feb 7 2017
# function to create benchmark shapefiles for networking tools - GUI version

gen_ba_shp <- function(buildCsv, catchmentsShp, outFile, prjFile, shp.paths=""){
  
  library(raster)
  library(rgeos)
  library(maptools)
  
  #############################################################################################
  # Check that input files exist and have correct attributes
  if(file.exists(buildCsv)){
    print(paste0("loading csv: ", buildCsv))
    fi <- readLines(buildCsv)[-1]
    if(length(fi)==0){
      stop("No benchmarks in buildCsv!")
    }
  } else{
    stop(paste0("file does not exist: ", buildCsv))
  }
  
  if(file.exists(catchmentsShp)){
    print(paste0("loading shapefile: ", catchmentsShp))
    catchShp <- shapefile(catchmentsShp)
    if(!"CATCHNUM" %in% names(catchShp)){
      stop("No CATCHNUM column in catchShp!")
    }
  } else{
    stop(paste0("file does not exist: ", catchmentsShp))
  }
  
  # Make output dir if it doesn't exist.
  outFile_split <- strsplit(outFile, "/")[[1]]
  outFile_dir <- paste0(outFile_split[1:(length(outFile_split)-1)], collapse="/")
  if(!dir.exists(outFile_dir)){
    dir.create(outFile_dir)
  }
  
  # Check if outFile has .shp.
  if(!grepl(".shp", outFile)){
    stop("outFile is not a '.shp'. Add '.shp' to outFile path")
  }
  
  #############################################################################################
  # Generate shp files
  
  y <- strsplit(fi,",")
  PBnames <- sapply(y,function(x)x[1]) # extract PB_xxx names i.e., first element of each list
  
  # remove all unecessary columns from input file based on Builder version
  res <- lapply(y,function(x)as.integer(x[-c(1:9)]))
  names(res) <- PBnames # assign PB_xxx names to the list of catchments
  
  # add empty PB attribute to catchShp - this will be populated with PB values when each benchmarks catchments are extracted.
  if (!"PB" %in% names(catchShp)) {
    catchShp@data$PB <- ""
  }
  
  if(length(res) > 0) {
    
    # Loop through each benchmark
    sumRows <- 0
    for (i in 1:length(res)) {
      
      catch <- catchShp[catchShp@data$CATCHNUM %in% unlist(res[i]),]
      catch@data$PB <- PBnames[i]
      
      # dissolve catchments
      row.names(catch) <- row.names(catch@data)
      catch <- spChFIDs(catch, row.names(catch))
      
      catch <- gUnaryUnion(catch, id=catch@data$PB)
      #aggregate(catch,dissolve=TRUE)
      #unionSpatialPolygons(catch, IDs=catch@data$PB)
      #catch <- gBuffer(catch, byid=F, width=0) # this dissolves into single feature so no need to run unionSpatialPolygons if using gBuffer method
      
      # delete sliver polygons
      jj <- length(catch@polygons[[1]]@Polygons)
      dList <- NULL
      for (j in 1:jj) { # makes list of polygons with area < 1
        if (catch@polygons[[1]]@Polygons[[j]]@area < 1) {
          dList <- c(dList, j)
        }
      }
      if (length(dList) > 0) { # delete dList polygons
        counter = 0
        for (d in dList) {
          catch@polygons[[1]]@Polygons[[d-counter]] <- NULL
          counter = counter + 1
        }
      }
      
      # add PB name
      row.names(catch) <- as.character(1:length(catch))
      lu <- as.data.frame(PBnames[i])
      colnames(lu) <- "PB"
      catch <- SpatialPolygonsDataFrame(catch, lu)
      
      if (sumRows==0) { # add new dissolved benchmark to pbShp
        pbShp <- spChFIDs(catch, as.character(1:nrow(catch)))
      } else {
        catch1 <- spChFIDs(catch, as.character((sumRows+1):(sumRows+nrow(catch))))
        pbShp <- spRbind(pbShp, catch1)
      }
      sumRows <- sumRows + nrow(catch)
      
      if (i %% 100 == 0) {
        flush.console()
        cat("    ...",i,"/",length(res),"\n")
      }
    }
    ################################################################################
    
    # add shapefiles if provided
    if(any(nchar(shp.paths) > 0)){
      for(shpDir in shp.paths){
        
        print(paste0("adding additional shapefile: ", shpDir))
        
        # check path has .shp
        if(!grepl(".shp", shpDir)){ 
          stop("shp.paths file does not have '.shp'. Add '.shp' to file path")
        }
        
        shp <- shapefile(shpDir) # open
        
        # Check PB column is present
        if(!"PB" %in% names(shp)){
          stop(paste0("No PB column in ", shpDir))
        }
        shp@data <- shp@data["PB"] # subset to just PB column
        row.names(shp) <- as.character((nrow(pbShp)+1):(nrow(pbShp)+nrow(shp))) # update row names
        shp <- spChFIDs(shp, row.names(shp))
        pbShp <- spRbind(pbShp, shp) # append to pbShp
      }
    }
    
    # remove any extra SP_ID columns
    for(x in colnames(pbShp@data)){ 
      if(grepl("SP_ID",x)==TRUE)
        pbShp@data[[x]] <- NULL
    }
    
    writePolyShape(pbShp, outFile)
    file.copy(prjFile, paste0(strsplit(outFile,".shp")[[1]], ".prj"), overwrite = TRUE)
  }
}