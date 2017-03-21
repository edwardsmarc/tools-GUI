# Marc Edwards - BEACONs
# March 15 2017
# Function for GUI to generate upstream or downstream areas for networks of benchmarks
  # Use builder output to get upstream/downstream catchments for each benchmark using the COLUMNS files
  # Argument for additional upstream/downstream catchments table. Adds upstream/downstream catchnum list for benchmarks not in builder output. Alternatively, if multiple upstream/downstream files from builder, user could merge them manually into a single csv
  # Build full upstream/downstream area for network
  # Optional check box - Use builder ranker file and optional additional benchmark catchments table to remove any catchments in the network from the upstream/downstream area.
  # Optional check box - DropCatch argument should drop catchments from all networks processed. Allows existing PAs for example to be left out of upstream/downstream calcs.
  # Results saved to colName with suffixes of "km2" and "AWI" - users should limit total characters in column names to 10 characters if using ArcGIS to visulize shapefiles

metricsHydro <- function(netPath, idColNet, catchDbfPath, intactCol, areaCol, newCol, upCatchPath="", upCatchAddPath="", buildCsvPath="", buildCsvAddPath="", dropCatchPath="", savePath="", catchShpPath="", prjFile=""){
   
  # netPath - file path to networks shapefile
  # idColNet - column in netPath identifying networks
  # upCatchPath (optional) - upstream/downstream csv from builder output, this or upCatchAddPath must be provided
  # upCatchAddPath (optional) - additional upstream/downstream catchments for benchmarks not in upCatchPath, this or upCatchPath must be provided. Table with benchmark names as column names and rows of CATCHNUMs.
  # catchDbfPath - catchments DBF file path
  # intactCol - intactness column in catchDbf
  # areaCol - area column in catchDbf
  # savePath (optional) - directory to save upstream/downstream polygons into. If provided, polygons will be saved
  # catchShpPath (optional) - catchments shp - only needed if saveDir provided
  # buildCsvPath (optional) - builder ranker file detailing catchments in each benchmark. If provided, catchments in the network are removed from upstream/downstream area calcs
  # buildCsvAddPath (optional) - additional benchmark catchments for benchmarks not in buildCsv - Table with benchmark names as column names and rows of CATCHNUMs.
  # dropCatchPath (optional) - catchments to be dropped from all calculations - e.g. existing PA network catchments. Csv table with CATCHNUM column listing catchments to be dropped
  # newCol - name of column to save output - will have suffixes added for "km2" (upstream/downstream area) and "AWI" (upstream/downstream area weighted intactness)
  
  library(foreign, lib.loc = "../packages/")
  library(maptools, lib.loc = "../packages/")
  library(rgeos, lib.loc = "../packages/")
  library(raster, lib.loc = "../packages/")
  
  
  ### CHECKS ##############################################################################
  
  print("Loading data...")
  
  # Check netPath is shp, exists and has idColNet column
  if(!grepl(".shp$", netPath)){
    stop(paste0("File path should end in .shp: ", netPath))
  }
  if(file.exists(netPath)){
    netShp <- shapefile(netPath)
    if(!idColNet %in% names(netShp)){
      stop(paste0("No ", idColNet, " column in file: ", netPath))
    }
  } else{
    stop(paste0("File does not exist: "), netPath)
  }
  
  # if provided, check upCatchPath exists and is csv
  if(nchar(upCatchPath) > 0){
    if(!grepl(".csv$", upCatchPath)){
      stop(paste0("File path should end in .csv: ", upCatchPath))
    }
    if(file.exists(upCatchPath)){
      upCatch <- read.csv(upCatchPath)
    } else{
      stop(paste0("File does not exist: "), upCatchPath)
    }
  }
  
  # if provided, check upCatchAddPath exists and is csv
  if(nchar(upCatchAddPath) > 0){
    if(!grepl(".csv$", upCatchAddPath)){
      stop(paste0("File path should end in .csv: ", upCatchAddPath))
    }
    if(file.exists(upCatchAddPath)){
      upCatchAdd <- read.csv(upCatchAddPath)
    } else{
      stop(paste0("File does not exist: "), upCatchAddPath)
    }
  }
  
  # check at least one of upcatchPath or upCatchAddPath provided
  if(nchar(upCatchPath) == 0 & nchar(upCatchAddPath) == 0){
    stop("One of upstream/downstream file options must be provided")
  }
  
  # check catchDbf exists and is dbf, and has intactCol and areaCol columns
  if(nchar(catchDbfPath) > 0){
    if(!grepl(".dbf$", catchDbfPath)){
      stop(paste0("File path should end in .dbf: ", catchDbfPath))
    }
    if(file.exists(catchDbfPath)){
      catchDbf <- read.dbf(catchDbfPath)
      if(!intactCol %in% names(catchDbf)){
        stop(paste0("Column: ", intactCol, " not in file: ", catchDbfPath))
      }
      if(!areaCol %in% names(catchDbf)){
        stop(paste0("Column: ", areaCol, " not in file: ", catchDbfPath))
      }
    } else{
      stop(paste0("File does not exist: "), catchDbfPath)
    }
  }
  
  # if provided, check savePath is shp
  if(nchar(savePath) > 0){
    if(!grepl(".shp$", savePath)){
      stop(paste0("File path should end in .shp: ", savePath))
    }
  }
  
  # Check prjFile exists
  if(nchar(prjFile) > 0){
    if(!file.exists(prjFile)){
      stop(paste0("File does not exist: ", prjFile))
    }
  }
  
  # if provided, check catchShpPath exists and is shp
  if(nchar(catchShpPath) > 0){
    if(!grepl(".shp$", catchShpPath)){
      stop(paste0("File path should end in .shp: ", catchShpPath))
    }
    if(file.exists(catchShpPath)){
      catchShp <- shapefile(catchShpPath)
      if(!"CATCHNUM" %in% names(catchShp)){
        stop(paste0("No CATCHNUM column in file: ", catchShpPath))
      }
    } else{
      stop(paste0("File does not exist: "), catchShpPath)
    }
  }
  
  # if provided, check buildCsvPath exists and is csv
  if(nchar(buildCsvPath) > 0){
    if(!grepl(".csv$", buildCsvPath)){
      stop(paste0("File path should end in .csv: ", buildCsvPath))
    }
    if(file.exists(buildCsvPath)){
      buildCsv <- readLines(buildCsvPath)[-1]
      if(length(buildCsv)==0){
        warning("No benchmarks in buildCsv!")
      }
    } else{
      stop(paste0("File does not exist: "), buildCsvPath)
    }
  }
  
  # if provided, check buildCsvAdd Path exists and is csv
  if(nchar(buildCsvAddPath) > 0){
    if(!grepl(".csv$", buildCsvAddPath)){
      stop(paste0("File path should end in .csv: ", buildCsvAddPath))
    }
    if(file.exists(buildCsvAddPath)){
      buildCsvAdd <- read.csv(buildCsvAddPath)
    } else{
      stop(paste0("File does not exist: "), buildCsvAddPath)
    }
  }
  
  # if provided, check dropCatch exists and is csv and has "CATCHNUM" column
  # check catchDbf exists and is dbf, and has intactCol and areaCol columns
  if(nchar(dropCatchPath) > 0){
    if(!grepl(".csv$", dropCatchPath)){
      stop(paste0("File path should end in .csv: ", dropCatchPath))
    }
    if(file.exists(dropCatchPath)){
      dropCatch <- read.csv(dropCatchPath)
      if(!"CATCHNUM" %in% names(dropCatch)){
        stop(paste0("No CATCHNUM column in file: ", dropCatchPath))
      }
    } else{
      stop(paste0("File does not exist: "), dropCatchPath)
    }
  }
  
  
  ### MAKE UPSTREAM AND BENCHMARK CATCHMENT LISTS AND ADDITIONAL LISTS #################################################################
  
  # make list of all upstream/downstream catchments in upCatch
  upCatchments <- list()
  if(nchar(upCatchPath) > 0){
    upCatchments <- lapply(upCatch[!names(upCatch) %in% "OID"], function(x) as.integer(unlist(x[!is.na(x)])))
  }
  
  # make list of all additional upstream/downstream catchments in upCatch
  upCatchmentsAdd <- list()
  if(nchar(upCatchAddPath) > 0){
    upCatchmentsAdd <- lapply(upCatchAdd, function(x) unlist(x[!is.na(x)]))
  }
  
  # make list of all benchmarks in buildCsv and their catchments
  baCatchments <- list()
  if(nchar(buildCsvPath) > 0){
    y <- strsplit(buildCsv,",")
    PBnames <- sapply(y,function(x)x[1]) # extract PB_xxx names i.e., first element of each list
    baCatchments <- lapply(y,function(x)as.integer(x[-c(1:9)])) # remove unecessary columns
    names(baCatchments) <- PBnames # assign PB_xxx names to the list of catchments
  }
  
  # make list of additional benchmark catchments if provided
  baCatchmentsAdd <- list()
  if(nchar(buildCsvAddPath) > 0){
    baCatchmentsAdd <- lapply(buildCsvAdd, function(x) unlist(x[!is.na(x)]))
  }
  
  # make list of drop catchments if provided
  dropCatchments <- c()
  if(nchar(dropCatchPath) > 0){
    dropCatchments <- unique(dropCatch$CATCHNUM)
  }
  
  
  ### PROCESS NETWORKS #####################################################################################################
  
  netList <- unique(netShp@data[[idColNet]])
  
  counter <- 1
  saveCount <- 1
  for(net in netList){
    
    if(counter == 1 | counter %% 50 == 0){print(paste0("Processing ", counter, " of ", length(netList)))}
    
    # make list of BA names from networks names
    baList <- c()
    splits <- strsplit(net, split="_")[[1]]
    for(i in 1:length(splits)){
      if((i + 1) %% 2 == 0){
        baList <- c(baList, paste0(splits[i], "_", splits[i+1]))
      }
    }
    
    # Collect upstream/downstream catchments for each benchmark in network from upCatchments and upCatchmentsAdd
    upCatchList <- list()
    for(b in baList){
      # Error if a benchmark is in both lists but catchments don't match
      if(b %in% names(upCatchments) & b %in% names(upCatchmentsAdd)){
        if(!identical(upCatchments[[b]], upCatchmentsAdd[[b]])){
          stop(paste0("Benchmark in both upstream/downstream lists but catchments don't match: ", b))
        }
      }
      if(b %in% names(upCatchments)){
        upCatchList[[b]] <- upCatchments[[b]]
      } else if(b %in% names(upCatchmentsAdd)){
        upCatchList[[b]] <- upCatchmentsAdd[[b]]
      }
    }
    
    # Remove benchmark catchments if provided
    # Collect benchmark catchment list for each benchmark in network from baCatchments and baCatchmentsAdd - optional
    baCatchList <- list()
    if(nchar(buildCsvPath) > 0 | nchar(buildCsvAddPath) > 0){
      for(b in baList){
        # Error if a benchmark is in both lists but catchments don't match
        if(b %in% names(baCatchments) & b %in% names(baCatchmentsAdd)){
          if(!identical(baCatchments[[b]], baCatchmentsAdd[[b]])){
            stop(paste0("Benchmark in both catchment lists but catchments don't match: ", b))
          }
        }
        if(b %in% names(baCatchments)){
          baCatchList[[b]] <- baCatchments[[b]]
        } else if(b %in% names(baCatchmentsAdd)){
          baCatchList[[b]] <- baCatchmentsAdd[[b]]
        }
      }
    }
    
    # Get final upstream/downstream list by removing any dropCatchments and benchmark catchments from the upstream/downstream list
    upFinal <- unique(as.vector(unlist(upCatchList)))
    upFinal <- upFinal[!upFinal %in% unique(as.vector(unlist(baCatchList)))]
    upFinal <- upFinal[!upFinal %in% dropCatchments]
    
    if(length(upFinal) == 0){print(paste0("No catchments selected: ", net))} # report if no upstream/downstream catchments
    
    # calc values and save
    if(length(upFinal) == 0){
      netShp@data[[paste0(newCol,"km2")]][netShp@data[[idColNet]] == net] <- 0
      netShp@data[[paste0(newCol,"AWI")]][netShp@data[[idColNet]] == net] <- 0
    } else{
      
      upDbf <- catchDbf[catchDbf$CATCHNUM %in% upFinal,]
      
      # calc area and AWI and add to netShp
      netShp@data[[paste0(newCol,"km2")]][netShp@data[[idColNet]] == net] <- round(sum(upDbf[[areaCol]]) / 1000000, 0)
      netShp@data[[paste0(newCol,"AWI")]][netShp@data[[idColNet]] == net] <- round(sum(upDbf[[intactCol]] * upDbf[[areaCol]]) / sum(upDbf[[areaCol]]),4)
      
      # if save requested, add to sp object
      if(nchar(savePath) > 0){
        upShp <- catchShp[catchShp@data$CATCHNUM %in% upFinal,] # subset
        if (!gIsValid(upShp)) { # fix topology
          upShp <- buffer(upShp, 0, dissolve=F)
        }
        upShp@data$diss <- 1 # create common value for all rows to dissolve on
        #catchShp2Buff <- buffer(catchShp2, 0.1, dissolve=FALSE)
        upShpDiss <- unionSpatialPolygons(upShp, upShp@data$diss) # dissolve
        upShpDiss <- spChFIDs(upShpDiss, as.character(1:length(row.names(upShpDiss))))
        upShpOut <- SpatialPolygonsDataFrame(upShpDiss, data.frame(networks=net)) # make sp df
        upShpOut <- spChFIDs(upShpOut, as.character(saveCount))
        
        # add to master
        if(saveCount == 1){
          masterShp <- upShpOut
        } else{
          masterShp <- spRbind(masterShp, upShpOut)
        }
        saveCount <- saveCount + 1
      }
    }
    
    counter <- counter + 1
  }
  
  ### SAVE POLYGONS ################################################################################################
  if(nchar(savePath) > 0 & saveCount == 1){print("No hydrology areas to save")}
  if(nchar(savePath) > 0 & saveCount > 1){ # only save if requested, and if some polygons have been processed
    writePolyShape(masterShp, savePath) # save
    file.copy(prjFile, paste0(substr(savePath, 1, nchar(savePath)-4), ".prj"), overwrite = TRUE)
  }
  
  ### SAVE NETWORKS ################################################################################################
  # remove any extra SP_ID columns
  for(x in colnames(netShp@data)){ 
    if(grepl("SP_ID",x)==TRUE)
      netShp@data[[x]] <- NULL
  }
  writePolyShape(netShp, netPath)
}