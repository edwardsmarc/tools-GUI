# Marc Edwards - BEACONs
# March 14 2017
# Function to calculate proportion of holes in networks
# Steps for each network:
  # 1. run overlap code on catchments to group benchmarks into patches
  # 2. if overlaps, merge the overlapping polygons
  # 3. for list of contiguous polygons, get outer area and holes area and calculate proportion of holes

# Overlap is analysed using catchments in each benchmark. If benchmark is not in builder ranker csv table, optional catchList can be used to list catchnums in benchmarks.
  # function combines the builder ranker and cathList values into a single list. If a benchmark name is in both it will remove the duplicate if catchnums match, otherwise will throw and error.

metricsHoles <- function(netPath, idColNet, baPath, idColBa,  buildCsvPath="", newCol, buildCsvAddPath=""){
  
  # netPath - file path to networks shapefile
  # baPath - file path to benchmarks shapefile
  # buildCsvPath (optional) - builder ranker file, optional because all catchments could be provided in buildCsvAdd
  # newCol - new column name
  # buildCsvAddPath (optional) - file path to csv table with column headings of benchmark names and rows of catchnum values, optional because all catchments could be provided by buildCsv
  
  library(raster, lib.loc = "../packages/")
  library(rgeos, lib.loc = "../packages/")
  library(foreign, lib.loc = "../packages/")
  library(maptools, lib.loc = "../packages/")
  
  
  ### CHECKS ##########
  # Check that input files exist and have correct attributes
  if(nchar(buildCsvPath) > 0){
    if(file.exists(buildCsvPath)){
      buildCsv <- readLines(buildCsvPath)[-1]
      if(length(buildCsv)==0){
        warning("No benchmarks in buildCsv!")
      }
    } else{
      stop(paste0("file does not exist: ", buildCsvPath))
    }
  }
  
  # Check netShp and baShp exist and open
  if(file.exists(netPath)){
    netShp <- shapefile(netPath)
    if(!idColNet %in% names(netShp)){
      stop(paste0("No ", idColNet, " column in file: ", netPath))
    }
  } else{
    stop(paste0("File does not exist: "), netPath)
  }
  
  if(file.exists(baPath)){
    baShp <- shapefile(baPath)
    if(!idColBa %in% names(baShp)){
      stop(paste0("No ", idColBa, " column in file: "), baPath)
    }
  } else{
    stop(paste0("File does not exist: "), baPath)
  }

  # if buildCsvAdd provided, check it exists and open
  if(nchar(buildCsvAddPath) > 0){
    if(file.exists(buildCsvAddPath)){
      buildCsvAdd <- read.csv(buildCsvAddPath)
    } else{
      stop(paste0("File does not exists: "), buildCsvAddPath)
    }
  }
  
  
  ### PROCESS NETWORKS ############
  
  netList <- unique(netShp@data[[idColNet]])
  
  # this makes list of all benchmarks in buildCsv and their catchments
  if(nchar(buildCsvPath) > 0){
    y <- strsplit(buildCsv,",")
    PBnames <- sapply(y,function(x)x[1]) # extract PB_xxx names i.e., first element of each list
    baCatchments <- lapply(y,function(x)as.integer(x[-c(1:9)])) # remove unecessary columns
    names(baCatchments) <- PBnames # assign PB_xxx names to the list of catchments
  }
  
  # make list of additional benchmark catchments if provided
  if(nchar(buildCsvAddPath) > 0){
    baCatchmentsAdd <- lapply(buildCsvAdd, function(x) unlist(x[!is.na(x)]))
  }
  
  counter <- 1
  for(net in netList){
    
    if(counter %% 50 == 0){print(paste0("Processing ", counter, " of ", length(netList)))}
    
    
    baList <- c()
    baCatchList <- list()
    
    # make list of BA names from networks names
    splits <- strsplit(net, split="_")[[1]]
    for(i in 1:length(splits)){
      if((i + 1) %% 2 == 0){
        baList <- c(baList, paste0(splits[i], "_", splits[i+1]))
      }
    }
    
    # Collect catchment list for each benchmark in network from baCatchments and baCatchmentsAdd
    for(b in baList){
      # Error if a benchmark is in both lists but catchments don't match
      if(b %in% names(baCatchments) & b %in% names(baCatchmentsAdd)){
        if((!all(baCatchments[[b]] %in% baCatchmentsAdd[[b]]) & all(baCatchmentsAdd[[b]] %in% baCatchments[[b]]))){
          stop(paste0("Benchmark in both lists but catchments don't match: ", b))
        }
      }
      if(b %in% names(baCatchments)){
        baCatchList[[b]] <- baCatchments[[b]]
      } else if(b %in% names(baCatchmentsAdd)){
        baCatchList[[b]] <- baCatchmentsAdd[[b]]
      }
    }
    
    overlapList <- list() # first make list with pairs of overlapping BAs
    j <- 1
    for(ba in baList){ # for each benchmark
      for(ba2 in baList[!baList %in% ba]){ 
        if(any(baCatchList[[ba]] %in% baCatchList[[ba2]])){ # test if any overlap with other benchmarks
          overlapList[[j]] <- c(ba, ba2) # if yes, add the overlapping BA to the BAs list element
          j <- j + 1
        }
      }
    }
    overlapList2 <- list() # then combine into unique overlapping groups
    jj <- 1
    if(length(overlapList) > 0){  
      for(k in 1:length(overlapList)){ # for each pair
        bas <- overlapList[[k]] # get names of pair
        if(any(bas %in% unlist(overlapList2))){ # if either have already been added to a group
          for(l in 1:length(overlapList2)){ # find the list element they are in and add the two names to it
            if(any(bas %in% overlapList2[[l]])){
              overlapList2[[l]] <- c(overlapList2[[l]], bas)
            }
          }
        } else{ # else add a new list element with the names
          overlapList2[[jj]] <- bas
          jj <- jj + 1
        }
      }
    }
    overlapList2 <- lapply(overlapList2, function(x) unique(x))
    
    # For each item in overlap list, merge all together and add to final BAs list
    baPolys <- baShp[baShp@data[[idColBa]] %in% baList,] # subset BA shapefile
    baPolys@data <- baPolys@data[names(baPolys@data) %in% idColBa] # remove all columns except names
    baPolys <- spChFIDs(baPolys, as.character(baPolys@data[[idColBa]]))
    
    if(length(overlapList2) > 0){
      for(m in 1:length(overlapList2)){
        
        # merge overlapping polys
        agg <- baPolys[baPolys@data[[idColBa]] %in% overlapList2[[m]],]
        if (!gIsValid(agg)) {
          cat(net,": fixed topology issues.\n")
          agg <- buffer(agg, 0, dissolve=F)
        }
        if (!gIsValid(agg)) { # found one case where this needed to be run twice to fix the topology issue (error: orphaned hole, cannot find containing polygon for hole at index 30).
          cat(net,": fixed topology issues.\n")
          agg <- buffer(agg, 0, dissolve=F)
        }
        mrg <- aggregate(agg, dissolve=TRUE)
        mrg <- spChFIDs(mrg, paste0(overlapList2[[m]], collapse="_"))
        df <- data.frame("PB" = paste0(overlapList2[[m]], collapse="_"))
        names(df) <- idColBa
        row.names(df) <- df[[idColBa]]
        mrg2 <- SpatialPolygonsDataFrame(mrg, df)
        
        # add merged version to baPolys
        baPolys2 <- spRbind(baPolys, mrg2)
        #df2 <- data.frame("PB" = row.names(baPolys2))
        #names(df2) <- idColBa
        #row.names(df2) <- df2[[idColBa]]
        #baPolys3 <- SpatialPolygonsDataFrame(baPolys2, df2)
        baPolys3 <- baPolys2
        
        # remove overlapping BAs from baPolys
        baPolys3 <- buffer(baPolys3,0,dissolve=F) # added this to fix an error where baPolys3 had "orphaned holes" - ME AUg10 2016
        baPolys3 <- baPolys3[!baPolys3@data[[idColBa]] %in% overlapList2[[m]],]
      }
    } else{
      baPolys3 <- baPolys
    }
    
    net_outer_area <- 0 # area of outer polygons in network
    holes_net <- 0 # area of holes in network
    for (x in 1:nrow(baPolys3)) {
      ba_outer_area <- 0 # area of outer polygons in BA
      holes_ba <- 0 # area of holes in BA
      for (a in 1:length(baPolys3@polygons[[x]]@Polygons)) {
        if(baPolys3@polygons[[x]]@Polygons[[a]]@hole == FALSE){ # add areas of outer polygons i.e polygons that are not holes
          ba_outer_area <- ba_outer_area + baPolys3@polygons[[x]]@Polygons[[a]]@area
        }
        if(baPolys3@polygons[[x]]@Polygons[[a]]@hole == TRUE){ # add areas of holes
          holes_ba <- holes_ba + baPolys3@polygons[[x]]@Polygons[[a]]@area
        }
      }
      net_outer_area <- net_outer_area + ba_outer_area
      holes_net <- holes_net + holes_ba
    }
    
    netShp@data[[newCol]][netShp@data[[idColNet]]==net] <- round(holes_net / net_outer_area,3) # proportion of network that is holes
    
    counter <- counter + 1
  }
  
  # remove any extra SP_ID columns
  for(x in colnames(netShp@data)){ 
    if(grepl("SP_ID",x)==TRUE)
      netShp@data[[x]] <- NULL
  }
  
  # SAVE
  #######################################
  writePolyShape(netShp, netPath)
  
}