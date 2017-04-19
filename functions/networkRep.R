# Marc Edwards - BEACONs
# Feb 13 2017
# networkRep function for tools GUI. Based on netRep.R from NWB which was in turn based on addBAs2PAs from PBA.

networkRep <- function(singlesCsvPath, outFile, nBA="", filterCsv="", overlapTablePath="", catchDbfPath="", buildCsvPath="", buildCsvAddPath="", targetTabPath="", crit="", shpPath="", baPath="", idColBa="", clipRaster="", cellsize="", disaggregate.factor="") {
  
  # Function designed to evalutate representation for networks of benchmarks using the target table and the denovo singles results as input.
  # If an overlap table is provided, networks of overlapping benchmarks are assessed first, then regular non-overlapping networks are added to the results table.
  # if any benchmarks in overlapping networks are provided as shapefiles rather than builder output, the benchmarks in that networks are merged and the represenation is assessed by clipping the raster - as in benchmarkRep() when shapefile are provided.
  
  # Arguments for standard analysis without using overlap table
      # singlesCsv: results table for denovo singles, long format with columns: networks...class1...class2...class3...etc.
      # outFile: file path to save output table to. Output table will have same columns as input but network names will be new
      # nBA: number of benchmarks per network to calculate. All combinations will be tested. Either this or filterCsv must be provided.
      # filterCsv: if provided, this list will over-ride the default of all combinations. Use this if you only want to test a subset of possible networks. Must be a file path to a csv table with a "networks" column containing the network names to be processed. Network name should be of the format: PB_1_PB_2 for a network made up of two benchmarks named PB_1 and PB_2. 
  
  # Arguments when using overlap table, but no additional shp benchmarks
      # overlapTablePath - If provided, networks listed in the overlapTablePath will be processed separately - benchmarks that overlap will be dissolved and assessed for representation as a single area, then combined with any non-overlapping benchmarks in that network as per the usual network representation method. The table is the output table from the findOverlappingBenchmarks() function. The networks column lists networks and the groups column groups benchmarks into overlapping patches within each network. Each network will have a row for each overlapping group of benchmarks. e.g. a network of two non-overlapping benchmarks will have two rows. A networks of two overlapping benchmarks will have one row.
      # catchDbfPath - catchments dbf with tabulated criteria added
      # buildCsvPath - builder ranker file
      # buildCsvAddPath - additional benchmark catchments for benchmarks not in buildCsvPath - Table with benchmark names as column names and rows of CATCHNUMs
      # targetTabPath - target table for the criteria being evaluated
      # crit - criteria name, as reported in other tools - used to identify class columns in the catchments dbf
  
  # Arguments when using an additional shp in the analysis and an overlap table
      # shpPath - must have PB column. Must also be added to baShp using benchmarks Shp tool
      # baPath - file path to benchmarks shapefile. Used to get benchmark shps when dissolving overlapping networks that contain a benchmark from the shpPath
      # idColBa - unique ientifier for benchmarks in baPath
      # clipRaster - criteria raster to be clipped when analysing overlap networks that had additional shp benchmarks. Should match that used in benchmarkRep()
      # cellsize - cellsize of clipRaster
      # disaggregate.factor - factor to reclassify raster by during analysis. Should match that in benchmarkRep()
  # shpPath - must have PB column. Must also be added to baShp using benchmarks Shp tool
  
  library(dplyr, lib.loc = "../packages/")
  library(readr, lib.loc = "../packages/")
  
  # CHECKS 
  ##############################################
  
  # One of nBA or filterCsv has to be provided
  if(nchar(nBA) == 0 & nchar(filterCsv) == 0){
    stop("Either number of 'benchmarks' or 'filter table' must be provided")
  }
  
  # make nBA numeric
  if(nchar(nBA) > 0){
    nBA <- as.numeric(nBA)
  }
  
  # Check singles csv exists
  if(nchar(singlesCsvPath) > 0){
    if(file.exists(singlesCsvPath)){
      y1 <- read.csv(singlesCsvPath)
    } else{
      stop("Singles csv file does not exist.")
    }
    # check networks column exists
    if(!"networks" %in% names(y1)){
      stop("Singles results table should have a column named 'networks'")
    }
  }
  
  # Check filterCsv exists if provided
  if(nchar(filterCsv) > 0){
    if(file.exists(filterCsv)){
      filterTab <- read.csv(filterCsv)
      if(nrow(filterTab) == 0){
        stop("Empty filter table provided")
      }
      if(!"networks" %in% names(filterTab)){
        stop("Filter table does not have 'networks' column")
      }
    } else{
      stop("Filter csv file does not exist.")
    }
  }
  
  # check outfile has .csv
  if(!grepl(".csv$",outFile)){
    stop(paste0("Outfile --- ", outFile, " --- needs to end in '.csv'"))
  }
  
  # Check overlapTablePath exists, is csv, and has networks and groups columns
  if(nchar(overlapTablePath) > 0){
    if(!grepl(".csv$",overlapTablePath)){
      stop(paste0("Outfile --- ", outFile, " --- needs to end in '.csv'"))
    }
    if(file.exists(overlapTablePath)){
      overlapTable <- read.csv(overlapTablePath)
      if(!"networks" %in% names(overlapTable)){
        stop("Overlap table does not have 'networks' column")
      }
      if(!"groups" %in% names(overlapTable)){
        stop("Overlap table does not have 'groups' column")
      }
      if(nrow(overlapTable) == 0){
        stop("No results in overlap table")
      }
    }
  }
  
  # if provided, check catchDbfPath exists and is csv, check CATCHNUM is an attribute
  if(nchar(catchDbfPath) > 0){
    if (!file.exists(catchDbfPath)) {
      stop(paste("File does not exist: ", catchDbf))
    } else {
      catchDbf <- read.dbf(catchDbfPath)
      if(!"CATCHNUM" %in% names(catchDbf)){
        stop("No CATCHNUM attribute in catchments dbf")
      }
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
  
  # check target table exists
  if(nchar(targetTabPath) > 0){
    if(!file.exists(targetTabPath)){
      stop(paste0("File does not exist: ", targetTab))
    } else{
      targetTab <- read.csv(targetTabPath)
    }
  }
  
  # if shapefile is provided...
  if(nchar(shpPath) > 0){
    if(file.exists(shpPath)){ # ...load shapefile if it exists, check it has PB column
      shp <- shapefile(shpPath)
      if(!"PB" %in% names(shp)){
        stop(paste("No 'PB' column in file: ", shpPath))
      }
    } else{
      stop(paste0("File does not exist: ", shpPath))
    }
    
    # ...cell size numeric, disag numeric, calc area
    if(nchar(cellsize)==0 | nchar(disaggregate.factor)==0){stop("cellsize and disaggregate factor need to be provided if shapefiles being used for representation")}
    cellsize <- as.numeric(cellsize)
    disaggregate.factor <- as.numeric(disaggregate.factor)
    cellarea <- ((cellsize / disaggregate.factor) * (cellsize / disaggregate.factor)) / 1000000
    
    # ...open raster and disaggregate the raster
    if(nchar(clipRaster) > 0){
      if(file.exists(clipRaster)){
        ras <- raster(clipRaster)
      } else{
        stop(paste0("File does not exist: ", clipRaster))
      }
    } else{
      stop(paste0("raster file needs to be provided if shapefiles being used for representation"))
    }
    print("disaggregating raster")
    rasD <- disaggregate(ras, disaggregate.factor, method = '')
    
    # ...check baShp exists - then open
    if(nchar(baPath) > 0){
      if(file.exists(baPath)){
        baShp <- shapefile(baPath)
        if(!idColBa %in% names(baShp)){
          stop(paste0("No ", idColBa, " column in file: ", baPath))
        }
      } else{
        stop(paste0("File does not exist: ", baPath))
      }
    } else{paste0("Benchmark shapefile needs to be provided if shapefiles being used for representation")}
  }
  
  # GET SINGLES LIST
  ###############################################
  singles <- as.character(y1$networks)
  
  # GET NETWORKS LIST
  ###############################################
  if(nchar(filterCsv) > 0){
    netList <- as.character(filterTab$networks)
  } else{
    pbs <- combn(singles, nBA, simplify=FALSE) # simplify=FALSE returns a list
    netList <- sapply(pbs, function(x) paste0(x,collapse="_")) # this works for any nBA value - makes network names from the list of benchmarks
  }
  
  if(length(netList) == 0){stop("No networks to process, check singles results table and filter table (if provided) are correct")}
  
  print(paste0("Creating BA x ", nBA, " networks from table: ", singlesCsvPath))
  
  
  
  #######################################################################################################################
  ### ASSESS OVERLAP REPRESENTATION #####################
  if(nchar(overlapTablePath) > 0){
    print("Processing overlap networks...")
    
    # Convert overlapTable back to list of lists
    megaOut <- list()
    for(net in as.character(unique(overlapTable$networks))){
      subdf <- overlapTable[overlapTable$networks == net,]
      list1 <- lapply(as.character(subdf$groups), function(x) strsplit(x, ",")[[1]])
      megaOut[[net]] <- list1
    }
    
    
    # make list of all benchmarks in buildCsv and their catchments
    baCatchments <- list()
    if(nchar(buildCsvPath) > 0){
      y <- strsplit(buildCsv,",")
      
      PBnames <- sapply(y,function(x)x[1]) # extract PB_xxx names i.e., first element of each list
      baCatchments <- lapply(y,function(x){
        x <- x[x!=""] # remove blanks
        as.integer(x[-c(1:9)]) # remove unecessary columns
      })
      names(baCatchments) <- PBnames # assign PB_xxx names to the list of catchments
    }
    
    # add additional benchmarks to baCatchments if provided
    if(nchar(buildCsvAddPath) > 0){
      for(b in names(buildCsvAdd)){
        bCatch <- as.integer(buildCsvAdd[,b])
        bCatch <- unlist(bCatch[!is.na(bCatch)])
        # Error if a benchmark is in both lists but catchments don't match
        if(b %in% names(baCatchments)){
          if(!setequal(baCatchments[[b]], bCatch)){
            stop(paste0("Benchmark in both catchment lists but catchments don't match: ", b))
          }
        } else{
          baCatchments[[b]] <- bCatch
        }
      }
    }
    
    
    # split networks into groups of 5000
    grps <- split(names(megaOut), ceiling(seq_along(names(megaOut))/5000))
    
    for(i in 1:length(grps)){
      
      grpNets <- grps[i][[1]]
      print(paste0("Running group ", i, " of ", length(grps)))
      
      # for each network of PBs in list of networks
      counter <- 1
      for (net in grpNets) {
        
        # get megaOutput list for network
        megaNet <- megaOut[[net]]
        
        # assign groups in megaOutput to categories (mega BA, mega BA shp, single BA) using their indexes
        megaIndex <- c()
        megaShpIndex <- c()
        baIndex <- c()
        for(j in 1:length(megaNet)){
          if(length(megaNet[[j]]) > 1){ # if group has more than one benchmark...
            if(nchar(shpPath) > 0){ # ...was shpPath provided?
              if(any(shp@data$PB %in% megaNet[[j]])){ # ...if any shp benchmarks in group, add to megaShpIndex
                megaShpIndex <- c(megaShpIndex, j)
              } else{
                megaIndex <- c(megaIndex, j) # ...if not shp benchmarks, add to megaIndex
              }
            } else{
              megaIndex <- c(megaIndex, j) # ...if no shpPath, add to megaIndex
            }
          } else{
            baIndex <- c(baIndex, j) # else add to baIndex
          }
        }
        
        megaNames <- c() # make fresh megaNames list
        t1 <- targetTab # open fresh target table
        
        #########
        # evaluate megaIndex benchmarks using catchments and store as rows in results table
        for(k in megaIndex){
          
          megaName <- paste0("Mega_",k)
          megaNames <- c(megaNames, megaName)
          
          baList <- megaNet[[k]] # benchmarks in group
          if(!all(baList %in% names(baCatchments))){stop(paste0(paste(baList,collapse=" "), ": Benchmarks are not in builder output or additional benchmark table"))}
          baList_catchments <- baCatchments[names(baCatchments) %in% baList] # catchments in group bas
          baList_catchments <- unique(as.character(unlist(baList_catchments))) # remove duplicates
          baList_catchments <- baList_catchments[baList_catchments!=""] # remove empties
          catchSub <- catchDbf[(catchDbf$CATCHNUM %in% baList_catchments),] # select catchments that are in benchmark a from the catchDbf
          for (b in 1:nrow(targetTab)) { # for each row in the target table
            
            bb <- t1$class[b] # identify class in input file. Field becomes the name of the classes column in the catchments table
            field <- paste0(crit,bb)
            
            # populate new column for PB name with proportion of target represented (or 1 if 100% met)
            # sum(ba[field]) is the area of each target class in the catchments e.g. GPP18
            # if field doesn't exist assign it a 999 (this shouldn't happen)
            # if the sum of the class in the BA catchments is less than the target...assign value to the proportion represented
            # if the sum of the class in the BA catchments is more than the target...set the value to 1
            
            if (!field %in% names(catchSub)) {
              t1[b,megaName] <- 999
              warning("Field name is not in catchments file")
            } else if ((sum(catchSub[field] / 1000000) - t1[b,"target_km2"]) < 0) { 
              # if target is not met in the catchments, return the proportion that is met.
              t1[b,megaName] <- 1 - round((t1[b,"target_km2"] - sum(catchSub[field] / 1000000)) / t1[b,"target_km2"], 2) 
            } else {
              t1[b,megaName] <- 1 # otherwise, target is met in catchments so assign 1.
            }
          }
        }
        
        ##########
        # Evaluate megaShpIndex groups using shp method from benchmarkRep
        for(m in megaShpIndex){
          
          megaName <- paste0("Mega_",m)
          megaNames <- c(megaNames, megaName)
          
          # dissolve group polygons using baShp and clip raster 
          if(!all(megaNet[[m]] %in% baShp[[idColBa]])){ # check all benchmarks in baShp
            stop(paste0("Benchmarks not in Benchmark shapefile: ", paste0(megaNet[[m]][!megaNet[[m]] %in% baShp[[idColBa]]]),collapse=" "))
          }
          baSub <- baShp[baShp[[idColBa]] %in% megaNet[[m]],]
          baSub@data$Diss_id <- 1
          baDiss <- unionSpatialPolygons(baSub, baSub@data$Diss_id)
          rasVals <- unlist(extract(rasD, baDiss))
          
          # calculate proportion of targets met
          for (v in t1$class) { # for each row in the target table
            
            cArea <- length(rasVals[rasVals %in% v]) * cellarea # area of class in PB (km2)
            targ <- t1$target_km2[t1$class == v] # target (km2)
            
            if(cArea < targ){ # if less than target, report proportion
              t1[[megaName]][t1$class == v] <- round((cArea / targ), 2)
            } else if(cArea >= targ){ # if greater than or equal to target, report 1
              t1[[megaName]][t1$class == v] <- 1.00
            }
          }
        }
        # transpose
        y.names <- paste0("class",t1$class)
        yt <- t1 %>%
          select_(.dots=megaNames) # select results columns only
        netResults <- data.frame(names(yt), t(yt)) # merge names with transposed results
        names(netResults) <- c("networks",y.names)
        ##########
        
        # if there are single bas, add them to the netResults table
        singlesList <- c()
        for(l in baIndex){
          singlesList <- c(singlesList,megaNet[[l]])
        }
        netResults <- rbind(netResults,y1[as.character(y1$networks) %in% singlesList,])
        ##########
        
        # Calculate max values of results and add to master group table 
        if(counter==1){ # for first network, make data frame using dplyr
          grpTab <- netResults %>% # calculate row values
            select(-contains("networks")) %>%
            summarise_each(funs(max)) %>%
            mutate(networks=net)
          grpTab <- grpTab[c(which(colnames(grpTab)=="networks"),1:(length(colnames(grpTab))-1))] # order
        } else{ # then add rows as vectors using apply - faster
          # clac max and bind to group table. Ignore networks column and any columns met by the PA
          grpTab <- rbind(grpTab,as.vector(c(net ,apply(netResults[,!colnames(netResults) %in% "networks"], 2, function(x) round(max(x),2))))) 
        }
        
        counter <- counter + 1
      }
      
      # merge results to master table that holds targets
      if(i==1){
        ynets_O <- grpTab
      } else{
        ynets_O <- rbind(ynets_O,grpTab)
      }
      ynets_O$networks <- as.factor(ynets_O$networks)
    }
    
    # Remove overlap networks from netList
    netList2 <- netList[!netList %in% names(megaOut)]
  } else{
    netList2 <- netList
  }
  
  
  
  
  #######################################################################################################################
  ### ASSES NON OVERLAP REPRESENTATION ######################
  
  if(length(netList2) > 0){
  
    print("Processing non-overlap networks...")
    
    # split networks into groups of 5000
    grps <- c()
    grps <- split(netList2, ceiling(seq_along(netList2)/5000))
    
    for(gp in 1:length(grps)){
      # Make the group table and pre-allocate with 0's
      grpNets <- grps[gp][[1]]
      print(paste0("Processing group ", gp, " of ", length(grps)))
      
      # for each network of PBs in list of networks
      counter <- 1
      for (pb in grpNets) {
        
        # make list of singles included in each pb. Works for any nBA
        splits <- strsplit(pb, split="_")[[1]]
        pbList <- c()
        for(i in 1:length(splits)){
          if((i + 1) %% 2 == 0){
            pbList <- c(pbList, paste0(splits[i], "_", splits[i+1]))
          }
        }
        
        if(all(pbList %in% singles)){
          
          if(counter==1){ # for first network, make data frame using dplyr
            grpTab <- y1 %>% # calculate row values
              filter(networks %in% pbList) %>%
              dplyr::select(-contains("networks")) %>%
              summarise_each(funs(max)) %>%
              mutate(networks=pb)
            grpTab <- grpTab[c(which(colnames(grpTab)=="networks"),1:(length(colnames(grpTab))-1))] # order
          } else{ # then add rows as vectors using apply - faster
            grpTab <- rbind(grpTab,as.vector(c(pb ,apply(y1[y1$networks %in% pbList, !colnames(y1) %in% "networks"], 2, function(x) round(max(x),2))))) 
          }
        } else{
          stop(paste0("Benchmarks not in singles table...",pb))
        }
        counter <- counter + 1
      }
      # merge results to master table that holds targets
      if(gp==1){
        ynets_noO <- grpTab
      } else{
        ynets_noO <- rbind(ynets_noO,grpTab)
      }
      ynets_noO$networks <- as.factor(ynets_noO$networks)
    }
  } else{
    print("No non-overlapping networks to process")
  }
  
  
  
  
  #######################################################################################################################
  # Merge overlap and non-overlap results tables together
  if(nchar(overlapTablePath) > 0 & length(netList2) == 0){ # if overlap results but no non-overlap results, master becomes overlap results
    masterDf <- ynets_O
  } else if(nchar(overlapTablePath) > 0 & length(netList2) > 0){ # if both overlap and non-overlap results, merge
    masterDf <- rbind(ynets_O, ynets_noO)
  } else if(nchar(overlapTablePath) == 0 & length(netList2) > 0){ # if no overlap table, master becomes non-overlap results
    masterDf <- ynets_noO
  } else if(nchar(overlapTablePath) == 0 & length(netList2) == 0){
    
  }
  
  print("saving...")
  write_csv(masterDf, outFile)
  #write.csv(ynets, outFile, quote=T, row.names = FALSE)
  
  if(!all(netList %in% as.character(masterDf$networks))){stop("All original networks are not in final results table. Check inputs. Output table has still been saved.")}
}
