# Marc Edwards - BEACONs
# March 20 2017
# Function to detect overlapping benchmarks in networks and save csv table grouping benchmarks in each network by overlap 
# input is a list of networks
# output is a list of lists, each networks list element contains a list of vectors where each vector is a set of benchmarks that are overlapping - list is converted to csv so results can be saved. Csv is converted back to list in later functions

findOverlappingBenchmarks <- function(singlesCsv, nBA, catchDbfPath, buildCsvPath, summaryOut, buildCsvAddPath="", filterCsv=""){
  
  # netList - list of network names
  
  # catchDbfPath - file path to catchments dbf file
  
  # buildCsvPath - file path to builder ranker file
  
  # buildCsvAddPath - additional benchmark catchments for benchmarks not in buildCsvPath - Table with benchmark names as column names and rows of CATCHNUMs.
  
  
  ### CHECKS ############################################
  
  # make nBA numeric
  nBA <- as.numeric(nBA)
  
  # Check singles csv exists
  if(file.exists(singlesCsv)){
    y1 <- read.csv(singlesCsv)
  } else{
    stop("Singles csv file does not exist.")
  }
  
  # Check filterCsv exists if provided
  if(nchar(filterCsv) > 0){
    if(!file.exists(filterCsv)){
      stop("Filter csv file does not exist.")
    }
  }
  
  # check networks column exists
  if(!"networks" %in% names(y1)){
    stop("Singles results table should have a column named 'networks'")
  }
  
  # check catch dbf ends in dbf and open
  if(nchar(catchDbfPath) > 0){
    if(!grepl(".dbf$", catchDbfPath)){
      stop(paste0("File path should end in .dbf: ", catchDbfPath))
    }
    if(file.exists(catchDbfPath)){
      catchDbf <- read.dbf(catchDbfPath)
    } else{
      stop(paste0("File does not exist: "), catchDbfPath)
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
  
  # GET SINGLES LIST
  ###############################################
  singles <- as.character(y1$networks)
  
  # GET NETWORKS LIST
  ###############################################
  if(nchar(filterCsv) > 0){
    filterTab <- read.csv(filterCsv)
    nets <- as.character(filterTab$networks)
  } else{
    pbs <- combn(singles, nBA, simplify=FALSE) # simplify=FALSE returns a list
    netList <- sapply(pbs, function(x) paste0(x,collapse="_")) # this works for any nBA value - makes network names from the list of benchmarks
  }
  
  ### MAKE BENCHMARK CATCHMENT LIST AND ADDITIONAL LISTS #################################################################
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
  
  
  megaOut <- lapply(netList, function(x){
    
    # make list of BA names from networks names
    baList <- c()
    splits <- strsplit(x, split="_")[[1]]
    for(i in 1:length(splits)){
      if((i + 1) %% 2 == 0){
        baList <- c(baList, paste0(splits[i], "_", splits[i+1]))
      }
    }
    
    # subset the full catchments list by the BAs in network - Collect benchmark catchment list for each benchmark in network from baCatchments and baCatchmentsAdd - optional
    baCatchList <- list()
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
    
    # Get list of unique overlapping benchmark groups
    # first make list with pairs of overlapping BAs
    overlapList <- list() 
    j <- 1
    for(ba in baList){ # for each benchmark
      for(ba2 in baList[!baList %in% ba]){ 
        if(any(baCatchList[[ba]] %in% baCatchList[[ba2]])){ # test if any overlap with other benchmarks
          overlapList[[j]] <- c(ba, ba2) # if yes, add the overlapping BA to the BAs list element
          j <- j + 1
        } 
      }
    }
    no_overlap <- as.list(baList[!baList %in% unlist(overlapList)])
    overlapList2 <- no_overlap # start with non overlapping bas, then add additional list elements for each group of overlapping bas
    
    # now combine into unique overlapping groups
    #overlapList2 <- list() # new list of groups
    jj <- length(overlapList2) + 1
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
    lapply(overlapList2, function(x) unique(x)) # return list of groups. Unique removes duplicate entries within a group
  })
  names(megaOut) <- netList
  
  
  # save table of network overlap groups - one row for each overlap group in each network
  df <- data.frame(networks = as.character(), groups = as.character())
  for(i in names(megaOut)){
    counts <- c()
    for(l in 1:length(megaOut[[i]])){
      new_row <- data.frame(networks = i, groups = paste0(megaOut[[i]][l][[1]], collapse = ","))
      df <- rbind(df, new_row)
    }
  }
    
  # save table
  write.csv(df, summaryOut)
}
