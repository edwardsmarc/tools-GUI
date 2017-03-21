# Marc Edwards
# March 9 2017
# Function to add network level gaps to network shapefile
# calculates all gaps, and gaps for classes making up >=x% of the ecoregion. 
# based on metricsNetGaps_transposed() function from NWB, this version developed for GUI

# performs the same summary as results_summary() function but add columns one at a time and allows user to shoose column name

metricsRepresentation <- function(netPath, resultsTab, rareThreshold = 0, gapThreshold = 100, targetTable = "", newCol) {
  
  # netShp - network shape file made by buildNetworksShp.R - must have 'networks' column
  # resultsTab - file path to representation results that will be used to calculate gaps. Must have column called "networks" containing network names
  # rareThreshold - only classes with bnd_prop column greater than this percentage value are counted as gaps.
  # gapThreshold - the percentage of the target needed to be met. Defaults to 100%. If set to e.g. 90%, gaps only counted if <90% of the target is met.
  # targetTab (optional) - file path to the target table. Only needed if rareThreshold is >0
  # newCol - name of new column to store gap values
  
  library(raster, lib.loc = "../packages/")
  library(data.table, lib.loc = "../packages/")
  
  ### CHECKS ########
  
  # check netShp exists and is shp
  if(!grepl(".shp$", netPath)){
    stop(paste0("File should end in '.shp': ", netPath))
  }
  if(file.exists(netPath)){
    netShp <- shapefile(netPath)
  } else{
    stop(paste0("File does not exist: ", netPath))
  }
  
  # check results table exists and is csv and has 'networks' col
  if(!grepl(".csv$", resultsTab)){
    stop(paste0("File should end in '.csv': ", resultsTab))
  }
  if(file.exists(resultsTab)){
    dfResults <- read.csv(resultsTab)
  } else{
    stop(paste0("File does not exist: ", resultsTab))
  }
  if(!"networks" %in% names(dfResults)){
    stop(paste0("No 'networks' column in results table: ", resultsTab))
  }
  
  # check if all netShp networks are in results
  if(!all(netShp@data$networks %in% as.character(dfResults$networks))){
    warning("Not all shapefile networks are in the results csv - missing networks will be assigned '999'")
  }
  
  rareProp <- as.numeric(rareThreshold)/100
  gapProp <- as.numeric(gapThreshold)/100
  
  
  ### ADD COLUMN ################
  
  # if removing rare classes:
    # Check for target table and open - check csv
    # check target table and results have same classes
  if(rareThreshold > 0){
    
    # open target table
    if(!grepl(".csv$", targetTab)){ # check .csv
      stop(paste0("File should end in '.csv': ", targetTab))
    }
    if(file.exists(targetTab)){ # open if exists
      dfTarget <- read.csv(targetTab)
    } else{
      stop(paste0("File does not exist: ", targetTab))
    }
    if(!"bnd_prop" %in% names(dfTarget)){ # check for bnd_prop column
      stop(paste0("No 'bnd_prop' column in results table: ", targetTab))
    }
    
    # check classes match between target and results tables
    dfTarget$class2 <- make.names(paste0("class",dfTarget$class)) # add class column to match results column names
    for(i in dfTarget$class2){
      if(!i %in% names(dfResults)){
        stop("class names do not match between target table and results table")
      }  
    }
    # identify non-rare classes - make list of names
    nonrare <- dfTarget %>%
      filter(bnd_prop>=rareProp) %>%
      .$class2 # . is stand in for the data frame, so this prints the class2 column as a vector
  } else{
    nonrare <- names(dplyr::select(dfResults,-starts_with("networks"))) # otherwise just use all classes
  }
  
  # prepare summarized results
  dfResults$gaps <- apply(dfResults[colnames(dfResults) %in% nonrare], 1, function(x) length(x[x<gapProp]))
  dfResults <- dplyr::select(dfResults, networks,gaps)
  names(dfResults)[names(dfResults)=="gaps"] <- newCol # change col name
  
  # merge to shp
  outShp <- sp::merge(netShp, dfResults, by="networks")
  
  # assign NA to 999
  outShp@data[[newCol]][is.na(outShp@data[[newCol]])] <- 999
  
  
  # remove any extra SP_ID columns
  for(x in colnames(outShp@data)){ 
    if(grepl("SP_ID",x)==TRUE)
      outShp@data[[x]] <- NULL
  }
  
  #save shapefile
  writePolyShape(outShp, netPath)
}