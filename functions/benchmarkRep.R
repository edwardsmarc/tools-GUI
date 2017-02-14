# Marc Edwards - BEACONs
# Feb 10 2017
# Function to evaluate benchmark representation.
# new version of denovoRep function used in NWB that can accept either a list of benchmarks matching the ranker input file, or a shp of polygons to be analysed using GIS tools. If both are provided the names should be unique and results should be output to a single results table. For the shapefiles option, clip criteria maps and calculate targets.

benchmarkRep <- function(catchDbf,buildCsv,targetTab,crit,outFile,filterCsv="") {
  
  # catchDbf - path to catchments dbf file. This is the dbf file associated with the catchments shp file. Must have the criteria values tabulated for each criteria raster map - i.e. the dbf file associated with the output from "criteria2catch.R".
  # buildCsv - path to output ranker file from builder, lists all benchmark names and the catchments that make up each benchmark.
  # targetTab - path to target table for the criteria being tested, i.e. output from gen_target_table.R.
  # crit - name of the criteria being tested. Should match that used in gen_target_table.R.
  # outFile - path to output csv table where results will be saved.
  # filterCsv - optional. If provided, should be a path to a csv table that has a PB column with a list of benchmark names. This list should be a subset of the benchmarks in buildCsv. Only benchmarks in the filterCsv table will be processed.
  
  library(foreign)
  library(dplyr)
  
  # CHECKS 
  ##############################################
  
  # Create output folder if it doesn't exist 
  outFile_spl <- strsplit(outFile, "/")[[1]]
  outDir <- paste0(outFile_spl[1:(length(outFile_spl)-1)], collapse="/")
  if (!file.exists(outDir)) {
    dir.create(outDir, recursive=TRUE)
  }
  
  # check catchments exist and CATCHNUM column present
  if (!file.exists(catchDbf)) {
    stop(paste("File does not exist: ", catchDbf))
  } else {
    catch <- read.dbf(catchDbf)
    if(!"CATCHNUM" %in% names(catch)){
      stop("No CATCHNUM attribute in catchments dbf")
    }
  }
    
  # check builder csv table exists
  if (!file.exists(buildCsv)) {
    stop(paste("File does not exist: ", buildCsv))
  } else {
    builder <- readLines(buildCsv)[-1]
  }
  
  # check outfile has .csv
  if(!grepl(".csv$",outFile)){
    stop(paste0("Outfile --- ", outFile, " --- needs to end in '.csv'"))
  }
  
  # check that PB names are standard i.e., PB_1234
  r1 <- strsplit(builder[1], split=",")[[1]][1]
  r11 <- unlist(strsplit(r1, split="_"))
  if (length(r11) != 2) {
    stop("ERROR: Check PB names in Builder output file - should look like PB_1234")
  }
  
  # get list of PBs
  pbs <- NULL
  for (a in 1:length(builder)) {
    r <- strsplit(builder[a], split=",")
    pbs <- c(pbs,r[[1]][1])
  }
  
  
  # FILTERLIST
  ##############################################
  
  # Code to open a csv table that contains the filter list.
  if(nchar(filterCsv)>0){
    filterTab <- read.csv(filterCsv)
    if(!"PB" %in% names(filterTab)){ # Check PB column exists
      stop("Filter table needs to have a PB column containing the benchmark names.")
    }
    proList <- as.character(filterTab$PB)
  } else{
    proList <- pbs
  }
  
  # Check that all benchmarks in the processing list (proList) are in the buildCsv file
  if(!all(proList %in% pbs)){
    stop(paste0("Cannot evaluate representation for benchmarks that are not in the buildCsv file: ", as.character(proList[!proList %in% pbs])))
  }
  
  # read in target table
  y <- read.csv(targetTab)
  buildCount <- length(builder)
  for (a in 1:length(builder)) { # builder - one row for each benchmark. Last column is list of catchments in the benchmark
    
    if(a %% 50 == 0){print(paste0("Processing benchmark ",a, "of", buildCount))}
    
    r <- strsplit(builder[a], split=",") # name of benchmark
    pb <- r[[1]][1]
    
    if (pb %in% proList){ # Only evluate if the benchmark is in list of benchmark names
      
      cs <- r[[1]][10:length(r[[1]])] # List of catchments in benchmark a
      cs <- cs[cs!=""] # keep only catchments not empty spaces
      catchSub <- catch[(catch$CATCHNUM %in% cs),] # select catchments that are in benchmark a from the catchDbf
      
      for (b in 1:nrow(y)) { # for each row in the target table
        
        bb <- y$class[b] # identify class in input file. Field becomes the name of the classes column in the catchments table
        
        #field <- paste0(toupper(crit),bb)
        field <- paste0(crit,bb)
        
        # populate new column for PB name with proportion of target represented (or 1 if 100% met)
        # sum(ba[field]) is the area of each target class in the catchments e.g. GPP18
        # if field doesn't exist assign it a 999 (this shouldn't happen)
        # if the sum of the class in the BA catchments is less than the target...assign value to the proportion represented
        # if the sum of the class in the BA catchments is more than the target...set the value to 1
        
        if (!field %in% names(catchSub)) {
          y[b,pb] <- 999
          warning("Field name is not in catchments file")
        } else if ((sum(catchSub[field] / 1000000) - y[b,"target_km2"]) < 0) { 
          # if target is not met in the catchments, return the proportion that is met.
          y[b,pb] <- 1 - round((y[b,"target_km2"] - sum(catchSub[field] / 1000000)) / y[b,"target_km2"], 2) 
        } else {
          y[b,pb] <- 1 # otherwise, target is met in catchments so assign 1.
        }
      }
    } 
  }
  
  y.names <- paste0("class",y$class)
  
  # transpose
  yt <- y %>%
    select_(.dots=proList) # select results columns only
  yt2 <- data.frame(names(yt), t(yt)) # merge names with transposed results
  names(yt2) <- c("networks",y.names)
 
  ##################################
  ##################################
  # ADD CODE TO:
    # LOAD shapefile
    # LOAD CRITERIA Map
    # CLIP RASTER BY SHP
    # SUM UP CLASSES IN CLIPPED RASTER
    # EVALUATE TARGETS
    # APPEND RESULTS TO RESULTS TABLE
  # This should work even if no builder svd is provided - make sure arguments are optional.
  ##################################
  ##################################
   
  write.csv(yt2, outFile, quote=T, row.names=F)
}
