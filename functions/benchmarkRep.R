# Marc Edwards - BEACONs
# Feb 10 2017
# Function to evaluate benchmark representation.
# new version of denovoRep function used in NWB that can accept either a list of benchmarks matching the ranker input file, or a shp of polygons to be analysed using GIS tools. If both are provided the names should be unique and results should be output to a single results table. For the shapefiles option, clip criteria maps and calculate targets.

benchmarkRep <- function(catchDbf="",buildCsv="",targetTab,crit="",outFile,filterCsv="", shpPath="", clipRaster="", cellsize="", disaggregate.factor="") {
  
  # catchDbf (optional)  - path to catchments dbf file. This is the dbf file associated with the catchments shp file. Must have the criteria values tabulated for each criteria raster map - i.e. the dbf file associated with the output from "criteria2catch.R".
  
  # buildCsv (optional)  - path to output ranker file from builder, lists all benchmark names and the catchments that make up each benchmark.
  
  # targetTab - path to target table for the criteria being tested, i.e. output from gen_target_table.R.
  
  # crit - name of the criteria being tested. Should match that used in gen_target_table.R.
  
  # outFile - path to output csv table where results will be saved.
  
  # filterCsv - optional. If provided, should be a path to a csv table that has a PB column with a list of benchmark names. This list should be a subset of the benchmarks in buildCsv. Only benchmarks in the filterCsv table will be processed.
  
  # shpPath (optional)  - file path to a shapefile. Must have a 'PB' column with a unique benchmark name. Each row in the shapefile will be used to clip the raster and evaluate targets.
  
  # clipRaster (optional)  - raster for the criteria being evaluated. Should be the same raster supplied to the catchments2criteria tool.
  
  # cellsize (optional) - cellsize of the raster
  
  library(foreign, lib.loc = "../packages/")
  library(dplyr, lib.loc = "../packages/")
  
  # GLOBAL CHECKS
  # check target table exists
  if(!file.exists(targetTab)){
    stop(paste0("File does not exist: ", targetTab))
  }
  
  # check outfile has .csv
  if(!grepl(".csv$", outFile)){
    stop(paste0("File should end in .csv: ", outFile))
  }
  
  # BUILDER METHOD #####
  ######################
  if(nchar(buildCsv) > 0){
    
    print("Running builder method...")
    
    # CHECKS 
    ##############################################
    
    # check catchments exist and CATCHNUM column present
    if(!grepl(".dbf$",catchDbf)){
      stop(paste0("catchment file --- ", outFile, " --- needs to end in '.dbf'"))
    }
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
      
      if(a %% 50 == 0){print(paste0("Processing benchmark ",a, " of ", buildCount))}
      
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
  }
  
  # SHP METHOD ###
  ################
  
  if(nchar(shpPath) > 0){
    
    print("Running shapefile method...")
    
    # CHECKS
    # load shapefile if it exists, check it has PB column
    if(file.exists(shpPath)){
      shp <- shapefile(shpPath)
      if(!"PB" %in% names(shp)){
        stop(paste("No 'PB' column in file: ", shpPath))
      }
    } else{
      stop(paste0("File does not exist: ", shpPath))
    }
    
    # if builder method ran, check PB names are unique
    if(nchar(buildCsv) > 0){
      if(any(as.vector(table(c(shp@data$PB, as.character(yt2$networks))))>1)){
        warning("PB names are not unique.")
      }
    }
    
    # Check raster exists and open
    if(file.exists(clipRaster)){
      ras <- raster(clipRaster)
    } else{
      stop(paste0("File does not exist: ", clipRaster))
    }
    
    # cell size numeric
    cellsize <- as.numeric(cellsize)
    disaggregate.factor <- as.numeric(disaggregate.factor)
    
    cellarea <- ((cellsize / disaggregate.factor) * (cellsize / disaggregate.factor)) / 1000000
    
    ##### RUN CALCS FOR EACH PB IN SHP
    
    # disaggregate raster using disaggregate.factor  ##################
    rasD <- disaggregate(ras, disaggregate.factor, method = '')
    
    # clip raster
    ext <- extract(rasD, shp)
    names(ext) <- shp@data$PB
    
    # read in target table
    y <- read.csv(targetTab)
    
    # for each PB, calc class area for each class in target table, then add proportions to y table
    counter <- 1
    for(pb in names(ext)){ # for each PB
      
      if(counter %% 50 == 0){print(paste0("Processing benchmark ",counter, " of ", buildCount))}
      
      rasVals <- ext[[pb]] # get values
      
      for (v in y$class) { # for each row in the target table
        
        cArea <- length(rasVals[rasVals %in% v]) * cellarea # area of class in PB (km2)
        t <- y$target_km2[y$class == v] # target (km2)
        
        if(cArea < t){ # if less than target, report proportion
          y[[pb]][y$class == v] <- round((cArea / t), 2)
        } else if(cArea >= t){ # if greater than or equal to target, report 1
          y[[pb]][y$class == v] <- 1.00
        }
      }
      counter <- counter + 1
    }
    y.names <- paste0("class",y$class)
    
    # transpose
    yt <- y %>%
      select_(.dots = names(ext)) # select results columns only
    yt3 <- data.frame(names(yt), t(yt)) # merge names with transposed results
    names(yt3) <- c("networks",y.names)
  }
  
  ### Merge tables, or carry existing table through if only one method used #########
  if(nchar(buildCsv) > 0 & nchar(shpPath) > 0){
    # check columns match
    if(!all(names(yt2) %in% names(yt3)) & all(names(yt3) %in% names(yt2))){
      stop(paste0("Results class column from builder and shapefile method do not match\n", "builder method results column names: ", paste0(names(yt2),collapse=", "), "\n", "shapefile method results column names: ", paste0(names(yt3),collapse=", ")))
    }
    # merge tables
    ytOut <- rbind(yt2, yt3)
  } else if(nchar(buildCsv) > 0 & nchar(shpPath) == 0){
    ytOut <- yt2
  } else if(nchar(buildCsv) == 0 & nchar(shpPath) > 0){
    ytOut <- yt3
  }
  
  write.csv(ytOut, outFile, quote=T, row.names=F)
}
