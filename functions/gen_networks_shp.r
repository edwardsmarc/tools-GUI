# Marc Edwards - BEACONs
# Feb 15 2017
# Function to build networks of shapefile. Function takes a shp file of benchmarks, and a list of networks in the "PB_1_PB_2_PB_3" format, and produces a shp file with a new row for each benchmark in each network, grouped by adding a network column.

# Note - only run this function on one process at a time. If the function is running twice and writing chunks to the same tmp file, the chunks will get mixed up.
  
gen_networks_shp <- function(baFile, idColBa, networksCsv="", randomNet="", nBA="", outFile, Dissolve = FALSE, prjFile, tmp){
  
  
  # i. combine each benchmark in the network into a shapefile called netRows. e.g. triples network: 3 rows
  # ii. add network name and number identifiers to the attribute table
  # iii. merge netRows into a master shapefile called chunk
  # iv. continually adding rows to the master shapefile gets very slow as the shapefile gets bigger. Solution is to save the chunk every 300 networks and restart with a new chunk. Smaller chunks could be faster.
  # v. Once all networks have been added to a chunk, merge the chunk shapefiles together to get the final table.
  
  # baShp: shape file of benchmarks with a "PB" column listing benchmark names
  # idColBa: unique idenifier column for each benchmark (e.g. PB)
  # networksCsv: csv file with a "networks" column listing networks to be made 
  # outFile: file path and name of new shape file with .shp extension
  # tmp: a temporary directory to save the CHUNKS. Any chunk files in the tmp folder at the start of the process are deleted.
  
  # NOTE: file size for ecoregion 139 which only has 54 single benchmarks and ~24000 networks is 1.2 gb. Could easily run over the 3gb shp file limit. If so will have to add code to split into multiple files. Maybe based on the number of chunks it takes to get to a certain file size. might be best to limit it to 1gb per shp.
  
  library(sp, lib.loc = "../packages/")
  library(raster, lib.loc = "../packages/")
  library(maptools, lib.loc = "../packages/")
  library(rgeos, lib.loc = "../packages/")
  
  # CHECKS
  ##########################################
  # check baShp exists - then open
  if(file.exists(baFile)){
    baShp <- shapefile(baFile)
    if(!idColBa %in% names(baShp)){
      stop(paste0("No ", idColBa, " column in file: ", baFile))
    }
  } else{
    stop(paste0("File does not exist: ", baFile))
  }
  
  # Remove any left over SP_ID columns from previous processing
  for(x in names(baShp)){
    if(grepl("SP_ID",x)==TRUE)
      baShp@data[[x]] <- NULL
  }
  
  # create tmp if it doesn't exists
  if(!dir.exists(tmp)){
    dir.create(tmp, recursive = TRUE)
  }
  
  # remove any existsing chunks from tmp
  file.remove(list.files(tmp, pattern=c("CHUNK"), full.names=TRUE))
  
  
  # Check prjFile exists
  if(!file.exists(prjFile)){
    stop(paste0("File does not exist: ", prjFile))
  }
  
  ## NETLIST ##
  # Check networksCsv exists and has networks column - then get netList
  if(nchar(networksCsv) > 0 & nchar(randomNet) > 0){stop("Only one of 'Networks file' or 'random number' can be provided.")} # either generate provided networks, or random selection of all networks. Can't do both.
  
  if(nchar(networksCsv) == 0 & nchar(randomNet) == 0){stop("One of 'Networks file' or 'random number' needs to be provided.")}
  
  if(nchar(networksCsv) == 0 & nchar(randomNet) > 0){ # if using random generation method...
    
    # make nBA  and randomNets numeric
    if(nchar(nBA) > 0){
      nBAval <- as.numeric(nBA)
    }
    if(nchar(randomNet) > 0){
      randomNetval <- as.numeric(randomNet)
    }
    
    # make full list of networks
    singles <- baShp@data$PB
    pbs <- combn(singles, nBAval, simplify=FALSE) # simplify=FALSE returns a list
    netListFull <- sapply(pbs, function(x) paste0(x,collapse="_")) # this works for any nBA value - makes network names from the list of benchmarks
    
    # if sample number is less than full, list, take the random sample, otherwise use the full list
    if(randomNetval < length(netListFull)){
      netList <- sample(x = netListFull, size = randomNetval, replace = FALSE) # takes random sample without replacement
    } else{
      netList <- netListFull
    }
  }
  
  # if using user provided networks...
  if(nchar(networksCsv) > 0 & nchar(randomNet) == 0){
    if(file.exists(networksCsv)){
      netFile <- read.csv(networksCsv)
      if("networks" %in% names(netFile)){
        netList <- as.character(netFile$networks)
      } else{
        stop(paste0("No 'networks' column in file: "), networksCsv)
      }
    } else{
      stop(paste0("File does not exist: ", networksCsv))
    }
  }
  
  print("building networks shapefile")
  
  # BUILD CHUNKS
  #######################################################
  
  rowName <- 1
  network <- 1
  chunkCount <- 1
  for (net in netList){
    if(network==1 | network %% 100==0){print(paste0("Processing network ",network, " of ", length(netList)))}

    # make list of singles included in each pb. Works for any nBA
    splits <- strsplit(net, split="_")[[1]]
    bas <- c()
    for(i in 1:length(splits)){
      if((i + 1) %% 2 == 0){
        bas <- c(bas, paste0(splits[i], "_", splits[i+1]))
      }
    }
    
    # merge into netRows
    row <- 1
    for(pb in bas){
      if(pb %in% baShp@data[[idColBa]]){ # ADD ERROR CATCHING FOR WHEN PB1 NOT IN BASHP
        row1 <- baShp[baShp@data[[idColBa]]==pb,] # subset to get shp file of pb1
        row1 <- spChFIDs(row1, as.character(rowName)) # assign unique row IDs
        row1@data$network_id <- network
        row1@data$networks <- net
        rowName <- rowName + 1
        if(row == 1){
          netRows <- row1
          row <- row + 1
        } else{
          netRows <- spRbind(netRows,row1)
          row <- row + 1
        }
      } else{
        stop(paste0(pb, " not in file: ", baFile))
      }
    }
    
    # make chunk file
    if(network==1 | (network%%300==1)==TRUE){ #restart building chunk every 300 rows
      chunk <- netRows
    } else{
      chunk <- spRbind(chunk,netRows)
    }
    if((network%%300==0)==TRUE | network==length(netList)){ # save chunk every 300 networks
      
      # note when shp gets saved the row names are automatically changed to 0:length(rows). SP_ID column gets added and takes the old row names so use SP_ID when joining chunks together at the end.
      print("Saving chunk")
      writePolyShape(chunk, paste0(tmp, "/", chunkCount, "_CHUNK.shp"))
      file.copy(prjFile, paste0(tmp, "/", chunkCount, "_CHUNK.prj"))
      
      chunkCount <- chunkCount + 1
    }
    network <- network + 1  
  }
  
  # JOIN CHUNKS
  ###############################################
  
  # Join chunks together to get masterShp using SP_ID as the row names
  print("joining chunks...")
  chunkFiles <- list.files(paste0(tmp), pattern=c("CHUNK.shp"), full.names=TRUE)
  counter <- 1
  for(i in chunkFiles){
    cShp <- shapefile(i)
    cShp <- spChFIDs(cShp, cShp@data$SP_ID) # change row names back to original names that were assigned using SP_ID.
    if(counter==1){
      masterShp <- cShp
    } else{
      masterShp <- spRbind(masterShp, cShp) # join all chunks together
    }
    counter <- counter + 1
  }
  masterShp <- spChFIDs(masterShp, masterShp@data$SP_ID) # check that all row names match SP_ID
  masterShp@data$SP_ID <- NULL # remove the SP_ID column to clean.
  masterShp <- spTransform(masterShp, proj4string(baShp)) # set projection to match baShp
  
  
  # SAVE
  #############################################
  print("saving shp...")
  writePolyShape(masterShp, outFile) # save shp
  file.copy(prjFile, paste0(substr(outFile, 1, nchar(outFile)-4), ".prj"))
  
  print("deleting chunks...")
  file.remove(list.files(tmp, pattern=c("CHUNK"), full.names=TRUE))
  
  # DISSOLVE
  if(Dissolve == TRUE){
    
    print("Dissolving...")
    
    # Check topology and fix
    if(!gIsValid(masterShp)){
      masterShp <- buffer(masterShp, 0, dissolve = FALSE)
    }
    masterShp_dslv <- unionSpatialPolygons(masterShp, masterShp@data$networks) #dissolve, row.names becomes networks field
    
    # delete sliver polygons
    for(i in 1:length(masterShp_dslv@polygons)){
      jj <- length(masterShp_dslv@polygons[[i]]@Polygons)
      dList <- NULL
      for (j in 1:jj) { # makes list of polygons with area < 1
        if (masterShp_dslv@polygons[[i]]@Polygons[[j]]@area < 1) {
          dList <- c(dList, j)
        }
      }
      if (length(dList) > 0) { # delete dList polygons
        counter <- 0
        for (d in dList) {
          masterShp_dslv@polygons[[i]]@Polygons[[d-counter]] <- NULL
          counter = counter + 1
        }
      }
    }
    
    df <- data.frame(networks=row.names(masterShp_dslv)) # make attribute table
    row.names(df) <- df$networks
    masterShp_dslv_df <- SpatialPolygonsDataFrame(masterShp_dslv, df) # join dissolved polygons and attribute table
    
    # copy back network ids
    masterShp_dslv_df@data$network_id <- 0
    for(d in as.character(masterShp_dslv_df@data$networks)){
      masterShp_dslv_df@data$network_id[masterShp_dslv_df@data$networks == d] <- masterShp@data$network_id[masterShp@data$networks == d][1]
    }
    
    # order columns and rows
    masterShp_dslv_df <- masterShp_dslv_df[order(masterShp_dslv_df@data$network_id),c("network_id","networks")]
    row.names(masterShp_dslv_df) <- as.character(1:nrow(masterShp_dslv_df))
    
    # save
    writePolyShape(masterShp_dslv_df, paste0(substr(outFile, 1, nchar(outFile)-4), "_dslv.shp")) # save shp
    file.copy(prjFile, paste0(substr(outFile, 1, nchar(outFile)-4), "_dslv.prj"))
  }
}