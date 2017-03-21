# Marc Edwards - BEACONs
# Feb 20 2017
# function for adding dissimilarity metrics to shapefiles. Should work with both benchmark and network shapefiles

metricsDissimilarity <- function(netPath, idColNet, baPath, idColBa, raster1, raster2, continuous, newCol, plotDir=""){
  
  # Calculates dissimilarity statistics for networks of denovo benchmarks. 
  # raster2 is clipped to each network in netShp. The distribution in the resulting clipped raster is compared to the distribution of raster 1.
  # raster1 and raster2 should be either the continuous variable maps that the representation maps were derived from (e.g. cmi map), or a categorical map such as land cover. If continuous = TRUE, raster must contain continuous values.
  
  # netShp - network shapefile made by buildNetworksShp.R function
  # idColNet - unique ID for networks in netShp (e.g. "netName")
  # baPath - benchmark shapefile of individual benchmarks. Must contain all benchmarks that appear in networks in netShp. If netShp contains networks of single benchmarks, netShp and baShp can be the same.
  # idColBa - unique ID column in baShp
  # raster1 - file path to the raster file that each network will be referenced against.
  # raster2 - the raster that is clipped to each networks shapefile. Must cover the extent of all networks. Can be the same raster as raster1 as long as all networks are covered.
  # continuous - logical, if true, ks test used, if false, bray curtis used.
  # newCol - name of new column to be made. Should not contain spaces
  
  library(sp, lib.loc = "../packages/")
  library(raster, lib.loc = "../packages/")
  library(maptools, lib.loc = "../packages/")
  library(ggplot2, lib.loc = "../packages/")
  
  # CHECKS
  ######################################################################
  
  # Check rasters exist and open
  if(file.exists(raster1)){
    r1 <- raster(raster1)
  } else{
    stop(paste0("File does not exist: "), raster1)
  }
  if(file.exists(raster2)){
    r2 <- raster(raster2)
  } else{
    stop(paste0("File does not exist: "), raster2)
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
  
  # Make plot directory
  # if(nchar(plotDir)>0){
  #   if(dir.exists(plotDir)==FALSE){
  #     dir.create(plotDir, recursive=TRUE)
  #   }
  # }
  
  # PROCESSING
  #################################################################################
  
  # Get reference values
  refVals <- values(r1)
  
  # get list of networks
  netList <- unique(netShp@data[[idColNet]])
  
  # get list of all benchmarks present in netList
  baList2 <- unique(unlist(lapply(netList, function(x){
    splits <- strsplit(x, split="_")[[1]]
    pbList <- c()
    for(i in 1:length(splits)){
      if((i + 1) %% 2 == 0){
        pbList <- c(pbList, paste0(splits[i], "_", splits[i+1]))
      }
    }
    pbList
  })))
  
  ### Make raster library: This loop makes a list of clipped rasters and names each element in the list (e.g. "cmi_PB_0001"). Can then access the rasters later on by calling the unique name.
  print("Making raster library")

  counter <- 1
  rasLib <- list()
  for(ba in baList2){ 
    if(counter==1 | counter %% 50 == 0){print(paste0("Raster library: ", counter, " of ", length(baList2)))}
    baPoly <- baShp[baShp@data[[idColBa]]==ba,]
    rasLib[[ba]] <- mask(r2, baPoly)
    counter <- counter + 1
  }
  
  print(paste0("Calculating dissimilarity"))
  
  # pre-allocate columns
  if(continuous == TRUE){
    netShp@data[[newCol]] <- 0 # pre-allocate columns
  } else{
    netShp@data[[newCol]] <- 0 # pre-allocate columns
  }
  
  counter <- 1
  for(net in unique(netShp@data[[idColNet]])){ # this loop makes a networks raster based on the network name and gets the values. ME Nov 2 2016, added unique() to avoid repetition of calcs
    if(counter %% 1000==0){print(paste0("Calculating network ", counter, " of ", length(unique(netShp@data[[idColNet]]))))}
    
    # add all rasters in network to baRasList - can be any combination of BAs and/or PAs as long as they are in the library
    baRasList <- list()
    splits <- strsplit(net, split="_")[[1]]
    baList <- c()
    for(i in 1:length(splits)){
      if((i + 1) %% 2 == 0){
        baList <- c(baList, paste0(splits[i], "_", splits[i+1]))
      }
    }
    baRasList <- lapply(baList, function(x) rasLib[[x]])
    
    netVals <- values(do.call(merge, baRasList))
    
    # run calcs
    if(continuous == TRUE){ # calculate statistic for KS
      if(nchar(plotDir)>0){ # make plot if directory provided
        plotOut <- paste0(plotDir, "/", newCol, "_", net, ".png")
        title <- paste0(newCol, ":   ", net)
        netShp@data[[newCol]][netShp@data[[idColNet]]==net] <- as.numeric(ksPlot(refVal = refVals, netVal = netVals, plotTitle = title, saveAs = plotOut))
      } else{ # otherwise just return value
        netShp@data[[newCol]][netShp@data[[idColNet]]==net] <- as.numeric(ksPlot(refVals, netVals))
      }
    } else{ # calculate statistic for BC
      if(nchar(plotDir)>0){ # make plot if directory provided
        plotOut <- paste0(plotDir, "/", newCol, "_", net, ".png")
        title <- paste0(newCol, ":   ", net)
        netShp@data[[newCol]][netShp@data[[idColNet]]==net] <- as.numeric(bcPlot(refVals, netVals, plotTitle = title, saveAs = plotOut)) 
      } else{ # otherwise just return value
        netShp@data[[newCol]][netShp@data[[idColNet]]==net] <- as.numeric(bcStat(refVals, netVals)) 
      }
    }
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
  # doesn't save prj file because we are overwriting the original netShp so original prj file will be preserved.
}
