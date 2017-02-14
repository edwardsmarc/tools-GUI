# Marc Edwards - BEACONs
# Feb 13 2017
# networkRep function for tools GUI. Based on netRep.R from NWB which was in turn based on addBAs2PAs from PBA.

networkRep <- function(singlesCsv,nBA,outFile,filterCsv="") {
  
  # Function designed to evalutate representation for networks of benchmarks using the target table and the denovo singles results as input
  # The input singles should be in long format, with a networks column listing benchmarks in the standard PB_xxx format. Each class should have a column with a value between 0 and 1 indicating the proportion of the target met by the benchmark (i.e. the output table from the benchmarksRep function)
  
  # singlesCsv: results table for denovo singles, long format with columns: networks...class1...class2...class3...etc.
  # outTab: file path to save output table to. Output table will have same columns as input but network names will be new
  # nBA: number of benchmarks per network to calculate. All combinations will be tested.
  # filterCsv: if provided, this list will over-ride the default of all combinations. Use this if you only want to test a subset of possible networks. Must be a file path to a csv table with a "networks" column containing the network names to be processed. Network name should be of the format: PB_1_PB_2 for a network made up of two benchmarks named PB_1 and PB_2. 
  
  library(dplyr)
  library(readr)
  
  # CHECKS 
  ##############################################
  
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
  
  # check outfile has .csv
  if(!grepl(".csv$",outFile)){
    stop(paste0("Outfile --- ", outFile, " --- needs to end in '.csv'"))
  }
  
  # Create output folder if it doesn't exist 
  outFile_spl <- strsplit(outFile, "/")[[1]]
  outDir <- paste0(outFile_spl[1:(length(outFile_spl)-1)], collapse="/")
  if (!file.exists(outDir)) {
    dir.create(outDir, recursive=TRUE)
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
    nets <- sapply(pbs, function(x) paste0(x,collapse="_")) # this works for any nBA value - makes network names from the list of benchmarks
  }
  
  # COMBINE BENCHMARKS INTO NETWORKS
  ##############################################
  
  print(paste0("Creating BA x ", nBA, " networks from table: ", singlesCsv))
  
  # split networks into groups of 5000
  grps <- split(nets, ceiling(seq_along(nets)/5000))
  
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
            select(-contains("networks")) %>%
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
      ynets <- grpTab
    } else{
      ynets <- rbind(ynets,grpTab)
    }
    ynets$networks <- as.factor(ynets$networks)
  }
  
  print("saving...")
  write_csv(ynets, outFile)
}
