# Marc Edwards - BEACONs
# Feb 14 2017
# GUI version of rankAddedSolution.R - this version just requires a dataDir folder, it will then go and find the criteria results and targets in that folder

results_summary <- function(criteria, summaryDir, rareThreshold = 0, gapThreshold = 100, outFile) {
  
  # criteria - string of criteria to include in results table. seperated by commas.
  
  # summaryDir - directory that the tables to be summarized are stored in. Function will look ofr one results table and one target table for each  criteria (criteria name nust be surround by underscores). If more than one results or target table are found for a criteria, the function will fail.
    # results files must have e.g. "_cmi_" in file name. Must not have the workd  "target".
    # target tables must have both "target" and "cmi" in filename (e.g. target_cmi.csv)
  
  # rareThreshold - only classes with bnd_prop column greater than this percentage value are counted as gaps.
  
  # gapThreshold - the percentage of the target needed to be met. Defaults to 100%. If set to e.g. 90%, gaps only counted if <90% of the target is met.
  
  # outFil - directory to save table in
  
  # results table must have 'networks' column.
  # target table must have 'bnd_prop' column
  
  library(dplyr, lib.loc = "../packages/")
  
  # CHECKS 
  ##############################################
  
  # convert criteria string to vector
  criteriaV <- gsub(" ","",unlist(strsplit(criteria,","))) # converts string from GUI into vecotr. Must be separated by commas, spaces removed if present
  
  # Checks done in code below:  
    # check a table exists for each criteria provided. Print tables to be summarised, print if any criteria are missing tables
    # check target tables exist for all criteria provided
    # check target tables all have bnd_prop column
    # check results tables all have networks column
  
  # check outfile has .csv
  if(!grepl(".csv$",outFile)){
    stop(paste0("Outfile --- ", outFile, " --- needs to end in '.csv'"))
  }
  
  # Create output folder if it doesn't exist 
  # outFile_spl <- strsplit(outFile, "/")[[1]]
  # outDir <- paste0(outFile_spl[1:(length(outFile_spl)-1)], collapse="/")
  # if (!file.exists(outDir)) {
  #   dir.create(outDir, recursive=TRUE)
  # }
  
  
  # SUMMARISE RESULTS FOR EACH CRITERIA, THEN COMBINE
  ##############################################
  
  rareProp <- as.numeric(rareThreshold)/100
  gapProp <- as.numeric(gapThreshold)/100
  
  # list all csv files in summeryDir
  fileList <- list.files(summaryDir)
  
  counter <- 1
  for(crit in criteriaV){
    
    print(paste0("processing... ", crit))
    
    # select results table
    resDir <- fileList[grepl(paste0("_",crit,"_"), fileList)] # select files with e.g. "_cmi_
    resDir <- resDir[!grepl("target", resDir)] # remove any files with "target"
    
    # error if more than one, or zero files selected
    if(length(resDir)>1){
      stop(paste0("More than one results file in summaryDir: ", paste0(resDir,collapse="; "))) # check only one was selected
    }
    if(length(resDir)==0){
      stop(paste0("No results tables in the summaryDir for criteria: ", crit))
    }
    resDir <- paste0(summaryDir, "/", resDir)
    
    # select target table
    targetDir <- fileList[grepl(paste0("_",crit,"_"), fileList)] # select files with e.g. "_cmi_
    targetDir <- targetDir[grepl("target", targetDir)]
    
    # error if more than one, or zero files selected
    if(length(targetDir)>1){
      stop(paste0("More than one target file in summaryDir: ", paste0(targetDir,collapse="; ")))
    }
    if(length(targetDir)==0){
      stop(paste0("No target tables in the summaryDir for criteria: ", crit))
    }
    targetDir <- paste0(summaryDir, "/", targetDir)
    
    
    if (file.exists(resDir) & file.exists(targetDir)){
      
      # open tables
      dfResults <- as.data.frame(read.csv(resDir)) 
      dfTarget <- as.data.frame(read.csv(targetDir))
      
      # check they have the same classes
      dfTarget$class2 <- make.names(paste0("class",dfTarget$class)) # add class column to match results column names
      for(i in dfTarget$class2){
        if(!i %in% names(dplyr::select(dfResults,-starts_with("networks")))){
          stop("class names do not match")
        }  
      }
      
      # Check results table has "networks" column
      if(!"networks" %in% names(dfResults)){
        stop(paste0("No 'networks' column in results table: "), resDir)
      }
      
      # Check target table has "bnd_prop" column
      if(!"bnd_prop" %in% names(dfTarget)){
        stop(paste0("No 'bnd_prop' column in results table: "), targetDir)
      }
      
      # identify non-rare classes - make list of names
      nonrare <- dfTarget %>%
        filter(bnd_prop>=rareProp) %>%
        .$class2 # . is stand in for the data frame, so this prints the class2 column as a vector
      
      # prepare summarized results
      dfResults$gaps <- apply(dfResults[colnames(dfResults) %in% nonrare], 1, function(x) length(x[x<gapProp]))
      dfResults <- dplyr::select(dfResults, networks,gaps)
      names(dfResults)[names(dfResults)=="gaps"] <- paste0(crit,"_gaps") # change col name
      
      # make master table
      if(counter==1){
        dfMaster <- dfResults
      } else{
        dfMaster <- merge(dfMaster, dfResults, by="networks") # only keeps common networks
      }
      
    } else{
      stop(paste0("Either ", dfDir, " or ", targetDir, " do not exist."))
    }
    counter <- counter + 1
  }
  dfMaster$Sum <- rowSums(dfMaster[!names(dfMaster) %in% "networks"])
  dfMaster <- dfMaster[order(dfMaster$Sum),]
  write.csv(dfMaster, outFile, quote=F, row.names=F)
}