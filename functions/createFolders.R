# Marc Edwards - BEACONs
# Feb 6 2017
# function to create folder structure for networking tools

createFolders <- function(parent, input=TRUE, results=TRUE, benchmarks=TRUE, networks=TRUE, other=""){
  
  # check parent exists
  if(!dir.exists(parent)){
    stop("Parent directory does not exist")
  }
  
  # for each file, check if it already exists, then create
  if(input==TRUE){
    if(!dir.exists(paste0(parent, "/input"))){
      dir.create(paste0(parent, "/input"))
    } else{
      print(paste0("Directory already exists: ", paste0(parent, "/input")))
    }
  }
  
  if(results==TRUE){
    if(!dir.exists(paste0(parent, "/results"))){
      dir.create(paste0(parent, "/results"))
    } else{
      print(paste0("Directory already exists: ", paste0(parent, "/results")))
    }
  }
  
  if(benchmarks==TRUE){
    if(!dir.exists(paste0(parent, "/benchmarks"))){
      dir.create(paste0(parent, "/benchmarks"))
    } else{
      print(paste0("Directory already exists: ", paste0(parent, "/benchmarks")))
    }
  }
  
  if(networks==TRUE){
    if(!dir.exists(paste0(parent, "/networks"))){
      dir.create(paste0(parent, "/networks"))
    } else{
      print(paste0("Directory already exists: ", paste0(parent, "/networks")))
    }
  }

  if(nchar(other)>0){
    if(!dir.exists(paste0(parent, "/", other))){
      dir.create(paste0(parent, "/", other))
    } else{
      print(paste0("Directory already exists: ", paste0(parent, "/", other)))
    }
  }
}