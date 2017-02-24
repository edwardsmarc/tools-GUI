.First <- function(){
  
  source("../functions/GUI_v2.R")
  gui <<- GUI_v2()
}

save(.First, file = "../GUI/GUI_V2.RData")