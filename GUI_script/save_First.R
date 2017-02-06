.First <- function(){
  source("test_GUI_v1.R")
  gui <<- test_GUI()
}

save(.First, file = "test_GUI.RData")