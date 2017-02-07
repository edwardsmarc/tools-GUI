# GUI for networking tools
# gWidgets is a high level package to create simple GUI's. It provides a common set of functions for accessing more complex GUI toolkits -  tcl/tk, Gtk, Java, Qt - it doesn't provide as much customization as using the underlying toolkits. 

# naming convention for groups (e.g. gp_1_parent) and objects:
# gp_
# 1_: the tab number
# parent: the argument being provided
# _btn; _lbl; _check - widget type

# all the elements in a widget need to be assigned to a container group. And each container group needs to be assigned to a parent container.

rm(list=ls())


GUI_v1 <- function(){
  
#library(gWidgets)
library(gWidgets2)
#library(gWidgetstcltk) # using a different package here will access a different GUI toolkit.
library(gWidgets2tcltk) # using a different package here will access a different GUI toolkit.
source("../functions/createFolders.R")

  
# set up high level containers here
win <- gwindow("GUI_v1") # the full window container that will hold everything
##
g <- ggroup(container = win, horizontal = FALSE, expand = TRUE) # a high level group that will hold everything
nb <- gnotebook(container = g) # a notebook container
##
nb1 <- ggroup(container = nb, label = "folders", horizontal = FALSE)
#nb2 <- ggroup(container = nb, label = "denovoRep", horizontal = FALSE)
##


###### tab 1 - folder set up ############################################################

# browse to select parent directory where folders will be added
gp_1_parent <- ggroup(container = nb1) 
gp_1_parent_lbl <- glabel("Parent folder: ", container = gp_1_parent) # this is text label for the group
gp_1_parent_btn <- gfilebrowse( # file browser - allows selection of file 
  text = "", 
  type = "selectdir", 
  quote = TRUE, 
  container = gp_1_parent, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Parent directory: ", svalue(gp_1_parent_btn))
  }
)

# checkboxes to select new folders to add
gp_1_folders <- ggroup(container = nb1, horizontal = FALSE)
gp_1_folders_lbl <- glabel("Create folders: ", container = gp_1_folders)
gp_1_folders_check_input <- gcheckbox(text = "input", checked=TRUE, container = gp_1_folders)
gp_1_folders_check_results <- gcheckbox(text = "results", checked=TRUE, container = gp_1_folders)
gp_1_folders_check_benchmarks <- gcheckbox(text = "benchmarks", checked=TRUE, container = gp_1_folders)
gp_1_folders_check_networks <- gcheckbox(text = "networks", checked=TRUE, container = gp_1_folders)

# optional other filenames to add
gp_1_folders_other <- ggroup(container = gp_1_folders, horizontal = TRUE)
gp_1_folders_other_lbl <- glabel("Other: ", container = gp_1_folders_other)
gp_1_folders_other_txt <- gedit(text = "", container = gp_1_folders_other)

# button to run the createFolders function
gp_1_run <- ggroup(container = nb1)
gp_1_run_btn <- gbutton(
  text = "Create folders",
  container = gp_1_run,
  handler = function(h, ...){ 
    tryCatch({
    createFolders(
      parent = svalue(gp_1_parent_btn), 
      input = svalue(gp_1_folders_check_input), 
      results = svalue(gp_1_folders_check_results), 
      benchmarks = svalue(gp_1_folders_check_benchmarks), 
      networks = svalue(gp_1_folders_check_networks), 
      other = svalue(gp_1_folders_other_txt))
    
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Folders created")
    }, error = function(cond){
      message("Could not create folders")
      message(cond)
    }
    )
  }
)




# output text box
frmOutput <- gframe("Output", container = g)
txtOutput <- gtext("", container = frmOutput)








###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
# make a button to upload a csv file
#grp_upload <- ggroup(container = nb1)
btn_upload <- gbutton(
  text = "Upload csv file", # button widget has an inbuilt text label argument
  #container = grp_upload,
  container = grp_name,
  handler = function(h, ...){ # the handler argument takes a function that runs when the button is pressed
    gfile( # widget to select file
    text = "Upload csv file",
    type = "open", # opens file, can also save
    handler = function(h, ...){ # tell gfile what to do with the file
      tryCatch({
        data_frame_name <- make.names(svalue(txt_data_frame_name)) # get data frame name from other widget
        the_data <- read.csv(h$file) # read the data - h$file is the csv you selected
        assign(data_frame_name, the_data, envir = globalenv())
        #svalue(status_bar) <- paste0(nrow(the_data), " records saved to variable ", data_frame_name)
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", nrow(the_data), " records saved to variable ", data_frame_name)
      #}, error = function(e) svalue(status_bar) <- "Could not upload data"
      }, error = function(e) svalue(txtOutput) <- "Could not upload data"
      )
    },
    filter = list("All files" = list(patterns = c("*")))
    )
  }
)

gseparator(horizontal = TRUE, container = nb1, expand=TRUE)

# new groups to add a column
grp_newCol <- ggroup(container = nb1)
lbl_newCol <- glabel("New column name: ", container = grp_newCol)
txt_newCol <- gedit("new_col", container = grp_newCol)

grp_newColVal <- ggroup(container = nb1)
lbl_newColVal <- glabel("New column value: ", container = grp_newColVal)
txt_newColVal <- gedit("NewNewNew", container = grp_newColVal)

grp_addCol <- ggroup(container = nb1)
btn_addCol <- gbutton(
  text = "Add column",
  container = grp_addCol,
  handler = function(h, ...){
    df <- get(data_frame_name)
    df[[svalue(txt_newCol)]] <- svalue(txt_newColVal)
    assign(data_frame_name, df, envir = globalenv())
  }
)

gseparator(horizontal = TRUE, container = nb1, expand = TRUE)

# button to save the table
grp_save <- ggroup(container = nb1)
btn_save <- gbutton(
  text = "Save csv file", # another button to save the csv with the gfile function
  container = grp_save,
  handler = function(h, ...){ 
    gfile( 
      text = "Save csv file",
      type = "save",
      handler = function(h, ...){ 
        tryCatch({
          df2 <- get(data_frame_name)
          write.csv(df2, h$file, row.names = FALSE)
          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "File saved as: ", h$file)
        }, error = function(e) svalue(status_bar) <- "Could not save data"
        )
      },
      filter = list("All files" = list(patterns = c("*")))
    )
  }
)
##########################################################################################

# denovoRep tab

## catch dbf
grp_catchDBF <- ggroup(container = nb2)
lbl_catchDBF <- glabel("Select catchment dbf file: ", container = grp_catchDBF)

# gfilebrowse allows typing or selecting filename, selected filename accessed through svalue()
catchDBF <- gfilebrowse( 
  text = "select file", 
  type = "open", 
  quote = TRUE, 
  container = grp_catchDBF, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "catchDBF: ", svalue(catchDBF))
  }
)

## build csv
grp_buildCSV <- ggroup(container = nb2)
lbl_buildCSV <- glabel("Select builder csv file: ", container = grp_buildCSV)

buildCSV <- gfilebrowse( 
  text = "select file", 
  type = "open", 
  quote = TRUE, 
  container = grp_buildCSV, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "buildCSV: ", svalue(buildCSV))
  }
)

## out directory
grp_outDir <- ggroup(container = nb2)
lbl_outDir <- glabel("Select output csv directory: ", container = grp_outDir)

outDir <- gfilebrowse( 
  text = "select file", 
  type = "save", 
  quote = TRUE, 
  container = grp_outDir, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "output csv directory: ", svalue(outDir))
  }
)

## out file
grp_outFile <- ggroup(container = nb2)
lbl_outFile <- glabel("Select output csv file path: ", container = grp_outFile)

outFile <- gfilebrowse( 
  text = "select file", 
  type = "save", 
  quote = TRUE, 
  container = grp_outFile, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "output csv file path: ", svalue(outFile))
  }
)

## target table
grp_targTab <- ggroup(container = nb2)
lbl_targTab <- glabel("Select target table csv: ", container = grp_targTab)

targTab <- gfilebrowse( 
  text = "select file", 
  type = "open", 
  quote = TRUE, 
  container = grp_targTab, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "target table: ", svalue(targTab))
  }
)

# crit
grp_crit <- ggroup(container = nb2)
lbl_crit <- glabel("Type criteria name", container = grp_crit)
crit <- gedit(text = "e.g. cmi", 
              container = grp_crit,
              handler = function(h,...){
                svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "criteria: ", svalue(crit))
              })

# button to run the denovoRep function
grp_denovoRep <- ggroup(container = nb2)
btn_denovoRep <- gbutton(
  text = "Run denovoRep tool",
  container = grp_denovoRep,
  handler = function(h, ...){ 
    denovoRep(catchDbf = svalue(catchDBF), 
              buildCsv = svalue(buildCSV), 
              outDir = svalue(outDir), 
              outTab = svalue(outFile), 
              targetTab = svalue(targTab), 
              crit = svalue(crit), 
              filterList = c())
  }
)
##########################################################################################

#status_bar <- gstatusbar("", container = win)
frmOutput <- gframe("Output", container = g)
txtOutput <- gtext("", container = frmOutput)


}