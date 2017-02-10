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
#library(gWidgets2)
#library(gWidgetstcltk) # using a different package here will access a different GUI toolkit.
library(gWidgets2tcltk) # using a different package here will access a different GUI toolkit.
#library(gWidgets2RGtk2)
#library(gWidgetsRGtk2)
source("../functions/createFolders.R")
source("../functions/gen_ba_shp.r")
source("../functions/gen_target_table.R")

  
# set up high level containers here
win <- gwindow("GUI_v1", toolkit = guiToolkit("tcltk")) # the full window container that will hold everything
##
g <- ggroup(container = win, horizontal = FALSE, expand = TRUE) # a high level group that will hold everything
nb <- gnotebook(container = g) # a notebook container
##
nb1 <- ggroup(container = nb, label = "setup", horizontal = FALSE)
nb2 <- ggroup(container = nb, label = "benchmark shp", horizontal = FALSE)
nb4 <- ggroup(container = nb, label = "target tables", horizontal = FALSE)
##


###### tab 1 - folder set up and projection file ############################################################

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

gseparator(horizontal = TRUE, container = nb1, expand = TRUE)

# browse to select builder csv file
gp_1_proj <- ggroup(container = nb1) 
gp_1_proj_lbl <- glabel("Shapefile projection file: ", container = gp_1_proj) # text label
gp_1_proj_btn <- gfilebrowse( # file browser
  text = "", 
  type = "open", 
  quote = TRUE, 
  container = gp_1_proj, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Shapefile projection file: ", svalue(gp_1_proj_btn))
  }
)

###### tab 2 - generate benchmark shapefiles ############################################################

# browse to select builder csv file
gp_2_buildCsv <- ggroup(container = nb2) 
gp_2_buildCsv_lbl <- glabel("Builder csv file: ", container = gp_2_buildCsv) # text label
gp_2_buildCsv_btn <- gfilebrowse( # file browser
  text = "", 
  type = "open", 
  quote = TRUE, 
  container = gp_2_buildCsv, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Builder csv file: ", svalue(gp_2_buildCsv_btn))
  }
)

# browse to select catchments shp
gp_2_catchShp <- ggroup(container = nb2) 
gp_2_catchShp_lbl <- glabel("Catchments shapefile: ", container = gp_2_catchShp) # text label
gp_2_catchShp_btn <- gfilebrowse( # file browser
  text = "", 
  type = "open", 
  quote = TRUE, 
  container = gp_2_catchShp, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_2_catchShp_btn))
  }
)

# browse to select output file
gp_2_outFile <- ggroup(container = nb2) 
gp_2_outFile_lbl <- glabel("Output shapefile: ", container = gp_2_outFile) # text label
gp_2_outFile_btn <- gfilebrowse( # file browser
  text = "", 
  type = "save", 
  quote = TRUE, 
  container = gp_2_outFile, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_2_outFile_btn))
  }
)

# browse to select optional shapefiles file
gp_2_addShp <- ggroup(container = nb2) 
gp_2_addShp_lbl <- glabel("Additional shapefiles (optional): ", container = gp_2_addShp) # text label
gp_2_addShp_btn <- gfilebrowse( # file browser
  text = "",
  type = "open",
  quote = TRUE,
  container = gp_2_addShp,
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Additional shapefiles: ", svalue(gp_2_addShp_btn))
  }
)


# button to run the createFolders function
gp_2_run <- ggroup(container = nb2)
gp_2_run_btn <- gbutton(
  text = "Generate benchmark shps",
  container = gp_2_run,
  handler = function(h, ...){ 
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
    if(nchar(svalue(gp_1_proj_btn))==0){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "No projection file selected")
      stop("No projection file selected")
    }
    tryCatch({
      gen_ba_shp(
        buildCsv = svalue(gp_2_buildCsv_btn), 
        catchmentsShp = svalue(gp_2_catchShp_btn), 
        outFile = svalue(gp_2_outFile_btn), 
        prjFile = svalue(gp_1_proj_btn), 
        shp.paths = svalue(gp_2_addShp_btn))
      
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Benchmark shapefiles created")
    }, error = function(cond){
      message("Could not generate shapefiles")
      message(cond)
    }
    )
  }
)

###### tab 4 - generate target tables ############################################################

# browse to select ecoregion criteria raster
gp_4_rastereco <- ggroup(container = nb4) 
gp_4_rastereco_lbl <- glabel("Raster file: ", container = gp_4_rastereco) # text label
gp_4_rastereco_btn <- gfilebrowse( # file browser
  text = "", 
  type = "open", 
  quote = TRUE, 
  container = gp_4_rastereco, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Raster criteria file: ", svalue(gp_4_rastereco_btn))
  }
)

gp_4_rasterbnd <- ggroup(container = nb4) 
gp_4_rasterbnd_lbl <- glabel("Raster file: ", container = gp_4_rasterbnd) # text label
gp_4_rasterbnd_btn <- gfilebrowse( # file browser
  text = "", 
  type = "open", 
  quote = TRUE, 
  container = gp_4_rasterbnd, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Raster boundary file: ", svalue(gp_4_rasterbnd_btn))
  }
)

gp_4_mdr <- ggroup(container = nb4)
gp_4_mdr_lbl <- glabel("MDR value: ", container = gp_4_mdr)
gp_4_mdr_txt <- gedit(text = "", 
                      container = gp_4_mdr,
                      handler = function(h,...){
                        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "MDR: ", svalue(gp_4_mdr_txt))
                      })

gp_4_outFile <- ggroup(container = nb4) 
gp_4_outFile_lbl <- glabel("Output shapefile: ", container = gp_4_outFile) # text label
gp_4_outFile_btn <- gfilebrowse( # file browser
  text = "", 
  type = "save", 
  quote = TRUE, 
  container = gp_4_outFile, 
  handler = function(h,...){
    svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_4_outFile_btn))
  }
)

# button to run the createFolders function
gp_4_run <- ggroup(container = nb4)
gp_4_run_btn <- gbutton(
  text = "Generate target table",
  container = gp_4_run,
  handler = function(h, ...){ 
    tryCatch({
      gen_target_table(
        mdrVal = svalue(gp_4_mdr_txt), 
        ecoRast = svalue(gp_4_rastereco_btn), 
        ecoBnd = svalue(gp_4_rasterbnd_btn), 
        outFile = svalue(gp_4_outFile_btn) 
        )
      
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Target tables created")
    }, error = function(cond){
      message("Could not generate target tables")
      message(cond)
    }
    )
  }
)


#### output text box ############################################################

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