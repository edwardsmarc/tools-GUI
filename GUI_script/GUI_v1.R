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
  source("../functions/criteria2catch.R")
  source("../functions/benchmarkRep.R")
  source("../functions/networkRep.R")
  
    
  # set up high level containers here
  win <- gwindow("GUI_v1", toolkit = guiToolkit("tcltk")) # the full window container that will hold everything
  ##
  g <- ggroup(container = win, horizontal = FALSE, expand = TRUE) # a high level group that will hold everything
  nb <- gnotebook(container = g) # a notebook container
  ##
  nb1 <- ggroup(container = nb, label = "setup", horizontal = FALSE)
  nb2 <- ggroup(container = nb, label = "benchmark shp", horizontal = FALSE)
  nb3 <- ggroup(container = nb, label = "criteria to catchments", horizontal = FALSE)
  nb4 <- ggroup(container = nb, label = "target tables", horizontal = FALSE)
  nb5 <- ggroup(container = nb, label = "benchmark representation", horizontal = FALSE)
  nb6 <- ggroup(container = nb, label = "network representation", horizontal = FALSE)
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
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile: ", svalue(gp_2_outFile_btn))
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
  
  ###### tab 3 - add criteria values to catchments ############################################################
  
  # brose to select catchments shp
  gp_3_catchShp <- ggroup(container = nb3) 
  gp_3_catchShp_lbl <- glabel("Catchments shapefile: ", container = gp_3_catchShp) # text label
  gp_3_catchShp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_3_catchShp, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_3_catchShp_btn))
    }
  )
  
  # browse to select criteria raster
  gp_3_raster <- ggroup(container = nb3) 
  gp_3_raster_lbl <- glabel("Criteria raster file: ", container = gp_3_raster) # text label
  gp_3_raster_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_3_raster, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria raster file: ", svalue(gp_3_raster_btn))
    }
  )
  
  # text box to input cell size
  gp_3_cellsize <- ggroup(container = nb3)
  gp_3_cellsize_lbl <- glabel("Cell size (metres): ", container = gp_3_cellsize)
  gp_3_cellsize_txt <- gedit(text = "", 
                        container = gp_3_cellsize,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Cell size (metres): ", svalue(gp_3_cellsize_txt))
                        })
  
  # text box to input disaggregate factor
  gp_3_factor <- ggroup(container = nb3)
  gp_3_factor_lbl <- glabel("Disaggregate factor value: ", container = gp_3_factor)
  gp_3_factor_txt <- gedit(text = "1", 
                        container = gp_3_factor,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Disaggregate factor value: ", svalue(gp_3_factor_txt))
                        })
  
  # text box to input criteria name
  gp_3_criteria <- ggroup(container = nb3)
  gp_3_criteria_lbl <- glabel("Criteria name: ", container = gp_3_criteria)
  gp_3_criteria_txt <- gedit(text = "", 
                        container = gp_3_criteria,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria name: ", svalue(gp_3_criteria_txt))
                        })
  
  # text box to input chunk size
  gp_3_chunk <- ggroup(container = nb3)
  gp_3_chunk_lbl <- glabel("Chunk size: ", container = gp_3_chunk)
  gp_3_chunk_txt <- gedit(text = "", 
                        container = gp_3_chunk,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Chunk size: ", svalue(gp_3_chunk_txt))
                        })
  
  gseparator(horizontal = TRUE, container = nb3, expand=TRUE)
  
  # check box to select whether columns should be added to the input or saved as a new output
  gp_3_newShp <- ggroup(container = nb3, horizontal = TRUE)
  gp_3_newShp_check <- gcheckbox(text = "Save as new file?", checked=FALSE, container = gp_3_newShp)
  
  # browser for output shapefile
  #gp_3_outFile <- ggroup(container = nb3) 
  #gp_3_outFile_lbl <- glabel("Output shapefile: ", container = gp_3_newShp) # text label
  gp_3_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = gp_3_newShp, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile (optional): ", svalue(gp_3_outFile_btn))
    }
  )
  
  gseparator(horizontal = TRUE, container = nb3, expand=TRUE)
  
  # button to run the criteria2catchments function
  gp_3_run <- ggroup(container = nb3)
  gp_3_run_btn <- gbutton(
    text = "Add criteria to catchments",
    container = gp_3_run,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(nchar(svalue(gp_1_proj_btn))==0){
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "No projection file selected")
        stop("No projection file selected")
      }
      # need to force this using a checkbox. Just leaving the browse widget blank does not work when repeating the analysis on multiple datasets.
      if(svalue(gp_3_newShp_check) == TRUE){ 
        gp_3_outputFile <- svalue(gp_3_outFile_btn)
      } else{
        gp_3_outputFile <- ""
      }
      tryCatch({
        criteria2catch(
          catchmentsShp = svalue(gp_3_catchShp_btn), 
          rasterPath = svalue(gp_3_raster_btn), 
          cellsize = svalue(gp_3_cellsize_txt), 
          disaggregate.factor = svalue(gp_3_factor_txt),
          criteria = svalue(gp_3_criteria_txt),
          chunksize = svalue(gp_3_chunk_txt), 
          outFile = gp_3_outputFile,
          prjFile = svalue(gp_1_proj_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria added to catchments")
      }, error = function(cond){
        message("Could not add criteria to catchments")
        message(cond)
      }
      )
    }
  )
  
  ###### tab 4 - generate target tables ############################################################
  
  # browse to select ecoregion criteria raster
  gp_4_rastereco <- ggroup(container = nb4) 
  gp_4_rastereco_lbl <- glabel("Criteria raster file: ", container = gp_4_rastereco) # text label
  gp_4_rastereco_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_4_rastereco, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria raster file: ", svalue(gp_4_rastereco_btn))
    }
  )
  
  # browse to select raster boundary file
  gp_4_rasterbnd <- ggroup(container = nb4) 
  gp_4_rasterbnd_lbl <- glabel("Boundary raster file: ", container = gp_4_rasterbnd) # text label
  gp_4_rasterbnd_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_4_rasterbnd, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Boundary raster file: ", svalue(gp_4_rasterbnd_btn))
    }
  )
  
  # text box to select mdr value
  gp_4_mdr <- ggroup(container = nb4)
  gp_4_mdr_lbl <- glabel("MDR value: ", container = gp_4_mdr)
  gp_4_mdr_txt <- gedit(text = "", 
                        container = gp_4_mdr,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "MDR: ", svalue(gp_4_mdr_txt))
                        })
  
  # browse to select outut csv
  gp_4_outFile <- ggroup(container = nb4) 
  gp_4_outFile_lbl <- glabel("Output csv file: ", container = gp_4_outFile) # text label
  gp_4_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = gp_4_outFile, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_4_outFile_btn))
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
  
  
  ###### tab 5 - representation evaluation - single benchmarks ############################################################
  
  # browse to select catchments dbf file
  gp_5_catchDbf <- ggroup(container = nb5) 
  gp_5_catchDbf_lbl <- glabel("Catchments dbf: ", container = gp_5_catchDbf) # text label
  gp_5_catchDbf_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_5_catchDbf, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments dbf: ", svalue(gp_5_catchDbf_btn))
    }
  )
  
  # browse to select builder csv file
  gp_5_buildCsv <- ggroup(container = nb5) 
  gp_5_buildCsv_lbl <- glabel("Builder csv file: ", container = gp_5_buildCsv) # text label
  gp_5_buildCsv_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_5_buildCsv, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Builder csv file: ", svalue(gp_5_buildCsv_btn))
    }
  )
  
  # browse to select target table
  gp_5_target <- ggroup(container = nb5) 
  gp_5_target_lbl <- glabel("Target csv file: ", container = gp_5_target) # text label
  gp_5_target_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_5_target, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Target csv file: ", svalue(gp_5_target_btn))
    }
  )
  
  # text box to inpiut criteria
  gp_5_criteria <- ggroup(container = nb5)
  gp_5_criteria_lbl <- glabel("Criteria name: ", container = gp_5_criteria)
  gp_5_criteria_txt <- gedit(text = "", 
                             container = gp_5_criteria,
                             handler = function(h,...){
                               svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria name: ", svalue(gp_5_criteria_txt))
                             })
  
  # browse to select output csv file
  gp_5_outFile <- ggroup(container = nb5) 
  gp_5_outFile_lbl <- glabel("Output csv file: ", container = gp_5_outFile) # text label
  gp_5_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = gp_5_outFile, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_5_outFile_btn))
    }
  )
  
  # browse to select optional filter csv table
  gp_5_Filter <- ggroup(container = nb5) 
  gp_5_Filter_lbl <- glabel("Filter csv file: ", container = gp_5_Filter) # text label
  gp_5_Filter_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_5_Filter, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Filter csv file: ", svalue(gp_5_Filter))
    }
  )
  
  # button to run the benchmarkRep function
  gp_5_run <- ggroup(container = nb5)
  gp_5_run_btn <- gbutton(
    text = "Assess benchmark representation",
    container = gp_5_run,
    handler = function(h, ...){ 
      tryCatch({
        benchmarkRep(
          catchDbf = svalue(gp_5_catchDbf_btn), 
          buildCsv = svalue(gp_5_buildCsv_btn), 
          targetTab = svalue(gp_5_target_btn), 
          crit = svalue(gp_5_criteria_txt), 
          outFile = svalue(gp_5_outFile_btn), 
          filterCsv = svalue(gp_5_Filter_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Results tables created")
      }, error = function(cond){
        message("Could not assess representation")
        message(cond)
      }
      )
    }
  )
  
  ###### tab 6 - representation evaluation - networks ############################################################
  
  # browse to select singles csv results table
  gp_6_singles <- ggroup(container = nb6) 
  gp_6_singles_lbl <- glabel("Singles csv file: ", container = gp_6_singles) # text label
  gp_6_singles_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_6_singles, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Singles csv file: ", svalue(gp_6_singles))
    }
  )
  
  # text box to input number of benchmarks in network
  gp_6_nba <- ggroup(container = nb5)
  gp_6_nba_lbl <- glabel("Number of benchmarks in network: ", container = gp_6_nba)
  gp_6_nba_txt <- gedit(text = "", 
                             container = gp_6_nba,
                             handler = function(h,...){
                               svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Number of benchmarks in network: ", svalue(gp_6_nba))
                             })
  
  # browse to select output csv file
  gp_6_outFile <- ggroup(container = nb6) 
  gp_6_outFile_lbl <- glabel("Output csv file: ", container = gp_6_outFile) # text label
  gp_6_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = gp_6_outFile, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_6_outFile_btn))
    }
  )
  
  # browse to select optional filter csv table
  gp_6_Filter <- ggroup(container = nb6) 
  gp_6_Filter_lbl <- glabel("Filter csv file: ", container = gp_6_Filter) # text label
  gp_6_Filter_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = gp_6_Filter, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Filter csv file: ", svalue(gp_6_Filter))
    }
  )
  
  # button to run the benchmarkRep function
  gp_6_run <- ggroup(container = nb6)
  gp_6_run_btn <- gbutton(
    text = "Assess benchmark representation",
    container = gp_6_run,
    handler = function(h, ...){ 
      tryCatch({
        networkRep(
          singlesCsv = svalue(gp_6_singles_btn), 
          nBA = svalue(gp_6_nba_txt), 
          outFile = svalue(gp_6_outFile_btn), 
          filterCsv = svalue(gp_6_Filter_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Results tables created")
      }, error = function(cond){
        message("Could not assess representation")
        message(cond)
      }
      )
    }
  )
  
  #### output text box ############################################################
  
  frmOutput <- gframe("Output", container = g)
  txtOutput <- gtext("", container = frmOutput)

}


GUI_v1()



###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
# make a button to upload a csv file
#grp_upload <- ggroup(container = nb1)
# btn_upload <- gbutton(
#   text = "Upload csv file", # button widget has an inbuilt text label argument
#   #container = grp_upload,
#   container = grp_name,
#   handler = function(h, ...){ # the handler argument takes a function that runs when the button is pressed
#     gfile( # widget to select file
#     text = "Upload csv file",
#     type = "open", # opens file, can also save
#     handler = function(h, ...){ # tell gfile what to do with the file
#       tryCatch({
#         data_frame_name <- make.names(svalue(txt_data_frame_name)) # get data frame name from other widget
#         the_data <- read.csv(h$file) # read the data - h$file is the csv you selected
#         assign(data_frame_name, the_data, envir = globalenv())
#         #svalue(status_bar) <- paste0(nrow(the_data), " records saved to variable ", data_frame_name)
#         svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", nrow(the_data), " records saved to variable ", data_frame_name)
#       #}, error = function(e) svalue(status_bar) <- "Could not upload data"
#       }, error = function(e) svalue(txtOutput) <- "Could not upload data"
#       )
#     },
#     filter = list("All files" = list(patterns = c("*")))
#     )
#   }
# )
# 
# gseparator(horizontal = TRUE, container = nb1, expand=TRUE)
# 
# # new groups to add a column
# grp_newCol <- ggroup(container = nb1)
# lbl_newCol <- glabel("New column name: ", container = grp_newCol)
# txt_newCol <- gedit("new_col", container = grp_newCol)
# 
# grp_newColVal <- ggroup(container = nb1)
# lbl_newColVal <- glabel("New column value: ", container = grp_newColVal)
# txt_newColVal <- gedit("NewNewNew", container = grp_newColVal)
# 
# grp_addCol <- ggroup(container = nb1)
# btn_addCol <- gbutton(
#   text = "Add column",
#   container = grp_addCol,
#   handler = function(h, ...){
#     df <- get(data_frame_name)
#     df[[svalue(txt_newCol)]] <- svalue(txt_newColVal)
#     assign(data_frame_name, df, envir = globalenv())
#   }
# )
# 
# gseparator(horizontal = TRUE, container = nb1, expand = TRUE)
# 
# # button to save the table
# grp_save <- ggroup(container = nb1)
# btn_save <- gbutton(
#   text = "Save csv file", # another button to save the csv with the gfile function
#   container = grp_save,
#   handler = function(h, ...){ 
#     gfile( 
#       text = "Save csv file",
#       type = "save",
#       handler = function(h, ...){ 
#         tryCatch({
#           df2 <- get(data_frame_name)
#           write.csv(df2, h$file, row.names = FALSE)
#           svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "File saved as: ", h$file)
#         }, error = function(e) svalue(status_bar) <- "Could not save data"
#         )
#       },
#       filter = list("All files" = list(patterns = c("*")))
#     )
#   }
# )
# ##########################################################################################
# 
# # denovoRep tab
# 
# ## catch dbf
# grp_catchDBF <- ggroup(container = nb2)
# lbl_catchDBF <- glabel("Select catchment dbf file: ", container = grp_catchDBF)
# 
# # gfilebrowse allows typing or selecting filename, selected filename accessed through svalue()
# catchDBF <- gfilebrowse( 
#   text = "select file", 
#   type = "open", 
#   quote = TRUE, 
#   container = grp_catchDBF, 
#   handler = function(h,...){
#     svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "catchDBF: ", svalue(catchDBF))
#   }
# )
# 
# ## build csv
# grp_buildCSV <- ggroup(container = nb2)
# lbl_buildCSV <- glabel("Select builder csv file: ", container = grp_buildCSV)
# 
# buildCSV <- gfilebrowse( 
#   text = "select file", 
#   type = "open", 
#   quote = TRUE, 
#   container = grp_buildCSV, 
#   handler = function(h,...){
#     svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "buildCSV: ", svalue(buildCSV))
#   }
# )
# 
# ## out directory
# grp_outDir <- ggroup(container = nb2)
# lbl_outDir <- glabel("Select output csv directory: ", container = grp_outDir)
# 
# outDir <- gfilebrowse( 
#   text = "select file", 
#   type = "save", 
#   quote = TRUE, 
#   container = grp_outDir, 
#   handler = function(h,...){
#     svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "output csv directory: ", svalue(outDir))
#   }
# )
# 
# ## out file
# grp_outFile <- ggroup(container = nb2)
# lbl_outFile <- glabel("Select output csv file path: ", container = grp_outFile)
# 
# outFile <- gfilebrowse( 
#   text = "select file", 
#   type = "save", 
#   quote = TRUE, 
#   container = grp_outFile, 
#   handler = function(h,...){
#     svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "output csv file path: ", svalue(outFile))
#   }
# )
# 
# ## target table
# grp_targTab <- ggroup(container = nb2)
# lbl_targTab <- glabel("Select target table csv: ", container = grp_targTab)
# 
# targTab <- gfilebrowse( 
#   text = "select file", 
#   type = "open", 
#   quote = TRUE, 
#   container = grp_targTab, 
#   handler = function(h,...){
#     svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "target table: ", svalue(targTab))
#   }
# )
# 
# # crit
# grp_crit <- ggroup(container = nb2)
# lbl_crit <- glabel("Type criteria name", container = grp_crit)
# crit <- gedit(text = "e.g. cmi", 
#               container = grp_crit,
#               handler = function(h,...){
#                 svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "criteria: ", svalue(crit))
#               })
# 
# # button to run the denovoRep function
# grp_denovoRep <- ggroup(container = nb2)
# btn_denovoRep <- gbutton(
#   text = "Run denovoRep tool",
#   container = grp_denovoRep,
#   handler = function(h, ...){ 
#     denovoRep(catchDbf = svalue(catchDBF), 
#               buildCsv = svalue(buildCSV), 
#               outDir = svalue(outDir), 
#               outTab = svalue(outFile), 
#               targetTab = svalue(targTab), 
#               crit = svalue(crit), 
#               filterList = c())
#   }
# )
# ##########################################################################################
# 
# #status_bar <- gstatusbar("", container = win)
# frmOutput <- gframe("Output", container = g)
# txtOutput <- gtext("", container = frmOutput)
# 
# 
# }