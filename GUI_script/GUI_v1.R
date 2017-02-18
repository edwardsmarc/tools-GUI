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
  #library(gWidgets2tcltk) # using a different package here will access a different GUI toolkit.
  library(gWidgets2RGtk2)
  #library(gWidgetsRGtk2)
  source("../functions/createFolders.R")
  source("../functions/gen_ba_shp.r")
  source("../functions/gen_target_table.R")
  source("../functions/criteria2catch.R")
  source("../functions/benchmarkRep.R")
  source("../functions/networkRep.R")
  source("../functions/results_summary.R")
  source("../functions/gen_networks_shp.R")
  source("../functions/evaluate_pa_shp.R")
  
    
  # set up high level containers here
  win <- gwindow("baNetworkR") # the full window container that will hold everything
  ##
  g <- ggroup(container = win, horizontal = FALSE, expand = TRUE) # a high level group that will hold everything
  nb <- gnotebook(container = g) # a notebook container
  ##
  nb1 <- ggroup(container = nb, label = "Setup", horizontal = FALSE)
  nb2 <- ggroup(container = nb, label = "Benchmark shp", horizontal = FALSE)
  nb3 <- ggroup(container = nb, label = "Criteria to catchments", horizontal = FALSE)
  nb4 <- ggroup(container = nb, label = "Target tables", horizontal = FALSE)
  nb5 <- ggroup(container = nb, label = "Benchmark representation", horizontal = FALSE)
  nb6 <- ggroup(container = nb, label = "Network representation", horizontal = FALSE)
  nb7 <- ggroup(container = nb, label = "Representation summary", horizontal = FALSE)
  nb8 <- ggroup(container = nb, label = "Network shp", horizontal = FALSE)
  nb9 <- ggroup(container = nb, label = "Evaluate existing areas", horizontal = FALSE)
  ##
  
  
  ###### tab 1 - folder set up and projection file ############################################################
  # Framing - glayout grid continaing 3 frames. Each frame contains another layout grid for placing widgets. 
  # Spacing: 10 for separation in frames, top widget in each frame starts at row 2.
  # Frame 1 - Folder structure - widgets for creating folders. Columns 1:2 in glayout. 
  # Frame 2 - Projection file - widget to select projection file. Columns 1:2 in glayout.
  # Frame 3 - text describing inputs. Third column in glayout.
  
  lyt1 <- glayout(container = nb1)
  
  lyt1[1:9, 1:2] <- gp_1_frm_1 <- gframe("Folder structure", container = lyt1, spacing = 10)
  lyt1a <- glayout(container = gp_1_frm_1)
  
  # browse to select parent directory where folders will be added
  lyt1a[2,1, anchor = c(1,0)] <- "Parent folder" 
  lyt1a[2,2] <- gp_1_parent_btn <- gfilebrowse( # file browser - allows selection of file 
    text = "", 
    type = "selectdir", 
    quote = TRUE, 
    container = lyt1a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Parent directory: ", svalue(gp_1_parent_btn))
    }
  )
  
  # checkboxes to select new folders to add
  lyt1a[4,1:2, anchor = c(1,0)] <- "Create folders"
  lyt1a[5,1] <- gp_1_folders_check_input <- gcheckbox(text = "input", checked=TRUE, container = lyt1a)
  lyt1a[5,2] <- gp_1_folders_check_results <- gcheckbox(text = "results", checked=TRUE, container = lyt1a)
  lyt1a[6,1] <- gp_1_folders_check_benchmarks <- gcheckbox(text = "benchmarks", checked=TRUE, container = lyt1a)
  lyt1a[6,2] <- gp_1_folders_check_networks <- gcheckbox(text = "networks", checked=TRUE, container = lyt1a)
  
  # optional other filenames to add
  lyt1a[7,1, anchor = c(1,0)] <- "Other"
  lyt1a[7,2] <- gp_1_folders_other_txt <- gedit(text = "", container = lyt1a)
  
  # button to run the createFolders function
  lyt1a[8,1:2] <- gp_1_run_btn <- gbutton(
    text = "Create folders",
    container = lyt1a,
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
  
  lyt1[10:13, 1:2] <- gp_1_frm_2 <- gframe("Projection file", container = lyt1)
  lyt1b <- glayout(container = gp_1_frm_2, spacing = 10)
  
  # browse to select projection file
  lyt1b[2,1, anchor = c(0,1)] <- "Shapefile projection file"
  lyt1b[2,2] <- gp_1_proj_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt1b, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Shapefile projection file: ", svalue(gp_1_proj_btn))
    }
  )
  
  lyt1[1:13, 3] <- gp_1_text <- gframe("Tab description", container = lyt1, expand = TRUE, pos = 1)
  lyt1c <- glayout(container = gp_1_text, spacing = 10)
  lyt1c[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))

  
  ###### tab 2 - generate benchmark shapefiles ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run gen_ba_shp. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt2 <- glayout(container = nb2)
  
  lyt2[1:6, 1:2] <- gp_2_frm_1 <- gframe("Generate benchmark shapefiles", container = lyt2, spacing = 10, expand = TRUE)
  lyt2a <- glayout(container = gp_2_frm_1)
  
  # browse to select builder csv file
  lyt2a[2,1] <- "Builder csv file"
  lyt2a[2,2] <- gp_2_buildCsv_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt2a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Builder csv file: ", svalue(gp_2_buildCsv_btn))
    }
  )
  
  # browse to select catchments shp
  lyt2a[3,1] <- "Catchments shapefile"
  lyt2a[3,2] <- gp_2_catchShp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt2a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_2_catchShp_btn))
    }
  )
  
  # browse to select output file
  
  lyt2a[4,1] <- "Output shapefile"
  lyt2a[4,2] <- gp_2_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt2a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile: ", svalue(gp_2_outFile_btn))
    }
  )
  
  # browse to select optional shapefiles file
  lyt2a[5,1] <- "Additional shapefiles (optional)"
  lyt2a[5,2] <- gp_2_addShp_btn <- gfilebrowse( # file browser
    text = "",
    type = "open",
    quote = TRUE,
    container = lyt2a,
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Additional shapefiles: ", svalue(gp_2_addShp_btn))
    }
  )
  
  
  # button to run the createFolders function
  lyt2a[6,1:2] <- gp_2_run_btn <- gbutton(
    text = "Generate benchmark shapefiles",
    container = lyt2a,
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
  
  lyt2[1:6, 3] <- gp_2_text <- gframe("Tab description", container = lyt2, expand = TRUE, pos = 1)
  lyt2b <- glayout(container = gp_2_text, spacing = 10)
  lyt2b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  ###### tab 3 - add criteria values to catchments ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run criteria2catch. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt3 <- glayout(container = nb3)
  
  lyt3[1:11, 1:2] <- gp_3_frm_1 <- gframe("Tabulate criteria classes in catchments", container = lyt3, spacing = 10, expand = TRUE)
  lyt3a <- glayout(container = gp_3_frm_1)
  
  # browse to select catchments shp
  lyt3a[2,1] <- "Catchments shapefile"
  lyt3a[2,2] <- gp_3_catchShp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt3a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_3_catchShp_btn))
    }
  )
  
  # browse to select criteria raster
  lyt3a[3,1] <- "Criteria raster file"
  lyt3a[3,2] <- gp_3_raster_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt3a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria raster file: ", svalue(gp_3_raster_btn))
    }
  )
  
  # text box to input cell size
  lyt3a[4,1] <- "Cell size (metres)"
  lyt3a[4,2] <- gp_3_cellsize_txt <- gedit(text = "", 
                        container = lyt3a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Cell size (metres): ", svalue(gp_3_cellsize_txt))
                        })
  
  # text box to input disaggregate factor
  lyt3a[5,1] <- "Disaggregate factor value"
  lyt3a[5,2] <- gp_3_factor_txt <- gedit(text = "1", 
                        container = lyt3a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Disaggregate factor value: ", svalue(gp_3_factor_txt))
                        })
  
  # text box to input criteria name
  lyt3a[6,1] <- "Criteria name"
  lyt3a[6,2] <- gp_3_criteria_txt <- gedit(text = "", 
                        container = lyt3a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria name: ", svalue(gp_3_criteria_txt))
                        })
  
  # text box to input chunk size
  lyt3a[7,1] <- "Chunk size"
  lyt3a[7,2] <- gp_3_chunk_txt <- gedit(text = "", 
                        container = lyt3a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Chunk size: ", svalue(gp_3_chunk_txt))
                        })
  
  lyt3a[8,1:2] <- gseparator(horizontal = TRUE, container = lyt3a, expand=TRUE)
  
  # check box to select whether columns should be added to the input or saved as a new output
  lyt3a[9,1] <- gcheckbox(text = "Save as new file?", checked=FALSE, container = lyt3a)
  
  # browser for output shapefile
  lyt3a[9,2] <- gp_3_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt3a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile (optional): ", svalue(gp_3_outFile_btn))
    }
  )
  
  lyt3a[10,1:2] <- gseparator(horizontal = TRUE, container = lyt3a, expand=TRUE)
  
  # button to run the criteria2catchments function
  lyt3a[11,1:2] <- gp_2_outFile_btn <- gbutton(
    text = "Add criteria to catchments",
    container = lyt3a,
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
  
  lyt3[1:11, 3] <- gp_3_text <- gframe("Tab description", container = lyt3, expand = TRUE, pos = 1)
  lyt3b <- glayout(container = gp_3_text, spacing = 10)
  lyt3b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  ###### tab 4 - generate target tables ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run gen_target-table. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt4 <- glayout(container = nb4)
  
  lyt4[1:6, 1:2] <- gp_4_frm_1 <- gframe("Generate target tables", container = lyt4, spacing = 10, expand = TRUE)
  lyt4a <- glayout(container = gp_4_frm_1)
  
  # browse to select ecoregion criteria raster
  lyt4a[2,1] <- "Criteria raster file"
  lyt4a[2,2] <- gp_4_rastereco_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt4a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria raster file: ", svalue(gp_4_rastereco_btn))
    }
  )
  
  # browse to select raster boundary file
  lyt4a[3,1] <- "Boundary raster file"
  lyt4a[3,2] <- gp_4_rasterbnd_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt4a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Boundary raster file: ", svalue(gp_4_rasterbnd_btn))
    }
  )
  
  # text box to select area target value
  lyt4a[4,1] <- "area target"
  lyt4a[4,2] <- gp_4_mdr_txt <- gedit(text = "", 
                        container = lyt4a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "MDR: ", svalue(gp_4_mdr_txt))
                        })
  
  # browse to select outut csv
  lyt4a[5,1] <- "Output csv file"
  lyt4a[5,2] <- gp_4_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt4a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_4_outFile_btn))
    }
  )
  
  # button to run the gen_target_table function
  lyt4a[6,1:2] <- gp_4_run_btn <- gbutton(
    text = "Generate target table",
    container = lyt4a,
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
  
  lyt4[1:6, 3] <- gp_4_text <- gframe("Tab description", container = lyt4, expand = TRUE, pos = 1)
  lyt4b <- glayout(container = gp_4_text, spacing = 10)
  lyt4b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  ###### tab 5 - representation evaluation - single benchmarks ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run benchmarkRep. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt5 <- glayout(container = nb5)
  
  lyt5[1:8, 1:2] <- gp_5_frm_1 <- gframe("Assess benchmark representation", container = lyt5, spacing = 10, expand = TRUE)
  lyt5a <- glayout(container = gp_5_frm_1)
  
  # browse to select catchments dbf file
  lyt5a[2,1] <- "Catchments dbf"
  lyt5a[2,2] <- gp_5_catchDbf_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments dbf: ", svalue(gp_5_catchDbf_btn))
    }
  )
  
  # browse to select builder csv file
  lyt5a[3,1] <- "Builder csv file"
  lyt5a[3,2] <- gp_5_buildCsv_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Builder csv file: ", svalue(gp_5_buildCsv_btn))
    }
  )
  
  # browse to select target table
  lyt5a[4,1] <- "Target csv file"
  lyt5a[4,2] <- gp_5_target_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Target csv file: ", svalue(gp_5_target_btn))
    }
  )
  
  # text box to inpiut criteria
  lyt5a[5,1] <- "Criteria name"
  lyt5a[5,2] <- gp_5_criteria_txt <- gedit(text = "", 
                             container = lyt5a,
                             handler = function(h,...){
                               svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria name: ", svalue(gp_5_criteria_txt))
                             })
  
  # browse to select output csv file
  lyt5a[6,1] <- "Output csv file"
  lyt5a[6,2] <- gp_5_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_5_outFile_btn))
    }
  )
  
  # browse to select optional filter csv table
  lyt5a[7,1] <- "Filter csv file (optional)"
  lyt5a[7,2] <- gp_5_Filter_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Filter csv file: ", svalue(gp_5_Filter_btn))
    }
  )
  
  # button to run the benchmarkRep function
  lyt5a[8,1:2] <- gp_5_run_btn <- gbutton(
    text = "Assess benchmark representation",
    container = lyt5a,
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
  
  lyt5[1:8, 3] <- gp_5_text <- gframe("Tab description", container = lyt5, expand = TRUE, pos = 1)
  lyt5b <- glayout(container = gp_5_text, spacing = 10)
  lyt5b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  ###### tab 6 - representation evaluation - networks ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run networkRep. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt6 <- glayout(container = nb6)
  
  lyt6[1:6, 1:2] <- gp_6_frm_1 <- gframe("Assess network representation", container = lyt6, spacing = 10, expand = TRUE)
  lyt6a <- glayout(container = gp_6_frm_1)
  
  # browse to select singles csv results table
  lyt6a[2,1] <- "Singles csv file"
  lyt6a[2,2] <- gp_6_singles_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt6a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Singles csv file: ", svalue(gp_6_singles_btn))
    }
  )
  
  # text box to input number of benchmarks in network
  lyt6a[3,1] <- "Number of benchmarks in network"
  lyt6a[3,2] <- gp_6_nba_txt <- gedit(text = "", 
                             container = lyt6a,
                             handler = function(h,...){
                               svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Number of benchmarks in network: ", svalue(gp_6_nba_txt))
                             })
  
  # browse to select output csv file
  lyt6a[4,1] <- "Output csv file"
  lyt6a[4,2] <- gp_6_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt6a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_6_outFile_btn))
    }
  )
  
  # browse to select optional filter csv table
  lyt6a[5,1] <- "Filter csv file"
  lyt6a[5,2] <- gp_6_Filter_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt6a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Filter csv file: ", svalue(gp_6_Filter_btn))
    }
  )
  
  # button to run the networkRep function
  lyt6a[6,1:2] <- gp_6_run_btn <- gbutton(
    text = "Assess benchmark representation",
    container = lyt6a,
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
  
  lyt6[1:6, 3] <- gp_6_text <- gframe("Tab description", container = lyt6, expand = TRUE, pos = 1)
  lyt6b <- glayout(container = gp_6_text, spacing = 10)
  lyt6b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  ###### tab 7 - representation evaluation - summary ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run results_summary. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt7 <- glayout(container = nb7)
  
  lyt7[1:6, 1:2] <- gp_7_frm_1 <- gframe("Summarise representation results", container = lyt7, spacing = 10, expand = TRUE)
  lyt7a <- glayout(container = gp_7_frm_1)
  
  # text box to input number of benchmarks in network
  lyt7a[2,1] <- "Criteria: (e.g. cmi,gpp,lcc)"
  lyt7a[2,2] <- gp_7_crit_txt <- gedit(text = "", 
                        container = lyt7a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria to summarise: ", svalue(gp_7_crit_txt))
                        })
  
  # browse to select summary folder containing results and targets
  lyt7a[3,1] <- "Summary folder"
  lyt7a[3,2] <- gp_7_folder_btn <- gfilebrowse( # file browser
    text = "", 
    type = "selectdir", 
    quote = TRUE, 
    container = lyt7a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Summary folder: ", svalue(gp_7_folder_btn))
    }
  )
  
  # text box to input threshold value
  lyt7a[4,1] <- "Threshold value for class inclusion"
  lyt7a[4,2] <- gp_7_threshold_txt <- gedit(text = "", 
                         container = lyt7a,
                         handler = function(h,...){
                           svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Threshold value for class inclusion: ", svalue(gp_7_threshold_txt))
                         })
  
  # browse to select output csv file
  lyt7a[5,1] <- "Output csv file"
  lyt7a[5,2] <- gp_7_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt7a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_7_outFile_btn))
    }
  )
  
  # button to run the results_summary function
  lyt7a[6,1:2] <- gp_7_run_btn <- gbutton(
    text = "Summarise representation results",
    container = lyt7a,
    handler = function(h, ...){ 
      tryCatch({
        results_summary(
          criteria = svalue(gp_7_crit_txt), 
          summaryDir = svalue(gp_7_folder_btn), 
          rareThreshold = svalue(gp_7_threshold_txt), 
          outFile = svalue(gp_7_outFile_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Summary table created")
      }, error = function(cond){
        message("Could not create summary table")
        message(cond)
      }
      )
    }
  )
  
  lyt7[1:6, 3] <- gp_7_text <- gframe("Tab description", container = lyt7, expand = TRUE, pos = 1)
  lyt7b <- glayout(container = gp_7_text, spacing = 10)
  lyt7b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  ###### tab 8 - generate network shapefiles ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run gen_networks_shp. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt8 <- glayout(container = nb8)
  
  lyt8[1:6, 1:2] <- gp_8_frm_1 <- gframe("Generate network shapefiles", container = lyt8, spacing = 10, expand = TRUE)
  lyt8a <- glayout(container = gp_8_frm_1)
  
  # browse to select benchmarks file
  lyt8a[2,1] <- "Benchmark shapefile"
  lyt8a[2,2] <- gp_8_baFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Benchmark shapefile: ", svalue(gp_8_baFile_btn))
    }
  )
  
  # browse to select benchmarks file
  lyt8a[3,1] <- "Networks csv file"
  lyt8a[3,2] <- gp_8_networks_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Networks csv file: ", svalue(gp_8_networks_btn))
    }
  )
  
  # browse to select output shp file
  lyt8a[4,1] <- "Output shapefile"
  lyt8a[4,2] <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile: ", svalue(gp_8_outFile_btn))
    }
  )
  
  # browse to select tmp folder
  lyt8a[5,1] <- "Temporary folder"
  lyt8a[5,2] <- gp_8_tmp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "selectdir", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Temporary folder: ", svalue(gp_8_tmp_btn))
    }
  )
  
  # button to run the gen_networks_shp function
  lyt8a[6,1:2] <- gp_8_run_btn <- gbutton(
    text = "Generate network shapefiles",
    container = lyt8a,
    handler = function(h, ...){ 
      tryCatch({
        gen_networks_shp(
          baFile = svalue(gp_8_baFile_btn), 
          idColBa = "PB", 
          networksCsv = svalue(gp_8_networks_btn), 
          outFile = svalue(gp_8_outFile_btn),
          prjFile = svalue(gp_1_proj_btn),
          tmp = svalue(gp_8_tmp_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Network shapefiles created")
      }, error = function(cond){
        message("Could not create network shapefiles")
        message(cond)
      }
      )
    }
  )
  
  lyt8[1:6, 3] <- gp_8_text <- gframe("Tab description", container = lyt8, expand = TRUE, pos = 1)
  lyt8b <- glayout(container = gp_8_text, spacing = 10)
  lyt8b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  ###### tab 9 - evaluate existing polygons for benchmark status ######################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run evaluate_pa_shp. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt9 <- glayout(container = nb9)
  
  lyt9[1:8, 1:2] <- gp_9_frm_1 <- gframe("Evaluate existing areas", container = lyt9, spacing = 10, expand = TRUE)
  lyt9a <- glayout(container = gp_9_frm_1)
  
  # browse to select input shapefile
  lyt9a[2,1] <- "Shapefile to evaluate"
  lyt9a[2,2] <- gp_9_inshp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt9a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Shapefile to evaluate: ", svalue(gp_9_inshp_btn))
    }
  )
  
  # browse to select input catchments
  lyt9a[3,1] <- "Catchments shapefile"
  lyt9a[3,2] <- gp_9_catch_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt9a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_9_catch_btn))
    }
  )
  
  # text box to input intactness column
  lyt9a[4,1] <- "Intactness column"
  lyt9a[4,2] <- gp_9_intactcol_txt <- gedit(text = "", 
                              container = lyt9a,
                              handler = function(h,...){
                                svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Intactness column: ", svalue(gp_9_intactcol_txt))
                              })
  
  # text box to input area target
  lyt9a[5,1] <- "Area target"
  lyt9a[5,2] <- gp_9_areatarget_txt <- gedit(text = "", 
                              container = lyt9a,
                              handler = function(h,...){
                                svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Area target: ", svalue(gp_9_areatarget_txt))
                              })
  
  # text box to input intactness threshold
  lyt9a[6,1] <- "Intactness threshold"
  lyt9a[6,2] <- gp_9_threshold_txt <- gedit(text = "", 
                              container = lyt9a,
                              handler = function(h,...){
                                svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Intactness threshold: ", svalue(gp_9_threshold_txt))
                              })
  
  # browse to select output shp file
  lyt9a[7,1] <- "Output shapefile"
  lyt9a[7,2] <- gp_9_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt9a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile: ", svalue(gp_9_outFile_btn))
    }
  )
  
  # button to run the gen_networks_shp function
  lyt9a[8,1:2] <- gp_9_run_btn <- gbutton(
    text = "Evaluate existing areas",
    container = lyt9a,
    handler = function(h, ...){ 
      tryCatch({
        evaluate_pa_shp(
          inputShp = svalue(gp_9_inshp_btn), 
          catchmentsShp = svalue(gp_9_catch_btn), 
          intactnessCol = svalue(gp_9_intactcol_txt), 
          areaTarget = svalue(gp_9_areatarget_txt),
          intactnessThreshold = svalue(gp_9_threshold_txt),
          outFile = svalue(gp_9_outFile_btn),
          prjFile = svalue(gp_1_proj_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Evaluation complete")
      }, error = function(cond){
        message("Could not complete evaluation")
        message(cond)
      }
      )
    }
  )
  
  lyt9[1:8, 3] <- gp_9_text <- gframe("Tab description", container = lyt9, expand = TRUE, pos = 1)
  lyt9b <- glayout(container = gp_9_text, spacing = 10)
  lyt9b[1,1] <- glabel(paste0("Testing text line 1                                                  \nTesting text line 2"))
  
  
  #### output text box ############################################################
  
  frmOutput <- gframe("Output", container = g, expand = TRUE, spacing = 10)
  #frmGroup <- ggroup(container = frmOutput)
  txtOutput <- gtext("", container = frmOutput, expand = TRUE)

}


GUI_v1()