# GUI for networking tools
# gWidgets is a high level package to create simple GUI's. It provides a common set of functions for accessing more complex GUI toolkits -  tcl/tk, Gtk, Java, Qt - it doesn't provide as much customization as using the underlying toolkits. 

# naming convention for groups (e.g. gp_1_parent) and objects:
# gp_
# 1_: the tab number
# parent: the argument being provided
# _btn; _lbl; _check - widget type

# all the elements in a widget need to be assigned to a container group. And each container group needs to be assigned to a parent container.

GUI_v2 <- function(){
  
  # memoise, digest, gWidget2, and RGtk2 are dependencies for gWidgets2RGtk2. These must be loaded in this order.
  library(memoise, lib.loc = "../packages/")  
  library(digest, lib.loc = "../packages/")  
  library(gWidgets2, lib.loc = "../packages/")  
  library(RGtk2, lib.loc = "../packages/")  
  library(gWidgets2RGtk2, lib.loc = "../packages/")  # using a different package here will access a different GUI toolkit.
  
  source("../functions/createFolders.R")
  source("../functions/gen_ba_shp.r")
  source("../functions/gen_target_table.R")
  source("../functions/criteria2catch.R")
  source("../functions/benchmarkRep.R")
  source("../functions/networkRep.R")
  source("../functions/results_summary.R")
  source("../functions/gen_networks_shp.R")
  source("../functions/evaluate_pa_shp.R")
  source("../functions/metricsDissimilarity.R")
  source("../functions/ksStat.R")
  source("../functions/bcStat.R")
  
    
  # set up high level containers here
  win <- gwindow("BEACONS benchmark networking tools v1") # the full window container that will hold everything
  ##
  g <- ggroup(container = win, horizontal = FALSE, expand = TRUE) # a high level group that will hold everything
  nb <- gnotebook(container = g) # a notebook container
  ##
  nb1 <- ggroup(container = nb, label = "Setup", horizontal = FALSE)
  nb2 <- ggroup(container = nb, label = "Benchmark shp", horizontal = FALSE)
  nb9 <- ggroup(container = nb, label = "Evaluate existing areas", horizontal = FALSE)
  nb3 <- ggroup(container = nb, label = "Criteria to catchments", horizontal = FALSE)
  nb4 <- ggroup(container = nb, label = "Target tables", horizontal = FALSE)
  nb5 <- ggroup(container = nb, label = "Benchmark representation", horizontal = FALSE)
  nb6 <- ggroup(container = nb, label = "Network representation", horizontal = FALSE)
  nb7 <- ggroup(container = nb, label = "Representation summary", horizontal = FALSE)
  nb8 <- ggroup(container = nb, label = "Network shp", horizontal = FALSE)
  nb10 <- ggroup(container = nb, label = "Add network attributes", horizontal = FALSE)
  nb10_nb <- gnotebook(container = nb10) 
  nb10.1 <- ggroup(container = nb10_nb, label = "Dissimilarity", horizontal = FALSE)
  ##
  
  
  ###### tab 1 - folder set up and projection file ############################################################
  # Framing - glayout grid continaing 3 frames. Each frame contains another layout grid for placing widgets. 
  # Spacing: 10 for separation in frames, top widget in each frame starts at row 2.
  # Frame 1 - Folder structure - widgets for creating folders. Columns 1:2 in glayout. 
  # Frame 2 - Projection file - widget to select projection file. Columns 1:2 in glayout.
  # Frame 3 - text describing inputs. Third column in glayout.
  
  lyt1 <- glayout(container = nb1)
  
  lyt1[1:10, 1:2] <- gp_1_frm_1 <- gframe("Folder structure", container = lyt1, spacing = 10)
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
  lyt1a[5,2] <- gp_1_folders_check_gisdata <- gcheckbox(text = "gisdata", checked=TRUE, container = lyt1a)
  lyt1a[6,1] <- gp_1_folders_check_benchmarks <- gcheckbox(text = "benchmarks", checked=TRUE, container = lyt1a)
  lyt1a[6,2] <- gp_1_folders_check_networks <- gcheckbox(text = "networks", checked=TRUE, container = lyt1a)
  lyt1a[7,1] <- gp_1_folders_check_results <- gcheckbox(text = "results", checked=TRUE, container = lyt1a)
  lyt1a[7,2] <- gp_1_folders_check_tmp <- gcheckbox(text = "tmp", checked=TRUE, container = lyt1a)
  
  # optional other filenames to add
  lyt1a[8,1, anchor = c(1,0)] <- "Other (optional)"
  lyt1a[8,2] <- gp_1_folders_other_txt <- gedit(text = "", container = lyt1a)
  
  # button to run the createFolders function
  lyt1a[9,1:2] <- gp_1_run_btn <- gbutton(
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
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not create folders")
      }
      )
    }
  )
  
  lyt1[11:14, 1:2] <- gp_1_frm_2 <- gframe("Projection file", container = lyt1)
  lyt1b <- glayout(container = gp_1_frm_2, spacing = 10)
  
  # browse to select projection file
  lyt1b[2,1, anchor = c(0,1)] <- "Projection file (.prj)"
  lyt1b[2,2] <- gp_1_proj_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt1b, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Shapefile projection file: ", svalue(gp_1_proj_btn))
    }
  )
  
  lyt1[1:14, 3] <- gp_1_text <- gframe("Tool description", container = lyt1, expand = TRUE, pos = 1)
  lyt1c <- glayout(container = gp_1_text, spacing = 10)
  lyt1c[1,1] <- glabel(paste0(
    "Folder structure                                                          
This tool creates the recommended folder structure
that will be used for storing and accessing data.\n
Parent folder: the folder where the project will be stored.\n
Create folders: each folder with a checked box will be created,
as well as a custom folder if defined in the text box.\n\n
Projection File
Select a .prj file that defines the projection to be used for all shapefiles.
See the PBA.prj file for an example.
    "))

  
  ###### tab 2 - generate benchmark shapefiles ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run gen_ba_shp. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt2 <- glayout(container = nb2)
  
  lyt2[1:6, 1:2] <- gp_2_frm_1 <- gframe("Generate benchmark shapefiles", container = lyt2, spacing = 10, expand = TRUE)
  lyt2a <- glayout(container = gp_2_frm_1)
  
  # browse to select builder csv file
  lyt2a[2,1] <- "Builder ranker file (.csv)"
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
  lyt2a[3,1] <- "Catchments file (.shp)"
  lyt2a[3,2] <- gp_2_catchShp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt2a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_2_catchShp_btn))
    }
  )
  
  # browse to select optional shapefiles file
  lyt2a[4,1] <- "Additional shapefiles (.shp; optional)"
  lyt2a[4,2] <- gp_2_addShp_btn <- gfilebrowse( # file browser
    text = "",
    type = "open",
    quote = TRUE,
    container = lyt2a,
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Additional shapefiles: ", svalue(gp_2_addShp_btn))
    }
  )
  
  # browse to select output file
  lyt2a[5,1] <- "Output benchmark file (.shp)"
  lyt2a[5,2] <- gp_2_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt2a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile: ", svalue(gp_2_outFile_btn))
    }
  )
  
  # button to run the gen_ba_shp function
  lyt2a[6,1:2] <- gp_2_run_btn <- gbutton(
    text = "Generate benchmark shapefiles",
    container = lyt2a,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(length(svalue(gp_1_proj_btn))==0){
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
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not generate shapefiles")
      }
      )
    }
  )
  
  lyt2[1:6, 3] <- gp_2_text <- gframe("Tool description", container = lyt2, expand = TRUE, pos = 1)
  lyt2b <- glayout(container = gp_2_text, spacing = 10)
  lyt2b[1,1] <- glabel(paste0("This tool makes a shapefile of benchmark polygons. The attribute table has a PB field that
contains the benchmark names as defined in the builder ranker csv file.\n
The benchmark names and their associated list of catchments
in the csv file are used along with the catchments shapefile
to generate a polygon for each benchmark listed in the 
BUILDER csv file.\n
An optional shapefile of additional polygons can be provided
and will be appended to the bottom of the output shapefile.
"))
  
  
  ###### tab 3 - add criteria values to catchments ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run criteria2catch. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt3 <- glayout(container = nb3)
  
  lyt3[1:11, 1:2] <- gp_3_frm_1 <- gframe("Tabulate criteria classes in catchments", container = lyt3, spacing = 10, expand = TRUE)
  lyt3a <- glayout(container = gp_3_frm_1)
  
  # browse to select catchments shp
  lyt3a[2,1] <- "Catchments file (.shp)"
  lyt3a[2,2] <- gp_3_catchShp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt3a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Catchments shapefile: ", svalue(gp_3_catchShp_btn))
    }
  )
  
  # text box to input criteria name
  lyt3a[3,1] <- "Criteria name (e.g. cmi):"
  lyt3a[3,2] <- gp_3_criteria_txt <- gedit(text = "", 
                                           container = lyt3a,
                                           handler = function(h,...){
                                             svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria name: ", svalue(gp_3_criteria_txt))
                                           })
  
  lyt3a[4,1:2] <- gseparator(horizontal = TRUE, container = lyt3a, expand=TRUE)
  
  # browse to select criteria raster
  lyt3a[5,1] <- "Criteria raster file (.asc)"
  lyt3a[5,2] <- gp_3_raster_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt3a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria raster file: ", svalue(gp_3_raster_btn))
    }
  )
  
  # text box to input cell size
  lyt3a[6,1] <- "Cell size (metres)"
  lyt3a[6,2] <- gp_3_cellsize_txt <- gedit(text = "", 
                        container = lyt3a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Cell size (metres): ", svalue(gp_3_cellsize_txt))
                        })
  
  # text box to input disaggregate factor
  lyt3a[7,1] <- "Disaggregate factor value"
  lyt3a[7,2] <- gp_3_factor_txt <- gedit(text = "1", 
                        container = lyt3a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Disaggregate factor value: ", svalue(gp_3_factor_txt))
                        })
  
  # text box to input chunk size
  # lyt3a[7,1] <- "Block size"
  # lyt3a[7,2] <- gp_3_chunk_txt <- gedit(text = "", 
  #                       container = lyt3a,
  #                       handler = function(h,...){
  #                         svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Block size: ", svalue(gp_3_chunk_txt))
  #                       })
  # 
  lyt3a[8,1:2] <- gseparator(horizontal = TRUE, container = lyt3a, expand=TRUE)
  
  # check box to select whether columns should be added to the input or saved as a new output
  lyt3a[9,1] <- gp_3_newShp_check <- gcheckbox(text = "Save as new file? (.shp; optional)", checked=FALSE, container = lyt3a)
  
  # browser for output shapefile
  lyt3a[9,2] <- gp_3_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt3a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile (.shp; optional): ", svalue(gp_3_outFile_btn))
    }
  )
  
  lyt3a[10,1:2] <- gseparator(horizontal = TRUE, container = lyt3a, expand=TRUE)
  
  # button to run the criteria2catchments function
  lyt3a[11,1:2] <- gp_3_run_btn <- gbutton(
    text = "Add criteria to catchments",
    container = lyt3a,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(length(svalue(gp_1_proj_btn))==0){
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
          chunksize = 50, 
          outFile = gp_3_outputFile,
          prjFile = svalue(gp_1_proj_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria added to catchments")
      }, error = function(cond){
        message("Could not add criteria to catchments")
        message(cond)
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not add criteria to catchments")
      }
      )
    }
  )
  
  lyt3[1:11, 3] <- gp_3_text <- gframe("Tool description", container = lyt3, expand = TRUE, pos = 1)
  lyt3b <- glayout(container = gp_3_text, spacing = 10)
  lyt3b[1,1] <- glabel(paste0("This tool evaluates user provided polygons for benchmark status based on size and
intactness parameters. It was originally designed for testing the existing protected area 
network. After removing catchments based on the intactness threshold, it adds the following 
fields for each contiguous polygon: Area (km2), minimum intactness of catchments in the
polygon, size ratio (Area / Area target). Polygons with size ratio values of 1.0 or higher meet 
the requirements for benchmark status.\n
The new fields in the catchments shapefile attribute table will be the criteria name appended
to each raster class value (e.g. gpp1, gpp2 etc.). The criteria name should be text characters
with no spaces.\n
The raster map should be in ascii format with cell size in metres. Values should be integers
representing classes of data. Either categorical classes or classes representing ranges of
continuous data.\n
The disaggregate factor is a factor to reduce the raster cell size by when tabulating areas
in each catchment. Reducing cell size for the tabulation gives more accurate area values
because cells have closer alignment with catchment boundaries. e.g. if 10 is used, a 1000m
resolution would change to a 100m resolution. Processing time will increase with larger
disaggregate factors.\n
If the 'Save as new file?' box is checked and a file path provided, a new shapefile is saved.
Otherwise the new fields are added to the catchments file provided.\n"))
  
  
  ###### tab 4 - generate target tables ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run gen_target-table. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt4 <- glayout(container = nb4)
  
  lyt4[1:6, 1:2] <- gp_4_frm_1 <- gframe("Generate target tables", container = lyt4, spacing = 10, expand = TRUE)
  lyt4a <- glayout(container = gp_4_frm_1)
  
  # browse to select ecoregion criteria raster
  lyt4a[2,1] <- "Criteria raster file (.asc)"
  lyt4a[2,2] <- gp_4_rastereco_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt4a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria raster file (.asc): ", svalue(gp_4_rastereco_btn))
    }
  )
  
  # browse to select raster boundary file
  lyt4a[3,1] <- "Boundary raster file (.asc)"
  lyt4a[3,2] <- gp_4_rasterbnd_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt4a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Boundary raster file (.asc): ", svalue(gp_4_rasterbnd_btn))
    }
  )
  
  # text box to select area target value
  lyt4a[4,1] <- "Area target (km2)"
  lyt4a[4,2] <- gp_4_mdr_txt <- gedit(text = "", 
                        container = lyt4a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Area Target (km2): ", svalue(gp_4_mdr_txt))
                        })
  
  # browse to select outut csv
  lyt4a[5,1] <- "Output target table (.csv)"
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
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not generate target tables")
      }
      )
    }
  )
  
  lyt4[1:6, 3] <- gp_4_text <- gframe("Tool description", container = lyt4, expand = TRUE, pos = 1)
  lyt4b <- glayout(container = gp_4_text, spacing = 10)
  lyt4b[1,1] <- glabel(paste0("This tool calculates representation targets within a boundary region (e.g. ecoregion)
based on benchmark area targets such as the Minimum Dynamic Reserve.\n
The criteria raster file should cover the boundary region, i.e. the area that benchmarks 
should represent.\n
The boundary raster should line up with the criteria raster but should have values of '1' 
covering the representation boundary area (e.g. ecoregion). This is used to calculate
the area of the boundary region. \n
The proportions of each criteria raster class within the boundary region are calculated 
and multipled by the area target to get the representation target in km2."))
  
  
  ###### tab 5 - representation evaluation - single benchmarks ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run benchmarkRep. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt5 <- glayout(container = nb5)
  
  lyt5[1:8, 1:2] <- gp_5_frm_1 <- gframe("Assess benchmark representation", container = lyt5, spacing = 10, expand = TRUE)
  lyt5a <- glayout(container = gp_5_frm_1)
  
  # browse to select catchments dbf file
  lyt5a[2,1] <- "Catchments file (.dbf)"
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
  lyt5a[3,1] <- "Builder ranker file (.csv)"
  lyt5a[3,2] <- gp_5_buildCsv_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Builder ranker csv file: ", svalue(gp_5_buildCsv_btn))
    }
  )
  
  # browse to select target table
  lyt5a[4,1] <- "Target table (.csv)"
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
  lyt5a[5,1] <- "Criteria name (e.g. cmi)"
  lyt5a[5,2] <- gp_5_criteria_txt <- gedit(text = "", 
                             container = lyt5a,
                             handler = function(h,...){
                               svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria name: ", svalue(gp_5_criteria_txt))
                             })
  
  # browse to select optional filter csv table
  lyt5a[6,1] <- "Filter file (.csv; optional)"
  lyt5a[6,2] <- gp_5_Filter_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Filter csv file: ", svalue(gp_5_Filter_btn))
    }
  )
  
  # browse to select output csv file
  lyt5a[7,1] <- "Output benchmarks results table (.csv)"
  lyt5a[7,2] <- gp_5_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt5a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_5_outFile_btn))
    }
  )
  
  # button to run the benchmarkRep function
  lyt5a[8,1:2] <- gp_5_run_btn <- gbutton(
    text = "Assess benchmark representation",
    container = lyt5a,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(length(svalue(gp_5_Filter_btn)) == 0){ 
        baFilter <- ""
      } else{
        baFilter <- svalue(gp_5_Filter_btn)
      }
      tryCatch({
        benchmarkRep(
          catchDbf = svalue(gp_5_catchDbf_btn), 
          buildCsv = svalue(gp_5_buildCsv_btn), 
          targetTab = svalue(gp_5_target_btn), 
          crit = svalue(gp_5_criteria_txt), 
          outFile = svalue(gp_5_outFile_btn), 
          filterCsv = baFilter
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Results tables created")
      }, error = function(cond){
        message("Could not assess representation")
        message(cond)
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not assess representation")
      }
      )
    }
  )
  
  lyt5[1:8, 3] <- gp_5_text <- gframe("Tool description", container = lyt5, expand = TRUE, pos = 1)
  lyt5b <- glayout(container = gp_5_text, spacing = 10)
  lyt5b[1,1] <- glabel(paste0("This tool uses the builder ranker table and the tabulated criteria values from the Criteria to 
Catchments tool, to calculate the area of each criteria class in each benchmark. It evaluates 
these against the target tables to see whether each benchmark meets the representation targets. 
All benchmarks in the builder ranker file are evaluated, unless the optional filter table is 
provided.\n
The criteria name should match that used in the 'Criteria to catchments' tool.\n
If a benchmark passes a given target, i.e. it contains at least the area of a target class defined
in the target table, it is assigned a '1' in the results table. Otherwise the proportion of the
target met is reported.\n
The filter file is a csv table with a 'PB' column listing the benchmark names to be evaluated.
All the benchmark names must be in the builder ranker file, but only those in the filter file will
be evaluated."))
  
  
  ###### tab 6 - representation evaluation - networks ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run networkRep. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt6 <- glayout(container = nb6)
  
  lyt6[1:10, 1:2] <- gp_6_frm_1 <- gframe("Assess network representation", container = lyt6, spacing = 10, expand = TRUE)
  lyt6a <- glayout(container = gp_6_frm_1)
  
  # browse to select singles csv results table
  lyt6a[2,1] <- "Benchmarks results table (.csv)"
  lyt6a[2,2] <- gp_6_singles_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt6a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Benchmark results csv file: ", svalue(gp_6_singles_btn))
    }
  )
  
  # text box to input number of benchmarks in network
  lyt6a[4,1] <- "Number of benchmarks in network"
  lyt6a[4,2] <- gp_6_nba_txt <- gedit(text = "2", 
                             container = lyt6a,
                             handler = function(h,...){
                               svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Number of benchmarks in network: ", svalue(gp_6_nba_txt))
                             })
  
  # browse to select optional filter csv table
  lyt6a[6,1] <- "Filter file (.csv; optional)"
  lyt6a[6,2] <- gp_6_Filter_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt6a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Filter csv file: ", svalue(gp_6_Filter_btn))
    }
  )
  
  # browse to select output csv file
  lyt6a[8,1] <- "Output networks results table (.csv)"
  lyt6a[8,2] <- gp_6_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt6a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_6_outFile_btn))
    }
  )
  
  # button to run the networkRep function
  lyt6a[10,1:2] <- gp_6_run_btn <- gbutton(
    text = "Assess network representation",
    container = lyt6a,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(length(svalue(gp_6_Filter_btn)) == 0){ 
        netFilter <- ""
      } else{
        netFilter <- svalue(gp_6_Filter_btn)
      }
      tryCatch({
        networkRep(
          singlesCsv = svalue(gp_6_singles_btn), 
          nBA = svalue(gp_6_nba_txt), 
          outFile = svalue(gp_6_outFile_btn), 
          filterCsv = netFilter
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Results tables created")
      }, error = function(cond){
        message("Could not assess representation")
        message(cond)
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not assess representation")
      }
      )
    }
  )
  
  lyt6[1:10, 3] <- gp_6_text <- gframe("Tool description", container = lyt6, expand = TRUE, pos = 1)
  lyt6b <- glayout(container = gp_6_text, spacing = 10)
  lyt6b[1,1] <- glabel(paste0("This tool uses the results from the benchmark representation tool and combines
multiple benchmarks into networks.\n
A network passes a representation target if the target is met in full by at least
one of the benchmarks in the network. If no benchmarks in the network pass the target,
the highest proportion value is reported.\n
Any number of benchmarks can be combined into networks. We recommend starting at 2
and increasing the value if no networks meet the targets.\n
By default, all combinations of benchmarks will be reported in the results table.
For networks with three or more benchmarks this can greatly increase processing time.\n
The optional filter file is a csv table with a 'networks' column listing the network
names to be evaluated. Network names are simply the names of the benchmark in the
network separated by '_' (e.g. PB_001_PB_002).\n
All the benchmarks included in the filter file networks must be in the benchmarks results
table."))
  
  
  ###### tab 7 - representation evaluation - summary ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run results_summary. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt7 <- glayout(container = nb7)
  
  lyt7[1:10, 1:2] <- gp_7_frm_1 <- gframe("Summarise representation results", container = lyt7, spacing = 10, expand = TRUE)
  lyt7a <- glayout(container = gp_7_frm_1)
  
  # text box to input number of benchmarks in network
  lyt7a[2,1] <- "Criteria names (e.g. cmi,lcc)"
  lyt7a[2,2] <- gp_7_crit_txt <- gedit(text = "", 
                        container = lyt7a,
                        handler = function(h,...){
                          svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Criteria to summarise: ", svalue(gp_7_crit_txt))
                        })
  
  # browse to select summary folder containing results and targets
  lyt7a[4,1] <- "Summary folder"
  lyt7a[4,2] <- gp_7_folder_btn <- gfilebrowse( # file browser
    text = "", 
    type = "selectdir", 
    quote = TRUE, 
    container = lyt7a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Summary folder: ", svalue(gp_7_folder_btn))
    }
  )
  
  # text box to input threshold value
  lyt7a[6,1] <- "Threshold value for class inclusion (%)"
  lyt7a[6,2] <- gp_7_threshold_txt <- gedit(text = "0", 
                         container = lyt7a,
                         handler = function(h,...){
                           svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Threshold value for class inclusion: ", svalue(gp_7_threshold_txt))
                         })
  
  # browse to select output csv file
  lyt7a[8,1] <- "Output summary results table (.csv)"
  lyt7a[8,2] <- gp_7_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt7a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output csv file: ", svalue(gp_7_outFile_btn))
    }
  )
  
  # button to run the results_summary function
  lyt7a[10,1:2] <- gp_7_run_btn <- gbutton(
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
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not create summary table")
      }
      )
    }
  )
  
  lyt7[1:10, 3] <- gp_7_text <- gframe("Tool description", container = lyt7, expand = TRUE, pos = 1)
  lyt7b <- glayout(container = gp_7_text, spacing = 10)
  lyt7b[1,1] <- glabel(paste0("This tool summarises results for each network and reports the number of
target gaps (representation targets not met by a network) for each criteria evaluated.\n
The criteria name should match that used in the 'Criteria to catchments' and
'Benchmark representation' tools.\n
The tool requires the results tables for networks being summarized (one table for each criteria),
as well as the target tables (one table for each criteria). These tables should be copied to the
'Summary folder'. The tool identifies these files using their file names:
      - Results files must have e.g. '_cmi_' in file name, and must not contain the word  'target'.
      - Target tables must have both 'target' and e.g. 'cmi' in their file names.\n
A summary of results using two criteria (cmi, gpp) for networks of two benchmarks could contain the
following four example files:
      - representation_results_cmi_pb2.csv
      - representation_results_gpp_pb2.csv
      - target_table_cmi.csv
      - target_table_gpp.csv\n
The 'Threshold value for class inclusion' allows target classes to be dropped from the summary if
their percent coverage in the boundary region is less than the provided value. This is done using
the bnd_prop column in the target table."))
  
  
  ###### tab 8 - generate network shapefiles ############################################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run gen_networks_shp. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt8 <- glayout(container = nb8)
  
  lyt8[1:9, 1:2] <- gp_8_frm_1 <- gframe("Generate network shapefiles", container = lyt8, spacing = 10, expand = TRUE)
  lyt8a <- glayout(container = gp_8_frm_1)
  
  # browse to select benchmarks file
  lyt8a[2,1] <- "Benchmark file (.shp)"
  lyt8a[2,2] <- gp_8_baFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Benchmark shapefile: ", svalue(gp_8_baFile_btn))
    }
  )
  
  # browse to select networks file
  lyt8a[3,1] <- "Networks file (.csv)"
  lyt8a[3,2] <- gp_8_networks_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Networks csv file: ", svalue(gp_8_networks_btn))
    }
  )
  
  # browse to select tmp folder
  lyt8a[4,1] <- "Temporary folder"
  lyt8a[4,2] <- gp_8_tmp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "selectdir", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Temporary folder: ", svalue(gp_8_tmp_btn))
    }
  )
  
  # browse to select output shp file
  lyt8a[5,1] <- "Output networks file (.shp)"
  lyt8a[5,2] <- gp_8_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt8a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile: ", svalue(gp_8_outFile_btn))
    }
  )
  
  lyt8a[6,1:2] <- gseparator(horizontal = TRUE, container = lyt8a, expand=TRUE)
  
  lyt8a[7,1] <- gp_8_check <- gcheckbox(text = "Dissolve?", checked=FALSE, container = lyt8a)
  
  lyt8a[8,1:2] <- gseparator(horizontal = TRUE, container = lyt8a, expand=TRUE)
  
  # button to run the gen_networks_shp function
  lyt8a[9,1:2] <- gp_8_run_btn <- gbutton(
    text = "Generate network shapefiles",
    container = lyt8a,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(length(svalue(gp_1_proj_btn))==0){
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "No projection file selected")
        stop("No projection file selected")
      }
      tryCatch({
        gen_networks_shp(
          baFile = svalue(gp_8_baFile_btn), 
          idColBa = "PB", 
          networksCsv = svalue(gp_8_networks_btn), 
          outFile = svalue(gp_8_outFile_btn),
          Dissolve = svalue(gp_8_check),
          prjFile = svalue(gp_1_proj_btn),
          tmp = svalue(gp_8_tmp_btn)
        )
        
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Network shapefiles created")
      }, error = function(cond){
        message("Could not create network shapefiles")
        message(cond)
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not create network shapefiles")
      }
      )
    }
  )
  
  lyt8[1:9, 3] <- gp_8_text <- gframe("Tool description", container = lyt8, expand = TRUE, pos = 1)
  lyt8b <- glayout(container = gp_8_text, spacing = 10)
  lyt8b[1,1] <- glabel(paste0("This tool uses the individual benchmark shapefiles created using the 'Benchmark shp'
tool to construct shapefiles for networks.\n
The list of networks to create is provided in the 'Networks file' csv table. The table must
have a 'networks' column listing the network names to create, in the standard naming 
convention (e.g. PB_1_PB_2). Typically the summary results table (or a subset of) 
produced by the 'Representation summary' tool is used for this.\n
Each benchmark in the list of network names must be in the benchmark file.\n
A temporary file is also needed for storing data during processing. This can be deleted
after the tool has finished.\n
The standard output shapefile has a row for each benchmark in each network, and a 'networks'
field that groups benchmarks into networks. If the Dissolve box is checked an additional
shapefile is output (with a '_dslv' suffix) where benchmarks are dissolved into networks."))
  
  
  ###### tab 9 - evaluate existing polygons for benchmark status ######################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run evaluate_pa_shp. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt9 <- glayout(container = nb9)
  
  lyt9[1:8, 1:2] <- gp_9_frm_1 <- gframe("Evaluate existing areas", container = lyt9, spacing = 10, expand = TRUE)
  lyt9a <- glayout(container = gp_9_frm_1)
  
  # browse to select input shapefile
  lyt9a[2,1] <- "Shapefile to evaluate (.shp)"
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
  lyt9a[3,1] <- "Catchments file (.shp)"
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
  lyt9a[5,1] <- "Area target (km2)"
  lyt9a[5,2] <- gp_9_areatarget_txt <- gedit(text = "", 
                              container = lyt9a,
                              handler = function(h,...){
                                svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Area target: ", svalue(gp_9_areatarget_txt))
                              })
  
  # text box to input intactness threshold
  lyt9a[6,1] <- "Intactness threshold (%)"
  lyt9a[6,2] <- gp_9_threshold_txt <- gedit(text = "", 
                              container = lyt9a,
                              handler = function(h,...){
                                svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Intactness threshold: ", svalue(gp_9_threshold_txt))
                              })
  
  # browse to select output shp file
  lyt9a[7,1] <- "Output file (.shp)"
  lyt9a[7,2] <- gp_9_outFile_btn <- gfilebrowse( # file browser
    text = "", 
    type = "save", 
    quote = TRUE, 
    container = lyt9a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Output shapefile: ", svalue(gp_9_outFile_btn))
    }
  )
  
  # button to run the evaluate_pa function
  lyt9a[8,1:2] <- gp_9_run_btn <- gbutton(
    text = "Evaluate existing areas",
    container = lyt9a,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(length(svalue(gp_1_proj_btn))==0){
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "No projection file selected")
        stop("No projection file selected")
      }
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
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not complete evaluation")
      }
      )
    }
  )
  
  lyt9[1:8, 3] <- gp_9_text <- gframe("Tool description", container = lyt9, expand = TRUE, pos = 1)
  lyt9b <- glayout(container = gp_9_text, spacing = 10)
  lyt9b[1,1] <- glabel(paste0("This tool evaluates existing areas for benchmark status using user defined size and intactness values.
It was originally designed for testing existing protected areas for benchark status, but the tool
will accept any polygon as input.\n
The 'Shapefile to evaluate' can contain any number of polygons, but processing will be slower 
with more polygons.\n
The catchments shapefile must cover all of the shapefiles being evaluated. The larger the extent
of the catchments, the slower the tool will run.\n
The intactness column refers to the column name in the catchments shapefile attribute table that 
represents catchment intactness.\n
The area target is the minimum size required for benchmark status.\n
Any catchments with intactness values less than the intactness threshold will be removed from
the polygon being evaluated.\n
After removing catchments based on intactness, the tool adds fields to the attribute table
reporting the following attributes for each contiguous polygon larger than 1km2:
      - Area (km2)
      - Area target (km2)
      - Minimum intactness catchment overlapping the polygon
      - Size ratio - Area (km2) / Area target (km2)\n
Any polygons in the results file with size ratio values greater than or equal to 1.0 meet the
requirements for benchmark status."))
  
  
  ###### tab 10.1 - Add dissimilarity values to networks or benchmarks ######################################
  # Framing - glayout window contining two gframes. Each frame contains another layout grid for placing widgets. 
  # Frame 1 - widgets to run metricsDissimilarity.R. Columns 1:2.
  # Frame 2 - text describing inputs. Column 3.
  
  lyt10.1 <- glayout(container = nb10.1)
  
  lyt10.1[1:10, 1:2] <- gp_10.1_frm_1 <- gframe("Calculate dissimilarity values", container = lyt10.1, spacing = 10, expand = TRUE)
  lyt10.1a <- glayout(container = gp_10.1_frm_1)
  
  # browse to select input shapefile
  lyt10.1a[2,1] <- "Network file (.shp)"
  lyt10.1a[2,2] <- gp_10.1_inshp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt10.1a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Network shapefile: ", svalue(gp_10.1_inshp_btn))
    }
  )
  
  # browse to select input shapefile
  lyt10.1a[3,1] <- "Benchmark file (.shp)"
  lyt10.1a[3,2] <- gp_10.1_bashp_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt10.1a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Benchmark shapefile: ", svalue(gp_10.1_bashp_btn))
    }
  )
  
  lyt10.1a[4,1:2] <- gseparator(horizontal = TRUE, container = lyt10.1a, expand=TRUE)
  
  # browse to select raster1
  lyt10.1a[5,1] <- "Reference raster (.asc)"
  lyt10.1a[5,2] <- gp_10.1_ras1_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt10.1a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Reference raster: ", svalue(gp_10.1_ras1_btn))
    }
  )
  
  # browse to select raster2
  lyt10.1a[6,1] <- "Clip raster (.asc)"
  lyt10.1a[6,2] <- gp_10.1_ras2_btn <- gfilebrowse( # file browser
    text = "", 
    type = "open", 
    quote = TRUE, 
    container = lyt10.1a, 
    handler = function(h,...){
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Clip raster: ", svalue(gp_10.1_ras2_btn))
    }
  )
  
  # Radio checkbox to select continuous or categorical raster map
  lyt10.1a[7,1] <- gp_10.1_continuous_radio <- gradio(items = c("Continuous", "Categorical"), container = lyt10.1a,selected = 1)
  
  lyt10.1a[8,1:2] <- gseparator(horizontal = TRUE, container = lyt10.1a, expand=TRUE)
  
  # Text box to input new column name
  lyt10.1a[9,1] <- "New column name"
  lyt10.1a[9,2] <- gp_10.1_newCol_txt <- gedit(text = "", 
                                             container = lyt10.1a,
                                             handler = function(h,...){
                                               svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "New column name: ", svalue(gp_10.1_newCol_txt))
                                             })
  
  # button to run the metricsDissimilarity function
  lyt10.1a[10,1:2] <- gp_10.1_run_btn <- gbutton(
    text = "Calculate dissimilarity",
    container = lyt10.1a,
    handler = function(h, ...){ 
      svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Working...")
      if(svalue(gp_10.1_continuous_radio) == "Continuous"){continuousLogical <- TRUE}
      if(svalue(gp_10.1_continuous_radio) == "Categorical"){continuousLogical <- FALSE}
      tryCatch({
        metricsDissimilarity(
          netPath = svalue(gp_10.1_inshp_btn), 
          idColNet = "networks", 
          baPath = svalue(gp_10.1_bashp_btn), 
          idColBa = "PB",
          raster1 = svalue(gp_10.1_ras1_btn),
          raster2 = svalue(gp_10.1_ras2_btn),
          continuous = continuousLogical,
          newCol = svalue(gp_10.1_newCol_txt)
        )
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Dissimilarity complete")
      }, error = function(cond){
        message("Could not calculate dissimilarity")
        message(cond)
        svalue(txtOutput) <- paste0(svalue(txtOutput), "\n", "Could not calculate dissimilarity")
      }
      )
    }
  )
  
  lyt10.1[1:10, 3] <- gp_10.1_text <- gframe("Tool description", container = lyt10.1, expand = TRUE, pos = 1)
  lyt10.1b <- glayout(container = gp_10.1_text, spacing = 10)
  lyt10.1b[1,1] <- glabel(paste0("Each tool in the 'Add network attributes' tab adds new fields to the networks shapefile.
It is recommended you keep a copy of the original file before adding new attributes.
      ----------
The dissimilarity tool calculates dissimilarity values for criteria maps by comparing
the distribution of criteria values within the network, to the distribution in the entire boundary
area. Dissimilarity values closer to 1.0 have more similar distributions.\n
The network shapefile containing the networks to be evaluated is needed, as well as the benchmark
shapefile containing all benchmarks that appear in the networks. The ID fields hold the
unique network and benchmark names.
Note: Dissimilarity values can be added to the benchmark shapefile by setting both the network
and benchmark shapefile options to be the same file.\n
The reference raster should cover the boundary area only. This is the reference area against
which networks will be compared. The clip raster is clipped by each network in turn and must
therefore extend to include all networks in the network shapefile. If none of the networks
extend beyond the boundary area, then the same raster can be used for both reference and clip.\n
The raster maps should either be categorical maps in which case the Bray-Curtis statistic
is used, or they should be continuous maps in which case the KS-statistic is used.\n
The dissimilarity values will be added to a new column defined in the 'New column name'
option. The name should not contain any spaces."))
  
  
  #### output text box ############################################################
  
  frmOutput <- gframe("Messages", container = g, expand = TRUE, spacing = 10)
  #frmGroup <- ggroup(container = frmOutput)
  txtOutput <- gtext("", container = frmOutput, expand = TRUE)

}
