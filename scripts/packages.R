# Marc Edwards - BEACONs
# Feb 21 2017
# add packages to packages folder
# functions will load packages from this folder which should avoid any package issues for new R instalations

# make packages directory, install packages, open packages
if(!dir.exists("../packages")){
  dir.create("../packages")
}

install.packages("raster", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("maptools", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("rgeos", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("sp", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("dplyr", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("readr", lib = "../packages/", repos='http://cran.stat.sfu.ca')
#install.packages("ggplot2", lib = "../packages/")
install.packages("foreign", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("memoise", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("digest", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("RGtk2", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("gWidgets2RGtk2", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("rgdal", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("ggplot2", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("ggplot2", lib = "../packages/", repos='http://cran.stat.sfu.ca')
install.packages("data.table", lib = "../packages/", repos='http://cran.stat.sfu.ca')
