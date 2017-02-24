bcStat <- function(refVal, netVal) {
  # Make sure to remove undesired categories e.g., class 0 or urban/nonveg, cropland

  r1.val <- refVal
  r1.tab <- table(r1.val)
  x1 <- data.frame(names(r1.tab),as.vector(r1.tab))
  names(x1) <- c("cat",cat="strata")

  r2.val <- netVal
  r2.tab <- table(r2.val)
  x2 <- data.frame(names(r2.tab),as.vector(r2.tab))
  names(x2) <- c("cat",cat="reserve")

  x <- merge(x1,x2,by="cat",all=T)
  x$cat <- as.character(x$cat)
  x$strata <- as.numeric(x$strata)
  x$reserve <- as.numeric(x$reserve)
  x$reserve[is.na(x$reserve)] <- 0
  x$reserve[is.na(x$strata)] <- 0 # this is needed in case there is one reserve pixel and no strata pixel
  x$strata[is.na(x$strata)] <- 0
  x$strata <- x$strata/sum(x$strata) #as.integer(x$strata)
  x$reserve <- x$reserve/sum(x$reserve) #as.integer(x$reserve)

  #print(x)

  # calculate Bray-Curtis dissimilariy
  ri <- sprintf("%.3f",sum(abs(x$strata-x$reserve))/(sum(x$strata)+sum(x$reserve)))

  return(ri)
}

# bcPlot <- function(refVal, netVal, plotTitle="", regLab="Region", netLab="Network", labels="", saveAs="") {
# 
#   if (labels=="") {
#     stop("No labels were defined!")
#   } else {
#     cat.labels <- labels
#   }
# 
#   r1.val <- refVal
#   r1.tab <- table(r1.val)
#   x1 <- data.frame(names(r1.tab),as.vector(r1.tab))
#   names(x1) <- c("cat",cat="strata")
# 
#   r2.val <- netVal
#   r2.tab <- table(r2.val)
#   x2 <- data.frame(names(r2.tab),as.vector(r2.tab))
#   names(x2) <- c("cat",cat="reserve")
# 
#   x <- merge(x1,x2,by="cat",all=T)
#   x$cat <- as.character(x$cat)
#   x$strata <- as.numeric(x$strata)
#   x$reserve <- as.numeric(x$reserve)
#   x$reserve[is.na(x$reserve)] <- 0
#   x$reserve[is.na(x$strata)] <- 0 # this is needed in case there is one reserve pixel and no strata pixel
#   x$strata[is.na(x$strata)] <- 0
#   x$strata <- x$strata/sum(x$strata) #as.integer(x$strata)
#   x$reserve <- x$reserve/sum(x$reserve) #as.integer(x$reserve)
#   x$cat <- paste(tolower(plotTitle),x$cat,sep="")
#   x$cat <- factor(x$cat, levels = names(cat.labels))
#   #x <- x[order(-as.integer(x$cat)),] # integer needed to sort properly 1, 2, 3 not 1, 10, 11
# 
#   # calculate Bray-Curtis dissimilariy
#   ri <- sprintf("%.3f",sum(abs(x$strata-x$reserve))/(sum(x$strata)+sum(x$reserve)))
# 
#   if (nchar(saveAs)>0) {
# 	#cat("BC =",ri,"\n")
#   #} else {
# 	  for (k in 1:nrow(x)) {
# 		x$cat2[k] <- unlist(cat.labels[x$cat[k]])
# 	  }
# 	  x$cat2 <- factor(x$cat2, levels = rev(as.character(cat.labels)))
# 	  #print(x)
# 	  # calculate location of dots
# 	  y.seq <- seq(0.7, nrow(x)*1.2, 1.2)
# 
# 	  p <- ggplot(x, aes(x=cat2, y=reserve)) + geom_bar(stat="identity", fill="white", colour="black") + coord_flip()
# 	  p <- p + geom_point(data=x, aes(x=cat2, y=strata), colour="black", size=3) + theme(legend.position = "none")
# 	  p <- p + labs(x="", y="Proportional area (dots indicate regional proportions)")
# 	  p <- p + ggtitle(paste(toupper(plotTitle)," (BC = ",ri,")",sep=""))
# 	  p <- p + theme(axis.text.y = element_text(colour="#000000"),axis.text.x  = element_text(colour="#000000"))
# 	  ggsave(p, file=saveAs)
# 	  #print(p)
#   }
# 
#   return(ri)
# }
