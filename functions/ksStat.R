ksStat <- function(refVal, netVal, plotTitle="", regLab="Region", netLab="Network", saveAs="") {
  r1.val <- refVal
  r2.val <- netVal
  
  if (length(r2.val)==0) {
    print(length(r1.val))
    print(length(r2.val))
    return(NA)
  } else {
    # calculate KS statistic (representation index)
    ri <- sprintf("%.3f",ks.test(r1.val, r2.val)[[1]][[1]])
  }
  return(ri) # return KS statistic
}


# ksPlot <- function(refVal, netVal, plotTitle="", regLab="Region", netLab="Network", saveAs="") {
#   
#   library(ggplot2)
#   
#   r1.val <- refVal
# 	r2.val <- netVal
# 
# 	if (length(r2.val)==0) {
# 		print(length(r1.val))
# 		print(length(r2.val))
# 		return(NA)
# 	} else {
# 		# calculate KS statistic (representation index)
# 		ri <- sprintf("%.3f",ks.test(r1.val, r2.val)[[1]][[1]])
# 
# 		if (nchar(saveAs)>0) {
# 		#	cat("KS =",ri,"\n")
# 		#} else {
# 			# stack values and create data.frame
# 			z1 <- c(r1.val, r2.val)
# 			z2 <- c(rep(regLab,length(r1.val)), rep(netLab,length(r2.val)))
# 			zz <- data.frame(cbind(z1,z2),stringsAsFactors=FALSE)
# 			names(zz) <- c("values","criteria")
# 			zz$values <- round(as.numeric(zz$values),3)
# 
# 			# create and save density plot
# 			p <- ggplot(zz, aes(x=values)) + geom_density(aes(group=criteria, color=criteria)) +
# 			   ggtitle(paste(plotTitle," (KS = ",ri,")",sep="")) +
# 			   labs(x="Indicator value", y="Density") +
# 			   theme(legend.title=element_blank())
# 			ggsave(p, file=saveAs)
# 			#print(p)
# 		}
# 
# 		return(ri) # return KS statistic
# 	}
# }
