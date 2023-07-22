#all: data analysis paper publish
#analysis: all.Rout imp.Rout cv_setup.Rout cv.Rout
#data: un_utilities.Rout data/rep.csv setup.Rout mi.Rout
#output: output fig tab

all: data
data: demand.Rout emsbed.Rout firestation.Rout

demand.Rout: R/demand.R
	R CMD BATCH R/demand.R
	
emsbed.Rout: R/emsbed.R demand.Rout
	R CMD BATCH R/emsbed.R
	
firestation.Rout: R/firestation.R emsbed.Rout
	R CMD BATCH R/firestation.R
	
#clean_all:
#	find . | egrep ".*((\.(RData|Rout|Rhistory))|~)$$" | xargs rm
#	rm -rf auto

#test.Rout: R/test.R
#	R CMD BATCH R/test.R
#ff
