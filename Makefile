#all: data analysis paper publish
#analysis: all.Rout imp.Rout cv_setup.Rout cv.Rout
#data: un_utilities.Rout data/rep.csv setup.Rout mi.Rout
#output: output fig tab

all: data
data: demand.Rout emsbed.Rout firestation.Rout

# demand estimation from ohca occurrence estimation
demand.Rout: script/demand.R
	R CMD BATCH script/demand.R

# ems beds from hira hospital information
emsbed.Rout: script/emsbed.R demand.Rout
	R CMD BATCH script/emsbed.R		
	
firestation.Rout: script/firestation.R
	R CMD BATCH script/firestation.R	
	
#clean_all:
#	find . | egrep ".*((\.(RData|Rout|Rhistory))|~)$$" | xargs rm
#	rm -rf auto

