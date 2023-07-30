#all: data analysis paper publish
#analysis: all.Rout imp.Rout cv_setup.Rout cv.Rout
#data: un_utilities.Rout data/rep.csv setup.Rout mi.Rout


all: data visualize analysis
data: demand.Rout emsbed.Rout firestation.Rout roadnetwork.Rout odcost.Rout
visualize: explore.Rout
analysis: access.Rout

# demand estimation from ohca occurrence estimation
demand.Rout: script/demand.R
	R CMD BATCH script/demand.R

# ems beds from hira hospital information
emsbed.Rout: script/emsbed.R demand.Rout
	R CMD BATCH script/emsbed.R		
	
firestation.Rout: script/firestation.R emsbed.Rout
	R CMD BATCH script/firestation.R	

roadnetwork.Rout: script/roadnetwork.R demand.Rout
	R CMD BATCH script/roadnetwork.R

explore.Rout: script/explore.R roadnetwork.Rout firestation.Rout emsbed.Rout
	R CMD BATCH script/explore.R

odcost.Rout: script/odcost.R roadnetwork.Rout firestation.Rout emsbed.Rout
	R CMD BATCH script/odcost.R

access.Rout: script/access.R odcost.Rout
	R CMD BATCH script/access.R

all:
	@echo "Done."


#clean_all:
#	find . | egrep ".*((\.(RData|Rout|Rhistory))|~)$$" | xargs rm
#	rm -rf auto

