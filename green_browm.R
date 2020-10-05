install.packages("googledrive")

library(googledrive)
library(geospaar)
library(greenbrown)
library(rgee)

data(ndvimap)
plot(ndvimap)

phenmap <- PhenologyRaster(ndvimap, start=c(2000, 1), freq=12,
                           tsgf="TSGFspline", approach="Deriv")
plot(phenmap, 20)
plot(phenmap, 1)

# Median value not needed for eah pixel??

plot(phenmap, grep("SOS.2002", names(phenmap))) # start of season 2002
plot(phenmap, grep("EOS.2002", names(phenmap))) # end of season 2002
plot(phenmap, grep("LOS.2002", names(phenmap))) # length of season 2002
#plot(phenmap, grep("POP.1982", names(phenmap))) # position of peak value 2002
#plot(phenmap, grep("POT.1982", names(phenmap))) # position of trough value 2002
# plot(phenmap, grep("MGS.1982", names(phenmap))) # mean growing season value 2002
# plot(phenmap, grep("PEAK.1982", names(phenmap))) # peak value 2002
# plot(phenmap, grep("TROUGH.1982", names(phenmap))) # trough value 2002
# plot(phenmap, grep("MSP.1982", names(phenmap))) # mean spring value 2002
# plot(phenmap, grep("MAU.1982", names(phenmap))) # mean autumn value 2002
# plot(phenmap, grep("RSP.1982", names(phenmap))) # rate of spring greenup 2002
# plot(phenmap, grep("RAU.1982", names(phenmap))) # rate of autumn senescence 2002



# calculate trends on length of season using TrendRaster
losmap <- subset(phenmap, grep("LOS", names(phenmap)))
plot(losmap)
lostrend <- TrendRaster(losmap, start=c(2000, 1), freq=1)
plot(lostrend)

# classify trends in length of season
lostrend.cl <- TrendClassification(lostrend)
plot(lostrend.cl, col=brgr.colors(3), breaks=c(-1.5, -0.5, 0.5, 1.5))
# only a few pixels have a positive trend in the length of growing season


#we have one EOS for each pixel- we are confused on where to go next with this

