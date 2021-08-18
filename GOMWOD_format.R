data <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021/WOD_GOM_CN_2021-08-18.csv", header = TRUE, stringsAsFactors = FALSE)

## Drop out pressure NA because we would not know how deep it is
data <- data[!is.na(data$Pressure),]

data$time <- as.Date(data$time, "%Y-%m-%d")
# plot(data$time, data$Temperature)

## Fix out of range data
summary(data)
data$Temperature[data$Temperature == min(data$Temperature)] <- NA
data$Salinity[data$Salinity == min(data$Salinity, na.rm = TRUE)] <- NA
data$Salinity[data$Salinity == min(data$Salinity, na.rm = TRUE)] <- NA
data$Salinity[data$Salinity > 50 ] <- NA



## Format pressure to depth

## look at the region
library(maps); library(mapdata)
library(ggplot2)
library(RColorBrewer)


#Atlantic and Pacific N:P lines (after Sherwood et al. Biogeosciences, submitted)
AW <- c(15.54, -0.26)
PW <- c(14.07, -11.53)

fy=  c(42.3, 44);   fx=c(-68.5, -66.5)  

stations <- unique(data[,c("time","lat","lon")])
# map('world', xlim = c(-72, -65), ylim = c(41,45)); map.axes()
# points(stations$lon, stations$lat, pch=16)

#################################################################################
## Getting the data that is inside the GOM:
ECOMON <- read.csv("ECOMON_spatial_data.csv", header = TRUE, stringsAsFactors = FALSE)

ECOMON_GOM <- ECOMON[ECOMON$region ==4,]
# map('world', xlim = c(-72, -65), ylim = c(41,45)); map.axes()
# points(ECOMON_GOM$lon, ECOMON_GOM$lat, pch=16)

# x = rnorm(100)
# y = rnorm(100)
x <- ECOMON_GOM$longitude
y <- ECOMON_GOM$latitude
Dat = data.frame(x,y)
plot(Dat)

Mx = mean(x)
My = mean(y)
CH = chull(Dat)
BumpX_GOM = x[CH] + 0.01*(x[CH]-Mx)
BumpY_GOM = y[CH] + 0.01*(y[CH]-My)
polygon(BumpX_GOM, BumpY_GOM)


ECOMON_GB <- ECOMON[ECOMON$region ==3,]
# map('world', xlim = c(-72, -65), ylim = c(41,45)); map.axes()
# points(ECOMON_GB$lon, ECOMON_GB$lat, pch=16)

x <- ECOMON_GB$longitude
y <- ECOMON_GB$latitude
Dat = data.frame(x,y)
plot(Dat)

Mx = mean(x)
My = mean(y)
CH = chull(Dat)
BumpX_GB = x[CH] + 0.01*(x[CH]-Mx)
BumpY_GB = y[CH] + 0.01*(y[CH]-My)
polygon(BumpX_GB, BumpY_GB, col = "red")



## Test with the smaler data set first 
# library(sp)
# test <- ECOMON[point.in.polygon(point.x = ECOMON$longitude, point.y = ECOMON$latitude, pol.x = BumpX_GOM, pol.y = BumpY_GOM, mode.checked=FALSE) == 1,]
# test <- test[point.in.polygon(point.x = test$longitude, point.y = test$latitude, pol.x = BumpX_GB, pol.y = BumpY_GB, mode.checked=FALSE) == 0,]
# 
# map('world', xlim = c(-72, -65), ylim = c(41,45)); map.axes()
# points(test$longitude, test$latitude, pch=16)


################################################################################
## Get the locations of GOM stations

library(sp)
GOMWOD_loc <- stations[point.in.polygon(point.x = stations$lon, point.y = stations$lat, pol.x = BumpX_GOM, pol.y = BumpY_GOM, mode.checked=FALSE) == 1,]
GOMWOD_loc <- GOMWOD_loc[point.in.polygon(point.x = GOMWOD_loc$lon, point.y = GOMWOD_loc$lat, pol.x = BumpX_GB, pol.y = BumpY_GB, mode.checked=FALSE) == 0,]

## check 
# map('world', xlim = c(-72, -65), ylim = c(41,45)); map.axes()
# points(GOMWOD_loc$lon, GOMWOD_loc$lat, pch=16)


################################################################################
## Cut out the extra stations!
GOMWOD_data <- merge(data, GOMWOD_loc, by = c("time", "lat", "lon"), all = FALSE)

## Check 
# map('world', xlim = c(-72, -65), ylim = c(41,45)); map.axes()
# points(GOMWOD_data$lon, GOMWOD_data$lat, pch=16)


################################################################################
## Convert pressure to depth 
library(oce)
GOMWOD_data$depth <- swDepth(GOMWOD_data$Pressure, latitude=GOMWOD_data$lat)

################################################################################
## Bin data into 50m depth intervals
GOMWOD_data$depth_bin <- NA
GOMWOD_data$depth_bin[GOMWOD_data$depth < 50] <- 50
GOMWOD_data$depth_bin[GOMWOD_data$depth > 50 &  GOMWOD_data$depth < 100] <- 100
GOMWOD_data$depth_bin[GOMWOD_data$depth > 100 &  GOMWOD_data$depth < 150] <- 150
GOMWOD_data$depth_bin[GOMWOD_data$depth > 150 &  GOMWOD_data$depth < 200] <- 200
GOMWOD_data$depth_bin[GOMWOD_data$depth > 250] <- 250

GOMWOD_data <- aggregate(GOMWOD_data, 
                         by = list(GOMWOD_data$time, GOMWOD_data$lat, GOMWOD_data$lon, GOMWOD_data$depth_bin), 
                         FUN = mean, 
                         na.rm = TRUE)


################################################################################
## Average across months
GOMWOD_data$date <- as.character(GOMWOD_data$time)
GOMWOD_data$year <- NA
GOMWOD_data$month <- NA
GOMWOD_data$day <- NA

for(i in 1:nrow(GOMWOD_data)){
  GOMWOD_data$year[i] <- as.numeric(unlist(strsplit(GOMWOD_data$date[i], split = "-"))[1])
  GOMWOD_data$month[i] <- as.numeric(unlist(strsplit(GOMWOD_data$date[i], split = "-"))[2])
  GOMWOD_data$day[i] <- as.numeric(unlist(strsplit(GOMWOD_data$date[i], split = "-"))[3])
  print((i/nrow(GOMWOD_data))*100)
}

GOMWOD_month <- aggregate(GOMWOD_data, 
                         by = list(GOMWOD_data$year, GOMWOD_data$month, GOMWOD_data$lat, GOMWOD_data$lon, GOMWOD_data$depth_bin), 
                         FUN = mean, 
                         na.rm = TRUE)

keeps <- c("time", "lat", "lon", "Temperature", "Salinity", "depth_bin", "year", "month")
GOMWOD_month <- GOMWOD_month[,names(GOMWOD_month)%in%keeps]


################################################################################
## Average over space

GOMWOD_fmt <- aggregate(GOMWOD_month, 
                          by = list(GOMWOD_month$year, GOMWOD_month$month, GOMWOD_month$depth_bin), 
                          FUN = mean, 
                          na.rm = TRUE)

keeps <- c("time", "Temperature", "Salinity", "depth_bin", "year", "month")
GOMWOD_fmt <- GOMWOD_fmt[,names(GOMWOD_fmt)%in%keeps]



################################################################################
################################################################################
################################################################################

library(ggplot2)
ggplot() +
  geom_point(data = GOMWOD_fmt, aes(x = time, y = Temperature, color = as.factor(depth_bin))) +
  geom_line(data = GOMWOD_fmt[GOMWOD_fmt$depth_bin == 50,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  geom_line(data = GOMWOD_fmt[GOMWOD_fmt$depth_bin == 250,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  geom_line(data = GOMWOD_fmt[GOMWOD_fmt$depth_bin == 200,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  geom_line(data = GOMWOD_fmt[GOMWOD_fmt$depth_bin == 150,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

write.csv(GOMWOD_fmt, paste0("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021", "/WODGOM_formatted_CN_",Sys.Date(), ".csv"))

################################################################################
################################################################################
################################################################################
## Extra code

# library(sf)
# outlines <- rbind(data.frame(longitude = BumpX_GOM, latitude = BumpY_GOM, id = "GOM"), 
#                   data.frame(longitude = BumpX_GB , latitude = BumpY_GB , id = "GB"))
# outlines <- st_as_sf(outlines, coords=c("longitude","latitude"))
# 
# polys = st_sf(
#   aggregate(outlines$geometry,list((outlines$id)),
#             function(g){st_cast(st_combine(g),"POLYGON")
#    }#fun
#   )#agg
# )#pol
# 
# 
# plot(polys[polys$Group.1 == "GB",])
# plot(polys[polys$Group.1 == "GOM",])
# plot(polys)

