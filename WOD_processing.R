# WOD Processing
# 08-16-2016

###############################################################################
# ## Unzip into CTD folder
# setwd("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021")
# untar("ocldb1629138968.12439.CTD.tar.gz",list=TRUE)  ## check contents
# untar("ocldb1629138968.12439.CTD.tar.gz", exdir = "C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021/CTD")

file_data <- "C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021/CTD"
## get list of files
file_names <- list.files(path = file_data)

###############################################################################
## Look at the summary file
# library(ncdf4)
# nc <- nc_open(paste0(file_data, "/", file_names[1]))
# print(nc)
# attributes(nc$var)$names
# 
# lat_info <- ncatt_get(nc, attributes(nc$var)$names[1])
# lon_info <- ncatt_get(nc, attributes(nc$var)$names[2])
# time_info <- ncatt_get(nc, attributes(nc$var)$names[3])
# cast_info <- ncatt_get(nc, attributes(nc$var)$names[4])
# 
# 
# lat <-  ncvar_get(nc, attributes(nc$var)$names[1])
# lon <-  ncvar_get(nc, attributes(nc$var)$names[2])
# time <-  ncvar_get(nc, attributes(nc$var)$names[3])
# cast <-  ncvar_get(nc, attributes(nc$var)$names[4])
# 
# nc_close(nc)
# 
# 
# summ_df <- data.frame(time = time, cast = cast, lat = lat, lon = lon)
# 
# summ_df$time <- as.Date(summ_df$time, origin = '1770-01-01 00:00:00')
# 
# ggplot()+
#   geom_point(data = summ_df, aes(x = time, y = cast))

################################################################################
##  Function to make a df
grab_CTD <- function(file_data, file_name ){
  print(file_name)
  library(ncdf4)
  nc <- nc_open(paste0(file_data, "/", file_name))
  nms_in_file <- attributes(nc$var)$names
  # print(nms_in_file)
  
  n_r <- length(ncvar_get(nc, "Temperature"))
  
  df <- data.frame(lat = rep(NA, n_r), 
                   lon = rep(NA, n_r),
                   time = rep(NA, n_r), 
                   Temperature = rep(NA, n_r),
                   Salinity = rep(NA, n_r),
                   Pressure = rep(NA, n_r))
  
  var_nms <- names(df)
  
  var_nms <- var_nms[var_nms %in% nms_in_file]
  
  for(i in 1:length(var_nms)){
    # print(i)
    a <- ncatt_get(nc, var_nms[i])
    assign(paste0(var_nms[i],"_info"), a)
    rm(a)
  }
  
  for(i in 1:length(var_nms)){
    # print(i)
    df[,var_nms[i]] <- ncvar_get(nc, var_nms[i])
  }
  
  nc_close(nc)
  
  df$time <- as.Date(df$time, origin = unlist(strsplit(time_info$units, split = " "))[3])
  
  return(df)
}

################################################################################
## DO the first one
data <- grab_CTD(file_data = file_data, file_name = file_names[2])

## Loop through all of the files
counter <- 1
# for(i in 3:length(file_names)){
for(i in 12854:length(file_names)){
    
  data <- rbind(data, grab_CTD(file_data = file_data, file_name = file_names[i]))
  ## deleated wod_003395308O.nc, wod_003402501O.nc, wod_003406974O.nc, wod_009297091O.nc, missed one,  because they didnt have temperature
  counter <- counter +1
  print(paste0(counter, "/" ,(length(file_names)-1), "     ",
                ((counter/(length(file_names)-1)) *100), "% Finished"   ))
}

################################################################################
## Write out csv of data merge

write.csv(data, paste0("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021", "/WOD_GOM_CN_",Sys.Date(), ".csv"))
