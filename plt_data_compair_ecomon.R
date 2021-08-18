library(ggplot2)
library(gridExtra)

## Load WOD and the Ecomon data
# WOD <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021/GOMWOD_08162021_Gitfiles/WODGOM_formatted_CN_2021-08-18.csv", header = TRUE, stringsAsFactors = FALSE)
WOD <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021/GOMWOD_08162021_Gitfiles/WODGOM_East_formatted_CN_2021-08-18.csv", header = TRUE, stringsAsFactors = FALSE)
# ECOMON <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/ecomon_sum_data.csv") ## cope_compact
ECOMON <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/og_ecomon_data.csv") ## original values (no interpolation)
EcoMon_Fits <- ECOMON

WOD$time <- round((WOD$year + ((WOD$month-1)/12)), digits = 3)
ECOMON$time <- round((ECOMON$year + ((ECOMON$month-1)/12)), digits = 3)


drops <- c("X", "date")
WOD <- WOD[,!names(WOD)%in%drops]
ECOMON <- ECOMON[,!names(ECOMON)%in%drops]
names(ECOMON) <- c("year", "month", "BT", "ST", "BS", "SS", "time")


df <- merge(WOD, ECOMON, by = c("time", "year", "month"), all =TRUE)

df <- df[!is.na(df$Temperature),]
df <- df[!is.na(df$BT),]


plt_S <- ggplot() +
  geom_point(data = df[df$depth_bin == 50, ], aes(x = Temperature, y =ST, color = "Surface")) +
  theme_bw()

plt_150 <- ggplot() +
  geom_point(data = df[df$depth_bin == 150, ], aes(x = Temperature, y =BT, color = "150")) +
  theme_bw()
plt_200 <- ggplot() +
  geom_point(data = df[df$depth_bin == 200, ], aes(x = Temperature, y =BT, color = "200")) +
  theme_bw()
plt_250 <- ggplot() +
  geom_point(data = df[df$depth_bin == 250, ], aes(x = Temperature, y =BT, color = "250")) +
  theme_bw()


grid.arrange(plt_S, plt_150, plt_200, plt_250)


summary(lm(data = df[df$depth_bin == 50 , ], BT~Temperature)) #r2: .99  n: 118 ## East r2: .17
summary(lm(data = df[df$depth_bin == 100, ], BT~Temperature)) #r2: .35  n: 118 ## East r2: .37
summary(lm(data = df[df$depth_bin == 150, ], BT~Temperature)) #r2: .75  n: 118 ## East r2: .56
summary(lm(data = df[df$depth_bin == 200, ], BT~Temperature)) #r2: .72  n: 116 ## East r2: .64
summary(lm(data = df[df$depth_bin == 250, ], BT~Temperature)) #r2: .94  n: 98  ## East r2: .23

# nrow(df[df$depth_bin == 100, ])

library(mgcv)
depth <- 50
y = df$ST[df$depth_bin == depth]
time = df$time[df$depth_bin == depth]
month = df$month[df$depth_bin == depth]
Temp <- df$Temperature[df$depth_bin == depth]
gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) + s(Temp, k = 4), method = "REML")
# gam_y <- gam(y ~ s(Temp, k = 4), method = "REML")
# gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) , method = "REML")

y_fit <- predict(gam_y, data.frame(time = WOD$time[WOD$depth_bin == depth], 
                                   month = WOD$month[WOD$depth_bin == depth], 
                                   Temp = WOD$Temperature[WOD$depth_bin == depth]))

ggplot() +
  geom_point(aes(x = time, y = y)) +
  geom_line(aes(x = time, y = y)) +
  geom_point(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  geom_line(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  theme_bw()
   
# plot(time,y, type = "l")
# points(time,y, col = "black")
# points(WOD$time[WOD$depth_bin == depth],y_fit, col = "red")

summary(gam_y)
plot(gam_y)

ST_df <- data.frame(time = WOD$time[WOD$depth_bin == depth], st_fit = y_fit)
EcoMon_Fits <- merge(EcoMon_Fits, ST_df, by = c("time"), all = TRUE)

##################################################
library(mgcv)
depth <- 200
y = df$BT[df$depth_bin == depth]
time = df$time[df$depth_bin == depth]
month = df$month[df$depth_bin == depth]
Temp <- df$Temperature[df$depth_bin == depth]
gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) + s(Temp, k = 4), method = "REML")
# gam_y <- gam(y ~ s(Temp, k = 4), method = "REML")
# gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) , method = "REML")

y_fit <- predict(gam_y, data.frame(time = WOD$time[WOD$depth_bin == depth], 
                                   month = WOD$month[WOD$depth_bin == depth], 
                                   Temp = WOD$Temperature[WOD$depth_bin == depth]))

ggplot() +
  geom_point(aes(x = time, y = y)) +
  geom_line(aes(x = time, y = y)) +
  geom_point(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  geom_line(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  theme_bw()

# plot(time,y, type = "l")
# points(time,y, col = "black")
# points(WOD$time[WOD$depth_bin == depth],y_fit, col = "red")

summary(gam_y)
plot(gam_y)

BT_df <- data.frame(time = WOD$time[WOD$depth_bin == depth], bt_fit = y_fit)
EcoMon_Fits <- merge(EcoMon_Fits, BT_df, by = c("time"), all = TRUE)

################################################################################
## Plot the bottom depths

plt_50 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 50,], aes(x = time, y = Temperature), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 50,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_100 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 100,], aes(x = time, y = Temperature), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 100,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_150 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 150,], aes(x = time, y = Temperature), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 150,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_200 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 200,], aes(x = time, y = Temperature), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 200,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_250 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 250,], aes(x = time, y = Temperature), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 250,], aes(x = time, y = Temperature, color = as.factor(depth_bin)), size = 1) +
  theme_bw()



grid.arrange(plt_50, plt_100, plt_150, plt_200, plt_250, ncol = 1)





#####################################################################################
## SALINITY



plt_S <- ggplot() +
  geom_point(data = df[df$depth_bin == 50, ], aes(x = Salinity, y =SS, color = "Surface")) +
  theme_bw()

plt_150 <- ggplot() +
  geom_point(data = df[df$depth_bin == 150, ], aes(x = Salinity, y =BS, color = "150")) +
  theme_bw()
plt_200 <- ggplot() +
  geom_point(data = df[df$depth_bin == 200, ], aes(x = Salinity, y =BS, color = "200")) +
  theme_bw()
plt_250 <- ggplot() +
  geom_point(data = df[df$depth_bin == 250, ], aes(x = Salinity, y =BS, color = "250")) +
  theme_bw()


grid.arrange(plt_S, plt_150, plt_200, plt_250)


summary(lm(data = df[df$depth_bin == 50 , ], SS~Salinity)) #r2: .28  n: 
summary(lm(data = df[df$depth_bin == 150, ], BS~Salinity)) #r2: .26  n: 
summary(lm(data = df[df$depth_bin == 200, ], BS~Salinity)) #r2: .23  n: 
summary(lm(data = df[df$depth_bin == 250, ], BS~Salinity)) #r2: .34  n: 

##################################################
library(mgcv)
depth <- 50
y = df$SS[df$depth_bin == depth]
time = df$time[df$depth_bin == depth]
month = df$month[df$depth_bin == depth]
Sal <- df$Salinity[df$depth_bin == depth]
gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) + s(Sal, k = 4), method = "REML")
# gam_y <- gam(y ~ s(Sal, k = 4), method = "REML")
# gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) , method = "REML")

y_fit <- predict(gam_y, data.frame(time = WOD$time[WOD$depth_bin == depth], 
                                   month = WOD$month[WOD$depth_bin == depth], 
                                   Sal = WOD$Salinity[WOD$depth_bin == depth]))

ggplot() +
  geom_point(aes(x = time, y = y)) +
  geom_line(aes(x = time, y = y)) +
  geom_point(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  geom_line(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  theme_bw()

# plot(time,y, type = "l")
# points(time,y, col = "black")
# points(WOD$time[WOD$depth_bin == depth],y_fit, col = "red")

summary(gam_y)
plot(gam_y)

SS_df <- data.frame(time = WOD$time[WOD$depth_bin == depth], ss_fit = y_fit)
EcoMon_Fits <- merge(EcoMon_Fits, SS_df, by = c("time"), all = TRUE)

##################################################
library(mgcv)
depth <- 200
y = df$BS[df$depth_bin == depth]
time = df$time[df$depth_bin == depth]
month = df$month[df$depth_bin == depth]
Sal <- df$Salinity[df$depth_bin == depth]
gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) + s(Sal, k = 4), method = "REML")
# gam_y <- gam(y ~ s(Sal, k = 4), method = "REML")
# gam_y <- gam(y ~ s(time, k = 4) + s(month, k = 4) , method = "REML")

y_fit <- predict(gam_y, data.frame(time = WOD$time[WOD$depth_bin == depth], 
                                   month = WOD$month[WOD$depth_bin == depth], 
                                   Sal = WOD$Salinity[WOD$depth_bin == depth]))

ggplot() +
  geom_point(aes(x = time, y = y)) +
  geom_line(aes(x = time, y = y)) +
  geom_point(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  geom_line(aes(x = WOD$time[WOD$depth_bin == depth], y = y_fit), color = "red") +
  theme_bw()

# plot(time,y, type = "l")
# points(time,y, col = "black")
# points(WOD$time[WOD$depth_bin == depth],y_fit, col = "red")

summary(gam_y)
plot(gam_y)


BS_df <- data.frame(time = WOD$time[WOD$depth_bin == depth], bs_fit = y_fit)
EcoMon_Fits <- merge(EcoMon_Fits, BS_df, by = c("time"), all = TRUE)
################################################################################
## Plot the bottom depths

plt_50 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 50,], aes(x = time, y = Salinity), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 50,], aes(x = time, y = Salinity, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_100 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 100,], aes(x = time, y = Salinity), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 100,], aes(x = time, y = Salinity, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_150 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 150,], aes(x = time, y = Salinity), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 150,], aes(x = time, y = Salinity, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_200 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 200,], aes(x = time, y = Salinity), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 200,], aes(x = time, y = Salinity, color = as.factor(depth_bin)), size = 1) +
  theme_bw()

plt_250 <- ggplot() +
  geom_point(data = WOD[WOD$depth_bin == 250,], aes(x = time, y = Salinity), size = 1) +
  geom_line(data = WOD[WOD$depth_bin == 250,], aes(x = time, y = Salinity, color = as.factor(depth_bin)), size = 1) +
  theme_bw()



grid.arrange(plt_50, plt_100, plt_150, plt_200, plt_250, ncol = 1)




##############################################################
## Format Ecomon data fits from the WOD to one vector

fn_df <- EcoMon_Fits
fn_df$BT[is.na(fn_df$BT)] <- fn_df$bt_fit[is.na(fn_df$BT)]
fn_df$ST[is.na(fn_df$ST)] <- fn_df$st_fit[is.na(fn_df$ST)]
fn_df$BS[is.na(fn_df$BS)] <- fn_df$bs_fit[is.na(fn_df$BS)]
fn_df$SS[is.na(fn_df$SS)] <- fn_df$ss_fit[is.na(fn_df$SS)]

drops <- c("st_fit",   "bt_fit",   "ss_fit",   "bs_fit")
fn_df <- fn_df[,!names(fn_df)%in% drops]

write.csv(fn_df, "EcoMon_adjustedbyEASTGOMWOD.csv")

bt_plt <- ggplot() +
  geom_point(data = EcoMon_Fits, aes(x = time, y = BT)) +
  geom_line(data = fn_df, aes(x = time, y = BT)) +
  geom_point(data = EcoMon_Fits, aes(x = time, y = bt_fit), color = "red") +
  # geom_line(data = EcoMon_Fits, aes(x = time, y = bt_fit), color = "red") +
  theme_bw()
st_plt <- ggplot() +
  geom_point(data = EcoMon_Fits, aes(x = time, y = ST)) +
  geom_line(data = fn_df, aes(x = time, y = ST)) +
  geom_point(data = EcoMon_Fits, aes(x = time, y = st_fit), color = "red") +
  # geom_line(data = EcoMon_Fits, aes(x = time, y = st_fit), color = "red") +
  theme_bw()
bs_plt <- ggplot() +
  geom_point(data = EcoMon_Fits, aes(x = time, y = BS)) +
  geom_line(data = fn_df, aes(x = time, y = BS)) +
  geom_point(data = EcoMon_Fits, aes(x = time, y = bs_fit), color = "red") +
  # geom_line(data = EcoMon_Fits, aes(x = time, y = bs_fit), color = "red") +
  theme_bw()
ss_plt <- ggplot() +
  geom_point(data = EcoMon_Fits, aes(x = time, y = SS)) +
  geom_line(data = fn_df, aes(x = time, y = SS)) +
  geom_point(data = EcoMon_Fits, aes(x = time, y = ss_fit), color = "red") +
  # geom_line(data = EcoMon_Fits, aes(x = time, y = ss_fit), color = "red") +
  theme_bw()
library(gridExtra)
grid.arrange(st_plt, ss_plt, bt_plt,bs_plt, ncol = 2)

