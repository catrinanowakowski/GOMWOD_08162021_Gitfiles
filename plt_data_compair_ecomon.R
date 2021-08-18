library(ggplot2)
library(gridExtra)

## Load WOD and the Ecomon data
WOD <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021/GOMWOD_08162021_Gitfiles/WODGOM_formatted_CN_2021-08-18.csv", header = TRUE, stringsAsFactors = FALSE)
# ECOMON <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/ecomon_sum_data.csv") ## cope_compact
ECOMON <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/og_ecomon_data.csv") ## original values (no interpolation)


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


summary(lm(data = df[df$depth_bin == 50 , ], BT~Temperature)) #r2: .99  n: 118
summary(lm(data = df[df$depth_bin == 100, ], BT~Temperature)) #r2: .35  n: 118
summary(lm(data = df[df$depth_bin == 150, ], BT~Temperature)) #r2: .75  n: 118
summary(lm(data = df[df$depth_bin == 200, ], BT~Temperature)) #r2: .72  n: 116
summary(lm(data = df[df$depth_bin == 250, ], BT~Temperature)) #r2: .94 n: 98

# nrow(df[df$depth_bin == 100, ])


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

