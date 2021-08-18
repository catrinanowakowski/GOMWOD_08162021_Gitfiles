library(ggplot2)
library(gridExtra)

## Load WOD and the Ecomon data
WOD <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/data/WOD/GOMWOD_08162021/WODGOM_formatted_CN_2021-08-18.csv", header = TRUE, stringsAsFactors = FALSE)
ECOMON <- read.csv("C:/Users/catri/OneDrive/Documents/Bio/home/Chapter_One_2021/ecomon_sum_data.csv")


WOD$time <- round((WOD$year + ((WOD$month-1)/12)), digits = 3)
ECOMON$time <- round(ECOMON$time, digits = 3)
drops <- c("X")
WOD <- WOD[,!names(WOD)%in%drops]
ECOMON <- ECOMON[,!names(ECOMON)%in%drops]

df <- merge(WOD, ECOMON, by = c("time"), all =TRUE)

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


summary(lm(data = df[df$depth_bin == 50 , ], BT~Temperature)) #r2: .86  n: 301
summary(lm(data = df[df$depth_bin == 150, ], BT~Temperature)) #r2: .79  n: 292
summary(lm(data = df[df$depth_bin == 200, ], BT~Temperature)) #r2: .82  n: 280
summary(lm(data = df[df$depth_bin == 250, ], BT~Temperature)) #r2: .89  n: 184


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


summary(lm(data = df[df$depth_bin == 50 , ], SS~Salinity)) #r2: .32  n: 301
summary(lm(data = df[df$depth_bin == 150, ], BS~Salinity)) #r2: .30  n: 292
summary(lm(data = df[df$depth_bin == 200, ], BS~Salinity)) #r2: .30  n: 280
summary(lm(data = df[df$depth_bin == 250, ], BS~Salinity)) #r2: .31  n: 184


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

