## County level probabilities
cols <- ncol(counties)
counties_slf <- counties
counties_slf_max <- counties
counties_slf_mean <- counties
layers <- seq(1:nlayers(slf_usa_1km))

for (i in layers) {
  slf_prob <- slf_usa_1km[[i]]
  slf_prob[slf_prob < 0] <- 0
  slf_prob[is.na(slf_prob)] <- 0
  col <- cols + i
  prob_slf_county_max <- extract(slf_prob, counties_slf,  fun = max)
  prob_slf_county_mean <- extract(slf_prob, counties_slf,  fun = mean)
  counties_slf_max[1:nrow(counties_slf_max), col] <- as.numeric(prob_slf_county_max)
  counties_slf_mean[1:nrow(counties_slf_mean), col] <- as.numeric(prob_slf_county_mean)
  print(i)
}
names(counties_slf_max)[8:ncol(counties_slf_max)] <- paste('Y',seq(2020,2050,1), sep='')
names(counties_slf_mean)[8:ncol(counties_slf_max)] <- paste('Y',seq(2020,2050,1), sep='')


## crops data translate
counties_crop <- counties

grapes <- read.csv(ffcrops("Grapes_county_2002_2017_census.csv"))
plums  <- read.csv(ffcrops("Plums_county_2002_2017_census.csv"))
peaches<- read.csv(ffcrops("Peaches_county_2002_2017_census.csv"))
hops   <- read.csv(ffcrops("Hops_county_2002_2017_census.csv"))
cherries<- read.csv(ffcrops("Cherries_county_2002_2017_census.csv"))
apricots<- read.csv(ffcrops("Apricots_county_2002_2017_census.csv"))
apples<- read.csv(ffcrops("Apples_county_2002_2017_census.csv"))
almonds<- read.csv(ffcrops("Almonds_county_2002_2017_census.csv"))
walnuts<- read.csv(ffcrops("Walnuts_county_2002_2017_census.csv"))

grapes_2107 <- grapes[grapes$Year == 2017 & grapes$Data.Item == "GRAPES - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
plums_2107 <- plums[plums$Year == 2017 & plums$Data.Item == "PLUMS - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
peaches_2107 <- peaches[peaches$Year == 2017 & peaches$Data.Item == "PEACHES - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
hops_2107 <- hops[hops$Year == 2017 & hops$Data.Item == "HOPS - ACRES HARVESTED", c("Year", "State", "County", "Value")]
cherries_2107_sweet <- cherries[cherries$Year == 2017 & cherries$Data.Item == "CHERRIES, SWEET - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
cherries_2107_tart <- cherries[cherries$Year == 2017 & cherries$Data.Item == "CHERRIES, TART - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
apricots_2107 <- apricots[apricots$Year == 2017 & apricots$Data.Item == "APRICOTS - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
apples_2107 <- apples[apples$Year == 2017 & apples$Data.Item == "APPLES - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
almonds_2107 <- almonds[almonds$Year == 2017 & almonds$Data.Item == "ALMONDS - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]
walnuts_2107 <- walnuts[walnuts$Year == 2017 & walnuts$Data.Item == "WALNUTS, ENGLISH - ACRES BEARING & NON-BEARING", c("Year", "State", "County", "Value")]

grapes_2107$Value <- as.numeric(gsub(",","",grapes_2107$Value))
plums_2107$Value <- as.numeric(gsub(",","",plums_2107$Value))
peaches_2107$Value <- as.numeric(gsub(",","",peaches_2107$Value))
hops_2107$Value <- as.numeric(gsub(",","",hops_2107$Value))
cherries_2107_sweet$Value <- as.numeric(gsub(",","",cherries_2107_sweet$Value))
cherries_2107_tart$Value <- as.numeric(gsub(",","",cherries_2107_tart$Value))
apricots_2107$Value <- as.numeric(gsub(",","",apricots_2107$Value))
apples_2107$Value <- as.numeric(gsub(",","",apples_2107$Value))
almonds_2107$Value <- as.numeric(gsub(",","",almonds_2107$Value))
walnuts_2107$Value <- as.numeric(gsub(",","",walnuts_2107$Value))

grapes_2107$Value[is.na(grapes_2107$Value)] <- 0
plums_2107$Value[is.na(plums_2107$Value)] <- 0
peaches_2107$Value[is.na(peaches_2107$Value)] <- 0
hops_2107$Value[is.na(hops_2107$Value)] <- 0
cherries_2107_sweet$Value[is.na(cherries_2107_sweet$Value)] <- 0
cherries_2107_tart$Value[is.na(cherries_2107_tart$Value)] <- 0
apricots_2107$Value[is.na(apricots_2107$Value)] <- 0
apples_2107$Value[is.na(apples_2107$Value)] <- 0
almonds_2107$Value[is.na(almonds_2107$Value)] <- 0
walnuts_2107$Value[is.na(walnuts_2107$Value)] <- 0

counties_crop$grapes <- 0
counties_crop$plums <- 0
counties_crop$peaches <- 0
counties_crop$hops <- 0
counties_crop$cherries_sweet <- 0
counties_crop$cherries_tart <- 0
counties_crop$apricots <- 0
counties_crop$apples <- 0
counties_crop$almonds <- 0
counties_crop$walnuts <- 0
count_gr = 0
count_pl = 0
count_pe = 0
count_ho = 0
count_at = 0
count_cht = 0
count_chs = 0
count_ap = 0
count_al = 0
count_wn = 0
for (i in 1:nrow(counties_crop)){
  if (nrow(grapes_2107[tolower(counties_crop$NAME[i]) == tolower(grapes_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(grapes_2107$State),]) > 0) {
    counties_crop$grapes[i] <- grapes_2107$Value[tolower(counties_crop$NAME[i]) == tolower(grapes_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(grapes_2107$State)]
    count_gr = count_gr + 1
  }
  if (nrow(plums_2107[tolower(counties_crop$NAME[i]) == tolower(plums_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(plums_2107$State),]) > 0) {
    counties_crop$plums[i] <- plums_2107$Value[tolower(counties_crop$NAME[i]) == tolower(plums_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(plums_2107$State)]
    count_pl = count_pl + 1
  }
  if (nrow(peaches_2107[tolower(counties_crop$NAME[i]) == tolower(peaches_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(peaches_2107$State),]) > 0) {
    counties_crop$peaches[i] <- peaches_2107$Value[tolower(counties_crop$NAME[i]) == tolower(peaches_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(peaches_2107$State)]
    count_pe = count_pe + 1
  }
  if (nrow(hops_2107[tolower(counties_crop$NAME[i]) == tolower(hops_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(hops_2107$State),]) > 0) {
    counties_crop$hops[i] <- hops_2107$Value[tolower(counties_crop$NAME[i]) == tolower(hops_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(hops_2107$State)]
    count_ho = count_ho + 1
  }
  if (nrow(apricots_2107[tolower(counties_crop$NAME[i]) == tolower(apricots_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(apricots_2107$State),]) > 0) {
    counties_crop$apricots[i] <- apricots_2107$Value[tolower(counties_crop$NAME[i]) == tolower(apricots_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(apricots_2107$State)]
    count_at = count_at + 1
  }
  if (nrow(cherries_2107_sweet[tolower(counties_crop$NAME[i]) == tolower(cherries_2107_sweet$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(cherries_2107_sweet$State),]) > 0) {
    counties_crop$cherries_sweet[i] <- cherries_2107_sweet$Value[tolower(counties_crop$NAME[i]) == tolower(cherries_2107_sweet$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(cherries_2107_sweet$State)]
    count_chs = count_chs + 1
  }
  if (nrow(cherries_2107_tart[tolower(counties_crop$NAME[i]) == tolower(cherries_2107_tart$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(cherries_2107_tart$State),]) > 0) {
    counties_crop$cherries_tart[i] <- cherries_2107_tart$Value[tolower(counties_crop$NAME[i]) == tolower(cherries_2107_tart$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(cherries_2107_tart$State)]
    count_cht = count_cht + 1
  }
  if (nrow(apples_2107[tolower(counties_crop$NAME[i]) == tolower(apples_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(apples_2107$State),]) > 0) {
    counties_crop$apples[i] <- apples_2107$Value[tolower(counties_crop$NAME[i]) == tolower(apples_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(apples_2107$State)]
    count_ap = count_ap + 1
  }
  if (nrow(almonds_2107[tolower(counties_crop$NAME[i]) == tolower(almonds_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(almonds_2107$State),]) > 0) {
    counties_crop$almonds[i] <- almonds_2107$Value[tolower(counties_crop$NAME[i]) == tolower(almonds_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(almonds_2107$State)]
    count_al = count_al + 1
  }
  if (nrow(walnuts_2107[tolower(counties_crop$NAME[i]) == tolower(walnuts_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(walnuts_2107$State),]) > 0) {
    counties_crop$walnuts[i] <- walnuts_2107$Value[tolower(counties_crop$NAME[i]) == tolower(walnuts_2107$County) & tolower(counties_crop$STATE_NAME[i]) == tolower(walnuts_2107$State)]
    count_wn = count_wn + 1
  }
  print(i)
}


counties_crops <- data.frame(counties_crop@data)
# counties_crops$grapes <- as.numeric(gsub(",","",counties_crops$grapes))
# counties_crops$plums <- as.numeric(gsub(",","",counties_crops$plums))
# counties_crops$peaches <- as.numeric(gsub(",","",counties_crops$peaches))
# counties_crops$hops <- as.numeric(gsub(",","",counties_crops$hops))
# counties_crops$cherries_sweet <- as.numeric(gsub(",","",counties_crops$cherries_sweet))
# counties_crops$cherries_tart <- as.numeric(gsub(",","",counties_crops$cherries_tart))
# counties_crops$apricots <- as.numeric(gsub(",","",counties_crops$apricots))
# counties_crops$apples <- as.numeric(gsub(",","",counties_crops$apples))
# counties_crops$almonds <- as.numeric(gsub(",","",counties_crops$almonds))
# counties_crops$walnuts <- as.numeric(gsub(",","",counties_crops$walnuts))
counties_crop$cherries = rowSums(counties_crops[,c("cherries_sweet", "cherries_tart")], na.rm = TRUE)

writeOGR(obj=counties_crop, dsn=ffIn("counties_crops"), layer ="counties_crops", driver="GPKG", overwrite_layer=TRUE)

counties_slf_risk_mean <- data.frame(counties_slf_mean@data)
counties_slf_risk_max <- data.frame(counties_slf_max@data)

for (i in 8:ncol(counties_slf_risk_mean)) {
  counties_slf_risk_mean[is.na(counties_slf_risk_mean[,i]), i] <- 0
  counties_slf_risk_mean[counties_slf_risk_mean[,i] <= 8.359 , i] <- 0
  counties_slf_risk_mean[counties_slf_risk_mean[,i] > 8.359  & counties_slf_risk_mean[,i] <= 26.89, i] <- 1
  counties_slf_risk_mean[counties_slf_risk_mean[,i] > 26.89 & counties_slf_risk_mean[,i] <= 51.99 , i] <- 2
  counties_slf_risk_mean[counties_slf_risk_mean[,i] > 51.99, i] <- 3
  
  counties_slf_risk_max[is.na(counties_slf_risk_max[,i]), i] <- 0
  counties_slf_risk_max[counties_slf_risk_max[,i] <= 8.359 , i] <- 0
  counties_slf_risk_max[counties_slf_risk_max[,i] > 8.359  & counties_slf_risk_max[,i] <= 26.89, i] <- 1
  counties_slf_risk_max[counties_slf_risk_max[,i] > 26.89 & counties_slf_risk_max[,i] <= 51.99 , i] <- 2
  counties_slf_risk_max[counties_slf_risk_max[,i] > 51.99, i] <- 3
}

counties_mean <- counties
counties_max <- counties
cols.num <- names(counties_slf_risk_mean)[8:38]
# counties_slf_risk_mean[cols.num] <- sapply(counties_slf_risk_mean[cols.num], as.numeric)
# counties_slf_risk_max[cols.num] <- sapply(counties_slf_risk_max[cols.num], as.numeric)

counties_mean[1:nrow(counties_mean),8:ncol(counties_slf_risk_mean)] <- counties_slf_risk_mean[,8:ncol(counties_slf_risk_mean)]
counties_max[1:nrow(counties_max),8:ncol(counties_slf_risk_max)] <- counties_slf_risk_max[,8:ncol(counties_slf_risk_max)]


plot(counties_mean, col=counties_mean$Y2050)
plot(counties_max, col=counties_max$Y2050)

#add crops
counties_crops <- data.frame(counties_crop@data)
counties_mean[1:nrow(counties_mean),39:49] <- counties_crops[1:nrow(counties_crops), 8:ncol(counties_crops)]
counties_max[1:nrow(counties_max),39:49] <- counties_crops[1:nrow(counties_crops), 8:ncol(counties_crops)]


counties_mean_prob <- counties
counties_max_prob <- counties

slf_mean <- data.frame(counties_slf_mean@data)
slf_max <- data.frame(counties_slf_max@data)

# slf_mean[cols.num] <- sapply(slf_mean[cols.num], as.numeric)
# slf_max[cols.num] <- sapply(slf_max[cols.num], as.numeric)

counties_mean_prob[1:nrow(counties_mean_prob),8:ncol(slf_mean)] <- slf_mean[,8:ncol(slf_mean)]
counties_max_prob[1:nrow(counties_max_prob),8:ncol(slf_max)] <- slf_max[,8:ncol(slf_max)]

counties_mean_prob[1:nrow(counties_mean_prob),39:49] <- counties_crops[1:nrow(counties_crops), 8:ncol(counties_crops)]
counties_max_prob[1:nrow(counties_max_prob),39:49] <- counties_crops[1:nrow(counties_crops), 8:ncol(counties_crops)]

# for (i in names(counties_mean)[8:38]) {
#   counties_mean$i <- as.numeric(counties_mean$i)
#   counties_max[1:nrow(counties_max),i] <- as.numeric(counties_max@data[1:nrow(counties_max),i])
#   counties_slf_mean[1:nrow(counties_slf_mean),i] <- as.numeric(counties_slf_mean@data[1:nrow(counties_slf_mean),i])
#   counties_slf_max[1:nrow(counties_slf_max),i] <- as.numeric(counties_slf_max@data[1:nrow(counties_slf_max),i])
# }

# 
# c_mean <- data.frame(counties_slf_risk_mean)
# sapply(c_mean, class)
# 
# c_mean[cols.num] <- sapply(c_mean[cols.num], as.numeric)
# counties_mean[, cols.num] <- c_mean[, cols.num]
# 
# counties_mean <- counties
# counties_mean[1:nrow(counties_mean), cols.num] <- c_mean[,cols.num]

writeOGR(obj=counties_mean, dsn=ffIn("counties_mean_risk"), layer ="counties_mean_risk", driver="GPKG", overwrite_layer=TRUE)
writeOGR(obj=counties_max, dsn=ffIn("counties_max_risk"), layer ="counties_max_risk", driver="GPKG", overwrite_layer=TRUE)
writeOGR(obj=counties_mean_prob, dsn=ffIn("counties_mean_probability"), layer ="counties_mean_probability", driver="GPKG", overwrite_layer=TRUE)
writeOGR(obj=counties_max_prob, dsn=ffIn("counties_max_probability"), layer ="counties_max_probability", driver="GPKG", overwrite_layer=TRUE)




## risk per category
load("C:/Users/Chris/Desktop/Projects/pops_casestudies/SLF_Example/data.RData")
counties_risky <- data.frame(counties_mean@data)
library(reshape2)
counties_crops1 <- counties_risky[,c(8,39:ncol(counties_risky))]
s <- aggregate(counties_crops1, by = list(probability = counties_risky[,8]), FUN = sum, na.rm = TRUE)
s[,2] <- as.numeric(strsplit(names(s)[2],"Y")[[1]][2])
names(s)[2] <- "Year"
crop_risk <- melt(data = s, id.vars = c("probability", "Year"))
for (j in 9:38) {
  counties_crops1 <- counties_risky[,c(j,39:ncol(counties_risky))]
  s <- aggregate(counties_crops1, by = list(probability = counties_risky[,j]), FUN = sum, na.rm = TRUE)
  s[,2] <- as.numeric(strsplit(names(s)[2],"Y")[[1]][2])
  names(s)[2] <- "Year"
  s2 <- melt(data = s, id.vars = c("probability", "Year"))
  crop_risk <- rbind(crop_risk, s2)
}


write.csv(crop_risk, ffIn("crop_risk2.csv"))
