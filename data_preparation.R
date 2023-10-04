#Data Preparation
#sentinel 1 data
#SM_S1_2022
#s1_monthly
#monthly_sm_i
#soil_texture, soil_type, height, inclination, exposition, landuse

#static_predictors1
library(TTR)
library(viridis)

ProbemetadataM1 <- as.data.frame(ProbeMetaDataMonthly)
ProbemetadataM1$end_date <- as.Date(ProbemetadataM1$end_date, format = "%d.%m.%Y")
ProbeM <- subset(ProbemetadataM1, end_date > "2022-01-01")
coordinates(ProbeM) = ~lon+lat

#weekly
ProbemetadataW1 <- as.data.frame(ProbeMetaDataWeekly)
ProbemetadataW1$end_date <- as.Date(ProbemetadataW1$end_date, format = "%d.%m.%Y")
ProbeW <- subset(ProbemetadataW1, end_date > "2022-01-01")
coordinates(ProbeW) = ~lon+lat

ProbeM.df <- as.data.frame(ProbeM)
#extracting value from raster
s1_month <- raster::extract(sentinel1, ProbeM)
##extracting value from raster(weekly)
s1_weekly <- raster::extract(sentinel1, ProbeW)

#storing df in a new data object
s1_month_smooth <- na.omit(s1_month)
#storing df in a new data object(weekly)
s1_weekly_smooth <- na.omit(s1_weekly)

#smoothening for the whole month
for(i in 1:ncol(s1_month_smooth)) {       # for-loop over columns
  s1_month_smooth[ , i] <- SMA(s1_month_smooth[ , i], n=5)
}

#smoothening for the whole month
for(i in 1:ncol(s1_weekly_smooth)) {       # for-loop over columns
  s1_weekly_smooth[ , i] <- SMA(s1_weekly_smooth[ , i], n=5)
}


combinePointValue=cbind(ProbeM,s1_month)
chumma <- as.data.frame(combinePointValue)
#weekly
combinePointValue_w=cbind(ProbeW,s1_weekly)
chumma_w <- as.data.frame(combinePointValue_w)

s2 <- select(chumma, c(-plot_id,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                       -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))

#omitting NA values
s4 <- na.omit(s2)
#weekly
s2_w <- select(chumma_w, c(-plot_id,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                           -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))

#omitting NA values
s4_w <- na.omit(s2_w)

#removing the non smoothened value from s4
s4_smooth <- select(s4, c(probe_id, landuse))
#combining the vlues
s4_s_combine <- cbind(s4_smooth, s1_month_smooth)
s4_smooth1 <- na.omit(s4_s_combine)
#weekly
#removing the non smoothened value from s4
s4_smooth_w <- select(s4_w, c(probe_id, landuse))
#combining the vlues
s4_s_combine_w <- cbind(s4_smooth_w, s1_weekly_smooth)
s4_smooth1_w <- na.omit(s4_s_combine_w)
#creating list for every raster layer(spatially)(smooth)
my_list_smooth <- list()
for (i in 1:59) {
  j = i+2
  s5 <- select(s4_smooth1, c(probe_id, landuse, names(s4_smooth1)[j]))
  my_list_smooth[[i]] <- s5
}


#creating list for every raster layer(spatially)
my_list <- list()
for (i in 1:59) {
  j = i+2
  s3 <- select(s4, c(probe_id, landuse, names(s4)[j]))
  my_list[[i]] <- s3
}
#weekly
#creating list for every raster layer(spatially)(smooth)
my_list_smooth_w <- list()
for (i in 1:59) {
  j = i+2
  s5 <- select(s4_smooth1_w, c(probe_id, landuse, names(s4_smooth1_w)[j]))
  my_list_smooth_w[[i]] <- s5
}


#creating list for every raster layer(spatially)
my_list_w <- list()
for (i in 1:59) {
  j = i+2
  s3 <- select(s4_w, c(probe_id, landuse, names(s4_w)[j]))
  my_list_w[[i]] <- s3
}

#include date to the table(smooth)
for (i in 1:length(my_list_smooth)) {
  my_list_smooth[[i]]['date'] = date_s1[i]
  colnames(my_list_smooth[[i]]) <- c("probe_id", "landuse", "backscatter_smooth", "date")
}

#include date to the table
for (i in 1:length(my_list)) {
  my_list[[i]]['date'] = date_s1[i]
  colnames(my_list[[i]]) <- c("probe_id", "landuse", "backscatter", "date")
}
#weekly
#include date to the table(smooth)
for (i in 1:length(my_list_smooth_w)) {
  my_list_smooth_w[[i]]['date'] = date_s1[i]
  colnames(my_list_smooth_w[[i]]) <- c("probe_id", "landuse", "backscatter_smooth", "date")
}

#include date to the table
for (i in 1:length(my_list_w)) {
  my_list_w[[i]]['date'] = date_s1[i]
  colnames(my_list_w[[i]]) <- c("probe_id", "landuse", "backscatter", "date")
}
#combining all the dfs
s1_monthly <- bind_rows(my_list)
#weekly
s1_weekly1 <- bind_rows(my_list_w)

#combining all the dfs(smooth)
s1_monthly_smooth <- bind_rows(my_list_smooth)
#weekly
s1_weekly1_smooth <- bind_rows(my_list_smooth_w)


#insitu-data preparation

#loop through the csv of month list to create a new column with probe_id
for (i in 1:length(csv_list_monthly_probe_data)) {
  y <- substr(names(csv_list_monthly_probe_data)[i], 1, 7)
  #print(y)
  csv_list_monthly_probe_data[[names(csv_list_monthly_probe_data)[i]]]['probe_id'] = y
  #print(csv_list_monthly_probe_data)
} 

#same for weekly
for (i in 1:length(csv_list_weekly_probe_data)) {
  y <- substr(names(csv_list_weekly_probe_data)[i], 1, 7)
  #print(y)
  csv_list_weekly_probe_data[[names(csv_list_weekly_probe_data)[i]]]['probe_id'] = y
  #print(csv_list_monthly_probe_data)
} 

#merging the dfs together
monthly_SM <- bind_rows(csv_list_monthly_probe_data)
#merging the dfs together(weekly)
weekly_SM <- bind_rows(csv_list_weekly_probe_data)

#suset only 2022
monthly_sm22 <- subset(monthly_SM, datetime > "2022-01-01 00:00:00")
#suset only 2022(weekly)
weekly_sm22 <- subset(weekly_SM, datetime > "2022-01-01 00:00:00")

#only M_05, probe id, datetime
monthly_sm20 <- select(monthly_sm22, c(datetime, probe_id, M_05))
#only M_05, probe id, datetime(weekly)
weekly_sm20 <- select(weekly_sm22, c(datetime, probe_id, M_05))

#changing to 1 hr resolution
monthly_sm2022 <- monthly_sm20 %>%
  mutate(Date = ymd_hms(datetime), dt = as_date(datetime), hr = hour(datetime)) %>%
  distinct(dt, hr, probe_id,.keep_all = T)
#changing to 1 hr resolution(weekly)
weekly_sm2022 <- weekly_sm20 %>%
  mutate(Date = ymd_hms(datetime), dt = as_date(datetime), hr = hour(datetime)) %>%
  distinct(dt, hr, probe_id,.keep_all = T)

#mean for each date and probe
monthly_sm_i <- monthly_sm2022 %>%
  group_by(probe_id, dt) %>%
  summarise(SM=(mean(M_05)))
colnames(monthly_sm_i) <- c("probe_id", "date", "SM")
#mean for each date and probe(weekly)
weekly_sm_i <- weekly_sm2022 %>%
  group_by(probe_id, dt) %>%
  summarise(SM=(mean(M_05)))
colnames(weekly_sm_i) <- c("probe_id", "date", "SM")

#combining both the table based on probe_id and date
SM_S1 <- full_join( monthly_sm_i,s1_monthly, by=c("probe_id", "date"))
#remove NA's
SM_S1_2022 <- na.omit(SM_S1)

#combining both the table based on probe_id and date(weekly)
SM_S1_w <- full_join( weekly_sm_i,s1_weekly1, by=c("probe_id", "date"))
#remove NA's
SM_S1_2022_w <- na.omit(SM_S1_w)


#join the smooth backscatter with the table
SM_S1_smooth_2022 <- full_join(SM_S1_2022, s1_monthly_smooth, by=c("probe_id", "date") )
SM_S1_smooth_2022_1 <- na.omit(SM_S1_smooth_2022)
#weekly
SM_S1_smooth_2022_w <- full_join(SM_S1_2022_w, s1_weekly1_smooth, by=c("probe_id", "date") )
SM_S1_smooth_2022_w1 <- na.omit(SM_S1_smooth_2022_w)


#join ndwi and ndvi
colnames(SM_S1_ndvi_ndwi_2022) <- c("probe_id", "date", "landuse", "SM", "Backscatter", "NDVI", "NDWI")
SM_S1_smooth_vi_wi_2022 <- full_join(SM_S1_smooth_2022, SM_S1_ndvi_ndwi_2022, by=c("probe_id", "date"))

#extracting ndvi
ndvi_month <- raster::extract(ndvi, ProbeM)
combineNdviValue <- cbind(ProbeM,ndvi_month)
ndvi_df <- as.data.frame(combineNdviValue)
ndvi1 <- select(ndvi_df, c(-plot_id,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                           -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))
#weekly
ndvi_weekly <- raster::extract(ndvi, ProbeW)
combineNdviValue_w <- cbind(ProbeW,ndvi_weekly)
ndvi_df_w <- as.data.frame(combineNdviValue_w)
ndvi1_w <- select(ndvi_df_w, c(-plot_id,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                               -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))

#creating list for every raster layer(spatially)
my_list_ndvi <- list()
for (i in 1:101) {
  j = i+2
  s3 <- select(ndvi1, c(probe_id, landuse, names(ndvi1)[j]))
  my_list_ndvi[[i]] <- s3
}
#weekly
my_list_ndvi_w <- list()
for (i in 1:101) {
  j = i+2
  s3 <- select(ndvi1_w, c(probe_id, landuse, names(ndvi1_w)[j]))
  my_list_ndvi_w[[i]] <- s3
}
#include date to the table
for (i in 1:length(my_list_ndvi)) {
  my_list_ndvi[[i]]['date'] = date_u1[i]
  colnames(my_list_ndvi[[i]]) <- c("probe_id", "landuse", "NDVI", "date")
}
#combining all the dfs
ndvi_monthly <- bind_rows(my_list_ndvi)
ndvi1_monthly <- na.omit(ndvi_monthly)
#weekly
#include date to the table
for (i in 1:length(my_list_ndvi_w)) {
  my_list_ndvi_w[[i]]['date'] = date_u1[i]
  colnames(my_list_ndvi_w[[i]]) <- c("probe_id", "landuse", "NDVI", "date")
}
#combining all the dfs
ndvi_weekly1 <- bind_rows(my_list_ndvi_w)
ndvi1_weekly <- na.omit(ndvi_weekly1)

library(data.table)
# coerce to data.table and append join columns to preserve the original columns 
setDT(ndvi1_monthly)[, join_date := date]
setDT(SM_S1_2022)[, join_date := date]
# rolling join
join <- ndvi1_monthly[SM_S1_2022, on = .(probe_id, join_date), roll = "nearest"]
join_1 = join[!duplicated(join$backscatter),]
#weekly
library(data.table)
# coerce to data.table and append join columns to preserve the original columns 
setDT(ndvi1_weekly)[, join_date := date]
setDT(SM_S1_2022_w)[, join_date := date]
# rolling join
join_w <- ndvi1_weekly[SM_S1_2022_w, on = .(probe_id, join_date), roll = "nearest"]
join_1_w = join_w[!duplicated(join_w$backscatter),]


#extracting ndwi
ndwi_month <- raster::extract(ndwi, ProbeM)
combineNdwiValue <- cbind(ProbeM,ndwi_month)
ndwi_df <- as.data.frame(combineNdwiValue)
ndwi1 <- select(ndwi_df, c(-plot_id,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                           -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))

#creating list for every raster layer(spatially)
my_list_ndwi <- list()
for (i in 1:101) {
  j = i+2
  s3 <- select(ndwi1, c(probe_id, landuse, names(ndwi1)[j]))
  my_list_ndwi[[i]] <- s3
}

#include date to the table
for (i in 1:length(my_list_ndwi)) {
  my_list_ndwi[[i]]['date'] = date_u1[i]
  colnames(my_list_ndwi[[i]]) <- c("probe_id", "landuse", "NDWI", "date")
}
#combining all the dfs
ndwi_monthly <- bind_rows(my_list_ndwi)
ndwi1_monthly <- na.omit(ndwi_monthly)
colnames(ndwi1_monthly) <- c("probe_id", "landuse", "NDWI", "date")

# coerce to data.table and append join columns to preserve the original columns 
setDT(ndwi1_monthly)[, join_date := date]
setDT(SM_S1_2022)[, join_date := date]
# rolling join
join_ndwi <- ndwi1_monthly[SM_S1_2022, on = .(probe_id, join_date), roll = "nearest"]
join_1_ndwi = join_ndwi[!duplicated(join_ndwi$backscatter),]



#weekly
#extracting ndwi
ndwi_weekly <- raster::extract(ndwi, ProbeW)
combineNdwiValue_w <- cbind(ProbeW,ndwi_weekly)
ndwi_df_w <- as.data.frame(combineNdwiValue_w)
ndwi1_w <- select(ndwi_df_w, c(-plot_id,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                               -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))

#creating list for every raster layer(spatially)
my_list_ndwi_w <- list()
for (i in 1:101) {
  j = i+2
  s3 <- select(ndwi1_w, c(probe_id, landuse, names(ndwi1_w)[j]))
  my_list_ndwi_w[[i]] <- s3
}

#include date to the table
for (i in 1:length(my_list_ndwi_w)) {
  my_list_ndwi_w[[i]]['date'] = date_u1[i]
  colnames(my_list_ndwi_w[[i]]) <- c("probe_id", "landuse", "NDWI", "date")
}
#combining all the dfs
ndwi_weekly1 <- bind_rows(my_list_ndwi_w)
ndwi1_weekly <- na.omit(ndwi_weekly1)
colnames(ndwi1_weekly) <- c("probe_id", "landuse", "NDWI", "date")

# coerce to data.table and append join columns to preserve the original columns 
setDT(ndwi1_weekly)[, join_date := date]
setDT(SM_S1_2022_w)[, join_date := date]
# rolling join
join_ndwi_w <- ndwi1_weekly[SM_S1_2022_w, on = .(probe_id, join_date), roll = "nearest"]
join_1_ndwi_w = join_ndwi_w[!duplicated(join_ndwi_w$backscatter),]

#combining tables with ndvi, ndwi, backscatter
SM_S1_ndvi_ndwi <- full_join( join_1, join_1_ndwi, by=c("probe_id", "join_date"))
#weekly
SM_S1_ndvi_ndwi_w <- full_join( join_1_w, join_1_ndwi_w, by=c("probe_id", "join_date"))
#selecting only the correct columns
SM_S1_ndvi_ndwi_2022 <- select(SM_S1_ndvi_ndwi, c("probe_id","join_date","landuse.x","SM.x","backscatter.x","NDVI","NDWI"))
#weekly
SM_S1_ndvi_ndwi_2022_w <- select(SM_S1_ndvi_ndwi_w, c("probe_id","join_date","landuse.x","SM.x","backscatter.x","NDVI","NDWI"))


#join ndwi and ndvi
colnames(SM_S1_ndvi_ndwi_2022) <- c("probe_id", "date", "landuse", "SM", "Backscatter", "NDVI", "NDWI")
SM_S1_smooth_vi_wi_2022 <- full_join(SM_S1_smooth_2022, SM_S1_ndvi_ndwi_2022, by=c("probe_id", "date"))
#weekly
#join ndwi and ndvi
colnames(SM_S1_ndvi_ndwi_2022_w) <- c("probe_id", "date", "landuse", "SM", "Backscatter", "NDVI", "NDWI")
SM_S1_smooth_vi_wi_2022_w <- full_join(SM_S1_smooth_2022_w, SM_S1_ndvi_ndwi_2022_w, by=c("probe_id", "date"))
#saving the table as csv
write.csv(SM_S1_ndvi_ndwi_2022, "E:/Thesis Research/data.csv", row.names=FALSE)
final_data <- select(SM_S1_smooth_vi_wi_2022, c('probe_id','SM.x','backscatter','backscatter_smooth','NDWI','NDVI'))
colnames(final_data) <- c('probe_id','SM','backscatter','backscatter_smooth','NDWI','NDVI')

#weekly
final_data_w <- select(SM_S1_smooth_vi_wi_2022_w, c('probe_id','SM.x','backscatter','backscatter_smooth','NDWI','NDVI'))
colnames(final_data_w) <- c('probe_id','SM','backscatter','backscatter_smooth','NDWI','NDVI')

#monthly and weekly
total <- rbind(final_data, final_data_w)
total1 <- na.omit(total)


#creating a function for extracting from all the 5 static predictors

crs(ProbeM)<- '+proj=longlat +datum=WGS84 +no_defs'
latlon <- spTransform(ProbeM, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs "))

static_predictors <- function(r){
  s_extract <- raster::extract(r, latlon)
  combinespValue <- cbind(ProbeM,s_extract)
  sp_df <- as.data.frame(combinespValue)
  sp1 <- select(sp_df, c(-plot_id,-landuse,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                         -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))
  return(sp1)
}



soiltype <- static_predictors(soil_type1)
colnames(soiltype) <- c("probe_id","soil_type")
soiltype_s = soiltype %>%
  mutate(organic = case_when(soil_type==1~"1",
                             soil_type!=1~"0"
  ))
soiltype_s = soiltype_s %>%
  mutate(semi_terrestial = case_when(soil_type==2~"1",
                                     soil_type!=2~"0"
  ))
soiltype_s = soiltype_s %>%
  mutate(terrestial = case_when(soil_type==3~"1",
                                soil_type!=3~"0"
  ))
soiltype_s = soiltype_s %>%
  mutate(stagnic = case_when(soil_type==4~"1",
                             soil_type!=4~"0"
  ))
soiltype_s = soiltype_s %>%
  mutate(water = case_when(soil_type==5~"1",
                           soil_type!=5~"0"
  ))
soiltype_s$soil_type[soiltype_s$soil_type == 1] <- "organic"
soiltype_s$soil_type[soiltype_s$soil_type == 2] <- "semi_terrestial"
soiltype_s$soil_type[soiltype_s$soil_type == 3] <- "terrestial"
soiltype_s$soil_type[soiltype_s$soil_type == 4] <- "stagnic"
soiltype_s$soil_type[soiltype_s$soil_type == 5] <- "water"

soiltexture <- static_predictors(soil_texture)
colnames(soiltexture) <- c("probe_id","soil_texture")
soiltexture_s <- soiltexture %>%
  mutate(clay = case_when(soil_texture==10~"1",
                          soil_texture!=10~"0"
  ))
soiltexture_s <- soiltexture_s %>%
  mutate(debris = case_when(soil_texture==20~"1",
                            soil_texture!=20~"0"
  ))
soiltexture_s <- soiltexture_s %>%
  mutate(loam = case_when(soil_texture==30~"1",
                          soil_texture!=30~"0"
  ))
soiltexture_s <- soiltexture_s %>%
  mutate(organic_st = case_when(soil_texture==40~"1",
                                soil_texture!=40~"0"
  ))
soiltexture_s <- soiltexture_s %>%
  mutate(sand = case_when(soil_texture==50~"1",
                          soil_texture!=50~"0"
  ))
soiltexture_s <- soiltexture_s %>%
  mutate(silt = case_when(soil_texture==60~"1",
                          soil_texture!=60~"0"
  ))
soiltexture_s <- soiltexture_s %>%
  mutate(water_st = case_when(soil_texture==70~"1",
                              soil_texture!=70~"0"
  ))
soiltexture_s$soil_texture[soiltexture_s$soil_texture == 10] <- "clay"
soiltexture_s$soil_texture[soiltexture_s$soil_texture == 20] <- "debris"
soiltexture_s$soil_texture[soiltexture_s$soil_texture == 30] <- "loam"
soiltexture_s$soil_texture[soiltexture_s$soil_texture == 40] <- "organic"
soiltexture_s$soil_texture[soiltexture_s$soil_texture == 50] <- "sand"
soiltexture_s$soil_texture[soiltexture_s$soil_texture == 60] <- "silt"
soiltexture_s$soil_texture[soiltexture_s$soil_texture == 70] <- "water"
height1 <- static_predictors(height)
colnames(height1) <- c("probe_id","height")

inclination1 <- static_predictors(inclination)
colnames(inclination1) <- c("probe_id","inclination")

exposition1 <- static_predictors(exposition)
colnames(exposition1) <- c("probe_id","exposition")

landuse1 <- static_predictors(landuse)
colnames(landuse1) <- c("probe_id","landuse1")
landuse_s = landuse1 %>%
  mutate(forest = case_when(landuse1==1000~"1",
                            landuse1!=1000~"0"
  ))
landuse_s<-landuse_s %>%
  mutate(arable_land = case_when(landuse1==1500~"1",
                                 landuse1!=1500~"0"
  ))
landuse_s<-landuse_s %>%
  mutate(grassland = case_when(landuse1==1600~"1",
                               landuse1!=1600~"0"
  ))
landuse_s$landuse1[landuse_s$landuse1 == 1000] <- "forest"
landuse_s$landuse1[landuse_s$landuse1 == 1500] <- "arable_land"
landuse_s$landuse1[landuse_s$landuse1 == 1600] <- "grassland"
topo <- static_predictors(topo_wetness)
colnames(topo) <-  c("probe_id","topographic_wetness")

#combining all the dfs together
type_texture <- merge(soiltype_s, soiltexture_s,
                      by=c("probe_id"))
tt_land <- merge(type_texture, landuse_s, by=c("probe_id"))
ttl_i <- merge(tt_land, inclination1,by=c("probe_id"))
ttli_h <- merge(ttl_i, height1, by=c("probe_id"))
static_predictors1 <- merge(ttli_h, exposition1, by=c("probe_id"))
static_predictors2 <- merge(static_predictors1, topo,by=c("probe_id"))
colnames(static_predictors2) <- c('probe_id', 'soil_type','organic','semi_terrestial','terrestial',
                                  'stagnic', 'water','soil_texture', 'clay','debris','loam','organic_st',
                                  'sand','silt','water_st','landuse1','forest','arable_land','grassland',
                                  'inclination','height','exposition','topographic_wetness')

#weekly
#creating a function for extracting from all the 5 static predictors

crs(ProbeW)<- '+proj=longlat +datum=WGS84 +no_defs'
latlon_w <- spTransform(ProbeW, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs "))

static_predictors_w <- function(r){
  s_extract <- raster::extract(r, latlon_w)
  combinespValue <- cbind(ProbeW,s_extract)
  sp_df <- as.data.frame(combinespValue)
  sp1 <- select(sp_df, c(-plot_id,-landuse,-start_date,-start_time,-end_date,-end_time,-location,-height,-org_layer,
                         -excess_length,-probe_length,-time_res,-available,-Hoehe_DGM,-Landn,-Expo,-Inkl,-BArt,-BTyp,-ESC,-geometry,-lon,-lat))
  return(sp1)
}



soiltype_w <- static_predictors_w(soil_type1)
colnames(soiltype_w) <- c("probe_id","soil_type")
soiltype_sw = soiltype_w %>%
  mutate(organic = case_when(soil_type==1~"1",
                             soil_type!=1~"0"
  ))
soiltype_sw = soiltype_sw %>%
  mutate(semi_terrestial = case_when(soil_type==2~"1",
                                     soil_type!=2~"0"
  ))
soiltype_sw = soiltype_sw %>%
  mutate(terrestial = case_when(soil_type==3~"1",
                                soil_type!=3~"0"
  ))
soiltype_sw = soiltype_sw %>%
  mutate(stagnic = case_when(soil_type==4~"1",
                             soil_type!=4~"0"
  ))
soiltype_sw = soiltype_sw %>%
  mutate(water = case_when(soil_type==5~"1",
                           soil_type!=5~"0"
  ))
soiltype_sw$soil_type[soiltype_sw$soil_type == 1] <- "organic"
soiltype_sw$soil_type[soiltype_sw$soil_type == 2] <- "semi_terrestial"
soiltype_sw$soil_type[soiltype_sw$soil_type == 3] <- "terrestial"
soiltype_sw$soil_type[soiltype_sw$soil_type == 4] <- "stagnic"
soiltype_sw$soil_type[soiltype_sw$soil_type == 5] <- "water"

soiltexture_w <- static_predictors_w(soil_texture)
colnames(soiltexture_w) <- c("probe_id","soil_texture")
soiltexture_sw <- soiltexture_w%>%
  mutate(clay = case_when(soil_texture==10~"1",
                          soil_texture!=10~"0"
  ))
soiltexture_sw <- soiltexture_sw %>%
  mutate(debris = case_when(soil_texture==20~"1",
                            soil_texture!=20~"0"
  ))
soiltexture_sw <- soiltexture_sw %>%
  mutate(loam = case_when(soil_texture==30~"1",
                          soil_texture!=30~"0"
  ))
soiltexture_sw <- soiltexture_sw %>%
  mutate(organic_st = case_when(soil_texture==40~"1",
                                soil_texture!=40~"0"
  ))
soiltexture_sw <- soiltexture_sw %>%
  mutate(sand = case_when(soil_texture==50~"1",
                          soil_texture!=50~"0"
  ))
soiltexture_sw <- soiltexture_sw %>%
  mutate(silt = case_when(soil_texture==60~"1",
                          soil_texture!=60~"0"
  ))
soiltexture_sw <- soiltexture_sw %>%
  mutate(water_st = case_when(soil_texture==70~"1",
                              soil_texture!=70~"0"
  ))
soiltexture_sw$soil_texture[soiltexture_sw$soil_texture == 10] <- "clay"
soiltexture_sw$soil_texture[soiltexture_sw$soil_texture == 20] <- "debris"
soiltexture_sw$soil_texture[soiltexture_sw$soil_texture == 30] <- "loam"
soiltexture_sw$soil_texture[soiltexture_sw$soil_texture == 40] <- "organic"
soiltexture_sw$soil_texture[soiltexture_sw$soil_texture == 50] <- "sand"
soiltexture_sw$soil_texture[soiltexture_sw$soil_texture == 60] <- "silt"
soiltexture_sw$soil_texture[soiltexture_sw$soil_texture == 70] <- "water"
height1_w <- static_predictors_w(height)
colnames(height1_w) <- c("probe_id","height")

inclination1_w <- static_predictors_w(inclination)
colnames(inclination1_w) <- c("probe_id","inclination")

exposition1_w <- static_predictors_w(exposition)
colnames(exposition1_w) <- c("probe_id","exposition")

landuse1_w <- static_predictors_w(landuse)
colnames(landuse1_w) <- c("probe_id","landuse1")
landuse_sw = landuse1_w %>%
  mutate(forest = case_when(landuse1==1000~"1",
                            landuse1!=1000~"0"
  ))
landuse_sw <- landuse_sw %>%
  mutate(arable_land = case_when(landuse1==1500~"1",
                                 landuse1!=1500~"0"
  ))
landuse_sw <- landuse_sw %>%
  mutate(grassland = case_when(landuse1==1600~"1",
                               landuse1!=1600~"0"
  ))
landuse_sw$landuse1[landuse_sw$landuse1 == 1000] <- "forest"
landuse_sw$landuse1[landuse_sw$landuse1 == 1500] <- "arable_land"
landuse_sw$landuse1[landuse_sw$landuse1 == 1600] <- "grassland"

topo_w <- static_predictors_w(topo_wetness)
colnames(topo_w) <-  c("probe_id","topographic_wetness")

#combining all the dfs together
type_texture_w <- merge(soiltype_sw, soiltexture_sw,
                        by=c("probe_id"))
tt_land_w <- merge(type_texture_w, landuse_sw, by=c("probe_id"))
ttl_i_w <- merge(tt_land_w, inclination1_w,by=c("probe_id"))
ttli_h_w <- merge(ttl_i_w, height1_w, by=c("probe_id"))
static_predictors1_w <- merge(ttli_h_w, exposition1_w, by=c("probe_id"))
static_predictors2_w <- merge(static_predictors1_w, topo_w,by=c("probe_id"))

#merge monthly and weekly static predictors value
sp_final <- rbind(static_predictors2, static_predictors2_w)
#combining static data to the final_data table
final_data_sp <- merge(total1, sp_final, by=c("probe_id"))
final_data_sp1 <- na.omit(final_data_sp)
#final_data_sp1_me <- final_data_sp1[final_data_sp1$landuse1 == 1600,]
#for running RF, selecting only the needed
ready_data_BS <- select(final_data_sp1, c('SM','backscatter','NDWI','NDVI','landuse1','soil_type',
                                          'soil_texture','inclination','height','exposition','topographic_wetness'))
ready_data_BS_S <-select(final_data_sp1, c('SM','backscatter_smooth','NDWI','NDVI','landuse1','soil_type',
                                           'soil_texture','inclination','height','exposition','topographic_wetness'))
numerical_data<- final_data_sp1
numerical_data_char <- numerical_data
numerical_data_char$landuse1 <- as.factor(numerical_data_char$landuse1)
#combining the predictors

e <- extent( 694605, 714585, 5548485, 5568485 )
extent_crop <- function(x){
  c <- crop(x, e)
  return(c)
}
h_c <- extent_crop(height)
l_c <- extent_crop(landuse)
sty_c <- extent_crop(soil_type1)
stex_c <- extent_crop(soil_texture)
i_c <- extent_crop(inclination)
e_c <- extent_crop(exposition)
t_w <- extent_crop(topo_wetness)

l_c_1000 <- l_c
l_c_1000[l_c_1000 > 1600] <- NA
l_c_1000[l_c_1000 > 1000] <- 1000

l_c_1500 <- l_c
l_c_1500[l_c_1500 > 1600] <- NA
l_c_1500[l_c_1500 < 1500 | l_c_1500 > 1500] <- 1500

l_c_1600 <- l_c
l_c_1600[l_c_1600 > 1600] <- NA
l_c_1600[l_c_1600 < 1600] <- 1600

sp_combine <- stack(h_c, l_c, sty_c, stex_c)
sp_combine_ie <- stack(i_c, e_c)
resam <- resample(sp_combine, t_w, method ="ngb")
static_predictors_all <- stack(t_w,resam)#all the static predictors are together combined in this
names(static_predictors_all) <- c("topographic_wetness","Height","Landuse1","Soil_type","Soil_texture")
sp_c.df <- as.data.frame(merge_sp_s1_s2)

#march
#combining sentinel1 data(1st) to the static predictors
sentinel1_march <- sentinel1$S1A_IW_GRDH_1SDV_20220305T170801_20220305T170826_042189_050719_3E34_VV
crs(sentinel1_march) = "+init=epsg:4326" # fix broken CRS



#changing crs of s1 to s2's
c <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs "
sentinel1_march1 <- projectRaster(sentinel1_march, crs = c)

#s1_rps1 <- projectRaster(sentinel1_march, crs = crs(landuse))
resams1 <- resample(sentinel1_march1, static_predictors_all, method ="ngb")
merge_sp_s1 <- stack(static_predictors_all, resams1)
#join ndwi
ndwi_march <- ndwi_i$Mar.2022
ndwi_rpndwi <- projectRaster(ndwi_march, crs = crs(static_predictors_all))
resamndwi <- resample(ndwi_rpndwi, static_predictors_all, method ="ngb")
merge_sp_s1_s2 <- stack(merge_sp_s1, resamndwi)

merge_sp_s1_s2[merge_sp_s1_s2$Landuse1 > 1600  ] <- NA
names(merge_sp_s1_s2) <- c("topographic_wetness","height","landuse1","soil_type","soil_texture","backscatter","NDWI")

merge_sp_s1_s2_1000 <-merge_sp_s1_s2

merge_sp_s1_s2_1000[merge_sp_s1_s2_1000$landuse1 > 1000  ] <- 1000
merge_sp_s1_s2_ara <- merge_sp_s1_s2
merge_sp_s1_s2_ara[merge_sp_s1_s2_ara$landuse1 > 1000  | merge_sp_s1_s2_ara$landuse1 < 1600   ] <- 1500
merge_sp_s1_s2_gra <- merge_sp_s1_s2
merge_sp_s1_s2_gra[merge_sp_s1_s2_gra$landuse1 < 1600  ] <- 1600
#merge_sp_s1_s2[ merge_sp_s1_s2$landuse>1000 &  merge_sp_s1_s2$landuse< 1500  ] <- NA
#merge_sp_s1_s2[merge_sp_s1_s2$landuse>1500 & merge_sp_s1_s2$landuse < 1600  ] <- NA
merge_sp_s1_s2[merge_sp_s1_s2$soil_texture == 20] <- NA
merge_sp_s1_s2[merge_sp_s1_s2$soil_texture == 70] <- NA
merge_sp_s1_s2[merge_sp_s1_s2$soil_type == 5] <- NA


#june
#sentinel1 june
sentinel1_june <- sentinel1$S1A_IW_GRDH_1SDV_20220604T170013_20220604T170038_043516_05321B_0D4D_VV
crs(sentinel1_june) = "+init=epsg:4326"

#changing crs of s1 to s2's
c <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs "
sentinel1_june1 <- projectRaster(sentinel1_june, crs = c)

#s1_rps1 <- projectRaster(sentinel1_march, crs = crs(landuse))
resams1_j <- resample(sentinel1_june1, static_predictors_all, method ="ngb")
merge_sp_s1_j <- stack(static_predictors_all, resams1_j)
#join ndwi
ndwi_june <- ndwi_i$Jun.2022
ndwi_rpndwi_j <- projectRaster(ndwi_june, crs = crs(static_predictors_all))
resamndwi_j <- resample(ndwi_rpndwi_j, static_predictors_all, method ="ngb")
merge_sp_s1_s2_j <- stack(merge_sp_s1_j, resamndwi_j)

#removing landuse that or not there
merge_sp_s1_s2_j[merge_sp_s1_s2_j$Landuse1 > 1600  ] <- NA

names(merge_sp_s1_s2_j) <- c("topographic_wetness","height","landuse1","soil_type","soil_texture","backscatter","NDWI")


merge_sp_s1_s2_j[merge_sp_s1_s2_j$soil_texture == 20] <- NA
merge_sp_s1_s2_j[merge_sp_s1_s2_j$soil_texture == 70] <- NA
merge_sp_s1_s2_j[merge_sp_s1_s2_j$soil_type == 5] <- NA





#september
#sentinel1 sept
sentinel1_sept <- sentinel1$S1A_IW_GRDH_1SDV_20220908T170019_20220908T170044_044916_055D72_537E_VV
crs(sentinel1_sept) = "+init=epsg:4326"

c <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs "
sentinel1_sept1 <- projectRaster(sentinel1_sept, crs = c)

#s1_rps1 <- projectRaster(sentinel1_march, crs = crs(landuse))
resams1_s <- resample(sentinel1_sept1, static_predictors_all, method ="ngb")
merge_sp_s1_s <- stack(static_predictors_all, resams1_s)
#join ndwi
ndwi_sept <- ndwi_i$Sep.2022
ndwi_rpndwi_s <- projectRaster(ndwi_sept, crs = crs(static_predictors_all))
resamndwi_s <- resample(ndwi_rpndwi_s, static_predictors_all, method ="ngb")
merge_sp_s1_s2_s <- stack(merge_sp_s1_s, resamndwi_s)

#removing landuse that or not there
merge_sp_s1_s2_s[merge_sp_s1_s2_s$Landuse1 > 1600  ] <- NA

names(merge_sp_s1_s2_s) <- c("topographic_wetness","height","landuse1","soil_type","soil_texture","backscatter","NDWI")


merge_sp_s1_s2_s[merge_sp_s1_s2_s$soil_texture == 20] <- NA
merge_sp_s1_s2_s[merge_sp_s1_s2_s$soil_texture == 70] <- NA
merge_sp_s1_s2_s[merge_sp_s1_s2_s$soil_type == 5] <- NA



#sentinel1 dec
sentinel1_dec <- sentinel1$S1A_IW_GRDH_1SDV_20221206T170810_20221206T170835_046214_05889F_FF86_VV
crs(sentinel1_dec) = "+init=epsg:4326"

c <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs "
sentinel1_dec1 <- projectRaster(sentinel1_dec, crs = c)


resams1_d <- resample(sentinel1_dec1, static_predictors_all, method ="ngb")
merge_sp_s1_d <- stack(static_predictors_all, resams1_d)
#join ndwi
ndwi_dec <- ndwi_i$Dec.2022
ndwi_rpndwi_d <- projectRaster(ndwi_dec, crs = crs(static_predictors_all))
resamndwi_d <- resample(ndwi_rpndwi_d, static_predictors_all, method ="ngb")
merge_sp_s1_s2_d <- stack(merge_sp_s1_d, resamndwi_d)

#removing landuse that or not there
merge_sp_s1_s2_d[merge_sp_s1_s2_d$Landuse1 > 1600  ] <- NA

names(merge_sp_s1_s2_d) <- c("topographic_wetness","height","landuse1","soil_type","soil_texture","backscatter","NDWI")


merge_sp_s1_s2_d[merge_sp_s1_s2_d$soil_texture == 20] <- NA
merge_sp_s1_s2_d[merge_sp_s1_s2_d$soil_texture == 70] <- NA
merge_sp_s1_s2_d[merge_sp_s1_s2_d$soil_type == 5] <- NA
