devtools::install_github("LOEK-RS/Carbon4D")
git2r::clone("https://github.com/MaikenBaumberger/Carbon4dData.git","E:/Thesis Research/data final2")


#Tables with measurement values (at the moment you only need the first one for the longterm data):
Carbon4D::load_longterm_probe_data("E:/Thesis Research/data final2")
Carbon4D::load_monthly_probe_data("E:/Thesis Research/data final2")
Carbon4D::load_weekly_probe_data("E:/Thesis Research/data final2")
#Meta data table (at the moment you only need the first one for the longterm data):
Carbon4D::load_probe_meta_data_longterm("E:/Thesis Research/data final2")
Carbon4D::load_probe_meta_data_monthly("E:/Thesis Research/data final2")
Carbon4D::load_probe_meta_data_weekly("E:/Thesis Research/data final2")

#Visualisation of the locations of the plots:
Carbon4D::plot_longterm_porbe_plots("E:/Thesis Research/data final2")
Carbon4D::plot_monthly_porbe_plots("E:/Thesis Research/data final2")
Carbon4D::plot_weekly_porbe_plots("E:/Thesis Research/data final2")
Carbon4D::plot_all_porbe_plots("E:/Thesis Research/data final2")
library(sf)
library(leaflet)
data <- st_read("E:/Thesis Research/data/SpatialBaseData/StudyArea.gpkg")
data <- st_transform(data, "+proj=longlat +datum=WGS84", "+init=epsg:4326")
leaflet() %>%  addTiles() %>% addPolygons(data = data)
summary(data)
data