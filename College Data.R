##############################
#
# College Campus Data
# 10/1/2024
# Ian Becker
#
##############################

# Loading libraries 

library(educationdata)
library(dplyr)
library(tidygeocoder)
library(tigris)
library(ggplot2)
library(auk)
library(terra)
library(sf)
library(maps)
library(geosphere)
library(elevatr)
library(sp)
library(tidycensus)
library(rstanarm)

##############################
#
# College data manipulation
#
##############################

## Loading in data using multiple methods for practice

data1 <- get_education_data(level = 'college-university',
                            source = 'ipeds',
                            topic = 'directory',
                            filters = list(year = 2020),
                            add_labels = TRUE)

## Initial cleaning of data

select_data <- data1 %>%
  filter(state_abbr %in% c("TX", "NM", "CO", "OK", "KS", "NE", "SD", "ND", "MT", "WY"))

sample_data <- select_data %>% filter(!(offering_highest_degree %in% c("Associate's degree", "Non&ndash,degree granting")))

sample_data <- read.csv("colleges_cflyway.csv")

sample_data <- sample_data %>%
  select(inst_name, state_abbr, city, county_name, sector, urban_centric_locale, offering_highest_degree, inst_category, land_grant, longitude, latitude, inst_size)



##############################
#
#   eBird data filtering
#
##############################

## Checklist filtering

# Filtering whole dataset for central flyway

sampling_file <- "C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/ebd_US_smp_relAug-2024/ebd_US_relAug-2024_sampling.txt/ebd_US_relAug-2024_sampling.txt"
filtered_file <- "C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/filtered_ebd_v3.txt"
  
ebd <- auk_ebd(ebd_file) %>%
  auk_bbox(bbox = c(-116.178058, 25.882659, -93.765949, 49.041888)) %>%  
  auk_complete()

filtered_ebd <- auk_filter(ebd, file = "filtered_ebd_v3.txt", sampling = sampling_file)


# Filtering for one state to troubleshoot

ebd_montana <- auk_ebd(filtered_file) 
ebd_colorado <- auk_ebd(filtered_file)
ebd_kansas <- auk_ebd(filtered_file)
ebd_nebraska <- auk_ebd(filtered_file)
ebd_newmexico <- auk_ebd(filtered_file)
ebd_northdakota <- auk_ebd(filtered_file)
ebd_oklahoma <- auk_ebd(filtered_file)
ebd_southdakota <- auk_ebd(filtered_file)
ebd_texas1 <- auk_ebd(filtered_file)
ebd_texas2 <- auk_ebd(filtered_file)
ebd_texas3 <- auk_ebd(filtered_file)
ebd_texas4 <- auk_ebd(filtered_file)
ebd_wyoming <- auk_ebd(filtered_file)

montana <- ebd_montana
colorado <- ebd_colorado
kansas <- ebd_kansas
nebraska <- ebd_nebraska
newmexico <- ebd_newmexico
northdakota <- ebd_northdakota
oklahoma <- ebd_oklahoma
southdakota <- ebd_southdakota
texas1 <- ebd_texas1
texas2 <- ebd_texas2
texas3 <- ebd_texas3
texas4 <- ebd_texas4
wyoming <- ebd_wyoming

ebd_montana <- auk_state(montana, "US-MT") %>% 
  auk_complete()
ebd_colorado <- auk_state(colorado, "US-CO") %>%
  auk_complete()
ebd_kansas <- auk_state(kansas, "US-KS") %>%
  auk_complete()
ebd_nebraska <- auk_state(nebraska, "US-NE") %>%
  auk_complete()
ebd_newmexico <- auk_state(newmexico, "US-NM") %>%
  auk_complete()
ebd_northdakota <- auk_state(northdakota, "US-ND") %>%
  auk_complete()
ebd_oklahoma <- auk_state(oklahoma, "US-OK") %>%
  auk_complete()
ebd_southdakota <- auk_state(southdakota, "US-SD") %>%
  auk_complete()
ebd_texas1 <- auk_state(texas1, "US-TX") %>%
  auk_bbox(bbox = c(-103.035922, 31.980950, -94.026840, 36.502360)) %>%  
  auk_complete()
ebd_texas2 <- auk_state(texas2, "US-TX") %>%
  auk_bbox(bbox = c(-106.634553, 30.276345, -93.519323, 32.010459)) %>%  
  auk_complete()
ebd_texas3 <- auk_state(texas3, "US-TX") %>%
  auk_bbox(bbox = c(-106.634553, 28.980498, -93.519323, 30.276345)) %>%  
  auk_complete()
ebd_texas4 <- auk_state(texas4, "US-TX") %>%
  auk_bbox(bbox = c(-106.634553, 25.835360, -93.519323, 28.980498)) %>%  
  auk_complete()
ebd_wyoming <- auk_state(wyoming, "US-WY") %>%
  auk_complete()

montana_filter <- auk_filter(ebd_montana, file = "filtered_montana", sampling = sampling_file)
colorado_filter <- auk_filter(ebd_colorado, file = "filtered_colorado", sampling = sampling_file, overwrite = TRUE)
kansas_filter <- auk_filter(ebd_kansas, file = "filtered_kansas", sampling = sampling_file, overwrite = TRUE)
nebraska_filter <- auk_filter(ebd_nebraska, file = "filtered_nebraska", sampling = sampling_file, overwrite = TRUE)
newmexico_filter <- auk_filter(ebd_newmexico, file = "filtered_newmexico", sampling = sampling_file, overwrite = TRUE)
northdakota_filter <- auk_filter(ebd_northdakota, file = "filtered_northdakota", sampling = sampling_file, overwrite = TRUE)
oklahoma_filter <- auk_filter(ebd_oklahoma, file = "filtered_oklahoma", sampling = sampling_file, overwrite = TRUE)
southdakota_filter <- auk_filter(ebd_southdakota, file = "filtered_southdakota", sampling = sampling_file, overwrite = TRUE)
texas1_filter <- auk_filter(ebd_texas1, file = "filtered_texas1", sampling = sampling_file, overwrite = TRUE)
texas2_filter <- auk_filter(ebd_texas2, file = "filtered_texas2", sampling = sampling_file, overwrite = TRUE)
texas3_filter <- auk_filter(ebd_texas3, file = "filtered_texas3", sampling = sampling_file, overwrite = TRUE)
texas4_filter <- auk_filter(ebd_texas4, file = "filtered_texas4", sampling = sampling_file, overwrite = TRUE)
wyoming_filter <- auk_filter(ebd_wyoming, file = "filtered_wyoming", sampling = sampling_file, overwrite = TRUE)

texas3_filter <- "C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/filtered_texas3"
texas4_filter <- "C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/filtered_texas4"

# Reading in Montana Data

mtdata <- read_ebd(montana_filter)
codata <- read_ebd(colorado_filter)
ksdata <- read_ebd(kansas_filter)
nedata <- read_ebd(nebraska_filter)
nmdata <- read_ebd(newmexico_filter)
nddata <- read_ebd(northdakota_filter)
okdata <- read_ebd(oklahoma_filter)
sddata <- read_ebd(southdakota_filter)
tx1data <- read_ebd(texas1_filter)
tx2data <- read_ebd(texas2_filter)
tx3data <- read_ebd(texas3_filter)
tx4data <- read_ebd(texas4_filter)
wydata <- read_ebd(wyoming_filter)

##############################
#
#   College Checklist Data
#
##############################

# Creating datasets by state with all unique checklists within a 5km buffer of each school

montana_schools <- sample_data %>%
  filter(state_abbr == "MT")
colorado_schools <- sample_data %>%
  filter(state_abbr == "CO")
kansas_schools <- sample_data %>%
  filter(state_abbr == "KS")
nebraska_schools <- sample_data %>%
  filter(state_abbr == "NE")
newmexico_schools <- sample_data %>%
  filter(state_abbr == "NM")
northdakota_schools <- sample_data %>%
  filter(state_abbr == "ND")
oklahoma_schools <- sample_data %>%
  filter(state_abbr == "OK")
southdakota_schools <- sample_data %>%
  filter(state_abbr == "SD")
texas_schools <- sample_data %>%
  filter(state_abbr == "TX")
wyoming_schools <- sample_data %>%
  filter(state_abbr == "WY")


unique_checklists <- mtdata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists1 <- codata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists2 <- ksdata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists3 <- nedata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists4 <- nmdata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists5 <- nddata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists6 <- okdata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists7 <- sddata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists8 <- tx1data %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists9 <- tx2data %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists10 <- tx3data %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists11 <- tx4data %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)
unique_checklists12 <- wydata %>%
  distinct(checklist_id, latitude, longitude, .keep_all = TRUE)

points_sf <- st_as_sf(montana_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf1 <- st_as_sf(colorado_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf2 <- st_as_sf(kansas_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf3 <- st_as_sf(nebraska_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf4 <- st_as_sf(newmexico_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf5 <- st_as_sf(northdakota_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf6 <- st_as_sf(oklahoma_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf7 <- st_as_sf(southdakota_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf8 <- st_as_sf(texas_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf9 <- st_as_sf(texas_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf10 <- st_as_sf(texas_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf11 <- st_as_sf(texas_schools, coords = c("longitude", "latitude"), crs = 4326)
points_sf12 <- st_as_sf(wyoming_schools, coords = c("longitude", "latitude"), crs = 4326)

checklists_sf <- st_as_sf(unique_checklists, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf1 <- st_as_sf(unique_checklists1, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf2 <- st_as_sf(unique_checklists2, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf3 <- st_as_sf(unique_checklists3, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf4 <- st_as_sf(unique_checklists4, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf5 <- st_as_sf(unique_checklists5, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf6 <- st_as_sf(unique_checklists6, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf7 <- st_as_sf(unique_checklists7, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf8 <- st_as_sf(unique_checklists8, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf9 <- st_as_sf(unique_checklists9, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf10 <- st_as_sf(unique_checklists10, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf11 <- st_as_sf(unique_checklists11, coords = c("longitude", "latitude"), crs = 4326)
checklists_sf12 <- st_as_sf(unique_checklists12, coords = c("longitude", "latitude"), crs = 4326)

buffers_sf <- st_buffer(points_sf, dist = 5000)
buffers_sf1 <- st_buffer(points_sf1, dist = 5000)
buffers_sf2 <- st_buffer(points_sf2, dist = 5000)
buffers_sf3 <- st_buffer(points_sf3, dist = 5000)
buffers_sf4 <- st_buffer(points_sf4, dist = 5000)
buffers_sf5 <- st_buffer(points_sf5, dist = 5000)
buffers_sf6 <- st_buffer(points_sf6, dist = 5000)
buffers_sf7 <- st_buffer(points_sf7, dist = 5000)
buffers_sf8 <- st_buffer(points_sf8, dist = 5000)
buffers_sf9 <- st_buffer(points_sf9, dist = 5000)
buffers_sf10 <- st_buffer(points_sf10, dist = 5000)
buffers_sf11 <- st_buffer(points_sf11, dist = 5000)
buffers_sf12 <- st_buffer(points_sf12, dist = 5000)

checklist_counts <- sapply(st_intersects(buffers_sf, checklists_sf), length)
checklist_counts1 <- sapply(st_intersects(buffers_sf1, checklists_sf1), length)
checklist_counts2 <- sapply(st_intersects(buffers_sf2, checklists_sf2), length)
checklist_counts3 <- sapply(st_intersects(buffers_sf3, checklists_sf3), length)
checklist_counts4 <- sapply(st_intersects(buffers_sf4, checklists_sf4), length)
checklist_counts5 <- sapply(st_intersects(buffers_sf5, checklists_sf5), length)
checklist_counts6 <- sapply(st_intersects(buffers_sf6, checklists_sf6), length)
checklist_counts7 <- sapply(st_intersects(buffers_sf7, checklists_sf7), length)
checklist_counts8 <- sapply(st_intersects(buffers_sf8, checklists_sf8), length)
checklist_counts9 <- sapply(st_intersects(buffers_sf9, checklists_sf9), length)
checklist_counts10 <- sapply(st_intersects(buffers_sf10, checklists_sf10), length)
checklist_counts11 <- sapply(st_intersects(buffers_sf11, checklists_sf11), length)
checklist_counts12 <- sapply(st_intersects(buffers_sf12, checklists_sf12), length)

summary_data <- data.frame(buffer_id = 1:nrow(buffers_sf), checklist_count = checklist_counts)
summary_data1 <- data.frame(buffer_id = 1:nrow(buffers_sf1), checklist_count = checklist_counts1)
summary_data2 <- data.frame(buffer_id = 1:nrow(buffers_sf2), checklist_count = checklist_counts2)
summary_data3 <- data.frame(buffer_id = 1:nrow(buffers_sf3), checklist_count = checklist_counts3)
summary_data4 <- data.frame(buffer_id = 1:nrow(buffers_sf4), checklist_count = checklist_counts4)
summary_data5 <- data.frame(buffer_id = 1:nrow(buffers_sf5), checklist_count = checklist_counts5)
summary_data6 <- data.frame(buffer_id = 1:nrow(buffers_sf6), checklist_count = checklist_counts6)
summary_data7 <- data.frame(buffer_id = 1:nrow(buffers_sf7), checklist_count = checklist_counts7)
summary_data8 <- data.frame(buffer_id = 1:nrow(buffers_sf8), checklist_count = checklist_counts8)
summary_data9 <- data.frame(buffer_id = 1:nrow(buffers_sf9), checklist_count = checklist_counts9)
summary_data10 <- data.frame(buffer_id = 1:nrow(buffers_sf10), checklist_count = checklist_counts10)
summary_data11 <- data.frame(buffer_id = 1:nrow(buffers_sf11), checklist_count = checklist_counts11)
summary_data12 <- data.frame(buffer_id = 1:nrow(buffers_sf12), checklist_count = checklist_counts12)

montana_schools$buffer_id <- 1:nrow(montana_schools)
colorado_schools$buffer_id <- 1:nrow(colorado_schools)
kansas_schools$buffer_id <- 1:nrow(kansas_schools)
nebraska_schools$buffer_id <- 1:nrow(nebraska_schools)
newmexico_schools$buffer_id <- 1:nrow(newmexico_schools)
northdakota_schools$buffer_id <- 1:nrow(northdakota_schools)
oklahoma_schools$buffer_id <- 1:nrow(oklahoma_schools)
southdakota_schools$buffer_id <- 1:nrow(southdakota_schools)
texas_schools$buffer_id <- 1:nrow(texas_schools)
texas_schools$buffer_id <- 1:nrow(texas_schools)
texas_schools$buffer_id <- 1:nrow(texas_schools)
texas_schools$buffer_id <- 1:nrow(texas_schools)
wyoming_schools$buffer_id <- 1:nrow(wyoming_schools)

montana_data <- merge(montana_schools, summary_data, by = "buffer_id", all.x = TRUE)
colorado_data <- merge(colorado_schools, summary_data1, by = "buffer_id", all.x = TRUE)
kansas_data <- merge(kansas_schools, summary_data2, by = "buffer_id", all.x = TRUE)
nebraska_data <- merge(nebraska_schools, summary_data3, by = "buffer_id", all.x = TRUE)
newmexico_data <- merge(newmexico_schools, summary_data4, by = "buffer_id", all.x = TRUE)
northdakota_data <- merge(northdakota_schools, summary_data5, by = "buffer_id", all.x = TRUE)
oklahoma_data <- merge(oklahoma_schools, summary_data6, by = "buffer_id", all.x = TRUE)
southdakota_data <- merge(southdakota_schools, summary_data7, by = "buffer_id", all.x = TRUE)
texas_data1 <- merge(texas_schools, summary_data8, by = "buffer_id", all.x = TRUE)
texas_data2 <- merge(texas_schools, summary_data9, by = "buffer_id", all.x = TRUE)
texas_data3 <- merge(texas_schools, summary_data10, by = "buffer_id", all.x = TRUE)
texas_data4 <- merge(texas_schools, summary_data11, by = "buffer_id", all.x = TRUE)
wyoming_data <- merge(wyoming_schools, summary_data12, by = "buffer_id", all.x = TRUE)

write.csv(dataset_with_counts, "MT_with_counts.csv")
write.csv(colorado_data, "CO_with_counts.csv")
write.csv(kansas_data, "KS_with_counts.csv")
write.csv(nebraska_data, "NE_with_counts.csv")
write.csv(newmexico_data, "NM_with_counts.csv")
write.csv(northdakota_data, "ND_with_counts.csv")
write.csv(oklahoma_data, "OK_with_counts.csv")
write.csv(southdakota_data, "SD_with_counts.csv")
write.csv(texas_data1, "TX1_with_counts.csv")
write.csv(texas_data2, "TX2_with_counts.csv")
write.csv(texas_data3, "TX3_with_counts.csv")
write.csv(texas_data4, "TX4_with_counts.csv")
write.csv(wyoming_data, "WY_with_counts.csv")

##############################
#
# Spatial Data Layers
#
##############################

####### Elevation

points_sf <- st_as_sf(finalstate, coords = c("longitude", "latitude"), crs = 4326)
elevation_data <- get_elev_point(points_sf, prj = 4326, src = "epqs")

# Extract coordinates from the spatial object

coords <- st_coordinates(elevation_data)

# Convert to a data frame and add elevation column

elevation_df <- data.frame(
  latitude = coords[, "Y"], 
  longitude = coords[, "X"], 
  elevation = elevation_data$elevation
)

# Merge datasets (elevation in meters)

finalstate$elevation <- elevation_df$elevation[match(
  paste(finalstate$latitude, finalstate$longitude),
  paste(elevation_df$latitude, elevation_df$longitude)
)]

write.csv(finalstate, "finalstate (elevation).csv")

######## Census Data

census_api_key("e528287dd0ec8d82adb0918630f9f905eca2d1ea")

finalstate <- read.csv("finalstate (elevation).csv")

states <- c("TX", "SD", "OK", "KS", "NM", "NE", "ND", "CO", "MT", "WY")

tracts_list <- lapply(states, function(state) {
  tracts(state = state, year = 2020, class = "sf")
})

tracts <- do.call(rbind, tracts_list)

finalstate_sf <- st_as_sf(finalstate, coords = c("longitude", "latitude"), crs = 4269)  
final_with_tracts <- st_join(finalstate_sf, tracts, join = st_within)

unique_tracts <- unique(final_with_tracts$GEOID)
income_data_list <- lapply(states, function(state) {
  get_acs(
    geography = "tract",
    variables = "B19013_001",  # Median Household Income
    year = 2020,
    state = state,
    geometry = FALSE
  )
})

income_data_combined <- bind_rows(income_data_list)

final_data <- final_with_tracts %>%
  left_join(income_data_combined, by = "GEOID")

finalstate_elcen <- final_data %>%
  select(-STATEFP, -COUNTYFP, -TRACTCE, -GEOID, -NAME.x, -NAMELSAD, -MTFCC, -FUNCSTAT, -ALAND, -AWATER, -geometry)

write.csv(finalstate_elcen, "finalstate_elevation_census.csv")

########### Impervious Data

nlcd_impervious <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/nlcd_2021_impervious_l48_20230630/nlcd_2021_impervious_l48_20230630.img")
points_data <- read.csv("finalstate_elevation_census.csv")
nlcd_impervious_raster <- rast(nlcd_impervious)

points_vect <- vect(points_data, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
points_vect_proj <- project(points_vect, crs(nlcd_impervious))
buffers <- buffer(points_vect_proj, width = 5000)

# Extracting impervious value for each buffer and adding to list

impervious_values <- lapply(1:nrow(points_data), function(i) {
  extract(nlcd_impervious, buffers[i], fun = mean, na.rm = TRUE)
})

points_data$avg_impervious_value <- sapply(impervious_values, function(x) x[, 2])

write.csv(points_data, "finalstate_elev_census_imperv.csv")

############## NDVI

ndvi <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/MOD13A3.061__1_km_monthly_NDVI_doy2020001_aid0001.tif")

points_vect <- vect(working_data, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
points_vect_proj <- project(points_vect, crs(ndvi))
buffers <- buffer(points_vect_proj, width = 5000)

vegetation_values <- lapply(1:nrow(working_data), function(i) {
  extract(ndvi, buffers[i], fun = mean, na.rm = TRUE)
})

working_data$vegetation_value <- sapply(vegetation_values, function(x) x[, 2])

write.csv(working_data, "finalstate_elev_census_imperv_vege.csv")

############## ALAN

alan1 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N060E_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N060E_vcmcfg_v10_c202208102300.avg_rade9h.tif")
alan2 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N060E_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N060E_vcmcfg_v10_c202208102300.cf_cvg.tif")
alan3 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N060E_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N060E_vcmcfg_v10_c202208102300.cvg.tif")

alan4 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N060W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N060W_vcmcfg_v10_c202208102300.avg_rade9h.tif")
alan5 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N060W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N060W_vcmcfg_v10_c202208102300.cf_cvg.tif")
alan6 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N060W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N060W_vcmcfg_v10_c202208102300.cvg.tif")

alan7 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N180W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N180W_vcmcfg_v10_c202208102300.avg_rade9h.tif")
alan8 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N180W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N180W_vcmcfg_v10_c202208102300.cf_cvg.tif")
alan9 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_00N180W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_00N180W_vcmcfg_v10_c202208102300.cvg.tif")

alan10 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_75N060E_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_75N060E_vcmcfg_v10_c202208102300.avg_rade9h.tif")
alan11 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_75N060E_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_75N060E_vcmcfg_v10_c202208102300.cf_cvg.tif")
alan12 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_75N060E_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_75N060E_vcmcfg_v10_c202208102300.cvg.tif")

alan13
alan14
alan15

alan16 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_75N180W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_75N180W_vcmcfg_v10_c202208102300.avg_rade9h.tif")
alan17 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_75N180W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_75N180W_vcmcfg_v10_c202208102300.cf_cvg.tif")
alan18 <- rast("C:/Users/ianbe/OneDrive/Desktop/Grad School/Research/Campus Acoustics Project/Data/Data Layers/SVDNB_npp_20220701-20220726_75N180W_vcmcfg_v10_c202208102300/SVDNB_npp_20220701-20220726_75N180W_vcmcfg_v10_c202208102300.cvg.tif")

alanmosaic1 <- mosaic(alan1, alan2, alan3, fun = mean)
alanmosaic2 <- mosaic(alan4, alan5, alan6, fun = mean)
alanmosaic3 <- mosaic(alan7, alan8, alan9, fun = mean)
alanmosaic4 <- mosaic(alan10, alan11, alan12, fun = mean)
alanmosaic5
alanmosaic6 <- mosaic(alan16, alan17, alan18, fun = mean)


writeRaster(alanmosaic, "alanmosaic1.tif", filetype = "GTiff")
writeRaster(alanmosaic2, "alanmosaic2.tif", filetype = "GTiff")
writeRaster(alanmosaic3, "alanmosaic3.tif", filetype = "GTiff")
writeRaster(alanmosaic6, "alanmosaic6(GOOD).tif", filetype = "GTiff")


points_vect <- vect(working_data, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
points_vect_proj <- project(points_vect, crs(alanmosaic6))
buffers <- buffer(points_vect_proj, width = 5000)

alan_values <- lapply(1:nrow(working_data), function(i) {
  extract(alanmosaic6, points_vect_proj[i], fun = mean, na.rm = TRUE)
})

plot(alanmosaic4)
working_data$alan_value <- sapply(alan_values, function(x) x[, 2])

write.csv(working_data, "finalstate_elev_census_imperv_vege_alan.csv")

##############################
#
#   Prelim analysis
#
##############################

tx_prelim <- working_data %>%
  filter(state_abbr == "TX")

working_data <- read.csv("finalstate_elev_census_imperv_vege.csv")

prelim <- stan_glm(checklist_count ~ vegetation_value + alan_value + avg_impervious_value + avg_impervious_value*alan_value, data = tx_prelim, family = gaussian(),
                   chains = 4, iter = 2000)
prelim

range(working_data$checklist_count)

ggplot(data = working_data, aes(x = alan_value, y = checklist_count)) +
  geom_point(size = 2)+
  geom_smooth(method = "lm", color = "blue", linetype = "dashed")+
  theme_minimal()


##############################
#
# Plotting State data on map
#
##############################

# Nicer map

finalstate <- read.csv("Final State.csv")

us_map <- map_data("state")

selected_states <- c("texas", "oklahoma", "kansas", "nebraska", "north dakota", "south dakota", "wyoming", "colorado", "montana", "new mexico")

us_map <- map_data("state") %>%
  filter(region %in% selected_states)

ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = finalstate, aes(x = longitude, y = latitude, alpha = 0.25), color = "red") +
  theme_minimal() +
  coord_fixed(1.3) +
  scale_size(range = c(4, 10), name = "Checklist Submissions") +
  scale_alpha(guide = "none") +  
  ggtitle("Campuses on the Central Flyway") +
  theme(
    axis.title = element_blank(),    
    axis.text = element_blank(),     
    axis.ticks = element_blank(),     
    panel.grid = element_blank()     
  ) 

ggplot() +
  geom_polygon(data = us_map, aes(x= long, y = lat, group = group), fill = "white", color = "black") +
  coord_fixed(1.3) + 
  theme_minimal() + 
  theme(
    axis.title = element_blank(),    
    axis.text = element_blank(),     
    axis.ticks = element_blank(),     
    panel.grid = element_blank()     
  ) 

## Building Raster (this was just a test, built raster using inst_size data for whole country)

point_data <- vect(sample_data, geom = c("longitude", "latitude"), crs="EPSG:4326")

raster_template <- rast(ext = ext(point_data), resolution = 0.5)

abd_raster <- rasterize(point_data, raster_template, field = "inst_size")

plot(abd_raster)

############################
# AWK stuff

Sys.setenv(PATH = paste("C:/rtools43/usr/bin", Sys.getenv("PATH"), sep = ";"))



