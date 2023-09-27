#download daily forecasts from Lake Maumelle

# Bulk of code to download forecasts from : https://github.com/eco4cast/gefs4cast
# which was authored by Carl Boettiger and R Quinn Thomas. Adapted here by Caleb Robbins from their "megacube_extract" function


library(tidyverse)
library(here)
source(here("getGEFS_scripts/gefs-methods.R"))

sites_lra <- tibble(site_name = "Little Rock Airport", site_id = "lra", 
                    latitude = 34.73027329054854, longitude = -92.22991074255197)


my_sites <- function(crs = sf::st_crs(grib_wkt()) ) {
  sites <- sites_lra
  sf_sites <- sf::st_as_sf(sites,coords=c("longitude", "latitude"), #process coordinates as sf for gdal_cube::extract_geom()
                           crs = 4326) |>
    tibble::rowid_to_column("FID") |>
    sf::st_transform(crs = crs)
}

#map of the site for funsies - needs tigris, tmap, and tmaptools packages
#ar <- tigris::states(cb = TRUE, resolution = "20m", class = "sf") %>%
#  filter(STUSPS == "AR")

#tm_shape(ar, is.main = TRUE, bbox = tmaptools::bb(ar, xlim = c(-95,-90), ylim = c(30,40)))+ 
#  tm_lines()+
#  tm_shape(sites)+
#  tm_symbols(col = "black", fill = "blue", fill_alpha = 0.2)

vars <- names(gefs_bands()) #relative directory should be correct in gefs_methods.R
sites <- my_sites()
sites_df <- sites |>
  tibble::as_tibble() |>
  dplyr::select(dplyr::any_of(c("FID", "site_id")))


    #set up
        
          gribs <- tidyr::expand_grid(ensemble = gefs_ensemble(),
                               cycle = "00",
                               reference_datetime = Sys.Date(),
                               horizon = gefs_horizon()) |>
            dplyr::mutate(url = gefs_urls(ensemble, reference_datetime,
                                          horizon, cycle=cycle),
                          time = as.Date(Sys.Date() + dplyr::row_number()))|>
            slice_head(n=1)
          
          
          time <- cycle <- reference_datetime <- NULL


gdalcubes::gdalcubes_options(parallel = 12)

df <- gdalcubes::stack_cube(gribs$url,
                      datetime_values = gribs$time,
                      band_names = gefs_all_bands()) |>
  gdalcubes::select_bands(gefs_bands()) |>
  gdalcubes::extract_geom(my_sites()) |>
                   tibble::as_tibble()|>
  dplyr::rename({gefs_bands()}) |>
  # unpack overloaded time dimension:
  dplyr::left_join(dplyr::mutate(gribs, time=as.character(time)), by="time") |>
  dplyr::select(-"url") |> dplyr::select(-"time") |>
  dplyr::mutate(datetime = reference_datetime +
                  lubridate::hours(cycle) +
                  lubridate::hours(horizon)) |>
  # feature-id to site_id:
  dplyr::inner_join(sites_df, by = "FID") |>
  dplyr::select(-"FID") |>
  # "long" (EFI) format
  tidyr::pivot_longer(vars,
                      names_to = "variable",
                      values_to = "prediction") |>
  dplyr::ungroup()|>
  write_csv(paste(here("GEFS_data/", paste("gefs_ens_", reference_datetime,".csv"))))
 
print(paste0("Finished Date: ", gefs_dates[i],". Number ", i, "/", length(gefs_dates), " at ", Sys.time())) 
  }, error = function(e){cat("ERROR : ", conditionMessage(e), "\n")})
}



