#this code will update latest meteorological integrated surface dataset readings
library(tidyverse)
library(devtools)
#install_github('davidcarslaw/worldmet')
library(worldmet)
library(dataRetrieval)
library(lubridate)


cors_meta <- getMeta(site = "adams field airport", plot = TRUE) 
cors_code <- cors_meta$code[1] #selecting Little Rock - query above also matches airport in Colorado
#north little rock airport is ~20 km/12 mi from Maumelle buoy, but has almost zero data available
#little rock airport 27 km away from buoy but great data

#info on ISD/variable descriptions here:https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00532
#maps of isd stations here: https://www.ncei.noaa.gov/maps/hourly/

met_cors <- importNOAA(code = cors_code, year = 2018:year(now(tz = "UTC")))

#Cloud cover is oktas, but code 9 and 10 define obscured view - setting these to NA - only a few data points
#winddirection is degrees clockwise from true N from which the wind is blowing
met_cors2 <- met_cors%>%
  mutate(Cloud_cov_fraction = ifelse(cl >8, NA_real_, cl/8), .keep = "unused")%>%
  #cols must be named: time, Clouds,AirTemp,RelHum,WindSpeed, Rainfall
  select(date,dew_point,Cloud_cov_fraction,ws,wd, air_temp, atmos_pres,RH)%>%
  rename(time = "date", AirTemp_C = "air_temp", DewPoint_C = "dew_point", RelHum_per = "RH", 
         Pressure_hPa = "atmos_pres", WindSpeed_m.s = "ws", 
         WindDirection_deg.clwise = "wd")%>%
  mutate(WWind_m.s = WindSpeed_m.s*-sin(WindDirection_deg.clwise*(pi/180)),
         SWind_m.s = WindSpeed_m.s*-cos(WindDirection_deg.clwise*(pi/180)))|>#triple-checked this was correct...
  mutate(WWind_m.s = ifelse(WindSpeed_m.s == 0, 0, WWind_m.s),
         SWind_m.s = ifelse(WindSpeed_m.s == 0, 0, SWind_m.s))


#write csv
met_cors2%>%write_csv(here("observation_data/isd/noaa_isd_obs.csv"))

# Maumelle has precipitation gages on upstream stream stations, but data starts in April 2023 - 
# but could presumably use something like code below...

#read NWIS always UTC   
#RC_intake_precip_total.inches <- readNWISuv(siteNumbers = "08064550", parameterCd = "00045")
#RC_intake_precip_total.inches2<- RC_intake_precip_total.inches%>%
#  rename(precip_total.in = "X_00045_00000")%>%
 # select(dateTime, precip_total.in)%>% 
#  mutate(time = ceiling_date(dateTime,unit = "hour"))%>% 
#  group_by(time)%>%
#  #sum each hours rainfall to get inches/hour
#  summarize(Rainfall_mm.s = sum(precip_total.in*25.4/3600))%>% #converting to mm/s which is equivalent to kg/m2 /s, which is same units of GEFS
#  mutate(Rainfall_mm.s = ifelse(time == as_datetime("2021-04-05 17:00:00"), 0, Rainfall_mm.s))%>%
#  #set to zero, obviously anomalous and next values are all zeros - gap in data record prior
###  filter(time >=as_datetime("2021-01-01"))

#met.j <- full_join(met_cors2,RC_intake_precip_total.inches2)%>%
#  mutate(Pressure_mmhg = Pressure_hPa*0.75006157584566)



