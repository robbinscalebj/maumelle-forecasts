#this code will update latest meteorological integrated surface dataset readings
library(tidyverse)
library(devtools)
#install_github('davidcarslaw/worldmet')
library(worldmet)
library(dataRetrieval)
library(lubridate)

setwd("/home/lakestudy/Profiler_Scripts/")

cors_meta <- getMeta(site = "corsicana", plot = FALSE) 
cors_code <- cors_meta$code
  
met_cors <- importNOAA(code = cors_code, year = 2018:year(now(tz = "UTC")))

  #Cloud cover from noaa is in oktas for three layers, GLM manual suggests averaging the oktas of the layers,
  #however, vast majority of cl_2 and cl_2 are NA so just using cl_1 and normalizing to [0,1]
#winddirection is degrees clockwise from true N from which the wind is blowing
  met_cors2 <- met_cors%>%
    mutate(Cloud_cov_fraction = cl_1/8, .keep = "unused")%>%
  #cols must be named: time, Clouds,AirTemp,RelHum,WindSpeed, Rainfall
    select(date,dew_point,Cloud_cov_fraction,ws,wd, air_temp, atmos_pres,RH)%>%
    rename(time = "date", AirTemp_C = "air_temp", DewPoint_C = "dew_point", RelHum_per = "RH", 
           Pressure_hPa = "atmos_pres", WindSpeed_m.s = "ws", 
           WindDirection_deg.clwise = "wd")%>%
    mutate(u10_m.s = WindSpeed_m.s*-sin(WindDirection_deg.clwise*(pi/180)),
           v10_m.s = WindSpeed_m.s*-cos(WindDirection_deg.clwise*(pi/180)))#triple-checked this was correct...
  

 
 
 #read NWIS always UTC   
    RC_intake_precip_total.inches <- readNWISuv(siteNumbers = "08064550", parameterCd = "00045")
    RC_intake_precip_total.inches2<- RC_intake_precip_total.inches%>%
      rename(precip_total.in = "X_00045_00000")%>%
      select(dateTime, precip_total.in)%>% 
      mutate(time = ceiling_date(dateTime,unit = "hour"))%>% 
      group_by(time)%>%
      #sum each hours rainfall to get inches/hour
      summarize(Rainfall_mm.s = sum(precip_total.in*25.4/3600))%>% #converting to mm/s which is equivalent to kg/m2 /s, which is same units of GEFS
      mutate(Rainfall_mm.s = ifelse(time == as_datetime("2021-04-05 17:00:00"), 0, Rainfall_mm.s))%>%
    #set to zero, obviously anomalous and next values are all zeros - gap in data record prior
      filter(time >=as_datetime("2021-01-01"))
    
met.j <- full_join(met_cors2,RC_intake_precip_total.inches2)%>%
  mutate(Pressure_mmhg = Pressure_hPa*0.75006157584566)


#write csv
met.j%>%write_csv("RC_noaa_isd_corsicana.csv")


  
