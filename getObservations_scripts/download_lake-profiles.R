#Original depth in feet
#Do is mg/L
#Temp is C
#Datetime is CDT
library(tidyverse)
library(lubridate)
library(data.table)
library(here)

here::i_am("Predictive_Models/Maumelle/maumelle_df_retrieve-tidy.R")

Begin_date = "2021-04-20"
End_date = "2021-12-31"

nwis <- read.table(paste0("https://nwis.waterdata.usgs.gov/ar/nwis/uv?cb_00010=on&cb_00010=on&cb_00010=on&cb_00010=on&cb_00010=on&cb_00010=on&cb_00010=on&cb_00010=on&cb_00010=on&cb_00010=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_00300=on&cb_72147=on&cb_72147=on&cb_72147=on&cb_72147=on&cb_72147=on&cb_72147=on&cb_72147=on&cb_72147=on&cb_72147=on&cb_72147=on&format=rdb&site_no=072632995&period=&begin_date=", Begin_date, "&end_date=", End_date), header = TRUE, skip = 46, sep = "\t", na.strings = "NA", dec = ".", strip.white = TRUE)

# Buoy index #
buoy_indx <- read.csv(here("Predictive_Models/Maumelle/Buoy_index_USGS072632995.csv"))

# Remove excess data #
nwis <- nwis[-1,] # removes first row of excess data
nwis <- nwis[,!grepl("_cd", names(nwis))] # remove colums with "p"
nwis$site_no <- NULL # removes first column

# Change column formats #
nwis$datetime <- as.POSIXct(nwis$datetime)

cols <- c(2:ncol(nwis))
nwis[cols] <- as.numeric(as.matrix(nwis[cols]))

# Create new column names based on pulled data #
nwis_cols <- (colnames(nwis[2:ncol(nwis)]))
nwis_cols1 <- str_extract(nwis_cols, "(?<=X)[^_]+")
nwis_cols2 <- matrix(unlist(nwis_cols1))
nwis <- setnames(nwis, 
                 old = nwis_cols, 
                 new = nwis_cols2)
nwis_cols3 <- data.frame(matrix(unlist(nwis_cols1)))
colnames(nwis_cols3) <- c("TS")
nwis_cols3$ID <- 1:nrow(nwis_cols3)


match <- merge(buoy_indx, nwis_cols3, by.x = "TS")
match <- match[order(match$ID),]
matchID <- as.character(match$Sensor)

nwis <- setnames(nwis, 
                 old = nwis_cols2, 
                 new = matchID)


# Reshape dataframe 
nwis2 <- nwis|>
  group_by(datetime)|>
  summarize(across(everything(), mean))|> #couldn't fix a daylight savings problem on four data points Nov 7th-mean is reasonable solution
  ungroup()|>
  pivot_longer(-datetime,names_to = "var")|>
  separate(var, into = c("Var", "ID"), sep = "_")|>
  pivot_wider(names_from = Var, values_from = value)|>
  mutate(Depth_m = as.numeric(ID)-1,.keep = "unused")|> #depths are the same and correspond to meter intervals
  rename(Temp_C = "Temp", DO_mg.L = "DO")|>
  mutate(datetime = force_tz(datetime, tzone = "US/Central"),
                      datetime = with_tz(datetime, tzone = "UTC"))|>
  select(-Depth)|>
  mutate(DO_mg.L = ifelse(DO_mg.L <0, 0, DO_mg.L))|> #anoxic values stored as -0.1
  arrange(datetime, Depth_m)

#ggplot(nwis2|>filter(Depth_m == 9), aes(datetime, DO_mg.L))+
 # geom_point()


#write data to file

nwis2|>write_csv(here(paste0("Predictive_Models/Maumelle/maumelle_tidied_data-", Begin_date,"_", End_date,".csv")))
