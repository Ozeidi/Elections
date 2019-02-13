#install.packages('ggmap')
Sys.setlocale("LC_CTYPE", locale="Arabic")
options(warn=-1)
library(tidyverse)
library(ggmap)
df <- readxl::read_xlsx('E:/Misc/01. Tech/01. Programing/03. Projects/04. Election Analysis/Scraper/spiders/output/sheet.xlsx',
                  sheet = 'Sheet1')
data.frame(w =unique(df$w)) %>% mutate(lat= 0, lon = 0, geoAddress = 0) -> Address

df %>%  select(wilaya_id, w) %>% distinct() %>%  mutate(lat= 0, lon = 0, geoAddress = 0) -> Address
Address
write.csv(Address, file = "W.csv",fileEncoding = 'utf8')

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(Address))
{
  # Print("Working...")
  tryCatch(
    {
      result <- geocode(as.character(paste(Address$w[i], 'ولاية')), output = "latlona", source = "google")
      Address$lon[i] <- as.numeric(result[1])
      Address$lat[i] <- as.numeric(result[2])
      Address$geoAddress[i] <- as.character(result[3])
      },
    error=function(e){
      print(' \n Cant geocode address')
    }
  )
}
# Some Wilayate were not available on Google Map Service and had to be geocoded manually, eg. Mahoot, Al Jazir
Address %>% filter_all(any_vars(is.na(.))) 
Address[rowSums(is.na(Address)) > 0,]
#Convert geoAddress to character rather than factor
Address$geoAddress <- as.character(Address$geoAddress)
#Get Lat Long Manually from google Maps
Address[48,3:5] <- c(20.131995, 57.830014, 'Willayat Mahoot')
Address[50,3:5] <- c(18.548172, 55.857593, 'Willayat Al Jazir')
Address[34,3:5] <- list(22.9329, 57.7686, 'Willayat Izki')

Address %>%  rename(Lon= lat, Lat = lon) -> Address
# Write a CSV file containing origAddress to the working directory
write.csv(Address, "geocoded.csv", row.names=FALSE)

Address <- read.csv('geocoded.csv')


df %>%  group_by(wilaya_id,Surname_neutral) %>%  summarise(n()) -> df_grouped
df_grouped %>% arrange(desc(`n()`)) %>% rename(n = `n()`) -> df_grouped


