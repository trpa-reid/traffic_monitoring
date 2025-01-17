---
title: ""
author: ""
date: ''
output:
  html_document:
    highlight: tango
    number_sections: no
    theme: yeti
    toc: no
    toc_depth: 3
    toc_float:
      collapsed: yes
      smooth_scroll: no
  pdf_document:
    toc: no
    toc_depth: '3'
---

<style type="text/css">
div.sourceCode {
    overflow: hidden;
}
</style>

# {.tabset}



```r
library(pacman)
p_load(tidyverse, readxl, lubridate, data.table, purrr, janitor, scales,DT, plotly, xfun)
```

## Code


```r
setwd("H:/model/model_update_2019/validation/CalTrans_Pems_2018_hourly")
file.list <- list.files(pattern='*.xlsx')
file.list <- setNames(file.list, file.list) # only needed when you need an id-column with the file-names
pems <- map_df(file.list, read_excel, .id = "id") %>%
  mutate(Hour=as.POSIXct(Hour, format="%m/%d/%Y %H:%M")) %>%
  mutate(station=sub("(^[^-]+)-.*", "\\1", id)) %>%
  rename(Date=Hour, Count=`Flow (Veh/Hour)`, station_code=id) %>%
select(Date, station, Count, station_code)

## Caltrans "Elfred" Data ##

alec<-read_excel("H:/model/model_update_2019/validation/Caltrans_other_hourly_2018/FINAL_FORMATS_HourlyandHighest.xlsx", sheet="Hourly")

elfred <- alec %>%
  filter(!Count %in% c("No_data","No_Data","No_data"))  %>%
  filter(!Date %in% c("No_data","No_Data","No_data"))  %>%
  mutate(Count= as.numeric(Count),
         Date=str_replace_all(Date, "\r\n", " "),
         Date=str_replace_all(Date, "  ", " "),
         Date1= paste(substring(Date, 5), "2018", sep=" "),
         Date1= as.Date(Date1, "%b %d %Y"),
         Hour1=case_when(Hour== "0-1" ~ "00:00:00",
                         Hour== "1-2" ~ "01:00:00",
                         Hour== "2-3" ~ "02:00:00",
                         Hour== "3-4" ~ "03:00:00",
                         Hour== "4-5" ~ "04:00:00",
                         Hour== "5-6" ~ "05:00:00",
                         Hour== "6-7" ~ "06:00:00",
                         Hour== "7-8" ~ "07:00:00",
                         Hour== "8-9" ~ "08:00:00",
                         Hour== "9-10" ~ "09:00:00",
                         Hour== "10-11" ~ "10:00:00",
                         Hour== "11-12" ~ "11:00:00",
                         Hour== "12-13" ~ "12:00:00",
                         Hour== "13-14" ~ "13:00:00",
                         Hour== "14-15" ~ "14:00:00",
                         Hour== "15-16" ~ "15:00:00",
                         Hour== "16-17" ~ "16:00:00",
                         Hour== "17-18" ~ "17:00:00",
                         Hour== "18-19" ~ "18:00:00",
                         Hour== "19-20" ~ "19:00:00",
                         Hour== "20-21" ~ "20:00:00",
                         Hour== "21-22" ~ "21:00:00",
                         Hour== "22-23" ~ "22:00:00",
                         Hour== "23-24" ~ "23:00:00"),
         Date1= paste(Date1, Hour1, sep=" "),
         Date= as.POSIXct(Date1, format="%Y-%m-%d %H:%M:%S")) %>%
  left_join(
    data.frame(File_Name=c("CS160EWhourlydata",
                           "cs651nshourlycount",
                           "hourlycountsCS659ns",
                           "CS164165935hourlydatapm9.240SRPLA 28",
                           "detailHourlycountCS154_667",
                           "Item34hrlydata",
                           "Item21CS935hrlydata",
                           "Item25hrlydata",
                           "item27hrlydata",
                           "Item31hrlydata",
                           "Item35hrlydata",
                           "Item#38HrlyData" ),
               station=c("Carnelian B0ay Road - SR 28",
                               "El Dorado/Placer County Line - SR 89",
                               "Squaw Valley - SR 89",
                               "Kings Beach Junction - SR 267 & SR 28",
                               "Tahoe City Junction - SR 89 & SR 28", 
                               "Park Avenue", 
                               "Kings Beach Junction - SR 28 & SR 267",
                               "Junction SR 89 South and US 50",
                               "Sawmill Road",
                               "Rufus Allen",
                               "Stateline, South Shore",
                               "Bliss Memorial State Park")),
    by="File_Name") %>%
  filter(!is.na(station)) %>%
  mutate(direction_category=case_when(Direction %in% c("North","South") ~ "North or South",
                                      Direction %in% c("East","West") ~ "East or West"))

elfred_final<-elfred %>% 
  filter(station %in% c("Carnelian Bay Road - SR 28","El Dorado/Placer County Line - SR 89","Kings Beach Junction - SR 28 & SR 267","Sawmill Road","Stateline, South Shore","Bliss Memorial State Park")) %>%
  group_by(Date, station, File_Name) %>% 
  summarise(Direction=paste(Direction, collapse = "-"), Count=sum(Count, na.rm=T)) %>%
  rename(station_code=File_Name) %>%
  filter(Direction %in% c("East-West", "North-South")) %>%
  select(Date, station, Count, station_code) %>%
  ungroup()

## NDOT ##

ndot<-function(filename){
  test2<-read_xls(paste0("H:/model/model_update_2019/validation/NDOT_2018_counts_hourly/all/",filename), sheet="DV02P", skip=8, col_names = F) %>% 
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  slice(1:26) %>%
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  fill(c(4,11, 18, 25, 32, 39,46)) %>%
  slice(-1) %>%
  fill(c(3,6,8,10,13,15,17, 20,22,24, 27,29, 31,34,36,38,41,43,45,48,50)) %>%
  slice(-1) %>%
  select(1,2,4, 9,11,16,18, 23,25, 30,32, 37,39, 44,46) %>%
  pivot_longer(cols=c(2,4,6,8,10,12,14)) %>%
  select(1,10)
test3<-read_xls(paste0("H:/model/model_update_2019/validation/NDOT_2018_counts_hourly/all/",filename), sheet="DV02P", skip=8, col_names = F) %>% 
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  slice(1:26) %>%
  remove_empty("rows") %>%
  remove_empty("cols") %>%
  fill(c(4,11, 18, 25, 32, 39,46)) %>%
  slice(-1) %>%
  fill(c(3,6,8,10,13,15,17, 20,22,24, 27,29, 31,34,36,38,41,43,45,48,50)) %>%
  slice(-1) %>%
  select(1,2,4, 9,11,16,18, 23,25, 30,32, 37,39, 44,46) %>%
  pivot_longer(cols=c(3,5,7,9,11,13,15)) %>%
  select(1,10)
test5<-bind_cols(test3, test2) %>%
  select(1,2,4) }
setwd("H:/model/model_update_2019/validation/NDOT_2018_counts_hourly/all")
file.list <- list.files(pattern='*.xls')
file.list <- setNames(file.list, file.list) # only needed when you need an id-column with the file-names
ndot <- map_df(file.list, possibly(ndot, otherwise = tibble(x="error reading")), .id = "id")

ndot_clean <- ndot %>%
  mutate(Date=as.POSIXct(paste(sub(" ","",substring(value, 6)), `...1`), format="%m/%d/%Y %H:%M"),
         station_code=substr(id,26,31),
         value1=as.numeric(value1),
         station=case_when(station_code== "005211" ~ "US 50 & Lake Parkway",
                           station_code== "005315" ~ "Lower Kingsbury (SR 207)",
                           station_code== "025212" ~ "US 50 & Carson St (Spooner Summit)",
                           station_code== "031224" ~ "SR 28 & Lakeshore Dr (Incline)")) %>%
  rename(Count=value1) %>%
  select(Date, station, Count, station_code)

final<-bind_rows(
  pems ,
  ndot_clean ,
  elfred_final
) %>%
  complete(station,Date) %>%
  mutate(weekday=wday(Date), Day=date(Date), month=month(Date), hour=hour(Date)) %>%
  select(station,Date, Day, weekday, hour, month, Count)
```

## Graph
preserveb760bfc9a686b71bpreservee4daf8ba3be70dac


