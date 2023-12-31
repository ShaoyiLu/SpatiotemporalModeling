---
title: "PestProsBase"
author: "Shaoyi Lu"
date: "2023-06-01"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning = FALSE,
                      error = TRUE, fig.height = 4)
```

```{r, include=FALSE}
library(FedData)
library(sp)
library(tidyverse)
library(kableExtra)
library(lubridate)
library(broman)
library(readr)
library(broom)
library(vroom)
library(dplyr)
library(ggplot2)
library(stringr)
library(geodata)
library(CropScapeR)
library(rgdal)
library(raster)
library(terra)
```

```{r, echo=FALSE}
data = read_csv("../PestProsBaseDataV2/PestProsBaseDataV2.csv")
```

```{r, echo=FALSE}
data = data %>%
  mutate(Abundance = CPBA + CPBL)

site_year_values = data %>%
  group_by(Lon, Lat, Year) %>%
  summarise(median = median(Abundance),
            max = max(Abundance))

site_year_values_df = as.data.frame(site_year_values)
site_year_values_df
```
```{r, echo=FALSE}

# Algorithm to get the month from the number of days
is_leap_year = function(year) {
  if (year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

infer_month = function(days, year) {
  days_in_month = c(31, 28 + is_leap_year(year), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  month = 1
  while (days > days_in_month[month]) {
    days = days - days_in_month[month]
    month = month + 1
  }
  return(month)
}
```

```{r, echo=FALSE}
get_clim_data = function(df, var) {
  df = as.data.frame(df)
  df$Month = mapply(infer_month, df$Day, df$Year)
  df[[paste0(var, "val")]] = NA
  
  for(i in 1:nrow(df)){
    x = worldclim_tile(var=var, lon=df$Lon[i], lat=df$Lat[i], res=0.5, path=tempdir())
    if(is.null(x)) {
      df[[paste0(var, "val")]][i] = 0
    } else {
      point = vect(cbind(df$Lon[i], df$Lat[i]))
      values = extract(x, point)
      df[[paste0(var, "val")]][i] = values[1, df$Month[i] + 1]
    }
  }
  
  return(df)
}

df <- get_clim_data(df, "tmin")
df <- get_clim_data(df, "tmax")
df <- get_clim_data(df, "prec")
df <- get_clim_data(df, "wind")

df
```

```{r, echo=FALSE}
write.csv(df, file = "C:/Users/16084/Desktop/Molecular Ecology/PestProsBaseDataV2/Climate.csv")
```

```{r, echo=FALSE}
new_data = read_csv("../PestProsBaseDataV2/Climate.csv")
```

```{r, echo=FALSE}
new_data = dplyr::select(new_data, -tmin, -tmax, -prec, -srad, -wind)
```

```{r, echo=FALSE}
climate_data = rename_(new_data, .dots = list(tmin="tminval", tmax="tmaxval", prec="precval", srad="sradval", wind="windval"))
climate_data
```

```{r, echo=FALSE}
write.csv(climate_data, file = "C:/Users/16084/Desktop/Molecular Ecology/PestProsBaseDataV2/FinalClimateData.csv")
```
