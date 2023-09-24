---
title: "PotatoIntensity"
author: "Shaoyi Lu"
date: "2023-06-13"
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
data = read_csv("../PestProsBaseDataV2/FinalClimateData.csv")
```


```{r}
result <- GetCDLData(aoi = c(-89.59255366, 44.01833313), year = 2007, type = 'p', crs = '+init=epsg:4326')

result
```

```{r}
Lon <- data$Lon[1]
Lat <- data$Lat[1]
Year <- data$Year[1]

aoi <- c(Lon, Lat)

result <- GetCDLData(aoi = aoi, year = Year, type = 'p', crs = '+init=epsg:4326')
result
```

```{r}
data <- as.data.frame(data)

data$x = NA
data$y = NA
data$value = NA
data$category = NA
data$color = NA

for (i in 1:nrow(data)) {
  Lon = data$Lon[i]
  Lat = data$Lat[i]
  Year = data$Year[i]
  
  aoi = c(Lon, Lat)
  
  result = tryCatch({
    GetCDLData(aoi = aoi, year = Year, type = 'p', crs = '+init=epsg:4326')
  }, error = function(e) {
    NA
  })
  
  data$x[i] = result$x
  data$y[i] = result$y
  data$value[i] = result$value
  data$category[i] = result$category
  data$color[i] = result$color
}

data
```

```{r, echo=FALSE}
data <- as.data.frame(data)
```

```{r, echo=FALSE}
write.csv(data, file = "C:/Users/16084/Desktop/Molecular Ecology/PotatoData/PotatoData.csv")
```
