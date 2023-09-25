---
title: "Graph"
author: "Shaoyi Lu"
date: "2023-07-06"
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
library(prism)
library(sf)
library(rgeos)
```

```{r, echo=FALSE}
df = read_csv("../PotatoData/ClimateAndPotatoData.csv")
colnames(df)[1]='Site'
df
```

```{r, echo=FALSE}
wgs84 = "+init=epsg:4326" #proj4 string for WGS1984
cdlproj4 = "+init=epsg:5070" #proj4 string for CDL rasters

shp = spTransform(SpatialPointsDataFrame(coords=cbind(df$Lon,df$Lat), 
                  data=df, proj4string=CRS(wgs84)), CRS(cdlproj4))
#shp = data.frame(shp)
shp
```

```{r, echo=FALSE}
df$Date = as.Date(df$Day, origin = paste(df$Year-1, "-12-31", sep=""))
df$Day_in_Month = as.integer(format(df$Date, "%d"))
df=data.frame(df)
df
```

```{r}
df_sub = df[df$Year >= 2007 & df$Year <= 2022 & df$category == "Potatoes", ]

out = data.frame('Site'=NA,'Year'=NA,'Prop_potato'=NA) #start empty data frame to store proportion potato data for each site*year
ox = 1 #row index for out

for (i in 1:nrow(df_sub)){
  r = raster(paste0("C:/Users/16084/Desktop/Molecular Ecology/RasterTIF/CDL_", df_sub$Year[i], ".tif"))
    
  point = st_point(c(df_sub$x[i], df_sub$y[i]))
  point = st_sfc(point)
  point = st_sf(point)

  st_crs(point) = st_crs(r)

  buffer = st_buffer(point, dist = 1500)

  r_cropped = crop(r, buffer)
  r_masked = mask(r_cropped, buffer)

  point_val = extract(r, cbind(df_sub$x[i], df_sub$y[i]))[1]

  col_map = rep(NA, max(r_masked[], na.rm = TRUE))
  col_map[point_val] = df_sub$color[i]
  
  r_masked_vector = as.vector(r_masked[]) # flatten into a vector
  prop_potato = length(which(r_masked_vector == point_val)) / length(r_masked_vector) # calculate the proportion of potato
  
  out[ox,1] = df_sub$Site[i]
  out[ox,2] = df_sub$Year[i]
  out[ox,3] = prop_potato
  ox = ox + 1
}
```

```{r}
new_df = read_csv("../PotatoData/ClimateAndPotatoData.csv")
colnames(new_df)[1]='Site'

df_merged = merge(new_df, out[, c("Site", "Year", "Prop_potato")], by = c("Site", "Year"), all.x = TRUE)

new_df = df_merged

new_df
```

```{r}
#Potato Intensity
out
```

```{r, echo=FALSE}
sites = unique(out$Site)
out2 = data.frame('Site'=sites,'Potato_intensification'=NA)
for (i in 1:length(sites)){
	out2[i,2] = mean(out$Prop_potato[which(out$Site==sites[i])])
}
out2
```

```{r, echo=FALSE}
#Just make sure the date of each row
new_df$Date = as.Date(new_df$Day, origin = paste(new_df$Year-1, "-12-31", sep=""))
new_df$Day_in_Month = as.integer(format(new_df$Date, "%d"))
new_df=data.frame(new_df)

new_df = new_df[, !(names(new_df) %in% c("Date", "Day_in_Month"))]
new_df
```

```{r, echo=FALSE}
write.csv(new_df, file = "C:/Users/16084/Desktop/Molecular Ecology/PotatoData/FinalPotatoIntensity.csv")
```

```{r, echo=FALSE}
df = df %>%
  distinct(x, y, Month, category, .keep_all = TRUE)
df
```

```{r}
unique_data = data.frame()

for (cat in unique(df$category)) {
  color = df$color[which(df$category == cat)][1]
  unique_data = rbind(unique_data, data.frame("category" = cat, "color" = color))
}

unique_data
```

```{r}
unique_categories = unique_data$category
unique_colors = unique_data$color
n_categories = nrow(unique_data)

par(mar=c(5, 5, 2, 2), cex=0.6)
plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(1, 2), ylim = c(1, n_categories), xaxt = 'n', yaxt = 'n')

for (i in 1:n_categories) {
  rect(1, i, 1.5, i + 1, col = unique_colors[i], border = NA)
  text(1.6, i + 0.5, labels = unique_categories[i])
}

par(mar=c(5, 4, 4, 2) + 0.1, cex=1)
```

```{r, echo=FALSE}
# Example
png(filename = paste0("C:/Users/16084/Desktop/Molecular Ecology/PotatoGraph/example.png"))

r = raster(paste0("C:/Users/16084/Desktop/Molecular Ecology/RasterTIF/CDL_", df$Year[1], ".tif"))
    
point = st_point(c(df$x[1], df$y[1]))
point = st_sfc(point)
point = st_sf(point)

st_crs(point) = st_crs(r)

buffer = st_buffer(point, dist = 1500)

r_cropped = crop(r, buffer)
r_masked = mask(r_cropped, buffer)

point_val = extract(r, cbind(df$x[1], df$y[1]))[1]

col_map = rep(NA, max(r_masked[], na.rm = TRUE))
col_map[point_val] = df$color[1]

plot(r_masked, col = col_map, legend = FALSE)
plot(buffer, add=TRUE)

title(main = paste("Year: ", df$Year[1]))

dev.off()
```

```{r, echo=FALSE}
df_sub = df[df$Year >= 2007 & df$Year <= 2022 & df$category == "Potatoes", ]

for (i in 1:nrow(df_sub)){
  png(filename = paste0("C:/Users/16084/Desktop/Molecular Ecology/PotatoGraph/Graph_", df_sub$Site[i], " Month_", df_sub$Month[i], ".png"))
  
  r = raster(paste0("C:/Users/16084/Desktop/Molecular Ecology/RasterTIF/CDL_", df_sub$Year[i], ".tif"))
    
  point = st_point(c(df_sub$x[i], df_sub$y[i]))
  point = st_sfc(point)
  point = st_sf(point)

  st_crs(point) = st_crs(r)

  buffer = st_buffer(point, dist = 1500)

  r_cropped = crop(r, buffer)
  r_masked = mask(r_cropped, buffer)

  point_val = extract(r, cbind(df_sub$x[i], df_sub$y[i]))[1]

  col_map = rep(NA, max(r_masked[], na.rm = TRUE))
  col_map[point_val] = df_sub$color[i]

  plot(r_masked, col = col_map, legend = FALSE, las=1)
  plot(buffer, add=TRUE)

  title(main = paste("Date: ", df_sub$Year[i], "-", sprintf("%02d", df_sub$Month[i]), "-", sprintf("%02d", df_sub$Day_in_Month[i])), line = 2.5)
  mtext(paste("Lon: ", df_sub$Lon[i], "Lat: ", df_sub$Lat[i]), line = 1)
  
  dev.off()
}
```
