---
title: "Untitled"
author: "Shaoyi Lu"
date: "2023-08-23"
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
library(reshape2)
library(maps)
```

```{r}
df = read_csv("../PotatoData/FinalPotatoIntensity.csv")
df
```

```{r}
df$Date = as.Date(df$Day, origin = paste(df$Year-1, "-12-31", sep=""))
df$Day_in_Month = as.integer(format(df$Date, "%d"))
df=data.frame(df)
df = df[df$Year >= 2007 & df$Year <= 2022 & df$category == "Potatoes", ]
df
```

```{r}
colnames(df)
```

```{r}
df_sub = df[df$Year >= 2007 & df$Year <= 2022 & df$category == "Potatoes", ]

out = data.frame('Site'=NA,'Year'=NA,'Prop_potato'=NA) #start empty data frame to store proportion potato data for each site*year
ox = 1 #row index for out

for (i in 1:nrow(df_sub)) {
  current_year = df_sub$Year[i]
  years_to_consider = seq(current_year-4, current_year)
  
  prop_potato_years = c() # Store potato proportions for 5 years
  
  for (year in years_to_consider) {
    r = raster(paste0("C:/Users/16084/Desktop/Molecular Ecology/RasterTIF/CDL_", year, ".tif"))
    
    point = st_point(c(df_sub$x[i], df_sub$y[i]))
    point = st_sfc(point)
    point = st_sf(point)

    st_crs(point) = st_crs(r)

    buffer = st_buffer(point, dist = 1500)

    r_cropped = crop(r, buffer)
    r_masked = mask(r_cropped, buffer)

    point_val = extract(r, cbind(df_sub$x[i], df_sub$y[i]))[1]
  
    r_masked_vector = as.vector(r_masked[]) # flatten into a vector
    prop_potato = length(which(r_masked_vector == point_val)) / length(r_masked_vector) # calculate the proportion of potato
    
    prop_potato_years = c(prop_potato_years, prop_potato)
  }
  
  avg_prop_potato = mean(prop_potato_years)
  
  out[ox,1] = df_sub$Site[i]
  out[ox,2] = current_year
  out[ox,3] = avg_prop_potato
  ox = ox + 1
}
```

```{r}
write.csv(out, file = "C:/Users/16084/Desktop/Molecular Ecology/PotatoData/out.csv")
```

```{r}
out = read_csv("../PotatoData/out.csv")
out
```

```{r}
out = out[order(out$Site), ]
out
df_sub = df[df$Year >= 2007 & df$Year <= 2022 & df$category == "Potatoes", ]
df_sub = df_sub[order(df_sub$Site), ]
df_sub
```

```{r}
out_1 = out
colnames(out_1)[ncol(out_1)] = "potato_intensity"
out_1
```

```{r}
df_sub = df_sub[order(df_sub$Year), ]
out_1 = out_1[order(out_1$Year), ]
model = lm(df_sub$Abundance ~ out_1$potato_intensity + df_sub$tavg)
summary(model)
```

```{r}
out_1$Site = as.numeric(as.character(out_1$Site))

out_1 = out_1[order(out_1$Site), ]

df_sub$Site = as.numeric(as.character(df_sub$Site))

merged_data = merge(out_1, df_sub, by = c("Site", "Year"), all = TRUE)
#merged_data = merged_data[order(merged_date$Site), ]
merged_data
```

```{r}
result = df_sub %>%
  group_by(Year, Lon, Lat) %>%
  filter(Abundance == max(Abundance))
result = result[order(result$Site), ]

result = result %>%
  arrange(Date)
result
```

```{r}
calculate_sample_frac <- function(abundance, prop_potato) {
  min(1, 0.2 + 0.001 * abundance + 0.5 * prop_potato)
}

sample_fractions = purrr::map2_dbl(result$Abundance, result$Prop_potato, calculate_sample_frac)

set.seed(123)
to_keep = result[runif(nrow(result)) < sample_fractions, ]

ggplot(to_keep, aes(x = Prop_potato, y = Abundance)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Abundance vs Potato Intensity", x = "Potato Intensity", y = "Abundance")
```


```{r}
wisconsin_map <- map_data("state", region = "wisconsin")

df_count <- df %>%
  group_by(Lon, Lat) %>%
  summarize(Count = n())

ggplot() +
  geom_polygon(data = wisconsin_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = df_count, aes(x = Lon, y = Lat, size = Count, color = Count), alpha = 0.7) +
  scale_size_continuous(guide = "legend", range = c(1, 10)) +
  scale_color_continuous(low = "yellow", high = "red") +
  coord_fixed(ratio = 1.3) + 
  theme_minimal() +
  labs(title = "The number of times the coordinate appears in Wisconsin",
       x = "Lon", y = "Lat")
```


```{r}
df_count_sorted = df_count %>%
  arrange(Count)

ggplot() +
  geom_polygon(data = wisconsin_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = df_count_sorted, aes(x = Lon, y = Lat, size = Count, color = Count), alpha = 0.5) +
  scale_size_continuous(guide = "legend", range = c(2, 2)) +
  scale_color_continuous(low = "lightpink", high = "darkred") +
  coord_fixed(ratio = 1.3) + 
  theme_minimal() +
  labs(title = "The number of times the coordinate appears in Wisconsin",
       x = "Lon", y = "Lat")
```

```{r}

```
