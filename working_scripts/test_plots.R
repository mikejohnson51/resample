library(raster)
library(dplyr)

nlcd = "/Volumes/GIS_Johnson/NLCD/NLCD_04_16/NLCD_2016_Land_Cover_L48_20190424.img" %>% raster()

AOI = getAOI(state = "CA", county = "San Diego") %>% sf::st_transform(nlcd@crs)

input  = crop(nlcd,  AOI, snap = 'out') #%>% deratify(att = 'ID')
output = prep_output(input, cellsize = 1000)

n = resampleData(input, output, 0, method = 'nn')
a = resampleData(input, output, 0, method = 'area')
m = resampleData(input, output, 0, method = 'maj')


par(mfrow = c(2,2), mar = c(1,0,3,0))

plot(input[[1]], breaks = col_lu$nlcd.code, col = col_lu$color, axes = F, box = F,legend = F, main = "NLCD")
plot(n, breaks = col_lu$nlcd.code, col = col_lu$color, axes = F, box = F,legend = F, main = "Nearest Neighbor")
plot(a, breaks = col_lu$nlcd.code, col = col_lu$color, axes = F, box = F,legend = F, main = "Area")
plot(m, breaks = col_lu$nlcd.code, col = col_lu$color, axes = F, box = F,legend = F, main = "Majority")

round(100*table(getValues(input[[1]])) / ncell(input[[1]]),0)
round(100*table(getValues(n)) / ncell(n))
round(100*table(getValues(a)) / ncell(a))
round(100*table(getValues(m)) / ncell(m))

nv = n[n != 0]
av = a[n != 0]
mv = m[n != 0]

length(nv)

sum(nv == av, na.rm = T) / length(nv)
sum(nv == mv, na.rm = T) / length(nv)
sum(av == mv, na.rm = T) / length(nv)



library(HydroData)

AOI = getAOI("Colorado Springs")
wbd = findWBD(sf::as_Spatial(AOI), level = 8, crop= F)

AOI = wbd$huc8[1,] %>% sf::st_transform(nlcd@crs)

input  = crop(nlcd,  AOI, snap = 'out') #%>% deratify(att = 'ID')
input  = mask(input, AOI, snap = 'out', updatevalue = 0)

output = prep_output(input, cellsize = 1000)

n = resampleData(input, output, 0, method = 'nn')
a = resampleData(input, output, no_data = NA, method = 'area')
m = resampleData(input, output, 0, method = 'maj')


