library(sf)
library(raster)
library(ggplot2)
library(dplyr)

mapTheme = function() {
  theme_void() +
    theme(
      text = element_text(size = 7),
      plot.title = element_text(size = 36, color = "brown", hjust = 0, vjust = 2, face = "bold"),
      plot.subtitle = element_text(size = 8, color = "#3474A2", hjust = 0, vjust = 0),
      axis.ticks = element_blank(),
      legend.direction = "vertical",
      legend.position = "right",
      legend.text = element_text(size = 18),
      plot.margin = margin(1, 1, 1, 1, 'cm'),
      legend.key.height = unit(4, "cm"), legend.key.width = unit(.8, "cm")
    )
}


nlcd = raster::raster('/Volumes/Backup/NLCD/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img')

AOI = AOI::aoi_get(list("Colorado Springs", 21, 21), km = T) %>% st_transform(nlcd@crs)

input  = crop(nlcd,  AOI, snap = 'out')
output = prep_output(input, cellsize = 1000)
input = crop(nlcd, output, snap = 'out')

plot(input)
plot(output$geometry, add = T)

n = resampleData(input, output, 0, method = 'nn')
a = resampleData(input, output, 0, method = 'area')
a[a == 0 ] = 21
table(getValues(a))
m = resampleData(input, output, 0, method = 'maj')

m1 = matrix(input) %>% as.vector
r2 = raster()
crs(r2) = crs(input)
extent(r2) = extent(input)
res(r2) = res(input)
r2[] <-  matrix(input)

plot(r2)

orig = raster::raster(m1)
crs(orig) = crs(input)
extent(orig) = extent(input)
plot(orig)

par(mfrow = c(2,2), mar = c(.5,.5,1,.5))
plot(r2, breaks = col_lu$nlcd.code, col = col_lu$color, axes = F, legend = F, box = F, main = "Raw")
plot(output$geometry, add = T)
plot(n, breaks = col_lu$nlcd.code, col = col_lu$color,
     axes = F, legend = F, box = F,
     main = "Nearest Neighbor")

plot(output$geometry, add = T)
plot(a, breaks = col_lu$nlcd.code,
     col = col_lu$color, axes = F,
     legend = F, box = F,
     main = "Area Preservation")

plot(output$geometry, add = T)
plot(m, breaks = col_lu$nlcd.code, col = col_lu$color, axes = F, legend = F, box = F, main = "Majority Rule")
plot(output$geometry, add = T)

nrow(output)
# 318, 319, 340
test = output[240,]

plot(r2)
plot(output$geometry, add = T)
plot(test$geometry, add = T, col = 'red')


tmp = crop(r2, test)
plot(tmp, breaks = col_lu$nlcd.code, col = col_lu$color, axes = F, legend = F, box = F, main = "Nearest Neighbor\nShurbland is choosen")
plot(st_centroid(test)$geometry, add = T, col = 'black', pch =25, cex = 1)

t = table(getValues(tmp)) %>%
  data.frame() %>%
  mutate(cells = round(nrow(output) * (Freq / sum(Freq)))) %>%
  filter(cells != 0)

ggplot(t, aes(x = reorder(Var1, -cells), y = cells)) + geom_bar(stat = "identity") +
  geom_text(aes(x = reorder(Var1, cells), y = cells + 5,
                label = paste0(round(cells, 2), " (",100*round(cells / sum(cells) ,2), "%)" )), size = 3) + theme_bw() +
  labs(title = "Majority Rule\nLow Intensity Developed Selected",
       x = "NLCD Class",
       y = "Number of Cells")



t = table(getValues(r2)) %>%
  data.frame() %>%
  mutate(cells = round(nrow(output) * (Freq / sum(Freq)))) %>%
  filter(cells != 0)

ggplot(t, aes(x = reorder(Var1, cells), y = cells)) + geom_bar(stat = "identity") +
  geom_text(aes(x = reorder(Var1, cells), y = cells + 5,
                label = paste0(round(cells, 2), " (",100*round(cells / sum(cells) ,2), "%)" )), size = 3) + theme_bw() +
  labs(title = "Domain Porportions",
       x = "NLCD Class",
       y = "Number of Cells")


t = table(getValues(tmp)) %>%
  data.frame()

t = table(getValues(r2)) %>%
  data.frame() %>%
  mutate(cells = round(nrow(output) * (Freq / sum(Freq)))) %>%
  filter(cells != 0)

ggplot(t, aes(x = reorder(Var1, cells), y = cells)) + geom_bar(stat = "identity") +
geom_text(aes(x = reorder(Var1, cells), y = cells +5, label = round(cells, 2))) + theme_bw()


out = summarize.cells(input, output, no_data = 0)

to_plot = order(t$Freq, decreasing = FALSE)#[1:9]

tmp = t$Var1[to_plot]

pal = RColorBrewer::brewer.pal(9, "YlGn")

ll = list()

for(i in 1:12){
  output[[as.character(t$Var1[to_plot[i]])]] = out$areal_per[[to_plot[i]]]
}

output[[as.character(t$Var1[to_plot[i]])]] = out$areal_per[[to_plot[i]]]

breaks = seq(0,1, length.out = 10)
plot(output['test'], pal = pal, breaks = breaks)














state = "CO"
conus = getAOI(state = 'conus') %>% st_transform(5070)
u = st_union(conus)
county = AOI
state = getAOI(state = state)

ggplot() +
  geom_sf(data = conus, fill = NA, size = .05) +
  geom_sf(data = u, color = "black", fill = NA, size = 1.5) +
  geom_sf(data = state, size = .5, fill = "gray90") +
  geom_sf(data = county, fill = 'brown', color = NA) +
  mapTheme()


to_plot

a1 = a
a1[!(a1 %in% t$Var1[to_plot[1:2]])] = NA
plot(a1, breaks = col_lu$nlcd.code, col = col_lu$color,
     axes = F, legend = F, box = F)
plot(output$geometry, add = T, lwd = .5)



# histograms
n = c("El Paso, Colorado", "Tompkins, New York", "Tuscaloosa, Alabama")

d  = list(elpaso.df = data.frame(lc =
  c("Grassland",
    "Evergreen\nForest",
    'Developed',
    'Mixed Forest',
    'Pasture',
    "Water",
    "Woody\nWetlands",
    "Barren",
    "Herbaceous\nWetlands",
    "Deciduous\nForest",
    'Cultivated\nCrops',
    'Shrub'),
  val = c(-377, -201, -66,1,2,9,27,29,44,48,229,255)),

ny.df = data.frame(lc =
                         c("Pasture",
                           "Deciduous\nForest",
                           'Cultivated\nCrops',
                           "Barren",
                           'Developed',
                           "Herbaceous\nWetlands",
                           "Water",
                           "Grassland",
                           'Mixed Forest',
                           "Woody\nWetlands",
                           "Evergreen\nForest",
                           'Shrub'),
                       val = c(-117, -112, -32,0,3,9,9,11,25,35,73,96)),

al.df = data.frame(lc =
                         c("Evergreen\nForest",
                           'Pasture',
                           'Ice/Snow',
                           'Developed',
                           "Herbaceous\nWetlands",
                           "Barren",
                           "Deciduous\nForest",
                           "Woody\nWetlands",
                           "Water",
                           'Cultivated\nCrops',
                           'Grassland',
                           'Mixed Forest',
                           'Shrub'),
                       val = c(-3655, -492, -6,29,51,114,115,137,262,578,671,919, 1265))
)

for(i in 1:length(n)){

ggplot(data = d[[i]], aes(x = reorder(lc, -val), y = val)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(x = reorder(lc, -val), y = ifelse(val < 0, val - 30, val + 30) , label = round(val, 2)),
            size = 2.5) +
  theme_bw() +
  labs(title = paste0(n[i], "\n(Area Preserving - Majority Rule)"),
       y = "Count",
       x = "Land Cover Class")

  ggsave(filename = paste0("/Users/mikejohnson/Desktop/AGU_poster/", n[i], ".png"), height = 4, width = 6)

}
