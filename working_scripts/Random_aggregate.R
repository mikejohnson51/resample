library(sf)
library(leaflet)
library(raster)
library(caret)
library(dplyr)

final = read_sf("/Users/mikejohnson/Desktop/DESKTOP/WRF_HYDRO/LC_resample_images/DATA/regions_04172019.shp")
nlcd = '/Volumes/GIS_Johnson/NLCD/NLCD_04_16/NLCD_2016_Land_Cover_L48_20190424.img' %>% raster()

p = final %>% st_transform(5070)
conus = AOI::aoi_get(state = 'conus') %>% st_transform(5070) %>% st_union()

ggplot(data = conus) +
  geom_sf(fill = 'white', col = 'gray50' ) +
  geom_sf(data = p, col = 'red') +
  theme_bw()


#write_sf(all, dsn = '/Users/mikejohnson/Desktop/WRF_HYDRO/LC_resample_images/DATA/regions_04172019.shp')

#df = data.frame(NN = numeric(), MAJ = numeric(), PA = numeric())

#final = AOI::getAOI(list('Colorado Springs, CO', 13, 13)) %>% st_as_sf()
# RUN FROM HERE -----------------------------------------------------------

mats = list()

for(kk in 1:nrow(final)){

  AOI = final[kk,] %>%
    st_transform(nlcd@crs)
  output = prep_output(AOI)
  bb  = AOI::bbox_get(output)
  input = crop(nlcd, bb, snap = "out")

  n = resampleData(input, output, 0, method = 'nn')
  a = resampleData(input, output, 0, method = 'area')
  m = resampleData(input, output, 0, method = 'maj')
  r = resampleData(input, output, 0, method = 'rna')

  n.fine = resample(n, input, method = 'ngb')
  m.fine = resample(m, input, method = 'ngb')
  a.fine = resample(a, input, method = 'ngb')
  r.fine = resample(r, input, method = 'ngb')

  classes = table(getValues(input))

  confusionTable = function(grid, input, levels){
    confusionMatrix(
      factor(getValues(grid),  levels = names(levels)),
      factor(getValues(input), levels = names(levels))
    )$table
  }

  n.tab = confusionTable(n.fine, input, classes)
  m.tab = confusionTable(m.fine, input, classes)
  a.tab = confusionTable(a.fine, input, classes)
  r.tab = confusionTable(r.fine, input, classes)


  u.mat = rbind(
    n.u = diag(n.tab) / rowSums(n.tab),
    m.u = diag(m.tab) / rowSums(m.tab),
    a.u = diag(a.tab) / rowSums(a.tab),
    r.u = diag(r.tab) / rowSums(r.tab))


  u.mat = u.mat[,colSums(is.na(u.mat))<nrow(u.mat)]
  u.mat[is.na(u.mat)] = 0

  class_count = function(mat){
    all = list()
    suppressWarnings({
    for(j in 1:ncol(mat)){
      max = max(mat[,j], na.rm = TRUE)
      all[[j]] = which(mat[,j] == max)
    }
    unlist(all) %>% table()

    })
    }


  colMax <- apply(u.mat, 1, function(x) max(x))
  colMin <- apply(u.mat, 1, function(x) min(x))
  colMean <- apply(u.mat, 1, function(x) mean(x))


  u.tab = class_count(u.mat)
  u.tab.all = data.frame(t(c(0,0,0,0))) %>%
    setNames(c(1:4))
  u.tab = dplyr::bind_rows(u.tab, u.tab.all) %>%
    colSums(na.rm = TRUE) %>%
    t()  %>%
    data.frame() %>%
    select(X1, X2, X3, X4)
  u.tab2 = 100 * u.tab / length(classes)

  u.tab2 = rbind(colMax,colMin,colMean,u.tab2)

  p.mat = rbind(
    n.p = diag(n.tab) / colSums(n.tab),
    m.p = diag(m.tab) / colSums(m.tab),
    a.p = diag(a.tab) / colSums(a.tab),
    r.p = diag(r.tab) / colSums(r.tab))

  p.tab = class_count(p.mat)
  p.tab.all = data.frame(t(c(0,0,0,0))) %>%
    setNames(c(1:4))
  p.tab = dplyr::bind_rows(p.tab, p.tab.all) %>%
    colSums(na.rm = TRUE) %>%
    t()  %>%
    data.frame() %>%
    select(X1, X2, X3, X4)
  p.tab2 = 100 * p.tab / length(classes)

  colMax <- apply(p.mat, 1, function(x) max(x))
  colMin <- apply(p.mat, 1, function(x) min(x))
  colMean <- apply(p.mat, 1, function(x) mean(x))

  p.tab2 = rbind(colMax,colMin,colMean,p.tab2)

  o.mat = 100*c(
    n.o = sum(diag(n.tab)) / sum(rowSums(n.tab)),
    m.o = sum(diag(m.tab)) / sum(rowSums(m.tab)),
    a.o = sum(diag(a.tab)) / sum(rowSums(a.tab)),
    r.o = sum(diag(r.tab)) / sum(rowSums(r.tab)))

  makeRAW = function(rast, raw){
    raw = raw[raw != 0]
    levels  = names(raw)[raw !=0]
    tab = 100 * table(factor(getValues(rast), levels = levels))  / ncell(rast)
    ran = (tab - raw)

    poss.miss = abs(raw[which(tab == 0 & raw != 0)])

    max.miss = ifelse(length(poss.miss) > 0, max(poss.miss), 0)

    c(range(ran),
      sum(tab == 0 & raw != 0),
      100* sum(tab == 0 & raw != 0) / length(levels),
      max.miss,
      sum(poss.miss)
      )
  }

  raw = 100 * table(factor(getValues(input), levels = names(classes)))  / ncell(input)

  threshold = 100 / nrow(output)
  raw[raw < threshold] = 0

  xx = rbind(
    makeRAW(n, raw),
    makeRAW(m, raw),
    makeRAW(a, raw),
    makeRAW(r, raw))

  all = c("NN", "MR", "AP", "RNA")

  mat = cbind(t(p.tab), t(u.tab),
              t(p.tab2), t(u.tab2),
              o.mat, xx) %>%
    data.frame() %>%
    setNames(c('Producers', 'Users',
               'ProducersMax', 'ProducersMin', 'ProducersMean', 'ProducersPer',
               'UsersMax', 'UsersMin', 'UsersMean', 'UsersPer',
               'Overall', 'Under', 'Over', 'NumMissing', "MissingPer", "MaxAreaMissing", "TotMissing")) %>%
    mutate(type = all,
           area = as.numeric(st_area(AOI)/ 1e6),
           id = kk)

  mats[[kk]] = mat

  message(kk)
}

df = mats %>% dplyr::bind_rows()

save(df, file = '/Users/mikejohnson/Desktop/resampling_final_work/random_region_stats.rda')


df3 = df %>% select(type, "ProducersMean", 'ProducersPer') %>% mutate(ProducersMean = ProducersMean * 100)
df4 = df3 %>% reshape2::melt('type')

tmpP1 = ggplot(data = df4, aes(x = type, y = value, fill = factor(variable, labels = c('Overall Mean', 'Best Classes')))) +
  geom_boxplot(outlier.size = .5) +
  theme_bw() +
  theme(legend.position = c(.8, .85)) +
  labs(title = "Producers Accuracy",
       x = "",
       y = "Percent",
       fill = "Meteric") +
  scale_fill_brewer(palette = "Accent")

df3 = df %>% select(type, "UsersMean", 'UsersPer') %>% mutate(UsersMean = UsersMean * 100)
df4 = df3 %>% reshape2::melt('type')

tmpP2 = ggplot(data = df4, aes(x = type, y = value, fill = factor(variable, labels = c('Overall Mean', 'Best Classes')))) +
  geom_boxplot(outlier.size = .5) +
  theme_bw() +
  theme(legend.position = c(.8, .85)) +
  labs(title = "Users Accuracy",
       x = "",
       y = "Percent",
       fill = "Meteric") +
  scale_fill_brewer(palette = "Accent")


make_boxplot(df, "Over", "Over Estimation", "Percent", T) +
  make_boxplot(df, "Under", "Under Estimation", "Percent", T) +
  make_boxplot(df, "NumMissing", "Missing Classes", "Percent", T) +
  tmpP1 + tmpP2 +
  make_boxplot(df, "Overall", "Overall Accuracy", "Percent", T)


ggsave(filename = '/Users/mikejohnson/Desktop/resampling_final_work/random_sample_summary.png', height = 9, width = 15)
