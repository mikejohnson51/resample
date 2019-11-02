library(sf)
library(leaflet)
library(raster)
library(caret)

final = read_sf("/Users/mikejohnson/Desktop/DESKTOP/WRF_HYDRO/LC_resample_images/DATA/regions_04172019.shp")
nlcd = "/Volumes/Seagate5tb/NLCD/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img" %>% raster()

#write_sf(all, dsn = '/Users/mikejohnson/Desktop/WRF_HYDRO/LC_resample_images/DATA/regions_04172019.shp')

#df = data.frame(NN = numeric(), MAJ = numeric(), PA = numeric())

#final = AOI::getAOI(list('Colorado Springs, CO', 13, 13)) %>% st_as_sf()
# RUN FROM HERE -----------------------------------------------------------

mats = list()

for(kk in 1:300){

  AOI = final[kk,]  %>% st_transform(nlcd@crs)
  input = crop(nlcd, AOI, snap = 'out')
  output = prep_output(input)

  n = resampleData(input, output, 0, method = 'nn')
  a = resampleData(input, output, 0, method = 'area')
  m = resampleData(input, output, 0, method = 'maj')

  n.fine = resample(n, input, method = 'ngb')
  m.fine = resample(m, input, method = 'ngb')
  a.fine = resample(a, input, method = 'ngb')

  classes = table(getValues(input))

  n.tab = confusionMatrix(
    factor(getValues(n.fine), levels = names(classes)),
    factor(getValues(input), levels = names(classes))
  )$table

  m.tab = confusionMatrix(
    factor(getValues(m.fine), levels = names(classes)),
    factor(getValues(input), levels = names(classes))
  )$table


  a.tab = confusionMatrix(
    factor(getValues(a.fine), levels = names(classes)),
    factor(getValues(input), levels = names(classes))
  )$table


  u.mat = rbind(
    n.u = diag(n.tab) / rowSums(n.tab),
    m.u = diag(m.tab) / rowSums(m.tab),
    a.u = diag(a.tab) / rowSums(a.tab))

  u.mat = unlist(apply(u.mat,2,which.max))
  u.tab = table(factor(u.mat, levels = c(1,2,3)))

  p.mat = rbind(
    n.p = diag(n.tab) / colSums(n.tab),
    m.p = diag(m.tab) / colSums(m.tab),
    a.p = diag(a.tab) / colSums(a.tab))

  p.mat = unlist(apply(p.mat,2,which.max))
  p.tab = table(factor(p.mat, levels = c(1,2,3)))

  n.o = sum(diag(n.tab)) / sum(rowSums(n.tab))
  m.o = sum(diag(m.tab)) / sum(rowSums(m.tab))
  a.o = sum(diag(a.tab)) / sum(rowSums(a.tab))


  raw =  table(factor(getValues(input), levels = names(classes))) / ncell(input)
  n.raw = table(factor(getValues(n), levels = names(classes)))  / ncell(n)
  m.raw = table(factor(getValues(m), levels = names(classes)))  / ncell(m)
  a.raw = table(factor(getValues(a), levels = names(classes)))  / ncell(a)

  miss = cbind(
    n.missing = sum(n.raw == 0),
    m.missing = sum(m.raw == 0),
    a.missing = sum(a.raw == 0))


  range = cbind(
    n.r = range(100 * (n.raw - raw)),
    m.r = range(100 * (m.raw - raw)),
    a.r = range(100 * (a.raw - raw)))


  mat1 = c(p.tab, u.tab, n.o, m.o, a.o, range[1,], range[2,], miss)
  mat = matrix(mat1, ncol = 6, byrow = F)

  mats[[kk]] = mat

  message(kk)
}

