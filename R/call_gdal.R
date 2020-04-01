#' @title Call GDAL
#' @param input input grid (raster)
#' @param method resampling method
#' @param cellsize output cell diminsion
#' @keywords internal
#' @return RasterLayer
#' @importFrom gdalUtilities gdal_translate
#' @importFrom raster writeRaster raster

call_gdal = function(input, method, cellsize){

  tmp = tempfile(pattern = "input", fileext = '.tif')
  nn  = tempfile(pattern = "nn", fileext = '.tif')

  writeRaster(input, filename = tmp, overwrite = TRUE)

  gdalUtilities::gdal_translate(
                            src_dataset = tmp,
                            dst_dataset = nn,
                            r = method,
                            tr = c(cellsize,cellsize)
  )

  rr = raster(nn)

  file.remove(tmp)

  rr

}
