#' @title Resample
#' @description resample an input raster to an output grid using three different methods
#' @param input a raster of fine resolution
#' @param output an output grid (can be created with \code{prep_output})
#' @param no_data a value to be treated as NO_DATA. Default to NA
#' @param method the method for resampling, nearest neighbor (nn), majority rule (maj) or Area-based (area)
#' @importFrom raster writeRaster raster
#' @importFrom sf st_centroid
#' @return a resampled raster
#' @export

resampleData = function(input, output, no_data = NA, method = "area"){

  #orig_crs = st_crs(output)
  #output = st_transform(output, st_crs(input))

  if (method == "nn") {
      message("Nearest Neighbor...")
      rr = call_gdal(input, "nearest", output$cellsize[1])
  } else if(method == "area"){
      message("Areal Proportions...")
      out = summarize.cells2(input, output, no_data = no_data)
      cat = reclass(areal_per = out$areal_per, output_count = out$output_count, no_data = no_data)
      rr = raster.from.vector(vec = cat, output = output)
    } else if (method == 'maj') {
      message("Majority Rule...")
      rr = call_gdal(input, "mode", output$cellsize[1])
    } else{
      message("RNA...")
      cat = rna(input, output)
      rr = raster.from.vector(vec = cat, output = output)
    }
}


