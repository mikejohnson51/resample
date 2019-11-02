#' @title Resample
#' @description resample an input raster to an output grid using three different methods
#' @param input a raster of fine resolution
#' @param output an output grid (can be created with \code{prep_output})
#' @param no_data a value to be treated as NO_DATA. Default to NA
#' @param method the method for resampling, nearest neighbor (nn), majority rule (maj) or Area-based (area)
#' @return a resampled raster
#' @export

resampleData = function(input, output, no_data = NA, method = "area"){

  if (method == "nn") {
    message("Nearest Neighbor...")
    o_cent = suppressWarnings( st_centroid(output) )
    cat = raster::extract(input, o_cent)
  } else {

    if(method == "area"){
      message("Areal Proportions...")
      out = summarize.cells(input, output, no_data = no_data)
      cat = reclass(areal_per = out$areal_per, output_count = out$output_count, no_data = no_data)
    } else {
      message("Majority Rule...")
      out = summarize.cells(input, output, no_data = NA)
      dist = out$areal_per
      cat = as.numeric(colnames(dist)[1:(ncol(dist)-1)][unlist(apply(dist[1:(ncol(dist)-1)],1,which.max))])
      }
  }

  raster.from.vector(vec = cat, output = output)
}
