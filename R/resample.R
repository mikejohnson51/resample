output_grid = function(input, cellsize){
  bb     = st_bbox(input)
  cols   = seq(bb$xmin,  bb$xmax, cellsize)
  rows   = seq(bb$ymax,  bb$ymin, -cellsize)
  cls    = round((bb$xmax - bb$xmin) / cellsize)
  rws    = round((bb$ymax - bb$ymin) / cellsize)

  output = st_make_grid(input,
               n        = c(cls, rws),
               offset   = c(min(cols), min(rows)),
               cellsize = c(cellsize, cellsize),
               crs      = st_crs(input),
               what     = "polygons") %>%
    st_sf()

  output$row_id = rep(c(1:rws), cls)
  output$col_id = rep(c(1:cls), each = rws)
  output
}


#' @title Resample
#' @description resample an input raster to an output grid using three different methods
#' @param input a raster of fine resolution
#' @param cellsize cell resolution of output target grid. In units of input CRS
#' @param method the method for resampling, nearest neighbor (nn), majority rule (maj), Area-based (area), or raw area (rawarea)
#' @param no_data a value to be treated as NO_DATA. Default to NA
#' @param seed seed number for random class assignment in rawarea
#' @importFrom raster writeRaster raster
#' @importFrom sf st_bbox st_centroid st_crs st_make_grid st_coordinates st_sf
#' @importFrom dplyr mutate
#' @return a resampled raster
#' @export

resampleData = function(input, cellsize = 1000,  method = "area", no_data = NA, seed = 10291991){

  if (method == "nn") {

      message("Nearest Neighbor...")
      rr = call_gdal(input, "near", cellsize = cellsize)

  } else if(method == "rawarea"){

    message("Raw Areal Proportions...")
    output = output_grid(input, cellsize = cellsize)
    out    = summarize.cells(input, output, no_data = no_data)
    vals   = unlist(lapply(1:length(out$output_count),
                         function(x){ as.numeric(rep(names(out$output_count)[x], out$output_count[x])) }
                  ))
    set.seed(seed)
    output$cat = sample(vals)
    rr = raster.from.vector(output)

  } else if(method == "area"){

      message("Areal Proportions...")
      output     = output_grid(input,
                               cellsize = cellsize)
      out        = summarize.cells(input,
                                   output = output,
                                   no_data = no_data)
      output$cat = reclass(areal_per = out$areal_per,
                           output_count = out$output_count,
                           no_data = no_data)
      rr         = raster.from.vector(output)

    } else if (method == 'maj') {

      message("Majority Rule...")
      rr = call_gdal(input, "mode", cellsize)

    } else{

      message("RNA...")
      output = output_grid(input, cellsize = cellsize)
      output$cat = rna(input, output)
      rr = raster.from.vector(output)
    }

  rr
}

