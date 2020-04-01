#' @title Prepare Output Grid
#' @param input the input grid to resample
#' @param cellsize the target cellsize of the output grid (in same units as input grid)
#' @return an `sf` object of polygons (grid)
#' @importFrom sf st_transform st_make_grid st_sf st_bbox st_as_sfc st_coordinates st_centroid
#' @importFrom dplyr mutate
#' @importFrom raster res
#' @export

prep_output = function(input, cellsize = 1000){

  bb = st_as_sfc(st_bbox(input))

  output =
    bb %>%
    st_transform(st_crs(bb)) %>%
    st_make_grid(c(cellsize, cellsize)) %>%
    sf::st_sf()

  cols = st_coordinates(st_centroid(output))[,1] %>%
     round(2) %>%
     sort() %>%
     unique() %>%
     length()

  rows = st_coordinates(st_centroid(output))[,2] %>%
    round(2) %>% sort() %>% unique() %>% length()

  mutate(output,
         row_id = rep(c(1:rows), each = cols),
         col_id = rep(c(1:cols), rows),
         cellsize = cellsize
         )

}
