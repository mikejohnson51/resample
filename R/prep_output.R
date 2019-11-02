#' @title Prepare Output Grid
#' @param input the input grid to resample
#' @param cellsize the target cellsize of the output grid (in same units as input grid)
#' @return an `sf` object of polygons (grid)
#' @export

prep_output = function(input, cellsize = 1000){

  bb = getBoundingBox(input)

  output = bb %>% st_transform(input@crs) %>% st_make_grid(c(cellsize, cellsize)) %>% st_sf()
  fac = res(input)[1] / cellsize
  rows = ceiling(nrow(input) * fac)
  cols = floor(NROW(output) / rows)

  mutate(output, row_id = rep(c(1:rows), each = cols),col_id = rep(c(1:cols), rows))
  #output$row_id = rep(c(1:rows), each = cols)
  #output$col_id = rep(c(1:cols), rows)
  #output
}

