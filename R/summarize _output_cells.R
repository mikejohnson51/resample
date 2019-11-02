#' @title Summarize Output Grid Cells
#' @description Summarizies the percentage of each output grid cell that is covered by each disiticnt class in the input grid
#' @param input an input grid of fine resolution data `raster`
#' @param output the output grid to resample to. `sf` object
#' @param no_data the value representing "NO DATA", default is NA
#' @return a tibble

summarize.cells = function(input, output, no_data = NA){

values = as_tibble(input)

values_filter = if(is.na(no_data)){values} else { values[values$cellvalue != no_data,] }

count.cells  = values_filter %>%
  group_by(cellvalue) %>%
  summarize(count = n()) %>%
  mutate(ncell = NROW(output) * (count / sum(count)))

ncell.floor = floor(count.cells$ncell)

diff = (NROW(output)) - sum(ncell.floor)

revtrunc <- function(x) { x - floor(x) }

output_count = ifelse(count.cells$cellvalue %in% count.cells$cellvalue[(order(revtrunc(count.cells$ncell), decreasing = T))[1:diff]],
             ncell.floor + 1,
             ncell.floor)

names(output_count) = count.cells$cellvalue

output_count = sort(output_count)
output_count = output_count[output_count != 0]
output_count = if(is.na(no_data)){ output_count } else { output_count[names(output_count) != no_data] }

cn <- cellnumbers(input, output)

findex = fastmatch::fmatch(cn$cell_, values_filter$cellindex)
cn$cellvalue_ = values_filter$cellvalue[findex]

areal_per = cn %>%
  group_by(object_, cellvalue_) %>%
  summarize(count = n()) %>%
  mutate(v.pct = count / sum(count)) %>%
  select(-count) %>% tidyr::spread(cellvalue_, v.pct) %>% mutate(id = object_ )

areal_per = areal_per[names(areal_per) %in% names(output_count)]

return( list(areal_per = areal_per, output_count = output_count) )

}

