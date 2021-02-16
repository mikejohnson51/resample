#' @title Summarize Output Grid Cells
#' @description Summarizies the percentage of each output grid cell that is covered by each disiticnt class in the input grid
#' @param input an input raster to resample
#' @param output the output grid to resample to. `sf` object
#' @param no_data the value representing "NO DATA", default is NA
#' @importFrom dplyr group_by summarize mutate select n rename ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom tabularaster cellnumbers as_tibble
#' @return a tibble

summarize.cells = function(input, output, no_data = NA){

  # Extract Raster as tibble, 1 column for cellvalues, 1 for cellindex
  values = as_tibble(input)

  # Determine the percentage of each category in the input
  # Identify the corresponding number of cells in the output
  count = nrow(output) * (table(values$cellvalue) / nrow(values))

  # Flood the needed output values
  output_count = floor(count)

  # Identify how many cells can be added to the floored values and add to those with the
  # largest decimal value
  index <- order((count - output_count), decreasing = TRUE)[1:(nrow(output) - sum(output_count))]
  output_count[index] = output_count[index] + 1

  # Sort and remove classes with 0 needed cells
  output_count = sort(output_count)
  output_count = output_count[output_count != 0]

  # Reprioritize  no_data value if provided
  output_count = if(is.na(no_data)){
    output_count
  } else {
    c(output_count[names(output_count) == no_data], output_count[names(output_count) != no_data] )
  }

  # Determine which input cells underlay each output object
  cn <- cellnumbers(raster(input), output$geometry)

  # Add new column matching the cell values form the input
  cn$cellvalue_ = values$cellvalue[match(cn$cell_, values$cellindex)]

  # Group by Output object and cell value, count, and determine percentage
  # Pivot into wide table and rename object to id
  areal_per = cn %>%
    dplyr::group_by(object_, cellvalue_) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::mutate(v.pct = count / sum(count)) %>%
    tidyr::pivot_wider(id_cols = object_, values_from = v.pct, names_from = cellvalue_) %>%
    dplyr::rename(id = object_ ) %>%
    dplyr::ungroup()

  #Clean up and return
  areal_per = areal_per[names(areal_per) %in% names(output_count)]

  return( list(areal_per = areal_per, output_count = output_count) )
}


