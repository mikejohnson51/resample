#' @title Random Allocation
#' @param input input grid
#' @param output output grid
#' @return a vector of classifications
#' @importFrom fastmatch fmatch
#' @importFrom tabularaster cellnumbers as_tibble
#' @importFrom dplyr group_by summarise
#' @export

rna = function(input, output){

  values = tabularaster::as_tibble(input)
  cn = tabularaster::cellnumbers(input, output)
  findex = fastmatch::fmatch(cn$cell_, values$cellindex)
  cn$cellvalue_ = values$cellvalue[findex]

  tmp = cn %>%
    dplyr::group_by(object_) %>%
    dplyr::summarise(rand = sample(cellvalue_, 1))

  tmp$rand

}
