#' @title Reclassify a Distribution Table by an Count value
#' @param areal_per the aeral percent of each cell covered by a class
#' @param output_count the number of target cells for each class in the output cell
#' @param no_data a value to set unclassified cells to
#' @return a vector
#' @export


reclass = function(areal_per, output_count, no_data = 0){

  dist.tmp = apply(areal_per == 1, 1, match, x=TRUE)
  areal_per$cat = as.numeric(names(areal_per)[dist.tmp])

  for (i in 1:length(output_count)) {
    col.index = grep(paste0("_",names(output_count)[i]), paste0("_",names(areal_per)))
    to.fill = output_count[i] - sum(areal_per$cat == as.numeric(names(output_count)[i]), na.rm = T)
    full.rows = which(!is.na(areal_per$cat))
    tmp.row = areal_per[[col.index]]
    tmp.row[full.rows] = NA
    to.fill = min(to.fill, sum(!is.na(tmp.row)))
    sub.vals = order(tmp.row,decreasing = T)[1:to.fill]
    areal_per$cat[sub.vals]= as.numeric(names(output_count)[i])
  }

  areal_per$cat[is.na(areal_per$cat)] = no_data

  areal_per$cat

}
