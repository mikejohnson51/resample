#' @title Reclassify a distribution table by a count value
#' @param areal_per the areal percent of each cell covered by a class
#' @param output_count the number of target cells for each class in the output cell
#' @param no_data the no data value in the native dataset
#' @return a vector
#' @export

reclass = function(areal_per, output_count, no_data = 0){
  # Fill all cells with 100% of one class with that class
  dist.tmp = apply(areal_per == 1, 1, match, x=TRUE)
  areal_per$cat = as.numeric(names(areal_per)[dist.tmp])
  #output_count = output_count[!is.na(names(output_count))]

  # For each class lets do the following:
  for (i in 1:length(output_count)) {
    # Identify the column in the summary table
    col.index = grep(paste0("_",names(output_count)[i], "$"), paste0("_",names(areal_per)))
    # Determine how many cells of this class need to be assigned
    to.fill   = output_count[i] - sum(areal_per$cat == as.numeric(names(output_count)[i]), na.rm = T)

    # Strip of the zonal proportions for this class
    tmp.row   = areal_per[[col.index]]
    # Set already classified cells to NA
    tmp.row[which(!is.na(areal_per$cat))] = NA
    # Set cells with less then 10% of this class to NA
    tmp.row[tmp.row <= .1] = NA

    # Identify how many cells can be filled  and fill those with the most coverage
    to.fill = min(to.fill, sum(!is.na(tmp.row)))
    sub.vals = order(tmp.row,decreasing = T)[1:to.fill]
    areal_per$cat[sub.vals]= as.numeric(names(output_count)[i])
  }

  # If a cell is unassigned, implement a majority rule
  ifelse(is.na(areal_per$cat),
         as.numeric(colnames(areal_per)[apply(areal_per[,-ncol(areal_per)],1,which.max)]),
         as.numeric(areal_per$cat))
}

