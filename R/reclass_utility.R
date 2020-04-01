#' @title Reclassify a Distribution Table by an Count value
#' @param areal_per the aeral percent of each cell covered by a class
#' @param output_count the number of target cells for each class in the output cell
#' @param no_data the no data value in the native dataset
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

  #areal_per$cat[is.na(areal_per$cat)] = no_data

  #a = fix_na_spaces(areal_per, output_count, no_data)

  as.numeric(areal_per$cat)

}


# fix_na_spaces = function(areal_per, output_count, no_data){
#
#   needed = sort(output_count) - sort(table(areal_per$cat[areal_per$cat != no_data]) )
#
#   if(sum(needed) > 0){
#   needed = names(needed[needed !=0])
#   to_fix = which(is.na(areal_per$cat))
#   areal_per2 = areal_per
#   areal_per2$index = 1:nrow(areal_per)
#   areal_per2 = areal_per2 %>% data.frame()
#
#   for(i in 1:length(to_fix)){
#
#     to_replace = names(which.max(areal_per[to_fix[i],]))
#     col_ind = which(names(areal_per) ==  needed )
#
#     areal_per3 = areal_per2[areal_per2$cat == to_replace,]
#     areal_per3 = areal_per3[order(areal_per3[,col_ind], decreasing = T, na.last = T),]
#
#     areal_per2$cat[which(areal_per2$index == areal_per3$index[i])] = needed
#     areal_per2$cat[to_fix[i]] = to_replace
#
#
#   }
#
#   areal_per2 %>% dplyr::arrange(index) %>% dplyr::select(-index)
#   } else {
#     areal_per
#   }
#
# }

