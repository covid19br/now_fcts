#' makeHospitalTable
#'
#' @param df.table df.table
#' @param dates dates
#' @param UTI UTI
#'
#' @export
#'
makeHospitalTable <- function(df.table, dates, UTI = FALSE){
  hospital_table = plyr::ldply(dates,
                         function(date) cbind(data.frame(date = date),
                                               countByAgeClass(getCurrentInBed(df.table, date, UTI = UTI))))
  hospital_table[is.na(hospital_table)] = 0
  hospital_table
}
