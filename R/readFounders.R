readFounders <- function(dataPath="/../data/people/")
{
  #' Compose the database for Companies
  #'
  #' @param dataPath: path of raw data
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr mutate distinct select pull filter group_by ungroup arrange rename sym lag mutate_if
  #' @importFrom tidyr na_replace
  #'
  #' @return The Founders dataset
  #' @export
  #'

  filesToRead <- list.files(path = paste0(getwd(), dataPath), pattern="*.csv")

  people <- Reduce(rbind, lapply(filesToRead,
                                 FUN=function(x) {
                                   read.csv(file.path(paste0(getwd(), dataPath), x),
                                            sep=",",
                                            header = T)
                                 })) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, 0) %>%
    dplyr::distinct() %>%
    dplyr::select(-Primary_Organization)
}
