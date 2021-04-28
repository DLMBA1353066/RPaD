composePeople <- function(dataPath="/../data/_people/")
{
  #' Compose the database for Companies
  #'
  #' @param dataPath: path of raw data
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr mutate distinct select pull filter group_by ungroup arrange rename sym lag
  #' @importFrom connectors queryBigQuery
  #' @importFrom lubridate hours month year day hour
  #'
  #' @return The People dataset
  #' @export
  #'

  filesToRead <- list.files(path = paste0(getwd(), dataPath), pattern="*.csv")

  people <- Reduce(rbind, lapply(filesToRead,
                                    FUN=function(x) {
                                      read.csv(file.path(paste0(getwd(), dataPath), x))
                                    })) %>%
    magrittr::set_colnames(c("Full_Name",
                           "Full_Name_URL",
                           "Primary_Organization",
                           "Primary_Organization_URL",
                           "Number_of_Exits",
                           "Number_of_Investments",
                           "Number_of_Exits_IPO",
                           "Number_of_Founded_Organizations",
                           "Number_of_Lead_Investments",
                           "Number_of_Partner_Investments",
                           "Number_of_Portfolio_Companies")) %>%
    dplyr::select(-Full_Name_URL, -Primary_Organization_URL) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Full_Name = stringr::str_remove_all(Full_Name, ", PhD"),
                  Full_Name = stringr::str_remove_all(Full_Name, ", Ph.D."),
                  Full_Name = stringr::str_remove_all(Full_Name, ", MD, MPH, MTM"))

}

#people %>%
#  write.table(file = paste0(getwd(), "/../data/people/", "Founders.csv"),
#                       col.names = T,
#                       row.names = F,
#                       sep=",")

