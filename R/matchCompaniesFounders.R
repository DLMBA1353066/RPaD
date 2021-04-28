matchCompaniesFounders <- function(companies, founders)
{
  #' Match companies and founders
  #'
  #' @param companies: the companies dataset
  #' @param founders: the founders
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr mutate distinct select pull filter group_by ungroup arrange rename sym lag
  #' @importFrom tidyr na_replace
  #' @importFrom stringr str_replace str_remove_all str_remove str_trim
  #'
  #' @return The matched dataset
  #' @export
  #'
  #'

  foundersList <- companies %>% dplyr::select(Organisation_Name, Founders)

  splittedFounders <- strsplit(companies %>% dplyr::pull(Founders), split = ",")
  splittedDataset <- data.frame(Organisation_Name = rep(companies %>% dplyr::pull(Organisation_Name),
                                                        sapply(splittedFounders, length)),
                                Full_Name = unlist(splittedFounders)) %>%
    dplyr::mutate(Full_Name = stringr::str_trim(Full_Name, "both")) %>%
    dplyr::distinct()

  matchedDataset <- splittedDataset %>%
    dplyr::left_join(founders) %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, 0) %>%
    dplyr::group_by(Organisation_Name) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm=T) %>% # SUM != MAX
    dplyr::ungroup()

  fullData <- companies %>%
    dplyr::inner_join(matchedDataset) %>%
    dplyr::select(-Founders)


}
