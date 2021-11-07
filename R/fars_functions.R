library(dplyr)
library(readr)
library(tidyr)
library(magrittr)

#set directory for data files


#' Fars Read
#'
#' @description This is a simple function that reads in a CSV file and returns a tbl_df/dataframe
#' as it's output.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr as_tibble
#'
#' @param filename A character string of the name of the CSV file that you'd like to
#' read into a dataframe
#'
#' @return This function returns the specified file CSV file as a tbl_df/dataframe object. An
#' error will be returned if the filename is unable to be found in the working directory.
#'
#' @examples
#' \dontrun{
#' fars_read("example.csv")
#'}


fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::as_tibble(data)
}


#' Make Filename
#'
#' @description This function converts a year into the properly formatted filename
#'
#' @param year a year in the format of "yyyy"
#'
#' @return This function returns the specified filename as a string to be used with the entered year.
#' This function returns an error if value entered cannot be converted properly into an
#' integer format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_filename(2016)
#'}

make_filename <- function(year) {
  system.file("extdata", sprintf("accident_%d.csv.bz2", year), package = "TestPackage")
}

make_filename(2013)

system.file()

#' Fars Read Years
#'
#' @description This functions takes years as inputs and returns a list containing the year/month combinations
#' found in the FAR files
#'
#' @details The functions uses the Fars_Read/Make_Filename functions to take the year provided and create a
#' list of the months/year combinations if found in the FAR files.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @inheritParams make_filename
#'
#' @return This function returns a list of the year/month found in the FAR file based on the years provided. It will
#' return an error if the year provided cannot be converted into an integer format or if the filename resulting from
#' the years specified cannot be found in the directory.
#'
#' @examples
#' \dontrun{
#'fars_read_years(2016)
#'fars_read_years(c(2014,2013))
#'}


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Fars Summarize Years
#'
#' @description This functions takes years as inputs and returns a tbl_df/dataframe object totaling the number of fatalities
#' per month for each year specified.
#'
#' @details The functions uses the fars_read, make_filename, fars_read_years functions.
#' This function requires the following external package functions:
#' @importFrom dplyr bind_rows group_by summarize n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @inheritParams fars_read_years
#'
#' @return This function returns a tbl_dataframe object totaling the number of fatalities per month for each year specified.
#' An error will be returned if a year provided cannot be converted to integer format, or if no file corresponding to the
#' year specified is found in the working directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'fars_summarize_years(2016)
#'fars_summarize_years(c(2014,2013))
#'}

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Fars Map States
#'
#' @description This functions takes a year and state number as inputs and returns a plot of the fatalities
#' plotted on a graphic of the state.

#' @param year a year in the format of "yyyy" (multiple years cannot be supplied to this funtion)
#' @param state.num a state number from 1 to 50 representing one of the states of the United States.
#'
#' @return This function returns a plot of the state indicated with all fatalities plotted by latitude/longitude.
#' An error will be return if an invalid state number is supplied, or the year supplied is not found in the
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#'
#' @export
#'
#' @examples
#' \dontrun{
#'fars_map_state(25,2013)
#' }
#'


fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
