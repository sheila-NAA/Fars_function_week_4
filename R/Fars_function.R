use_vignette("Fars_function")

test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "fars")),
                         c("accident_2013.csv.bz2",
                           "accident_2014.csv.bz2",
                           "accident_2015.csv.bz2"))
})

#'A File Reading Function
#'
#'This function is designed to read in a file into R.
#'It first off checks if the file exist.
#'If it does not, the function stops and returns a message 'file {filename} does
#' not exist'.
#' If the file exists, it reads it into R and places it in a dataframe format
#' that is easily worked with.
#'
#' @param filename The name of the file being read into R
#'
#' @return This function reads in the file in a tbl_df format if the filename exists
#' otherwise it returns the message 'file name does not exist'
#'
#' @examples fars_read(accident_2003.csv)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' A Make File Name Function
#' This function is designed to make the name of the file that is read in to
#' R based on the year specified.
#'
#' @param year  A string or integer of the year
#'
#' @return The function returns a string with the data path and the year specified.
#'
#' @examples make_filename(2014)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read Fars Years
#'
#' This function is an ancillary function for the function fars_summarize_years
#'
#' @param years A vectors with a list of years
#'
#' @return A dataframe with the months and years and provides NULL
#'  if the years are invalid
#'
#' @examples fars_read_years(c(2015,2016,2017))
#'
#' @importfrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
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

#' Summarize fars data by years
#'
#' This function summarises the yearly accident data by month
#'
#' @param year A vector with the list of years that are to be summarized
#'
#' @return Returns a summary of the number of accidents by year.
#'
#' @examples fars_summarize_years(c(2013,2014,2015))
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Function Mapping Accidents by state per year
#'
#' This function displays the state map with the accident location by year.
#' If the state number is invalid, an error is displayed.
#'
#' @param stat.num The number representing the state in question as defined in the
#' file in question.
#' @param year The year that is provided
#'
#'
#' @return None
#' @references 2014 FARS/NASS GES Coding and Validation Manual
#'
#' @examples
#' \dontrun{
#' fars_map_state(49, 2015)
#'
#' @export
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
