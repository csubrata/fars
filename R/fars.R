#' Read the Fatality Analysis Report Data
#'
#' @param filename A csv file that contains the Fatality Analysis report from the US National Highway Safety Administration
#'
#' @return This function returns a dataframe with Fatality Analysis report
#'
#' @source The source data is hosted on https://d3c33hcgiwev3.cloudfront.net
#'
#'
#' @examples
#' \dontrun{
#' ars_data <- fars_read("accident_2013.csv.bz2")
#' ars_data <- fars_read("accident_2014.csv.bz2")
#' ars_data <- fars_read("accident_2015.csv.bz2")
#'}
#' @importFrom
#' readr read_csv
#' tibble as_tibble
#'
#' @details This function raise errors when external library is not installed or invoked using library() function
#'
#' @export
fars_read <- function(filename) {
  filepath = paste0("/Users/dreamland/Documents/R/fars/data/", filename)
  if(!file.exists(filepath))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filepath, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Generate file name from year
#'
#' @param year A four digit valid year
#'
#' @return This function returns a string, filename with year.
#'
#' @details this function raise errors when year is passed as non-numeric value
#'
#' @examples
#' \dontrun{
#' filename <- make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' This function will reset the year column with provided year
#'
#' @param years A list of years for which data will be loaded
#'
#' @return This function returns a dataframe month and year after resetting the year column with provided year
#'
#' @examples
#' \dontrun{
#' fars_data <- fars_read_years(c(2013,2014))
#' }
#'
#' @details This function raise errors when year value is invalid
#'
#' @importFrom
#' dplyr mutate
#' dplyr select
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

#' This function summarizes fars data, group by year and month, and finally spread the summaries over year and count
#'
#' @param years A list of years for which data will be loaded
#'
#' @return This function returns summaries data as a dataframe
#'
#' @details This function raise errors when external library is not installed or invoked using library() function
#'
#' @importFrom
#' dplyr bind_rows
#' dplyr group_by
#' dplyr summarize
#' tidyr spread
#'
#' @examples
#' \dontrun{
#' summary <- fars_summarize_years(c(2013,2014))
#' }
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' This function maps state number and returns data for the state
#'
#' @param state.num A state identification number
#' @param year A four digit valid year
#'
#' @return This function returns data for a state and year
#'
#' @details This function raise errors when external library is not installed or invoked using library() function
#'
#' @importFrom
#' dplyr filter
#' maps map
#' graphics points
#'
#' @examples
#' \dontrun{
#' fars_state_data <- fars_map_state(10, 2013)
#' }
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
