#' Read the Fatality Analysis Report Data
#'
#' @param filename A csv file that contains the Fatality Analysis report from the US National Highway Safety Administration
#'
#' @return This function returns a dataframe with Fatality Analysis report
#'
#' @source The source data is hosted on https://d3c33hcgiwev3.cloudfront.net
#'
#' @examples
#' fars_data <- fars_read(filename)
#' fars_data <- fars_read("./fars_data.csv")
#'
#' @details This function raise errors when external library is not installed or invoked using library() function
#'
#' @importFrom readr read_csv
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

#' Generate file name from year
#'
#' @param year A four digit valid year
#'
#' @return This function returns a string, filename with year.
#'
#' @details this function raise errors when year is passed as non-numeric value
#'
#' @examples
#' filename <- make_filename(year)
#' filename <- make_filename(2023)
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
#' fars_data <- fars_read_years(years)
#' fars_data <- fars_read_years(c(2021,2022))
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
#' summary <- fars_summarize_years(years)
#' summary <- fars_summarize_years(c(2021,2022))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(dat_list$year, dat_list$MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(dat_list$year, n)
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
#' fars_state_data <- fars_map_state(10, 2021)
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
