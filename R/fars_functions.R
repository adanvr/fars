#'  fars_read
#'
#' This is a function that will read in a file if it exist and return a tibble (data frame tbl). This function needs to import read_csv
#' from the readr package as well as tbl_df from the dplyr package.
#'
#' @param filename The name of the file to be read as a character string 
#' 
#' @return This function returns a tibble of the filename.
#'
#' @examples
#' fars_read("accident_2013.csv")
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

#' make_filename
#'
#' This is a function that will  create a filename using user-inputted year.
#'
#' @param year The year you would like to coerce into a filename.
#' 
#' @return Returns the name of a given file
#'
#' @examples
#' make_filename(2000)
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This is a function that creates files from a vector of years. 
#' This function needs to import mutate and select from the dplyr package.
#' This function will throw an error if it is unable to read in the input vector.
#'
#' @param years A character or integer vector of the years a user wants to read in.
#' 
#' @return Returns a list of tibbles with the columns MONTH and year
#'
#' @examples
#' fars_read_years(c(2000,2001,2002))
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


#' fars_summarise_years
#'
#' This is a function that creates a data frame based on a years vector and summarizes counts by year.
#' This function needs to import bind_rows, group_by and summarize from the dplyr package as well as spread from tidyr.
#'
#' @param years A character or integer vector of the years a user wants to read in.
#' 
#' @return Wide data frame with counts by year
#'
#' @examples
#' fars_summarize_years(c(2000,2001,2002))
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' fars_map_state
#'
#' This function creates plots of all the accidents in a given state, year combination.
#' This function imports filter from the dplyr package, map from the maps package and points from the graphics package.
#' An error will be thrown if the state.num input is not in the STATE column of the dataset. It will alsow throw an error
#' if there are no accidents recorded in the data frame for the given year and state.num combination.
#'
#' @param year A character or integer vector of the years a user wants to read in.
#' @param state.num A character or integer vector of the state number.
#' 
#' @return Plot of accidents for a given year and state.
#'
#' @examples
#' fars_map_state_years(49, c(2000,2001,2002))
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