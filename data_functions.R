#' functions for data manipulation
#' EMC 9/8/21

# min-max normalization function
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}


#' @title 10 year moving average
#' @param index vector of index values to be smoothed
#' @param year vector of years associated with index
moving_avg_10y <- function(index, year) {
  smoothed = c()
  for (n in 10:length(index)) {
    avg10y = data.frame(index_smooth = mean(index[(n-9):n]),
                        year=year[n])
    smoothed = rbind(smoothed, avg10y)
  }
  return(smoothed)
}


#' @title 5 year moving average
#' @param index vector of index values to be smoothed
#' @param year vector of years associated with index
moving_avg_5y <- function(index, year) {
  smoothed = c()
  for (n in 5:length(index)) {
    avg10y = data.frame(index_smooth = mean(index[(n-4):n]),
                        year=year[n])
    smoothed = rbind(smoothed, avg10y)
  }
  return(smoothed)
}