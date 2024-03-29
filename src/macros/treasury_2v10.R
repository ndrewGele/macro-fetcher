fetch_treasury_2v10 <- function() {
  
  require(dplyr)
  
  years <- lubridate::year(Sys.Date()):(lubridate::year(Sys.Date())-2)
  
  results <- purrr::map_dfr(
    .x = years,
    .f = \(x) {
      read.csv(
        glue::glue(
          'https://home.treasury.gov/resource-center/data-chart-center/',
          'interest-rates/daily-treasury-rates.csv/{x}/all',
          '?type=daily_treasury_yield_curve&field_tdr_date_value={x}&page&_format=csv'
        )
      )
    }
  ) %>%
    transmute(
      name = 'treasury_2v10',
      date = lubridate::mdy(Date),
      value = X2.Yr/X10.Yr,
      update_timestamp = Sys.time()
    )

  all_dates <- data.frame(
    date = lubridate::as_date(min(results$date):max(results$date))
  )

  results <- results %>%
    full_join(all_dates, by = 'date') %>%
    arrange(date) %>%
    tidyr::fill(tidyselect::everything(), .direction = 'down')
    
  return(results)
  
}
