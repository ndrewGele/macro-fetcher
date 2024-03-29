investopedia_anxiety <- function() {
  
  require(dplyr)
  require(RSelenium)
  
  server <- RSelenium::remoteDriver(
    remoteServerAddr = Sys.getenv('SELENIUM_HOST'),
    port = 4444L,
    browserName = 'chrome'
  )
  
  server$open()
  
  server$navigate('https://www.investopedia.com/anxiety-index-explained/')
  
  iframes <- server$findElements(using = 'css', 'iframe[data-src*="datawrapper"]')
  srcs <- purrr::map_chr(
    .x = iframes,
    .f = \(x) unlist(x$getElementAttribute('data-src'))
  )
  
  print(srcs)
  
  web_results <- purrr::map(
    .x = srcs,
    .f = \(x) {
      server$navigate(x)
      res <- tryCatch(
        {
          line <- server$findElement(using = 'css', 'g.line > path')
          line$getElementAttribute('aria-datavaluearray')
        },
        error = function(e) e
      )
    }
  ) %>%
    unlist()
  
  server$close()
  
  print(web_results)
  data_string <- web_results[sapply(web_results, nchar) > 250]
  print(data_string)
  
  data_string <- gsub('\\],\\[', '\n', data_string)
  data_string <- gsub('\\[|\\]', '', data_string)
  
  data_table <- read.table(text = data_string, sep = ',') 
  print('rows before filter')
  print(nrow(data_table))
  
  data_table <- data_table %>% 
    filter(V2 != " #N/A") %>% 
    mutate(V2 = as.numeric(V2))
  print('rows after filter')
  print(nrow(data_table))
  
  result <- data_table %>% 
    transmute(
      name = 'investopedia_anxiety',
      date = lubridate::ymd(V1),
      value = V2,
      update_timestamp = Sys.time()
    )
  
  return(result)
  
}
