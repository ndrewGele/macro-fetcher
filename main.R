# This script will pick a macro indicator to pulll from the internet
# Then fetch that indicator via API or scraping
library(dplyr)
library(dbplyr)

# If database doesn't exist, just sleep
db_con <- tryCatch(
  expr = {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = Sys.getenv('POSTGRES_DB'),
      host = Sys.getenv('POSTGRES_HOST'),
      port = Sys.getenv('POSTGRES_PORT'),
      user = Sys.getenv('POSTGRES_USER'),
      password = Sys.getenv('POSTGRES_PASSWORD')
    )
  },
  error = function(e) e
)

if(inherits(db_con, 'error')){
  message('Error connecting to database:', db_con,
          'Sleeping for ten minutes before trying again.')
  Sys.sleep(60 * 10)
  stop('Done sleeping. Stopping process.')
}


# Prepare to pick a macro to fetch
fetcher_df <- purrr::map_dfr(
  .x = list.files('./src/macros', full.names = TRUE),
  .f = function(x) {
    data.frame(
      file = x,
      name = sub(pattern = '.R', replacement = '', x = x) %>% 
        sub(pattern = '^\\..*/', replacement = '', x = .)
    )
  }
)

# If no table, pick random macro
if(!DBI::dbExistsTable(db_con, 'macro_indicators')) {
  
  picked <- slice_sample(fetcher_df, n = 1)
  message('First process run. Picked ', 
          picked$name, ' randomly.')
  
} else {
  
  # If table exists, check for any not in db
  macro_stats <- db_con %>% 
    tbl('macro_indicators') %>% 
    group_by(name) %>% 
    summarise(max_update = max(update_timestamp, na.rm = TRUE)) %>% 
    collect()
  
  if(any(!fetcher_df$name %in% macro_stats$name)) {
    picked <- fetcher_df %>% 
      filter(!name %in% macro_stats$name) %>% 
      slice_sample(n = 1)
    
    message('Not all indicators present in table. Picked ', 
            picked$name, ' randomly from missing indicators')
    
  } else {
    
    # If all in db, pick oldest
    oldest_macro <- macro_stats %>% 
      slice_min(
        order_by = max_update,
        n = 1
      )
    
    picked <- filter(
      fetcher_df,
      name == oldest_macro$name
    )
    
    message('All indicators present in table. Picked ', picked$name, 
            ' randomly from least recently updated macro indicators.')
    
  }
  
} # end of macro picker

# Do the fetch
picked_fun <- source(picked$file)$value
df <- picked_fun()

# Save data to database
# Create indicators table if needed
if(!DBI::dbExistsTable(db_con, 'macro_indicators')) {
  
  df %>% 
    DBI::dbCreateTable(
      conn = db_con,
      name = 'macro_indicators',
      fields = .
    )
  
  new_df <- df
  
} else {
  
  # Check for duplicate data before writing to indicators table
  existing_df <- db_con %>% 
    tbl('macro_indicators') %>% 
    filter(name == !!picked$name) %>% 
    select(name, date) %>% 
    collect()
  
  new_df <- anti_join(
    df,
    existing_df,
    by = c('name', 'date')
  )
  
}

# If there is new data, append table and set sleep timer
if(nrow(new_df) > 0) {
  new_df %>% 
    DBI::dbAppendTable(
      conn = db_con,
      name = 'macro_indicators',
      value = .
    )
  sleep_time <- 5
} else {
  sleep_time <- 60 * 60 * 1
}  

# Whether there is new data or not, update the indicator's update_timestamp column
update_timestamp <- lubridate::floor_date(Sys.time(), unit = 'seconds')
DBI::dbExecute(
  conn = db_con,
  statement = glue::glue(
    'UPDATE macro_indicators ',
    'SET update_timestamp = \'{update_timestamp}\'' ,
    'WHERE name = \'{picked$name}\''
  )
)

# Log before sleeping and looping again
message('Wrote ', nrow(new_df), ' new records to indicators table. ',
        'Sleeping for ', sleep_time, ' seconds.')
DBI::dbDisconnect(db_con)
Sys.sleep(sleep_time)
stop('Done sleeping. Stopping process.')
