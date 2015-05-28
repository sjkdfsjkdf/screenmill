## ----eval = FALSE--------------------------------------------------------
#  # Write `plates.csv` and `screens.csv` to the same directory as `cm.txt`
#  screenmill::new_batch_metadata('path/to/cm.txt')
#  # If necessary, create a new strain collection keyfile
#  # screenmill::new_strain_collection(id = 'collection_id', nplates = 2, density = 96)

## ----eval = FALSE--------------------------------------------------------
#  ## ---- Settings: -------------------------------------------------------------
#  date       <- 'YYYY-MM-DD' # prepended to file name
#  replicates <- 4            # number of replicates of each strain
#  cm         <- 'path/to/cm-engine-log.txt'
#  dr         <- 'path/to/dr-engine-all-stats.txt'
#  aux        <- 'path/to/dr-engine-auxilliary-stats.txt'
#  screens    <- 'path/to/screens-metadata.csv'
#  plates     <- 'path/to/plates-metadata.csv'
#  custom_strain_collection = FALSE # or, 'path/to/strain-collection.csv'
#  ## ----------------------------------------------------------------------------
#  
#  # Load required packagaes and connect to the database
#  library(screenmill); library(rothscreen); library(dplyr)
#  db <- src_rothscreen()
#  
#  # Read in all required data
#  measurements <- read_cm(cm, replicates = replicates)
#  metadata     <- read_metadata(screens, plates)
#  exclusions   <- bind_rows(read_dr(dr), read_dr(aux))
#  strains      <- db %>% tbl('strains') %>% select(strain_id, strain_name) %>% collect
#  if (custom_strain_collection) {
#    collection <- read.csv(custom_strain_collection, stringsAsFactors = F)
#  } else {
#    collection <- db %>% tbl('strain_collections') %>% collect
#  }
#  
#  # Annotate measurements with node/edge IDs, incubation time, and exlusion data
#  raw_colony_sizes <-
#    measurements %>%
#    left_join(metadata) %>%
#    left_join(exclusions) %>%
#    left_join(collection) %>%
#    left_join(strains) %>%
#    select(
#      # Identification
#      screen_id, strain_id, strain_name, query_id, query_name,
#      plate, row, column, replicate,
#      # Measurements
#      size, size_dr, circ,
#      # Incubation time
#      timepoint, incubation, incubation_start, incubation_end,
#      # Exclusions and controls
#      excluded_query, excluded_control, plate_control
#    )
#  
#  # Write data to disk
#  name           <- paste('db', date, sep = '/')
#  name_raw_sizes <- paste0(name, '-raw-colony-sizes.csv')
#  name_screens   <- paste0(name, '-screens.csv')
#  write.csv(raw_colony_sizes, name_raw_sizes, row.names = FALSE)
#  file.copy(screens, name_screens, overwrite = TRUE)
#  if (custom_strain_collection) {
#    name_collection <- paste0('db/', basename(custom_strain_collection))
#    file.copy(custom_strain_collection, name_collection, overwrite = TRUE)
#  }
#  
#  # Record session information
#  devtools::session_info()

