library(screenmill)

# Current pipeline
where <- 'test-scans'
annotate_plates(where)
annotate_plates(where, overwrite = T)
calibrate_crop(where, overwrite = T)
crop(where, overwrite = T)
calibrate_grid(where, overwrite = T)
