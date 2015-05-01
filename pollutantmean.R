pollutantmean <- function(directory, pollutant, id = 1:332) {
        files_list <- list.files(directory, full.names=TRUE)
        dat <- vector()
        
        data_list <- lapply(files_list[id], read.csv)
        
        for (i in seq_along(data_list)) {
                dat <- c(dat, data_list[[i]][,pollutant])
        }
        round(mean(dat, na.rm=TRUE), 3)
}
