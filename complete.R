complete <- function(directory, id = 1:332) {
        files_list <- list.files(directory, full.names=TRUE)
        data_list <- lapply(files_list[id], read.csv)
        
        nobCount <- function(x) {
                sum(complete.cases(x))
        }
        
        count <- sapply(data_list, nobCount)
        data.frame(id, nobs = count)
}
