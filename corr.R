corr <- function(directory, threshold = 0) {
        files_list <- list.files(directory, full.names=TRUE)
        data_list <- lapply(files_list, read.csv)
 
        accept <- complete(directory)$nobs > threshold
        
        corrCount <- function(x) {
                cor(x[complete.cases(x), c("sulfate", "nitrate")])[1, 2]
        }
        
        sapply(data_list[accept], corrCount)
}