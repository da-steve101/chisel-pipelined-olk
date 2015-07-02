
args <- commandArgs(TRUE)

if ( length(args) < 1 ) {
    args <- c("--help")
}

if ( "--help" %in% args ) {
    cat("
        summarizeResults.R --input=inputFile --output=outputFile

        --input=inputFile => file that was sent to chisel NORMA
        --output=outputFile => file that was created by chisel NORMA
        --help => print this help
        \n\n")
    q(save="no")
}

parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))
names(argsL) <- argsDF$V1

if ( is.null(argsL$input) ) {
    cat("Need input, see --help")
    q(save="no")    
}

if ( is.null(argsL$output) ) {
    cat("Need output, see --help")
    q(save="no")
}

if ( !file.exists(argsL$input) ) {
    cat("Input file does not exist!\n")
    q(save="no")
}

if ( !file.exists(argsL$output) ) {
    cat("Output file does not exist!\n")
    q(save="no")
}

library(hmeasure)

formatInput <- function(rawDataset) {
    isTesting <- rawDataset$V2
    levels(isTesting) <- c("Training", "Testing")
    return(isTesting)
}

formatOutput <- function(rawDataset, isTesting) {
    rawDataset <- rawDataset[-1]
    names(rawDataset) <- c("Class", "Pred")
    return(split(rawDataset, isTesting))
}

inputData <- read.csv(argsL$input, header=FALSE)
isTesting <- formatInput(inputData)
outputData <- read.table(argsL$output, sep=",", skip=4)
formatedOut <- formatOutput(outputData, isTesting)
results <- HMeasure(formatedOut$Testing$Class, formatedOut$Testing$Pred)
print(paste0("Results AUC : ", results$metrics$AUC))
print(paste0("Results H : ", results$metrics$H))

diff <- abs(formatedOut$Testing$Class - formatedOut$Testing$Pred)
mae <- sum(diff)/length(diff)
mse <- sum(diff^2)/length(diff)

print(paste0("Results MAE : ", mae))
print(paste0("Results MSE : ", mse))




