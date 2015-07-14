
args <- commandArgs(TRUE)

if ( length(args) < 1 ) {
    args <- c("--help")
}

if ( "--help" %in% args ) {
    cat("
        summarizeResults.R --input=inputFile --output=outputFile

        --input=inputFile => file that was sent to chisel NORMA
        --output=outputFile => file that was created by chisel NORMA
        --print => print the output
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
    isTesting <- factor(rawDataset$V2)
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
resultH <- NULL
if ( length(unique(formatedOut$Testing$Class)) <= 2 ) {
    resultH <- suppressWarnings(HMeasure(formatedOut$Testing$Class, formatedOut$Testing$Pred))
} else {
    resultH <- data.frame( metrics = 0 )
    resultH$metrics <- data.frame(AUC = 0, H = 0)
}

diff <- abs(formatedOut$Testing$Class - formatedOut$Testing$Pred)
mae <- sum(diff)/length(diff)
mse <- sum(diff^2)/length(diff)

if ( ! is.null(argsL$print) ) {
    print(paste0("Results AUC : ", resultH$metrics$AUC))
    print(paste0("Results H : ", resultH$metrics$H))
    print(paste0("Results MAE : ", mae))
    print(paste0("Results MSE : ", mse))
}

resultsOut <- data.frame( AUC = resultH$metrics$AUC, H = resultH$metrics$H, MSE = mse, MAE = mae)
