
library(foreach)
library(hmeasure)
library(doMC)
registerDoMC(cores = 8)

# number of cross validate
noCV <- 2

# input data file
inputFilename = "artificialTwoClass.csv"

# parameters to test
bitWidth = 18
fracWidth = 12
log2Table = 4
features = 8
normaType = 1

bufferSizeParams = c(20, 60, 100, 150, 200)

# large number of stages to ensure tree finishes
stages = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)

# generate params
gammaParam = c(0.6071064, 1.214213)
etaParam = c(0.1, 0.05, 0.01)
nuParam = c(0.1, 0.4, 0.7)

# forget param
if ( normaType != 3 ) {
    forgetParam = 1 - etaParam
} else {
    forgetParam = c(0.99, 0.95, 0.9)
}

# params file prefix
paramPrefix = paste0("params_", inputFilename, "_")

# output file prefix
outputPrefix = paste0("output_", inputFilename, "_")

formatInput <- function(rawDataset) {
    isTesting <- rawDataset$V2
    names(rawDataset) <- c("reset", "forceNA", "Class", names(rawDataset[-(1:3)]))
    levels(isTesting) <- c("Training", "Testing")
    return(split(rawDataset, isTesting))
}

formatOutput <- function(rawDataset, isTesting) {
    rawDataset <- rawDataset[-1]
    names(rawDataset) <- c("Class", "Pred")
    isTesting <- relevel(isTesting, "FALSE")
    levels(isTesting) <- c("Training", "Testing")
    return(split(rawDataset, isTesting))
}

stopifnot(length(etaParam) == length(forgetParam))

# generate the noCV fold cv on datasets
dataset <- formatInput(read.csv(inputFilename, header=FALSE))
cvSize <- as.integer(length(dataset$Training$Class)/noCV)
cvSplitFilenames <- paste0(inputFilename, "_cv_", 1:noCV)
remaining <- dataset$Training

stopifnot(noCV == 2)

# Just to two for now
cvSamp <- sample(length(remaining$Class), cvSize)
cv1 <- (1:length(remaining$Class)) %in% cvSamp
cv2 <- !cv1

# write to file
remaining$forceNA <- cv1
write.table(remaining, file = paste0(cvSplitFilenames[1], ".csv"), row.names=FALSE, col.names=FALSE, sep=",")
remaining$forceNA <- cv2
write.table(remaining, file = paste0(cvSplitFilenames[2], ".csv"), row.names=FALSE, col.names=FALSE, sep=",")

cvTestingVec <- as.matrix(c(cv1, cv2))
dim(cvTestingVec) <- c(length(remaining$Class), noCV)
# cvTestingVec -> matrix of isTesting from a particular CV

# for all params
foreach (bufferSize=bufferSizeParams) %dopar% {
foreach (gamma=gammaParam) %dopar% {
    foreach (nu=nuParam) %dopar% {
        # open log file
        logFile = paste0("logs/", paramPrefix, bufferSize, "_", nu, ".log")
        for (i in 1:length(etaParam)) {
            forget = forgetParam[i]
            eta = etaParam[i]

            # generate param file
            paramFilename = paste0(paramPrefix, bufferSize, "_", nu, ".csv")
            topRow <- c(bitWidth, fracWidth, log2Table, bufferSize, features, normaType)
            middleRow <- paste0(stages)
            bottomRow <- c(gamma, forget, eta, nu)
            write.table(t(topRow), paramFilename, row.names=FALSE, col.names=FALSE, sep=",")
            write.table(t(middleRow), paramFilename, row.names=FALSE, col.names=FALSE, sep=",", append=TRUE)
            write.table(t(bottomRow), paramFilename, row.names=FALSE, col.names=FALSE, sep=",", append=TRUE)
            
            sumResults <- data.frame(AUC = 0, H = 0, MSE = 0, MAE = 0)
            for (j in 1:noCV) {
                cvFilenameIn <- paste0(cvSplitFilenames[i], ".csv")
                cvFilenameOut <- paste0(cvSplitFilenames[i], "_",  bufferSize, "_", nu, ".csv")
                system(paste0("rm -f normaRun && make normaRun PARAMSFILE=", paramFilename, " INPUTFILE=", cvFilenameIn, " OUTPUTFILE=", cvFilenameOut))
                normaFileOut <- read.table(cvFilenameOut, header=FALSE, skip=4, sep=",")
                formatedOut <- formatOutput(normaFileOut, factor(cvTestingVec[,i]))
                if ( normaType != 3 ) {
                    resultH <- suppressWarnings(HMeasure(formatedOut$Testing$Class, formatedOut$Testing$Pred))
                    sumResults$AUC <- sumResults$AUC + resultH$metrics$AUC
                    sumResults$H <- sumResults$H + resultH$metrics$H
                } else {
                    diff <- abs(formatedOut$Testing$Class - formatedOut$Testing$Pred)
                    mae <- sum(diff)/length(diff)
                    mse <- sum(diff^2)/length(diff)
                    sumResults$MSE <- sumResults$MSE + mse
                    sumResults$MAE <- sumResults$MAE + mae
                }
            }
            # average the CV
            sumResults <- sumResults/noCV

            # log to file with params
            logLine <- c(sumResults$AUC, sumResults$H, sumResults$MSE, sumResults$MAE)
            logLine <- c(logLine, bitWidth, fracWidth, log2Table, features, normaType)
            logLine <- c(logLine, bufferSize, gamma, eta, nu, forget)

            write.table(t(logLine), file=logFile, row.names=FALSE, col.names=FALSE, sep=",", append=TRUE)
        }
    }
}
}
