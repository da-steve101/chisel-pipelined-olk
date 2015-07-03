
# load our kernlab and caret packages
library(kernlab, lib.loc="kernlabInst/")
library(caret, lib.loc="caretInst/")
library(hmeasure)

library(doMC)
registerDoMC(cores = 8)

formatDataset <- function(rawDataset, isTwoClass) {
    isTesting <- rawDataset$V2
    levels(isTesting) <- c("Training", "Testing")
    rawDataset <- rawDataset[-(1:2)]
    names(rawDataset) <- c("Class", names(rawDataset[-1]))
    rawDataset <- rawDataset[c(names(rawDataset[-1]), "Class")]
    if ( isTwoClass ) {
        rawDataset$Class <- factor(rawDataset$Class)
        print(paste0("Levels changed from: (", levels(rawDataset$Class)[1], ",",
              levels(rawDataset$Class)[2], ") to (A,B)"))
        levels(rawDataset$Class) <- c("A", "B")
        if ( sum( rawDataset$Class == "A") > sum( rawDataset$Class == "B"))
            rawDataset$Class <- relevel(rawDataset$Class, "B")   
    }

  return(split(rawDataset, isTesting))
}

printTwoClassResults <- function(dataset, training, testing, normaTypes) {
    metrics = c("AUC", "H")
    sigmas <- sigest(as.matrix(training[,-which(names(training) %in% "Class")]),
                     na.action=na.omit, scaled = TRUE)
    normaGrid <- expand.grid(sigma = mean(as.vector(sigmas[-2]))*c(1,5,10,50,100),
                             nu = c(1,4,8)/10, lambda = (1 - 2^(-4:-1)), eta = 10^(-3:-1),
                             buffersize = c(20, 40, 60, 100, 150, 200))
    normaCtrl <- trainControl(method = "repeatedcv", classProbs=TRUE,
                              summaryFunction=twoClassSummaryH)
    normaGrid$kernel <- "rbfdot"
    for ( normaType in normaTypes) {
        normaGrid$normaType <- normaType
        for ( metric in metrics ) {
            normaFit <- train(Class~., data=training, trControl = normaCtrl,
                               method = "norma", tuneGrid = normaGrid, metric=metric)
            normaPrediction <- predict(normaFit, testing, type = "prob")

            normaResults <- suppressWarnings(HMeasure(testing$Class, normaPrediction$A))
            print(normaFit$bestTune)
            print(paste0("NORMA ", type(normaFit$finalModel), " ", dataset, " ", metric, " : ",
                  eval(parse(text=paste0("normaResults$metrics$", metric)))[1]))
        }
    }
}

printRegressionResults <- function(dataset, training, testing) {
    metrics = c("MAE", "MSE")
    sigmas <- sigest(as.matrix(training[,-which(names(training) %in% "Class")]),
                     na.action=na.omit, scaled = TRUE)
    normaGrid <- expand.grid(sigma = mean(as.vector(sigmas[-2]))*c(1,5,10,50,100),
                             nu = c(1,4,8)/10, lambda = (1 - 2^(-4:-1)), eta = 10^(-3:-1),
                             buffersize = c(20, 40, 60, 100, 150, 200))
    normaCtrl <- trainControl(method = "repeatedcv",
                              summaryFunction=regressionSummary)
    normaGrid$kernel <- "rbfdot"
    for ( normaType in c("regression")) {
        normaGrid$normaType <- normaType
        for ( metric in metrics ) {
            normaFit <- train(Class~., data=training, trControl = normaCtrl, maximize = FALSE,
                               method = "norma", tuneGrid = normaGrid, metric=metric)
            data <- data.frame(obs = testing$Class )
            data$pred <- predict(normaFit, testing)
            regSum <- caret::regressionSummary(data)
	    normaResults <- data.frame(MSE = regSum[1], MAE = regSum[2])
            print(normaFit$bestTune)
            print(paste0("NORMA ", type(normaFit$finalModel), " ", dataset, " ", metric, " : ",
                  eval(parse(text=paste0("normaResults$", metric)))[1]))
        }
    }
}

# CLASSIFICATION and NOVELTY

artificialTwoClass <- read.csv("artificialTwoClass.csv", header=FALSE)
formatedData <- formatDataset(artificialTwoClass, TRUE)
dataset = "artificial"
printTwoClassResults(dataset, formatedData$Training, formatedData$Testing, "novelty")

satellite <- read.csv("Satellite.csv", header=FALSE)
formatedData <- formatDataset(satellite, TRUE)
dataset = "satellite"
printTwoClassResults(dataset, formatedData$Training, formatedData$Testing, "novelty")

# REGRESSION

artificialTwoClass <- read.csv("artificialTwoClass.csv", header=FALSE)
formatedData <- formatDataset(artificialTwoClass, FALSE)
dataset = "artificial regression"
printRegressionResults(dataset, formatedData$Training, formatedData$Testing)

ccpp <- read.csv("ccpp.csv", header=FALSE)
formatedData <- formatDataset(ccpp, FALSE)
dataset = "ccpp"
printRegressionResults(dataset, formatedData$Training, formatedData$Testing)