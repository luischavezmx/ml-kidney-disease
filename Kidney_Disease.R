library(neuralnet)

#setwd("/Users/luichave/iCloud/Luis/Paper")
setwd("C:/Users/enriq/iCloudDrive/Education/Projects/Kidney Disease Paper")
#Preprocessed data
kidney.data <- read.csv("scaled_cleaned_kidney_for_R.csv")

#Descriptive methods
class(kidney.data)
summary(kidney.data)
str(kidney.data)
head(kidney.data)
rownames(kidney.data) <- kidney.data$ID

#Get training and test set. Stratified (same Ckd distribution in both sets)
trainingCkd.set <- kidney.data[1:320,]
testCkd.set <-kidney.data[321:400,]

#Train and show NN
formulaCkd <- Ckd~Age+BloodPressure+SpecificGravityUrine+Albumin+Sugar+RedBloodCells+PusCell+PusCellClumps+Bacteria+BloodGlucose+BloodUrea+SerumCreatine+Sodium+Potassium+Hemoglobin+PackedCellVolume+WhiteBloodCellCount+RedBloodCellCount+Hypertension+DiabetesMellitus+CoronaryArterieDisease+Appetite+PedalEdema+Anemia
NNCkd <- neuralnet(formula=formulaCkd, data = trainingCkd.set, hidden=10, threshold=.01, stepmax=1000000, act.fct = 'logistic', linear.output = FALSE)
plot(NNCkd,show.weights=FALSE,intercept = FALSE, dimension = 25,fontsize = 10,arrow.length = .2,radius = .25)

#Get predictions of test set
predictions_proba <- compute(NNCkd,testCkd.set)$net.result
predictions <- round(compute(NNCkd,testCkd.set)$net.result)
actual <- testCkd.set$Ckd

#Evaluate model
accurracy <- sum(actual == predictions) / length(predictions)

#Train and show NN after GA
formulaCkdAfterGA <- Ckd~Age+Albumin+Appetite+BloodGlucose+BloodPressure+BloodUrea+DiabetesMellitus+Hemoglobin+Hypertension+PackedCellVolume+PedalEdema+Potassium+RedBloodCellCount+SerumCreatine+Sodium
NNCkdAfterGA <- neuralnet(formula=formulaCkdAfterGA, data = trainingCkd.set, hidden=10, threshold=.01, stepmax=1000000, act.fct = 'logistic', linear.output = FALSE)
plot(NNCkdAfterGA,show.weights=FALSE,intercept = FALSE, dimension = 25,fontsize = 10,arrow.length = .2,radius = .25)

#Get predictions of test set
predictionsAfterGA_proba <- compute(NNCkdAfterGA,testCkd.set)$net.result
predictionsAfterGA <- round(compute(NNCkdAfterGA,testCkd.set)$net.result)
actualAfterGA <- testCkd.set$Ckd

#Evaluate model
accurracyAfterGA <- sum(actualAfterGA == predictionsAfterGA) / length(predictionsAfterGA)


write.csv(predictions_proba, 'NN 2nd iteration.csv')
write.csv(predictionsAfterGA_proba, 'NN 3rd iteration.csv')