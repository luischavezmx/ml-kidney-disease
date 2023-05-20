#Luis
install.packages("genalg")
library(genalg)

costs <- c(0,2.4,2,0,1.4,1.4,0,1.8,2,2.4,1.4,0,2.2,0,1.4,1.4,1.4,2,2.4,1.4,1.4,1.4,1.4,2)
benefits <- c(5.7,42,0.32,1.54,0.0162,32.64,14.04,17.46,0.052,22.88,209.93,24.24,102.72,2.46,11.8,2.43,0.033,18.97,0.129,108.27,44.1,67.55,3.54,5.24)
features <- c("Age","Albumin","Anemia","Appetite","Bacteria","Blood Glucose Random","Blood Pressure","Blood Urea","Coronary disease","Diabetes Mellitus","Hemoglobin ","Hypertension","Packed Cell Volume","Pedal Edema","Potassium","Pus Cell","Pus cell clumps","Red blood cell count","Red blood Cells","Serum Creatinine","Sodium","Specific Gravity","Sugar","White blood cell")

evalFunction <- function(param=c()){
  
  totalBenefits <- param %*% benefits
  totalCosts <- param %*% costs
  
  if(totalCosts > 20){
    return (Inf)
  }
  
  return(1/totalBenefits)
}

monitor <- function(obj) {
  minEval = min(obj$evaluations);
  plot(obj, type="hist");
}

ga.one <- rbga.bin(size=24,iters=5000,evalFun=evalFunction,verbose=F,monitorFunc=monitor)
summary(ga.one,echo=TRUE)
plot(ga.one)

mask <-as.logical(c(1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0))
features[mask]
sum(costs[mask])
sum(benefits[mask])
