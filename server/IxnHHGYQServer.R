library(mpath)
library(MASS)
library(MuMIn)
library(boot)
#check out pdredge for server

pupaecsv <- read.csv("GYQAaPupaeHHIMP.csv", header = T)

# Run all combinations 

nb_fit <- MASS::glm.nb(formula = PupaeIndex ~ NumChildren + NumAdults + InterruptFreq + 
                         Illiteracy + Unemployment + Overcrowding +
                         TrashCollectPerWk + LargeSolidColl + SewerConn + FumigLWs + 
                         AbateLWs + BiolarvLWs + CanopyUse + ProtectMesh + VolCriadero + WaterVol + 
                         week0 + week1 + week2, data = pupaecsv, na.action = "na.pass")
combinations <- MuMIn::dredge(nb_fit)


vars_of_interest <- colnames(combinations)[!is.na(combinations[1,])]
vars_of_interest <- vars_of_interest[2:(length(vars_of_interest)-5)]
formula_of_interest <- "PupaeIndex ~"
for(i in 1:length(vars_of_interest)){
  if(i != length(vars_of_interest)){
    formula_of_interest <- paste(formula_of_interest, vars_of_interest[i],"+")
  } else{
    formula_of_interest <- paste(formula_of_interest, vars_of_interest[i])
  }
}

cv_one<- mpath::glmregNB(formula_of_interest, data = pupaecsv, weights = NULL, nfolds = 10)
vec <- rep(NA, 100)
set.seed(27)
for(i in 1:100){
  vec[i] <- sqrt(boot::cv.glm(data = pupaemedcsv, glmfit = cv_one, K = 10)$delta) # a function that I want to run 100 times
}
mean(vec, trim = 0)