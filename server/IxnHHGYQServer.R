library(mpath)
library(MASS)
library(MuMIn)
library(boot)


#check out pdredge for server

pupaecsv <- read.csv("~/Berkeley/Ecuador/MPH Capstone/AAgyq-ecuador/server/GYQAaPupaeHHIMP.csv", header = T)


# Run all combinations 
nb_fit <- MASS::glm.nb(formula = PupaeIndex ~ NumChildren + NumAdults + InterruptFreq + 
                         Illiteracy + Unemployment + Overcrowding +
                         TrashCollectPerWk + LargeSolidColl + SewerConn + 
                         AbateLWs + BiolarvLWs + CanopyUse + WaterVol + 
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
  vec[i] <- sqrt(boot::cv.glm(data = pupaecsv, glmfit = cv_one, K = 10)$delta) # a function that I want to run 100 times
}
mean(vec, trim = 0)

# Interaction terms
# NumChildren*NumAdults + NumChildren*InterruptFreq + NumChildren*Illiteracy + NumChildren*Unemployment + NumChildren*Overcrowding +
# NumChildren*TrashCollectPerWk + NumChildren*LargeSolidColl + NumChildren*SewerConn + NumChildren*AbateLWs + NumChildren*BiolarvLWs + NumChildren*CanopyUse +
# NumChildren*WaterVol + NumChildren*week0 + NumChildren*week1 + NumChildren*week2 

# NumAdults*NumChildren + NumAdults*InterruptFreq + NumAdults*Illiteracy + NumAdults*Unemployment + NumAdults*Overcrowding +
# NumAdults*TrashCollectPerWk + NumAdults*LargeSolidColl + NumAdults*SewerConn + NumAdults*AbateLWs + NumAdults*BiolarvLWs + NumAdults*CanopyUse +
# NumAdults*WaterVol + NumAdults*week0 + NumAdults*week1 + NumAdults*week2 

# InterruptFreq*NumChildren + NumAdults*InterruptFreq + InterruptFreq*Illiteracy + InterruptFreq*Unemployment + InterruptFreq*Overcrowding +
# InterruptFreq*TrashCollectPerWk + InterruptFreq*LargeSolidColl + InterruptFreq*SewerConn + InterruptFreq*AbateLWs + InterruptFreq*BiolarvLWs + InterruptFreq*CanopyUse +
# InterruptFreq*WaterVol + InterruptFreq*week0 + InterruptFreq*week1 + InterruptFreq*week2 

# Illiteracy*NumChildren + Illiteracy*NumAdults + InterruptFreq*Illiteracy + Illiteracy*Unemployment + Illiteracy*Overcrowding +
# Illiteracy*TrashCollectPerWk + Illiteracy*LargeSolidColl + Illiteracy*SewerConn + Illiteracy*AbateLWs + Illiteracy*BiolarvLWs + Illiteracy*CanopyUse +
# Illiteracy*WaterVol + Illiteracy*week0 + Illiteracy*week1 + Illiteracy*week2 

# Unemployment*NumChildren + Unemployment*NumAdults + Unemployment*Illiteracy + Illiteracy*Unemployment + Unemployment*Overcrowding +
# Unemployment*TrashCollectPerWk + Unemployment*LargeSolidColl + Unemployment*SewerConn + Unemployment*AbateLWs + Unemployment*BiolarvLWs + Unemployment*CanopyUse +
# Unemployment*WaterVol + Unemployment*week0 + Unemployment*week1 + Unemployment*week2 

# Overcrowding*NumChildren + Overcrowding*InterruptFreq + Overcrowding*NumAdults + Illiteracy*Overcrowding + Unemployment*Overcrowding +
# Overcrowding*TrashCollectPerWk + Overcrowding*LargeSolidColl + Overcrowding*SewerConn + Overcrowding*AbateLWs + Overcrowding*BiolarvLWs + Overcrowding*CanopyUse +
# Overcrowding*WaterVol + Overcrowding*week0 + Overcrowding*week1 + Overcrowding*week2 

# TrashCollectPerWk*NumChildren + TrashCollectPerWk*InterruptFreq + TrashCollectPerWk*NumAdults + Illiteracy*TrashCollectPerWk + Unemployment*TrashCollectPerWk +
# Overcrowding*TrashCollectPerWk + TrashCollectPerWk*LargeSolidColl + TrashCollectPerWk*SewerConn + TrashCollectPerWk*AbateLWs + TrashCollectPerWk*BiolarvLWs + TrashCollectPerWk*CanopyUse +
# TrashCollectPerWk*WaterVol + TrashCollectPerWk*week0 + TrashCollectPerWk*week1 + TrashCollectPerWk*week2 

# LargeSolidColl*NumChildren + LargeSolidColl*InterruptFreq + LargeSolidColl*NumAdults + Illiteracy*LargeSolidColl + Unemployment*LargeSolidColl +
# Overcrowding*LargeSolidColl + TrashCollectPerWk*LargeSolidColl + LargeSolidColl*SewerConn + LargeSolidColl*AbateLWs + LargeSolidColl*BiolarvLWs + LargeSolidColl*CanopyUse +
# LargeSolidColl*WaterVol + LargeSolidColl*week0 + LargeSolidColl*week1 + LargeSolidColl*week2 

# SewerConn*NumChildren + SewerConn*InterruptFreq + SewerConn*NumAdults + Illiteracy*SewerConn + Unemployment*SewerConn +
# Overcrowding*SewerConn + TrashCollectPerWk*SewerConn + LargeSolidColl*SewerConn + SewerConn*AbateLWs + SewerConn*BiolarvLWs + SewerConn*CanopyUse +
# SewerConn*WaterVol + SewerConn*week0 + SewerConn*week1 + SewerConn*week2 

# AbateLWs*NumChildren + AbateLWs*InterruptFreq + AbateLWs*NumAdults + Illiteracy*AbateLWs + Unemployment*AbateLWs +
# Overcrowding*AbateLWs + TrashCollectPerWk*AbateLWs + LargeSolidColl*AbateLWs + SewerConn*AbateLWs + AbateLWs*BiolarvLWs + AbateLWs*CanopyUse +
# AbateLWs*WaterVol + AbateLWs*week0 + AbateLWs*week1 + AbateLWs*week2 

# BiolarvLWs*NumChildren + BiolarvLWs*InterruptFreq + BiolarvLWs*NumAdults + Illiteracy*BiolarvLWs + Unemployment*BiolarvLWs +
# Overcrowding*BiolarvLWs + TrashCollectPerWk*BiolarvLWs + LargeSolidColl*BiolarvLWs + SewerConn*BiolarvLWs + AbateLWs*BiolarvLWs + BiolarvLWs*CanopyUse +
# BiolarvLWs*WaterVol + BiolarvLWs*week0 + BiolarvLWs*week1 + BiolarvLWs*week2 

# CanopyUse*NumChildren + CanopyUse*InterruptFreq + CanopyUse*NumAdults + Illiteracy*CanopyUse + Unemployment*CanopyUse +
# Overcrowding*CanopyUse + TrashCollectPerWk*CanopyUse + LargeSolidColl*CanopyUse + SewerConn*CanopyUse + AbateLWs*CanopyUse + BiolarvLWs*CanopyUse +
# CanopyUse*WaterVol + CanopyUse*week0 + CanopyUse*week1 + CanopyUse*week2 
  
# WaterVol*NumChildren + WaterVol*InterruptFreq + WaterVol*NumAdults + Illiteracy*WaterVol + Unemployment*WaterVol +
# Overcrowding*WaterVol + TrashCollectPerWk*WaterVol + LargeSolidColl*WaterVol + SewerConn*WaterVol + AbateLWs*WaterVol + BiolarvLWs*WaterVol +
# CanopyUse*WaterVol + WaterVol*week0 + WaterVol*week1 + WaterVol*week2 

# WaterVol*NumChildren + WaterVol*InterruptFreq + WaterVol*NumAdults + Illiteracy*WaterVol + Unemployment*WaterVol +
# Overcrowding*WaterVol + TrashCollectPerWk*WaterVol + LargeSolidColl*WaterVol + SewerConn*WaterVol + AbateLWs*WaterVol + BiolarvLWs*WaterVol +
# CanopyUse*WaterVol + WaterVol*week0 + WaterVol*week1 + WaterVol*week2 

# week0*NumChildren + week0*InterruptFreq + week0*NumAdults + Illiteracy*week0 + Unemployment*week0 +
# Overcrowding*week0 + TrashCollectPerWk*week0 + LargeSolidColl*week0 + SewerConn*week0 + AbateLWs*week0 + BiolarvLWs*week0 +
# CanopyUse*week0 + WaterVol*week0 + week0*week1 + week0*week2 

# week0*NumChildren + week0*InterruptFreq + week0*NumAdults + Illiteracy*week0 + Unemployment*week0 +
# Overcrowding*week0 + TrashCollectPerWk*week0 + LargeSolidColl*week0 + SewerConn*week0 + AbateLWs*week0 + BiolarvLWs*week0 +
# CanopyUse*week0 + WaterVol*week0 + week0*week1 + week0*week2 

# week1*NumChildren + week1*InterruptFreq + week1*NumAdults + Illiteracy*week1 + Unemployment*week1 +
# Overcrowding*week1 + TrashCollectPerWk*week1 + LargeSolidColl*week1 + SewerConn*week1 + AbateLWs*week1 + BiolarvLWs*week1 +
# CanopyUse*week1 + WaterVol*week1 + week0*week1 + week1*week2 

# week2*NumChildren + week2*InterruptFreq + week2*NumAdults + Illiteracy*week2 + Unemployment*week2 +
# Overcrowding*week2 + TrashCollectPerWk*week2 + LargeSolidColl*week2 + SewerConn*week2 + AbateLWs*week2 + BiolarvLWs*week2 +
# CanopyUse*week2 + WaterVol*week2 + week0*week2 + week1*week2 
