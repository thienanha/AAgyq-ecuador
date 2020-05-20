library(mpath)
library(MASS)
library(MuMIn)
library(boot)
library(parallel)

pupaecsv <- read.csv("~/R_Projects/AAgyq-ecuador/server/GYQAaPupaeHHIMP.csv", header = T)


# Run all combinations 
nb_fit <- MASS::glm.nb(formula = PupaeIndex ~ NumChildren + NumAdults + InterruptFreq +
                         Illiteracy + Unemployment + Overcrowding +
                         TrashCollectPerWk + LargeSolidColl + SewerConn + 
                         AbateLWs + #BiolarvLWs + CanopyUse + WaterVol + 
                         week0 + week1 + week2, data = pupaecsv, na.action = "na.pass")

#nb_fit <- MASS::glm.nb(formula = PupaeIndex ~ NumChildren + NumAdults + InterruptFreq +Illiteracy + Unemployment + Overcrowding +TrashCollectPerWk + LargeSolidColl + SewerConn + AbateLWs + week0 + week1 + week2, data = pupaecsv, na.action = "na.pass")

# Set up the cluster
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 4), type = clusterType))
clusterExport(clust, "pupaecsv")
combinations <- MuMIn::pdredge(nb_fit, cluster = clust)

stopCluster(clust)
save(combinations, file = "combinations_all.Rda")

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
vec2 <- matrix(NA, nrow = 100, ncol = 2)
set.seed(27)
for(i in 1:100){
  vec2[i,] <- (boot::cv.glm(data = pupaecsv, glmfit = cv_one, K = 10)$delta) # a function that I want to run 100 times
}
#mean(vec, trim = 0)
output_list <- list(formula_of_interest = formula_of_interest, cv_one = cv_one, vec2 = vec2)
save(output_list, file = "output_list.Rda")

### Longer Formula

# Run all combinations 
nb_fit2 <- MASS::glm.nb(formula = PupaeIndex ~ NumChildren + NumAdults + InterruptFreq +
                         Illiteracy + Unemployment + Overcrowding +
                         TrashCollectPerWk + LargeSolidColl + SewerConn + 
                         AbateLWs + BiolarvLWs + CanopyUse + WaterVol + 
                         week0 + week1 + week2, data = pupaecsv, na.action = "na.pass")


#nb_fit <- MASS::glm.nb(formula = PupaeIndex ~ NumChildren + NumAdults + InterruptFreq +Illiteracy + Unemployment + Overcrowding +TrashCollectPerWk + LargeSolidColl + SewerConn + AbateLWs + week0 + week1 + week2, data = pupaecsv, na.action = "na.pass")

# Set up the cluster
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 4), type = clusterType))
clusterExport(clust, "pupaecsv")
combinations <- MuMIn::pdredge(nb_fit2, cluster = clust)

stopCluster(clust)
save(combinations2, file = "combinations_all2.Rda")

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
vec2 <- matrix(NA, nrow = 100, ncol = 2)
set.seed(27)
for(i in 1:100){
  vec2[i,] <- (boot::cv.glm(data = pupaecsv, glmfit = cv_one, K = 10)$delta) # a function that I want to run 100 times
}
#mean(vec, trim = 0)
output_list2 <- list(formula_of_interest = formula_of_interest, cv_one = cv_one, vec2 = vec2)
save(output_list2, file = "output_list2.Rda")



