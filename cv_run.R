load("comb_sub.Rda")
pc <- read.csv(file = "server/GYQAaPupaeHHFINAL.csv", header = TRUE)
cv_out <- vector(mode = "numeric", length = 31L)
for(i in 1:nrow(comb_sub)){
  comb_choice <- comb_sub[i,]
  vars_of_interest <- colnames(comb_choice)[!is.na(comb_choice[1,])]
  vars_of_interest <- vars_of_interest[2:(length(vars_of_interest)-5)]
  formula_of_interest <- "PupaeIndex ~"
  for(j in 1:length(vars_of_interest)){
    if(j != length(vars_of_interest)){
      formula_of_interest <- paste(formula_of_interest, vars_of_interest[j],"+")
    } else{
      formula_of_interest <- paste(formula_of_interest, vars_of_interest[j])
    }
  }
  cv_one <- mpath::glmregNB(formula_of_interest, data = pc, weights = NULL, nfolds = 10)
  vec <- rep(NA, 100)
  set.seed(27)
  for(k in 1:100){
    vec[k] <- sqrt(boot::cv.glm(data = pc, glmfit = cv_one, K = 10)$delta[1]) # a function that I want to run 100 times
  }
  cv_out[i] <- mean(vec)
}
save(cv_out, file = "cv_out.Rda")
  
  