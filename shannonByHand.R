# Shannon entropy method "by hand"
shannonByHand <- function(inpVec) {
  #y <- readline("Please, enter number series from keyboard: ")
  newInpVec <- c()
  for (i in 1:length(inpVec)) {
    Z <- (inpVec[i] - min(inpVec)) / (max(inpVec) - min(inpVec))
    newInpVec <- append(newInpVec, Z)
  }
  freqVec <- table(newInpVec) / length(newInpVec)
  formedVec <- as.data.frame(freqVec)[,2]
  H = -sum(formedVec * log2(formedVec))
  print(H)
}

res <- gui(shannonByHand, argEdit = c(600,300), title = "Shannon Entropy")