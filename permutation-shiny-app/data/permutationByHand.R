# Permutation entropy method "by hand"
#require(fgui)

#inpVec   - input numerical series 
# h       - correction parametr
permutationByHand <- function(inpVec, h) {
  grNum <- 3                                  # but can take from input, if will need
  serSize <- length(inpVec) - grNum + 1
  inpMatrix <- matrix(0, serSize, 3)
  for (i in 1:serSize) {
    inpMatrix[i,] <- c(inpVec[i], inpVec[i+1], inpVec[i+2])
  }
  
  patternsList = list(exUp = 0, exDown = 0, monUp = 0, monDown = 0, con = 0)
  
  absEdge <- abs(h * mean(inpVec))
  for (i in 1:serSize) {
    if ((inpMatrix[i,2] - inpMatrix[i,1] > h) && (inpMatrix[i,2] - inpMatrix[i,3] > h)) {
      patternsList$exUp <- patternsList$exUp + 1
    } else if ((inpMatrix[i,1] - inpMatrix[i,2] > h) && (inpMatrix[i,3] - inpMatrix[i,2] > h)) {
      patternsList$exDown <- patternsList$exDown + 1
    } else if ((inpMatrix[i,2] - inpMatrix[i,1] > h) 
               || (inpMatrix[i,3] - inpMatrix[i,2] > h) || (inpMatrix[i,3] - inpMatrix[i,1] > h)) {
      patternsList$monUp <- patternsList$monUp + 1
    } else if ((inpMatrix[i,1] - inpMatrix[i,2] > h) 
               || (inpMatrix[i,2] - inpMatrix[i,3] > h) || (inpMatrix[i,1] - inpMatrix[i,3] > h)) {
      patternsList$monDown <- patternsList$monDown + 1
    } else {
      patternsList$con <- patternsList$con + 1
    }
  }
  
  genSum <- 0
  for (i in names(patternsList)) {
    patternsList[[i]] <- patternsList[[i]] / serSize
    if (patternsList[[i]] != 0) {
      genSum <- genSum - (patternsList[[i]] * log2(patternsList[[i]])) 
    }
  }
  
  return (genSum)
}