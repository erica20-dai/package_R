#' Prédit la classe de nouveaux individus avec un svm
#'
#'  @param modele  resultat de svm_sousgradient()
#'  @param x_nouveau un tableau ou une matrice avec les mêmes colonnes
#'  que celle utilisées pour entrainer le modele
#'
#'  @return un vecteur  de classes predites(+1 ou-1)
#'  @export
predire_svm <-function( modele ,x_nouveau){
  x_nouveau <- as.matrix(x_nouveau)
  score <- as.vector(x_nouveau)
  score <- as.vector(x_nouveau %*% modele$w +modele$b)
  ifelse(score >= 0 , 1 ,-1)
}
