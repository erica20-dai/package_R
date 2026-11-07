#' Génére deux groupes  de points  pour le SVM(machine à vecteur de support)
#'
#' @param n le nombre  total de points (pair , 40 par defaut)
#' @param seed  un nombre fixé pour obtenir toujours  les mêmes  données
#'
#' @return un tableau avec x1, x2, et la classe y(+1 ou-1)
#' @export
 generer_donees <- function(n = 40 , seed = 1){
  set.seed(seed)
  m <- n/2
  x1<- c(rnorm(m, mean = 2 , sd = 0.7) , rnorm(m , mean = 5, sd = 0.7))
  x2<- c(rnorm(m, mean = 5 , sd = 0.7) , rnorm(m , mean = 2, sd = 0.7))
  y <- c(rep(1,m), rep(-1,m))
  data.frame(x1 = x1 , x2 = x2 , y = y)
}
