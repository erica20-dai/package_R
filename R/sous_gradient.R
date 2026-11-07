#'Algorithme du sous-gradient
#'
#'minimise une fonction convexe meme non derivable.
#'
#'@param f  la fonction à minimiser.
#'@param sous_grad la fonction qui donne un sous-gradient de f en x(donne la pente et le point de depart).
#'@param x0  le point de depart
#'@param n_iter  le nombre d'itération (100 par  defaut).
#'@param pas   le type de pas : "decroissant" (a/k), ou "croissant"(a).
#'@param a  la valeur de base du pas(1 par défaut)

#'@return une liste avec x_best ,f_best et l'historique de f_best.
#'@export
sous_gradient<- function(f, sous_grad , x0 ,n_iter=100 ,pas="decroissant" , a=1){
  x <- x0
  x_best <- x0
  f_best <- f(x0)
  historique <- numeric(n_iter)
  for(k in 1:n_iter){
    g <- sous_grad(x)
    alpha<-if(pas=="constant") a else a/k
    x <- x - alpha * g
    if(f(x) < f_best){
      x_best <- x
      f_best <- f(x)
    }
    historique[k] <- f_best
  }
  list(x_best= x_best, f_best= f_best, historique= historique)
}
