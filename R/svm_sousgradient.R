#' SVM lineaire entrainé par sous-gradient
#'
#' Trouver la droite qui separe deux classes(+1 et -1) en minimisant
#' lambda/2 * ||w||^2 + moyenne des pertes hinge, avec sous_gradient()
#'
#' @param x un tableau ou une matrice des variables (une colonne par variable)
#' @param y le vecteur des classe(+1 ou -1)
#' @param lambda  le poid de la regularisation(0.01 par defaut)
#' @param n_iter le nombre d'iteration ( 200 par defaut)
#' @param pas le type de pas ; "decroissant" ou "constant"
#' @param a la valeur de base du pas (1 par defaut)
#'
#' @return  une liste avec w , b, f_best  et  l'historique de convergence
#' @export
  svm_sousgradient <- function(x ,y , lambda = 0.01 , n_iter = 200 , pas = "decroissant" , a = 1){
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    #la fonction a minimiser

    f <- function(theta){
       w <- theta[1:p]
       b <- theta[p+1]
       marge <- as.vector(y * (x %*% w + b))
       lambda / 2 * sum(w^2) + mean(pmax(0 , 1 - marge))
    }

    # un sous-gradient de f
     sous_grad <- function (theta){
       w <- theta[1:p]
       b <- theta[p+1]
       marge <- as.vector(y * (x %*% w + b))
       viole <- marge < 1
       g_w <- lambda * w - as.vector(crossprod(x[viole, , drop = FALSE], y[viole])) / n
       g_b <-  - sum(y[viole]) / n
       c(g_w ,g_b)
     }
     res <- sous_gradient( f , sous_grad , x0 = rep(0 , p+1), n_iter = n_iter , pas = pas , a= a)
     list(w = res$x_best[1:p] , b = res$x_best[p+1] , f_best = res$f_best , historique = res$historique)
  }
