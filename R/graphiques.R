#' Trace les données et la  droite de separation  du svm
#'
#' @param donnees  un tableau avec les colonnes x1 ,x2 et y(+1 ou -1)
#'@param  modele  le resultat de svm_sousgradient()
#'@param  xlab le nom de  laxe horizontal
#'@param  ylab le nom  de laxe vertical
#'
#'@return un graphique ggplot2
#' @export
tracer_svm <- function(donnees , modele , xlab ="variable 1", ylab = "variable 2"){
  w <- modele$w
  b<- modele$b
  pente <- -w[1] /w[2]

  ggplot2:: ggplot(donnees , ggplot2:: aes(x= x1 ,y = x2 , color= factor(y))) +
    ggplot2:: geom_point(size = 3) +
    ggplot2:: geom_abline(intercept =  -b / w[2] , slope = pente , linewidth = 1,  color ="black")+
    ggplot2::geom_abline(intercept = (1- b) / w[2] , slope = pente , linetype ="dashed", color="grey40") +
    ggplot2:: geom_abline(intercept = (-1- b) /w[2], slope=pente ,linetype ="dashed", color="grey40")+
    ggplot2::scale_color_manual(values = c("1"="steelblue","-1" = "tomato"),labels = c("1" = "Validé", "-1"= "Non validé")) +
    ggplot2:: labs(title = "SVM entraîné par sous-gradient",x = xlab , y = ylab , color = "Resultat") +
    ggplot2:: theme_minimal()
}

#' Trace la convergence de l'algorithme
#'
#' @param  modeles  le resultat de svm_sousgradient() ou une liste nommé
#' de resultat à comparer (par exemple list("pas decroissant"= res1, "pas constant"= res2))
#'
#' @return un graphique ggplot2
#' @export
tracer_convergence <- function(modeles){
  if(!is.null(modeles$historique)){
    modeles <- list("modele"=modeles)
  }
  df <- do.call(rbind ,lapply(names(modeles), function(nom){
    h <- modeles[[nom]]$historique
    data.frame(iteration =seq_along(h), f_best = h , pas = nom)
  }))
  ggplot2:: ggplot(df , ggplot2:: aes(x = iteration , y= f_best, color = pas))+
    ggplot2:: geom_line(linewidth = 1)+
    ggplot2::labs(title = "Convergence du sous-gradient", x= "iteration", y=" meilleur valeur de f", color= "type de pas")+
    ggplot2:: theme_minimal()
}

