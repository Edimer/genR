#' Función de juguete
#'
#' La función genera dos variables aleatorias X y Y, de distribución
#' normal con desviación estándar igual a 1 y medias 5 y 10,
#' respectivamente. La función utiliza estos datos para graficar
#' el diagrama de dispersión de X vs Y y el histograma de la variable
#' aleatoria X.
#' 
#' Detalle importante de ftoy.
#' 
#' @param n tamaño muestral para X y Y, por defecto, igual a 100. 
#' 
#' @return Relación de X vs Y y el histograma de la variable aleatoria X.
#'
#' @export
ftoy <- function(n = 100) {
  
  #
  num_x = rnorm(n = n, mean = 5, sd = 1)
  num_y = rnorm(n = n, mean = 10, sd = 1)
  
  # 
  datos = data_frame(num_x, num_y)
  
  # 
  g1 <- ggplot(data = datos, aes(x = num_x, y = num_y)) +
    geom_point(color = "dodgerblue4") +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = "Variable X", y = "Variable Y",
         title = "Relación de X vs Y") +
    theme_linedraw()
  
  g2 <- ggplot(data = datos, aes(x = num_x)) +
    geom_histogram(color = "black", fill = "firebrick3", bins = 30) +
    labs(x = "Variable X", y = "Frecuencia",
         title = "Distribución de variable X") +
    theme_linedraw()
  
  grid.arrange(g1, g2, nrow = 1)
}
