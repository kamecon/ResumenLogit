#Libreria con bases de datos empleadas en el libro m ``Introductory Econometrics: A Modern Approach, 6e'' by Jeffrey M. Wooldridge
library("wooldridge")

#Se carga la base de datos 'mroz' ver descripcion en la pagina 107 del manual de la libreria https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
data('mroz')

#Se ejecuta el ejemplo
source("ResumenLogit.R")
prueba <- selecModelo(d = mroz, y = "inlf", x = c("nwifeinc", "educ", "exper", "age", "kidslt6", "kidsge6"))
