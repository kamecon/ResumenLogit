# ResumenLogit

El siguiente código emplea el paquete `glmulti` para seleccionar los 3 mejores modelos logit a partir de una fuente de datos y genera un documento en formato pdf que resume los 3 modelos de la siguiente forma:

- Una tabla con los coeficientes y el p-valor de los 3 mejores modelos obtenidos a través de una búsqueda exhaustiva empleando el paquete `glmulti`
- Una tabla con los efectos marginales calculados con el paquete `margins`
- Una tabla de evaluación donde se presentan el $R^2$, especificidad, sensibilidad, precisión y AUC. Los datos se calculan en la función `MatConf`, para el cálculo del AUC se utiliza el paquete `ROCR` 
- Dos gráficos del paquete `glmulti` que presentan la importancia relativa de las variables y la selección del número de mejores modelos usando el criterior AIC
- Los gráficos de las probabilidades relativas de los 3 mejores modelos utilizando el comando `plot_model`del paquete `sjPlot`
- Los gráficos de los efectos marginales de los 3 mejores modelos

Se incluye un script que contiene dos funciones:

- `MatConf` que genera una matriz de confunsión, el $R^2$, especificidad, sensibilidad, precisión y AUC
- `selecModelo` que realiza el proceso de selección de modelos y la elaboración del informe arriba descrito

Hay un script con un ejemplo `Ejemplo.R` empleando unos datos de ejemplo del libro de [Wooldridge](https://www.cengage.com/c/introductory-econometrics-a-modern-approach-7e-wooldridge/). Se accede a los datos usando la librería `wooldridge` (para más información consultar el [manual](https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf)

Asimismo hay un archivo .Rmd para crear el reporte en pdf