# ResumenLogit

El siguiente c�digo emplea el paquete `glmulti` para seleccionar los 3 mejores modelos logit a partir de una fuente de datos y genera un documento en formato pdf que resume los 3 modelos de la siguiente forma:

- Una tabla con los coeficientes y el p-valor de los 3 mejores modelos obtenidos a trav�s de una b�squeda exhaustiva empleando el paquete `glmulti`
- Una tabla con los efectos marginales calculados con el paquete `margins`
- Una tabla de evaluaci�n donde se presentan el $R^2$, especificidad, sensibilidad, precisi�n y AUC. Los datos se calculan en la funci�n `MatConf`, para el c�lculo del AUC se utiliza el paquete `ROCR` 
- Dos gr�ficos del paquete `glmulti` que presentan la importancia relativa de las variables y la selecci�n del n�mero de mejores modelos usando el criterior AIC
- Los gr�ficos de las probabilidades relativas de los 3 mejores modelos utilizando el comando `plot_model`del paquete `sjPlot`
- Los gr�ficos de los efectos marginales de los 3 mejores modelos

Se incluye un script que contiene dos funciones:

- `MatConf` que genera una matriz de confunsi�n, el $R^2$, especificidad, sensibilidad, precisi�n y AUC
- `selecModelo` que realiza el proceso de selecci�n de modelos y la elaboraci�n del informe arriba descrito

Hay un script con un ejemplo `Ejemplo.R` empleando unos datos de ejemplo del libro de [Wooldridge](https://www.cengage.com/c/introductory-econometrics-a-modern-approach-7e-wooldridge/). Se accede a los datos usando la librer�a `wooldridge` (para m�s informaci�n consultar el [manual](https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf)

Asimismo hay un archivo .Rmd para crear el reporte en pdf