# Diagnóstico de conteo electoral en R - versión 7

Esta versión corrige una inconsistencia de interpretación:

- El umbral nacional responde: ¿qué porcentaje necesita el segundo candidato usando solo el agregado nacional?
- El umbral territorial responde: ¿qué porcentaje necesita el segundo candidato usando los votos actuales y pendientes estimados por distrito?

No deben compararse directamente el voto pendiente territorial esperado con el umbral nacional. La comparación correcta para la proyección distrital es:

```text
voto pendiente esperado territorial vs. umbral territorial
```

Por eso la tabla "Proyección territorial al 100%" ahora incluye:

- K actual territorial
- R actual territorial
- K final estimado al 100%
- R final estimado al 100%
- Ganador final estimado
- K esperado entre votos pendientes
- R esperado entre votos pendientes
- K necesita en pendientes territoriales
- R necesita en pendientes territoriales

## Sobre el total nacional de votos válidos

El campo de total nacional de votos válidos ahora queda vacío por defecto. Es solo una validación opcional. Si no quieres usarlo, déjalo en blanco.

Si tienes un total esperado aproximado, por ejemplo 18.000.000 o 18.200.000, puedes ingresarlo para revisar si la suma distrital estimada es razonable.

## Configuración recomendada para tu caso

- Regla general para la columna de votos: `Votos contados hasta ahora`
- Excepciones: `25`
- Total nacional de votos válidos: dejar vacío o ingresar el esperado correcto.

## Ejecutar

```r
install.packages(c(
  "shiny", "bslib", "readxl", "dplyr", "ggplot2",
  "DT", "rmarkdown", "knitr", "scales"
))
```

Luego abre `app.R` en RStudio y aprieta **Run App**.
