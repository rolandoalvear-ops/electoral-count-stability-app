# electoral-count-stability-app
# Electoral Count Stability App

Aplicación en R/Shiny para analizar conteos electorales parciales y evaluar si un resultado está matemáticamente cerrado, territorialmente proyectado o estadísticamente estabilizado.

## Objetivo

El proyecto busca responder una pregunta frecuente en conteos electorales en vivo: cuándo una ventaja observada en resultados parciales puede considerarse irreversible, estable o todavía incierta.

Para ello, la aplicación combina tres enfoques:

* irreversibilidad matemática;
* proyección territorial estratificada;
* simulación Monte Carlo sobre votos pendientes.

## Datos requeridos

La aplicación utiliza un archivo Excel con dos hojas:

1. Una hoja nacional, con el porcentaje de votos contados y el porcentaje actual de cada candidatura.
2. Una hoja distrital, con el avance de conteo por distrito, votos reportados hasta el momento y porcentaje de cada candidatura en cada distrito.

La aplicación permite trabajar con datos donde no se conoce el total final de votos por distrito. En ese caso, estima el total esperado usando:

```r
total_estimado_distrito = votos_contados_actuales / porcentaje_contado_distrito
```

## Cálculos principales

### Irreversibilidad matemática

Un resultado se considera matemáticamente irreversible cuando la ventaja acumulada del candidato líder supera la totalidad de votos pendientes. Para dos candidaturas, la condición es:

```r
d * p > 1 - p
```

donde:

* `d` es la diferencia entre el líder y el segundo candidato;
* `p` es la proporción de votos ya contados.

Este criterio es estricto: pregunta si el resultado podría revertirse incluso bajo el escenario extremo de que todos los votos pendientes favorezcan al segundo candidato.

### Proyección territorial

La aplicación estima el resultado final al 100% considerando el avance de conteo y el comportamiento observado en cada distrito.

Esto permite corregir el problema de que algunos territorios cuentan más rápido que otros y que el promedio nacional puede ocultar diferencias territoriales relevantes.

### Simulación Monte Carlo

El modelo simula miles de escenarios posibles para los votos pendientes, considerando la distribución observada por distrito. El resultado entrega una probabilidad simulada de victoria para cada candidatura.

Por defecto, la aplicación utiliza 10.000 simulaciones, aunque este número puede modificarse desde la interfaz.

## Archivos principales

* `app.R`: interfaz interactiva de la aplicación en Shiny.
* `funciones_eleccion.R`: funciones de limpieza, cálculo, proyección y simulación.
* `reporte_eleccion.Rmd`: reporte reproducible en HTML.
* `README.md`: explicación general del proyecto.

## Requisitos

Para ejecutar el proyecto se requiere tener instalado R y RStudio.

También se deben instalar los siguientes paquetes de R:

```r
install.packages(c(
  "shiny", "bslib", "readxl", "dplyr", "ggplot2",
  "DT", "rmarkdown", "knitr", "scales"
))
```

## Cómo ejecutar la aplicación

Abrir el archivo `app.R` en RStudio y presionar **Run App**.

También puede ejecutarse desde la consola con:

```r
shiny::runApp("app.R")
```

Luego se debe cargar un archivo Excel con la estructura requerida y presionar el botón de cálculo dentro de la aplicación.

## Interpretación de resultados

La aplicación distingue entre tres niveles de certeza:

1. **Matemáticamente irreversible**: el resultado ya no puede revertirse ni bajo el escenario extremo de que todos los votos pendientes favorezcan al segundo candidato.
2. **Proyectado territorialmente**: el resultado final estimado surge de ponderar los votos pendientes por distrito.
3. **Estadísticamente estabilizado**: las simulaciones muestran una probabilidad muy alta de victoria para una candidatura bajo los supuestos del modelo.

## Limitaciones

La aplicación no reemplaza el conteo oficial. Sus resultados dependen de la calidad de los datos cargados, del avance real por distrito y del supuesto de que los votos pendientes dentro de cada distrito se comportan de manera similar al patrón observado hasta ese momento.

Cuando un distrito tiene un porcentaje muy bajo de conteo, la proyección puede ser sensible a pequeños cambios en la distribución inicial de votos.

El modelo debe interpretarse como una herramienta exploratoria y reproducible para analizar estabilidad electoral, no como una certificación oficial de resultados.

## Archivo de ejemplo

El repositorio incluye un archivo Excel ficticio en la carpeta `data/`, pensado solo para probar el funcionamiento de la aplicación.

Este archivo no corresponde a resultados electorales reales. Su objetivo es mostrar la estructura mínima que deben tener los datos para que la app pueda leerlos correctamente.

## Estructura esperada del Excel

El archivo Excel debe contener dos hojas:

### 1. Hoja nacional

Esta hoja resume el avance nacional del conteo.

Debe incluir las siguientes columnas:

| columna      | descripción                                                     |
| ------------ | --------------------------------------------------------------- |
| `conteo_pct` | Porcentaje nacional de votos válidos contados hasta el momento  |
| `k_pct`      | Porcentaje de votación del candidato K entre los votos contados |
| `r_pct`      | Porcentaje de votación del candidato R entre los votos contados |

Ejemplo:

| conteo_pct | k_pct | r_pct |
| ---------: | ----: | ----: |
|         75 |  52.3 |  47.7 |
|         85 |  51.1 |  48.9 |
|         93 |  50.1 |  49.9 |

Los porcentajes pueden escribirse como `93` o como `0.93`. La aplicación normaliza automáticamente estos valores.

### 2. Hoja distritos

Esta hoja contiene la información desagregada por distrito.

Debe incluir las siguientes columnas:

| columna         | descripción                                                                                                                                         |
| --------------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| `distrito`      | Nombre o número del distrito                                                                                                                        |
| `votos_totales` | Votos reportados en el distrito. Según la configuración de la app, pueden interpretarse como votos contados hasta ahora o como total final conocido |
| `conteo_pct`    | Porcentaje contado del distrito                                                                                                                     |
| `k_pct`         | Porcentaje de K en el distrito                                                                                                                      |
| `r_pct`         | Porcentaje de R en el distrito                                                                                                                      |

Ejemplo:

| distrito | votos_totales | conteo_pct | k_pct | r_pct |
| -------- | ------------: | ---------: | ----: | ----: |
| 1        |        450000 |         95 |  51.2 |  48.8 |
| 2        |        380000 |         88 |  47.5 |  52.5 |
| 3        |          1600 |          1 |  66.0 |  34.0 |

## Nota sobre la columna `votos_totales`

La columna `votos_totales` puede representar dos cosas distintas, según la información disponible:

1. **Votos contados hasta ahora**, cuando no se conoce el total final del distrito.
2. **Total final conocido o estimado**, cuando sí se conoce el tamaño total esperado del distrito.

La aplicación permite seleccionar esta interpretación desde la interfaz.

Cuando se selecciona “Votos contados hasta ahora”, el total final estimado se calcula como:

```r
total_estimado_distrito = votos_contados_actuales / conteo_pct_distrito
```

Cuando se selecciona “Total final conocido o estimado”, la aplicación calcula los votos contados como:

```r
votos_contados = total_final_distrito * conteo_pct_distrito
```

## Recomendación

Para evitar errores de interpretación, se recomienda que el archivo Excel use exactamente los nombres de columnas indicados:

```text
conteo_pct
k_pct
r_pct
distrito
votos_totales
```

También se recomienda revisar la tabla “Detalle distrital” dentro de la app para confirmar que los votos contados, votos pendientes y totales estimados fueron interpretados correctamente.
