# Tengen_app

[Aplicación web](https://clubtengen.shinyapps.io/tengen_app/) para presentar de una manera interactiva los datos de las partidas del [Club Tengen de Go](https://online-go.com/group/615).

# Contenidos

## Personas

Se puede seleccionar una o más personas para filtrar y enfocar los contenidos mostrados. Si no se selecciona a nadie se mostrarán todos los datos.

## Ratings

### Metodología

Los ratings se calculan con la metodología Glicko-2 [@glickman2001] utilizando el paquete R PlayerRatings [@stephenson2020]. 

Cada persona nueva al sistema se inicializa con rating de 1500, desviación 350 y volatilidad 0.06.  

Los ratings se transforman en rangos mediante la fórmula:

rango = log(rating/const_a)\*const_c + const_d

donde: 

const_a = 525

const_c = 23.15

const_d = -30

Los valores de las constantes y los valores iniciales se escogieron para comparabilidad con el sistema de rating OGS [@online-go.com2023]. El valor de const_d se ajustó considerando que rating 1918 sea rango 0 al igual que en OGS. Rangos mayores o iguales a 0 (rating 1918) se consideran dan, menores kyu. Se debe sumar 1 a los rangos positivos para obtener el rango dan - por ejemplo un rango de 0.5 se considera 1.5 dan. Se considera un rango mínimo de -30 (30 kyu).

El handicap se considera como una modificación del rating y se mide en las mismas unidades que el rating [@glickman2001].

Se puede calcular el valor de 1 unidad de rango en unidades del rating así:

rango1 = log(rating1/const_a)\*const_c + const_d

rango2 = log(rating2/const_a)\*const_c + const_d

(rango1 - rango2) = const_c\*log(rating1/rating2)

Cuando rango1 - rango2 = 1:

rating1 = exp(1/const_c)\*rating2

rating1 - rating2 = exp(1/const_c)\*rating2 - rating2

rating1 - rating2 = rating2\*(exp(1/const_c)-1)

Cuando rating2 = 1500 y rango1 - rango2 = 1, rating1 - rating2 = 66.21465

rating_por_unidad_handicap = 66.21465

El cálculo de rating se considera los siguientes factores de equivalencia entre las piedras handicap en los distintos tamaños de tablero [@alemitrani2023]:

handicap_factor_9x9 = 4

handicap_factor_13x13 = (16/9)

handicap_factor_19x19 = 1

El handicap entra en el cálculo del rating en unidades de rating:

handicap_rating = handicap\*handicap_factor\*rating_por_unidad_handicap

### Gráfico

Se muestra la evolución del rango de cada persona, con el rango en el eje-y y la fecha en el eje-x. Si no se especifican personas, se muestran las líneas de las 10 personas con más partidos registrados.

### Tabla de datos

Se muestra para cada persona el rango, el rating, la desviación del rating (indica el nivel de incertidumbre), y sus cantidades de partidas, victorias y derrotas.

## Resumen

Resumen de las partidas por persona, oponente, tamaño del tablero y handicap. Muestra los totales de partidas y victorias. Incluye también la tasa de victorias que es el total de victorias dividido por el total de partidas correspondiente.

## Partidas

Se muestran los datos detallados de todas las partidas, en orden descendiente de fecha/hora - los casos más recientes arriba. Hay una fila por persona-partida, es decir dos filas por partida.

## Importar resultados de torneos

Versión actual: 0.1.0 (`app_version` en `app.R`).

Los resultados de torneos (archivos de [OpenGotha](http://opengotha.info/) exportados a Excel) se agregan a los datos de partidas con scripts en R, que se ejecutan desde la carpeta del proyecto. Los scripts están en `scripts/` (no en `R/`, porque Shiny carga automáticamente todo lo que hay en `R/`) y las pruebas en `tests/testthat/`.

**Requisitos:** R con los paquetes `readxl`, `openxlsx`, `stringi`, `stringdist` y (para las pruebas) `testthat`.

### Formato esperado del archivo de torneos

Un libro de Excel con **una hoja por torneo**. En cada hoja: celda A1 = título del torneo; una fila de encabezado cuya primera celda es `Num` con las columnas `Name` y `R 1`, `R 2`, ...; una fila por jugador (`Name` = `APELLIDO NOMBRE`); y, debajo de la tabla, una celda que empieza con `OpenGotha` y **justo debajo de ella la fecha del torneo** (p. ej. `24/01/2026 18:32:00`). Si falta la fecha el script se detiene con un mensaje. Cada celda de resultado es `N+` (ganó contra el jugador N) o `N-` (perdió), con `/b0` o `/w0` opcional (jugó con negras o blancas; el número es el handicap). `0+`/`0-` (descanso, *bye*) se ignora. Si no se conoce el color se asume que quien ganó jugó con blancas. El tamaño del tablero se toma del nombre de la hoja o del título (`9x9`, `13x13`, `19x19`); si no aparece se asume 19 x 19.

### Procedimiento

1. Dejar los archivos en `data/`: `Club Tengen de Go - Registro de Partidas (Responses).xlsx` (copia de la Google Sheet) y el libro de torneos (nombre en las variables `tournaments_file` de ambos scripts).
2. **Construir el índice de jugadores:** `Rscript scripts/build_player_index.R`. Crea/actualiza `outputs/player_index.xlsx` con las hojas:
   - `index`: un jugador por fila, con `player_id` estable (P001...), los nombres usados en las partidas (`ratings_names`), los nombres en los torneos (`tournament_names`), el `recommended_name` (formato NOMBRE APELLIDO), el estado (`ratings_only`, `tournament_only`, `linked`) y `needs_review`.
   - `decisions`: jugadores de torneos que podrían ser alguien ya existente, con `suggested_matches`. **Si son la misma persona, escribir en `merge_into` el nombre (o `player_id`) del otro jugador** y volver a ejecutar el script. Si no se escribe nada, se mantienen como personas distintas.
   - `renames`: para cambiar el nombre recomendado de un jugador, escribir su `player_id` y `new_name`; volver a ejecutar el script.
   - `lookup`: tabla nombre → `player_id` que usa el segundo script (no editar).
3. **Regla de fusión:** solo se unen automáticamente nombres idénticos salvo tildes/mayúsculas/espacios (Alvaro = Álvaro) y las correcciones codificadas (Anibal/Aníbal, Nicolás/Nico, Panchito/Francisco). Nombres parecidos (p. ej. Cris y Cristian Villacura) **no** se unen sin una decisión explícita. Los nombres de los torneos se convierten a NOMBRE APELLIDO tomando la primera palabra como apellido (revisar apellidos compuestos en `renames`, p. ej. "San Martin Rodrigo").
4. **Agregar los torneos:** `Rscript scripts/merge_ratings.R`. Si quedan jugadores de torneos con coincidencias posibles sin resolver, el script se detiene y los lista (con `--allow-unresolved` se tratan como personas distintas). El script:
   - reemplaza los nombres de las partidas por el nombre recomendado;
   - detecta duplicados: una partida de torneo ya registrada (mismo par de personas y mismo ganador, con el comentario `Torneo <título> R<n>` o en el mismo día) no se agrega;
   - registra como `conflict` (y no agrega) las partidas del mismo torneo con distinto ganador;
   - agrega las partidas nuevas con fecha del torneo, hora desde las 12:00:00 (única para cada partida) y comentario `Torneo <título> R<n>`.
5. **Resultados** en `outputs/`: `Club Tengen de Go - Registro de Partidas (Responses).xlsx` (misma hoja y columnas que la Google Sheet; reemplazar los datos de la Google Sheet con este contenido) y `tournament_import_report.csv` (estado de cada partida de torneo: `new`, `duplicate`, `conflict`). Revisar el reporte antes de reemplazar los datos.
6. Para un torneo futuro: agregar su hoja al libro de torneos y repetir los pasos 2 a 5. Los `player_id` y las decisiones anteriores se conservan. Si después de reemplazar la Google Sheet se vuelve a ejecutar, no se agregan partidas duplicadas.

### Problemas frecuentes

- *tournament date not found*: la hoja no tiene la fecha bajo la celda `OpenGotha`.
- *Unrecognised result cell*: hay una celda con un formato no previsto (p. ej. empate `=`, no soportado).
- *Tournament names missing from the player index*: ejecutar de nuevo `scripts/build_player_index.R`.
- Advertencias de «inconsistent results» o «no reciprocal cell»: los resultados de los dos jugadores no coinciden; revisar la hoja.

### Pruebas

`Rscript -e 'testthat::test_dir("tests/testthat")'`

# Referencias

alemitrani, gennan, flovo, and Atorrante. 2023. "Ranking and Handicaps." <https://web.archive.org/web/20231014034254/https://forums.online-go.com/t/ranking-and-handicaps/17739/26?u=alemitrani>.

Glickman, Mark E. 2001. "Dynamic Paired Comparison Models with Stochastic Variances." *Journal of Applied Statistics* 28 (6): 673--89. <https://doi.org/10.1080/02664760120059219>.

Online-Go.com. 2023. *Rank_utils.ts*. <https://github.com/online-go/online-go.com/blob/2e9ccea12b16fefeba8fb86e0312875964e16857/src/lib/rank_utils.ts#L50C1-L51C17>.

Stephenson, Alec, and Jeff Sonas. 2020. *PlayerRatings: Dynamic Updating Methods for Player Ratings Estimation*. <https://cran.r-project.org/web/packages/PlayerRatings/index.html>.
