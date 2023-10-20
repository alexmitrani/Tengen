# Tengen_app

[Aplicación web](https://clubtengen.shinyapps.io/tengen_app/) para presentar de una manera interactiva los datos de las partidas del [Club Tengen de Go](https://online-go.com/group/615).

# Contenidos

## Personas

Se puede seleccionar una o más personas para filtrar y enfocar los contenidos mostrados. Si no se selecciona a nadie se mostrarán todos los datos.

## Ratings

### Metodología

Los ratings se calculan con la metodología Glicko-2 utilizando el paquete R PlayerRatings. Los ratings se transforman en rangos mediante la fórmula:

rango = log(rating/const_a)\*const_c + const_d

const_a = 525

const_c = 23.15

const_d = -30

Los valores de las constantes se escogieron para comparabilidad con el sistema de rating OGS. El valor de const_d se ajustó considerando que rating 1918 sea rango 0 al igual que en OGS. Rangos mayores o iguales a 0 (rating 1918) se consideran dan, menores kyu. Se debe sumar 1 a los rangos positivos para obtener el rango dan - por ejemplo un rango de 0.5 se considera 1.5 dan. Se considera un rango mínimo de -30 (30 kyu).

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

El cálculo de rating se considera los siguientes factores de equivalencia entre las piedras handicap en los distintos tamaños de tablero:

handicap_factor_9x9 = 4

handicap_factor_13x13 = (16/9)

handicap_factor_19x19 = 1

El handicap entra en el cálculo del rating en unidades de rating:

handicap_rating = handicap\*handicap_factor\*rating_por_unidad_handicap

### Gráfico

Se muestra la evolución del rango de cada persona, con el rango en el eje-y y la fecha en el eje-x. Si no se especifican personas, se muestran las líneas de las 10 personas más fuertes segun los ratings más actualizados.

### Tabla de datos

Se muestra para cada persona el rango, el rating, la desviación del rating (indica el nivel de incertidumbre), y sus cantidades de partidas, victorias y derrotas.

## Resumen

Resumen de las partidas por persona, oponente, tamaño del tablero y handicap. Muestra los totales de partidas y victorias. Incluye también la tasa de victorias que es el total de victorias dividido por el total de partidas correspondiente.

## Partidas

Se muestran los datos detallados de todas las partidas, en orden descendiente de fecha/hora - los casos más recientes arriba. Hay una fila por persona-partida, es decir dos filas por partida.

# Referencias

<https://cran.r-project.org/web/packages/PlayerRatings/index.html>

<https://www.englishchess.org.uk/wp-content/uploads/2012/04/ratings.pdf>

<http://www.glicko.net/research/dpcmsv.pdf>

<https://github.com/online-go/online-go.com/blob/2e9ccea12b16fefeba8fb86e0312875964e16857/src/lib/rank_utils.ts#L50C1-L51C17>

<https://web.archive.org/web/20231014034254/https://forums.online-go.com/t/ranking-and-handicaps/17739/26?u=alemitrani>
