# Tengen_app

[Aplicación web](https://clubtengen.shinyapps.io/tengen_app/) para presentar de una manera interactiva los datos de las partidas del [Club Tengen de Go](https://online-go.com/group/615).  

# Contenidos

## Personas

Se puede seleccionar una o más personas para filtrar y enfocar los contenidos mostrados. Si no se selecciona a nadie se mostrarán todos los datos.  

## Ratings

Los ratings se calculan con la metodología Glicko-2 utilizando el paquete R  PlayerRatings. Los ratings se transforman en rangos mediante la fórmula: 



### Gráfico

Se muestra la evolución del rango de cada persona, con el rango en el eje-y y la fecha en el eje-x. Si no se especifican personas, se muestran las líneas de las 10 personas más fuertes segun los ratings más actualizados. 

### Tabla de datos

## Resumen

Resumen de las partidas por persona, oponente, tamaño del tablero y handicap. Muestra los totales de partidas y victorias. Incluye también la tasa de victorias que es el total de victorias dividido por el total de partidas correspondiente.  

## Partidas

Se muestran los datos detallados de todas las partidas, en orden descendiente de fecha/hora - los casos más recientes arriba. Hay una fila por persona-partida, es decir dos filas por partida. 






