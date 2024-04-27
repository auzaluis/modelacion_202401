
library(NbClust)
library(FactoMineR)

# Iteración
clustering <- NbClust(
  data = DF10 |> select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn"
)


# Creación de 5 clusters (segmentos)
clustering2 <- NbClust(
  data = DF10 |> select(all_of(dimensiones)),
  distance = "euclidean",
  method = "ward.D2",
  index = "dunn",
  min.nc = 5,
  max.nc = 5
)

clustering2



# Peso de los segmentos
table(clustering2$Best.partition)
prop.table(table(clustering2$Best.partition))



# Agregar la columna clustering al DF
clustering2$Best.partition

DF11 <- DF10 |> 
  mutate(segmento = as.factor(clustering2$Best.partition))



# Analizando los segmentos ----

## Cruces de variables

DF11 |>
  group_by(segmento) |> 
  summarise(desarrollo = mean(desarrollo),
            materialismo = mean(materialismo),
            riesgo = mean(riesgo),
            social = mean(social),
            solitario = mean(solitario),
            tradición = mean(tradición))


## Análisis de correspondencias

### Convirtiendo valores positivos a negativos
library(scales)

DF12 <- DF11 |> 
  mutate_at(.vars = dimensiones,
            .funs = rescale)

DF13 <- DF12 |>
  group_by(segmento) |> 
  summarise(desarrollo = mean(desarrollo),
            materialismo = mean(materialismo),
            riesgo = mean(riesgo),
            social = mean(social),
            solitario = mean(solitario),
            tradición = mean(tradición)) |> 
  # select(-materialismo) |> 
  column_to_rownames("segmento")



### Corriendo el ANACOR
FactoMineR::CA(DF13)

View(DF12)




