
library(ggcorrplot)
library(FactoMineR)

frases3 <- as.vector(frases2)

# Matriz de correlaciones ----
r <- cor(DF6 |> select(all_of(frases3)),
    method = "spearman")

# Gráfico de la cor-matrix

ggplotly(
  ggcorrplot(
    corr = r,
    # type = "upper",
    colors = c("red", "white", "blue"),
    show.legend = F,
    tl.cex = 6
  ) +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank())
)



# PCA: Principal Component Analysis ----

## Dimensión: materialismo

### Definir el vector con las frases
materialismo <- frases3[c(16,10,2)]

### Crear la dimensión
PCA.materialismo <- FactoMineR::PCA(
  DF6 |> select(all_of(materialismo)),
  ncp = 1
)

### Eigenvalues
PCA.materialismo$eig

### Correlación entre la CP1 y las variables originales
PCA.materialismo$var$cor

### Valores de la CP1
PCA.materialismo$ind$coord |> head()

### Compara los valores originales con la CP1
tibble(
  CP1 = PCA.materialismo$ind$coord * -1,
  DF6 |> select(all_of(materialismo))
) |> View()


## Dimensión: tradición

### Definir el vector con las frases
tradición <- frases3[c(12,9,14)]

### Crear la dimensión
PCA.tradición <- FactoMineR::PCA(
  DF6 |> select(all_of(tradición)),
  ncp = 1
)

### Eigenvalues
PCA.tradición$eig

### Correlación entre la CP1 y las variables originales
PCA.tradición$var$cor



## Dimensión: desarrollo

### Definir el vector con las frases
desarrollo <- frases3[c(19,4)]

### Crear la dimensión
PCA.desarrollo <- FactoMineR::PCA(
  DF6 |> select(all_of(desarrollo)),
  ncp = 1
)

### Eigenvalues
PCA.desarrollo$eig

### Correlación entre la CP1 y las variables originales
PCA.desarrollo$var$cor



## Dimensión: solitario

### Definir el vector con las frases
solitario <- frases3[c(21,22)]

### Crear la dimensión
PCA.solitario <- FactoMineR::PCA(
  DF6 |> select(all_of(solitario)),
  ncp = 1
)

### Eigenvalues
PCA.solitario$eig

### Correlación entre la CP1 y las variables originales
PCA.solitario$var$cor



## Dimensión: social

### Definir el vector con las frases
social <- frases3[c(20,23)]

### Crear la dimensión
PCA.social <- FactoMineR::PCA(
  DF6 |> select(all_of(social)),
  ncp = 1
)

### Eigenvalues
PCA.social$eig

### Correlación entre la CP1 y las variables originales
PCA.social$var$cor



## Dimensión: riesgo

### Definir el vector con las frases
riesgo <- frases3[c(7,8)]

### Crear la dimensión
PCA.riesgo <- FactoMineR::PCA(
  DF6 |> select(all_of(riesgo)),
  ncp = 1
)

### Eigenvalues
PCA.riesgo$eig

### Correlación entre la CP1 y las variables originales
PCA.riesgo$var$cor



# Data frame con las dimensiones

DF10 <- DF6 |> 
  mutate(desarrollo =   PCA.desarrollo$ind$coord * -1,
         materialismo = PCA.materialismo$ind$coord * -1,
         riesgo =       PCA.riesgo$ind$coord * -1,
         social =       PCA.social$ind$coord * -1,
         solitario =    PCA.solitario$ind$coord * -1,
         tradición =    PCA.tradición$ind$coord * -1)

dimensiones <- c("desarrollo",
                 "materialismo",
                 "riesgo",
                 "social",
                 "solitario",
                 "tradición")

