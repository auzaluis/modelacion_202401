
# Tema 1: Carga de datos ----

## Carga local ----
DF <- read.csv(file = "Personalidad/Personalidad y uso de apps (respuestas).csv",
               check.names = FALSE)
colnames(DF)

## Carga en línea ----
install.packages("gsheet")
library(gsheet)

url_google <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

DF <- read.csv(text = gsheet2text(url = url_google),
               check.names = F)



## Estructura de un data frame ----
class(DF)
class(DF$Sexo)
class(DF$`Escribe tu edad exacta`)
nrow(DF) # Cantidad de filas
ncol(DF) # Cantidad de columnas



# Tema 2: Transformación de datos ----

## Valores perdidos ----

# Los NA pueden ser tratados de 2 maneras:
# 1. Imputación (reemplazo)
# 2. Ignorarlos / Eliminarlos

DF$`Escribe tu edad exacta`
is.na(DF$`Escribe tu edad exacta`)
summary(is.na(DF$`Escribe tu edad exacta`))

# Conociendo el pipe
DF$`Escribe tu edad exacta` |>
  is.na() |> 
  summary()

### Imputación | Reemplazo por la media ----
install.packages('tidyverse')
library(tidyverse)

mean(DF$`Escribe tu edad exacta`, na.rm = TRUE)

ifelse(test = DF$`Escribe tu edad exacta` |> is.na(),
       yes = mean(DF$`Escribe tu edad exacta`, na.rm = TRUE),
       no = DF$`Escribe tu edad exacta`)


DF2 <- DF |> 
  mutate(edad2 = ifelse(test = `Escribe tu edad exacta` |> is.na(),
                        yes = mean(`Escribe tu edad exacta`, na.rm = TRUE),
                        no = `Escribe tu edad exacta`)) |> 
  relocate(edad2, .after = `Escribe tu edad exacta`)



### Eliminar filas completas ----

DF2 <- DF |> na.omit()



## Estandarización de variables ----

### Normalización ----
DF2$`Escribe tu edad exacta` |> scale()
DF2$`Escribe tu edad exacta` |> mean()

# Comparando la variable original con la normalizada
data.frame(
  original = DF2$`Escribe tu edad exacta`,
  normalizada = DF2$`Escribe tu edad exacta` |> scale()
)



# Guardando la variable normalizada en un nuevo data frame
DF3 <- DF2 |> 
  mutate(edadZ = `Escribe tu edad exacta` |> scale()) |> 
  relocate(edadZ, .after = `Escribe tu edad exacta`)



### Rango ----
library(scales)
DF3$`Escribe tu edad exacta` |> rescale()

# Comparando la variable original con la tipo rango
data.frame(
  original = DF3$`Escribe tu edad exacta`,
  rango = DF3$`Escribe tu edad exacta` |> rescale()
)



## Agrupaciones ----

### Rangos numéricos ----

DF4 <- DF3 |> 
  mutate(edadGR = cut(`Escribe tu edad exacta`,
                      breaks = c(-Inf, 18, 21, Inf),
                      labels = c('18 o menos', '19 a 21', 'Más de 21'))) |> 
  relocate(edadGR, .after = `Escribe tu edad exacta`)



### Categóricas ----

unique(DF4$`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [No discrimino y trato a todos por igual]`)
unique(DF4[,7])


ifelse(
  test = DF4[,7] == "Un poco verdadero" |
    DF4[,7] == "Totalmente verdadero",
  yes = 1,
  no = 0
)



# Bucles ----

# Paso 1: Crear un vector con los nombres de las columnas

frases <- DF4 |> 
  select(starts_with("Según tu")) |> 
  colnames()

frases

# Paso 2: Crear el bucle

DF5 <- DF4

for (frase in frases) {
  
  DF5[,frase] <- ifelse(
    test = DF5[,frase] == "Totalmente verdadero" |
      DF5[,frase] == "Un poco verdadero",
    yes = 1,
    no = 0
  )
  
}



# Tema 3: Manipulación de datos ----

# Convirtiendo un data frame en tibble
DF5 <- DF5 |> as_tibble()

## Selección de columnas: select() ----
DF5 |> select(Sexo)
DF5 |> select(Sexo, `Escribe tu edad exacta`)
DF5 |> select(-`Marca temporal`)
DF5 |> select(starts_with("edad"))
DF5 |> select(ends_with("00:00"))
DF5 |> select(contains("edad"))



## Filtrado de filas: filter() ----
DF5 |> filter(Sexo == "Mujer") |> View() # View para previsualizar
DF5 |> filter(Sexo != "Mujer") |> View()
DF5 |> filter(`Escribe tu edad exacta` > 21) |> View()
DF5 |> filter(`Escribe tu edad exacta` <= 21) |> View()

# Resolviendo un filtrado de 4 maneras distintas
DF5 |> filter(between(`Escribe tu edad exacta`, 18, 21))
DF5 |> filter(`Escribe tu edad exacta` >= 18,
              `Escribe tu edad exacta` <= 21)
DF5 |> filter(`Escribe tu edad exacta`>= 18 &
                `Escribe tu edad exacta` <= 21)
DF5 |> filter(`Escribe tu edad exacta` %in% 18:21)

DF5 |>
  filter(Sexo == "Mujer",
         `Escribe tu edad exacta`%in% 18:21) |> 
  select(Sexo, `Escribe tu edad exacta`, edadGR) |> View()



## Cambio de nombre de columnas ----
DF6 <- DF5

# APPS
# Paso 1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

# Paso 2: Usar ese vector para asignar nombres
colnames(DF6)[32:35] <- apps



# Frases
# Paso 1: Crear vector con los nuevos nombres
frases2 <- frases |> 
  as_tibble() |> 
  
  separate(col = value,
           into = c("No sirve", "Sirve"),
           sep = "\\[") |> 
  select(-`No sirve`) |> 
  
  separate(col = "Sirve",
           into = c("Sirve", "No sirve"),
           sep = "\\]") |> 
  select(-`No sirve`) |> 
  
  as_vector()



# Paso 2: Usar ese vector para asignar nombres
colnames(DF6)[7:30] <- frases2



## Pivotado de base ----

### Pivot longer ----
DF7 <- DF6 |> 
  select(`Marca temporal`,
         Sexo,
         `Escribe tu edad exacta`,
         all_of(apps)) |> 
  pivot_longer(cols = apps,
               names_to = "app",
               values_to = "time")



### Pivot Wider ----
DF8 <- DF7 |> 
  pivot_wider(names_from = app,
              values_from = time)



# Tema 4: Valores Atípicos (outliers) ----

## Transformar las horas a número
class(DF7$time)
strsplit(DF7$time, split = ":") |> head()

## Transformación
DF7$time <- sapply(
  strsplit(DF7$time, split = ":"),
  function(x) {
    x <- as.numeric(x)
    x[1] + x[2]/60 + x[3]/60^2
  }
)

DF7$time


## Detección gráfica ----

# Boxplot (feo)
boxplot(DF7$time)

# Boxplot (bonito)
# install.packages('plotly')
library(plotly)

ggplotly(
  DF7 |> 
    ggplot(aes(x = app, y = time, fill = app)) +
    geom_boxplot() +
    theme_minimal() +
    labs(x = "",
         y = "Horas a la semana") +
    theme(legend.position = "none")
)



DF9 <- DF7 |> 
  
  mutate(
    outlier = case_when(
      app == "Facebook"  & time > 10    ~ "outlier",
      app == "Instagram" & time > 13.28 ~ "outlier",
      app == "TikTok"    & time > 16.13 ~ "outlier",
      app == "YouTube"   & time > 8.18  ~ "outlier",
      .default = "No outlier"
    )
  ) |> 
  
  group_by(app) |> 
  
  mutate(time2 = ifelse(test = outlier == "outlier",
                        yes = mean(time),
                        no = time)) |> 
  
  ungroup()


# Uso de count, group_by y summarise

DF9 |> 
  group_by(app, Sexo) |> 
  summarise(conteo = n(),
            promedio = mean(time),
            promedio2 = mean(time2))


DF9 |> 
  count(Sexo) |> 
  arrange(desc(n)) |> 
  mutate(prop = n/sum(n))



DF9 |> 
  
  mutate(edadGR = cut(`Escribe tu edad exacta`,
                      breaks = c(-Inf, 18, 21, Inf),
                      labels = c('18 o menos', '19 a 21', 'Más de 21'))) |> 
  
  count(Sexo, edadGR) |> 
  group_by(Sexo) |> 
  mutate(prop = n/sum(n)) |> 
  ungroup()







