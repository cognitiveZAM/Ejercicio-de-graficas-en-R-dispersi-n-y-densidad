# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# Nota: Los ingresos (Income) están expresados en dólares estadounidenses.
# Los datos analizados corresponden al período comprendido entre los años 2012 y 2014.

# Gráfico de dispersión: Ingresos vs. Consumo de carne
ggplot(marketing, aes(x = Income, y = MntMeatProducts)) +
  geom_point(color = "blue", size = 2) +
  coord_cartesian(xlim = c(0, 200000), ylim = c(0, 2000)) +
  labs(
    title = "Gráfico de Dispersión: Ingresos vs. Consumo de carne",
    x = "Ingresos (USD)",  # Especificando que los ingresos están en dólares
    y = "Consumo de Carne"
  ) +
  theme_minimal()

# Gráfico de dispersión: Ingresos vs. Consumo de vino
ggplot(marketing, aes(x = Income, y = MntWines)) +
  geom_point(color = "purple", size = 2) +
  coord_cartesian(xlim = c(0, 200000), ylim = c(0, 2000)) +
  labs(
    title = "Gráfico de Dispersión: Ingresos vs. Consumo de vino",
    x = "Ingresos (USD)",  # Especificando que los ingresos están en dólares
    y = "Consumo de Vino"
  ) +
  theme_minimal()

# Gráfico de dispersión: Ingresos vs. Consumo de frutas
ggplot(marketing, aes(x = Income, y = MntFruits)) +
  geom_point(color = "green", size = 2) +
  coord_cartesian(xlim = c(0, 200000), ylim = c(0, 300)) +
  labs(
    title = "Gráfico de Dispersión: Ingresos vs. Consumo de frutas",
    x = "Ingresos (USD)",  # Especificando que los ingresos están en dólares
    y = "Consumo de Frutas"
  ) +
  theme_minimal()

# Gráfico de dispersión: Ingresos vs. Consumo de dulcería
ggplot(marketing, aes(x = Income, y = MntSweetProducts)) +
  geom_point(color = "pink", size = 2) +
  coord_cartesian(xlim = c(0, 200000), ylim = c(0, 300)) +
  labs(
    title = "Gráfico de Dispersión: Ingresos vs. Consumo de Dulcería",
    x = "Ingresos (USD)",  # Especificando que los ingresos están en dólares
    y = "Consumo de Dulcería"
  ) +
  theme_minimal()

# Gráfico de dispersión: Ingresos vs. Consumo de productos de oro
ggplot(marketing, aes(x = Income, y = MntGoldProds)) +
  geom_point(color = "gold", size = 2) +
  coord_cartesian(xlim = c(0, 200000), ylim = c(0, 400)) +
  labs(
    title = "Gráfico de Dispersión: Ingresos vs. Consumo de productos de Oro",
    x = "Ingresos (USD)",  # Especificando que los ingresos están en dólares
    y = "Consumo de Productos de Oro"
  ) +
  theme_minimal()

# Transformar los datos al formato largo para gráficos combinados
marketing_largo <- marketing %>%
  pivot_longer(
    cols = starts_with("Mnt"),
    names_to = "Producto",
    values_to = "Gasto"
  )

# Gráfico combinado de dispersión: Ingresos vs. Gastos por Producto
ggplot(marketing_largo, aes(x = Income, y = Gasto, color = Producto, shape = Producto)) +
  geom_point(size = 2, alpha = 0.7) +
  coord_cartesian(xlim = c(0, 200000), ylim = c(0, 2000)) +
  scale_color_manual(values = c(
    MntMeatProducts = "blue",
    MntWines = "purple",
    MntFruits = "green",
    MntSweetProducts = "pink",
    MntGoldProds = "gold"
  )) +
  scale_shape_manual(values = c(
    MntMeatProducts = 16,  # Círculo lleno
    MntWines = 17,         # Triángulo hacia arriba
    MntFruits = 18,        # Diamante
    MntSweetProducts = 19, # Círculo sólido
    MntGoldProds = 15      # Cuadrado lleno
  )) +
  labs(
    title = "Relación entre Ingresos y Gastos por Producto",
    x = "Ingresos (USD)",  # Especificando que los ingresos están en dólares
    y = "Gasto",
    color = "Producto",
    shape = "Producto"
  ) +
  theme_minimal()

# Comparar curvas de densidad segun un
# estado civil especifico rerspecto a dos productos

# Transformar los datos al formato largo
marketing_largo <- marketing %>%
  select(Marital_Status, MntMeatProducts, MntWines) %>%
  pivot_longer(
    cols = c(MntMeatProducts, MntWines),
    names_to = "Producto",
    values_to = "Consumo"
  ) %>%
  mutate(Producto = recode(Producto,
                           MntMeatProducts = "Carne",
                           MntWines = "Vino"))
# Filtrar los datos para un estado civil específico, por ejemplo, "Married"
estado_civil_especifico <- marketing_largo %>%
  filter(Marital_Status == "Married")
# Crear el gráfico de densidad
ggplot(estado_civil_especifico, aes(x = Consumo, fill = Producto)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Distribución del Consumo de Carne y Vino para Personas Casadas",
    x = "Consumo",
    y = "Densidad",
    fill = "Producto"
  ) +
  theme_minimal()


estado_civil_especifico <- marketing_largo %>%
  filter(Marital_Status == "Absurd")
# Crear el gráfico de densidad
ggplot(estado_civil_especifico, aes(x = Consumo, fill = Producto)) +
  geom_density(alpha = 0.5) +
  xlim(min(estado_civil_especifico$Consumo), max(estado_civil_especifico$Consumo)) +
  labs(
    title = "Distribución del Consumo de Carne y Vino para Estado Civil Confuso(absurd)",
    x = "Consumo",
    y = "Densidad",
    fill = "Producto"
  ) +
  theme_minimal()


estado_civil_especifico <- marketing_largo %>%
  filter(Marital_Status == "YOLO")
# Crear el gráfico de densidad
ggplot(estado_civil_especifico, aes(x = Consumo, fill = Producto)) +
  geom_density(alpha = 0.5) +
  xlim(min(estado_civil_especifico$Consumo), max(estado_civil_especifico$Consumo)) +
  labs(
    title = "Distribución del Consumo de Carne y Vino para Personas Liberales(YOLO)",
    x = "Consumo",
    y = "Densidad",
    fill = "Producto"
  ) +
  theme_minimal()


estado_civil_especifico <- marketing_largo %>%
  filter(Marital_Status == "Single")
# Crear el gráfico de densidad
ggplot(estado_civil_especifico, aes(x = Consumo, fill = Producto)) +
  geom_density(alpha = 0.5) +
  xlim(min(estado_civil_especifico$Consumo), max(estado_civil_especifico$Consumo)) +
  labs(
    title = "Distribución del Consumo de Carne y Vino para Solteros",
    x = "Consumo",
    y = "Densidad",
    fill = "Producto"
  ) +
  theme_minimal()


estado_civil_especifico <- marketing_largo %>%
  filter(Marital_Status == "Divorced")
# Crear el gráfico de densidad
ggplot(estado_civil_especifico, aes(x = Consumo, fill = Producto)) +
  geom_density(alpha = 0.5) +
  xlim(min(estado_civil_especifico$Consumo), max(estado_civil_especifico$Consumo)) +
  labs(
    title = "Distribución del Consumo de Carne y Vino para Divorciados",
    x = "Consumo",
    y = "Densidad",
    fill = "Producto"
  ) +
  theme_minimal()


estado_civil_especifico <- marketing_largo %>%
  filter(Marital_Status == "Together")
# Crear el gráfico de densidad
ggplot(estado_civil_especifico, aes(x = Consumo, fill = Producto)) +
  geom_density(alpha = 0.5) +
  xlim(min(estado_civil_especifico$Consumo), max(estado_civil_especifico$Consumo)) +
  labs(
    title = "Distribución del Consumo de Carne y Vino para Union Libre",
    x = "Consumo",
    y = "Densidad",
    fill = "Producto"
  ) +
  theme_minimal()


estado_civil_especifico <- marketing_largo %>%
  filter(Marital_Status == "Widow")
# Crear el gráfico de densidad
ggplot(estado_civil_especifico, aes(x = Consumo, fill = Producto)) +
  geom_density(alpha = 0.5) +
  xlim(min(estado_civil_especifico$Consumo), max(estado_civil_especifico$Consumo)) +
  labs(
    title = "Distribución del Consumo de Carne y Vino para Personas Viudas",
    x = "Consumo",
    y = "Densidad",
    fill = "Producto"
  ) +
  theme_minimal()
