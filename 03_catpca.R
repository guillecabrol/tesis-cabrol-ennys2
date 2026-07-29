# ============================================================
# CATPCA — REDUCCIÓN DE DIMENSIONALIDAD
# Frecuencias de consumo de alimentos (17 grupos, ordinales)
# ============================================================

# ── 1. PAQUETES ──────────────────────────────────────────────
# Gifi es el paquete de referencia para CATPCA en R.
# La función princals() hace "PRINcipal components by ALternating
# least squares" — el nombre técnico del CATPCA.
# install.packages(c("Gifi", "tidyverse"))

library(Gifi)
library(tidyverse)
library(ggrepel)


# ── 2. CARGA DE DATOS ────────────────────────────────────────
# Usamos directamente el dataset que ya tiene todo: target,
# ponderadores, controles, entorno, y las 17 frecuencias de
# consumo crudas (T_C3_FCA_6_1_1 a _17) con sus 9 categorías.

datos <- read.csv("dataset_FCA_crudas.csv", stringsAsFactors = FALSE)
cat("Casos:", nrow(datos), "| Columnas:", ncol(datos), "\n")

# Las 17 variables de frecuencia de consumo
vars_fca <- paste0("T_C3_FCA_6_1_", 1:17)

# Etiquetas legibles de cada grupo (en el mismo orden 1..17)
etiquetas_fca <- c(
  "Lacteos", "Frutas", "Verduras", "Papas_Cereales_Refinados",
  "Cereales_Int_Legumbres", "Embutidos_Fiambres", "Carnes_Huevos",
  "Pescado", "Aceites_Vegetales", "Frutas_Secas",
  "Copetin", "Golosinas", "Facturas_Galletitas_Dulces",
  "Congelados_Preelaborados", "Bebidas_Sin_Azucar",
  "Bebidas_Con_Azucar", "Agua"
)


# ── 3. PREPARAR LAS VARIABLES COMO FACTORES ORDENADOS ────────
# CATPCA necesita que las variables sean factores ORDENADOS,
# con los niveles en el orden correcto de menor a mayor consumo.
# Si no le decimos el orden, las trataría como nominales (sin orden)
# y perderíamos la ventaja de CATPCA sobre MCA.

orden_freq <- c(
  "Nunca o menos de 1 vez al mes",
  "Entre 1 y 3 veces al mes",
  "1 vez por semana",
  "2 a 4 veces por semana",
  "5 a 6 veces por semana",
  "1 vez al día",
  "Entre 2 y 3 veces al día",
  "Entre 4 y 5 veces al día",
  "6 veces o más por día"
)

# Tomamos las 17 columnas, las renombramos a etiquetas legibles
# y las convertimos a factor ordenado.
datos_fca <- datos %>%
  select(all_of(vars_fca)) %>%
  set_names(etiquetas_fca) %>%
  mutate(across(everything(),
                ~ factor(.x, levels = orden_freq, ordered = TRUE)))

# Chequeo: ¿quedó alguna categoría sin mapear (NA inesperado)?
cat("\nNAs tras convertir a factor ordenado (deberían ser 0):\n")
print(colSums(is.na(datos_fca)))


# ── 4. CORRER EL CATPCA ──────────────────────────────────────
# princals() es la función de CATPCA.
#   ndim    = número de dimensiones a extraer (empezamos con varias
#             para después decidir cuántas retener)
#   ordinal = TRUE → respeta el orden de las categorías (clave)

set.seed(1213)
catpca <- princals(
  datos_fca,
  ndim    = 5,        # extraemos 5 para mirar cuántas valen la pena
  ordinal = TRUE      # tratamiento ordinal de todas las variables
)

cat("\n=== Resumen del CATPCA ===\n")
print(summary(catpca))


# ── 5. ¿CUÁNTAS DIMENSIONES RETENER? ─────────────────────────
# Cada dimensión explica un % de la varianza total.
# Criterio de Kaiser: se conservan las dimensiones con autovalor > 1, es decir, aquellas que
# sintetizan más varianza que una única variable original estandarizada.

evals <- catpca$evals
var_explicada <- evals / sum(evals) * 100
var_acumulada <- cumsum(var_explicada)

tabla_var <- tibble(
  Dimension     = seq_along(evals),
  Autovalor     = round(evals, 3),
  Var_Explicada = round(var_explicada, 2),
  Var_Acumulada = round(var_acumulada, 2)
)

cat("\n=== Varianza explicada por dimensión ===\n")
print(tabla_var, n = Inf)

# ── 6. INTERPRETAR LAS DIMENSIONES (LOADINGS) ────────────────
# Los "loadings" indican cuánto pesa cada grupo de alimento en
# cada dimensión. Un loading alto (positivo o negativo) significa
# que ese alimento define fuertemente esa dimensión.
# Interpretamos las dimensiones mirando qué alimentos se agrupan.

loadings <- as.data.frame(catpca$loadings)
loadings$Alimento <- rownames(loadings)

cat("\n=== Loadings (pesos de cada alimento en cada dimensión) ===\n")
print(loadings)

# Gráfico de loadings para las 2 primeras dimensiones
p_load <- ggplot(loadings, aes(x = D1, y = D2, label = Alimento)) +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  geom_segment(aes(x = 0, y = 0, xend = D1, yend = D2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "#185FA5", alpha = 0.6) +
  geom_text(size = 3, vjust = -0.5) +
  labs(title    = "Loadings \u2014 Dimensiones 1 y 2",
       subtitle = "Alimentos que apuntan en la misma dirección se asocian",
       x = "Dimensión 1", y = "Dimensión 2") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "gray40", hjust = 0.5))

print(p_load)


# ── 7. EXTRAER LOS SCORES ────────────────────────────────────
# Los "object scores" son la posición de cada adolescente en cada
# dimensión. Estos son los valores que vamos a usar como nuevas
# variables predictoras en la regresión logística.

scores <- as.data.frame(catpca$objectscores)
names(scores) <- paste0("Dim_Alimentos_", seq_len(ncol(scores)))

cat("\n=== Primeras filas de los scores ===\n")
print(head(scores))

# Pegamos los scores al dataset principal (manteniendo id, target,
# ponderadores y todo lo demás) para usarlos después en la regresión.
datos_con_scores <- bind_cols(datos, scores)

# Guardamos el dataset con los scores para el siguiente script
write.csv(datos_con_scores, "datos_con_scores_catpca.csv", row.names = FALSE)
