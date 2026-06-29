# ══════════════════════════════════════════════════════════════════════════════
# ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# ══════════════════════════════════════════════════════════════════════════════

# ──────────────────────────────────────────────────────────────────────────────
# 0. LIBRERÍAS
# ──────────────────────────────────────────────────────────────────────────────
library(tidyverse)   
library(survey)        
library(gtsummary)     
library(rstatix)  

# Reproducibilidad
set.seed(123)


# ──────────────────────────────────────────────────────────────────────────────
# 1. CARGA DE DATOS
#    - 'categorias' es el dataset principal del EDA: contiene las FCA recodificadas
#      en 3 niveles, las conductuales, las sociodemográficas y el entorno escolar.
#    - 'catpca' SOLO aporta los scores Dim_Alimentos_*. Como el CATPCA quedó relegado
#      a análisis de sensibilidad (no se reporta en el cuerpo), aquí se carga únicamente
#      para tenerlo disponible; no se usa en las figuras/tablas principales.
# ──────────────────────────────────────────────────────────────────────────────
datos_cat    <- read.csv("dataset.csv", stringsAsFactors = FALSE)

cat("\nDimensiones dataset categorias :", dim(datos_cat),    "\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2. PREPARACIÓN DE VARIABLES
# ──────────────────────────────────────────────────────────────────────────────

# 2.a Target binario y variable respuesta de 3 categorías ----------------------
#  - Target_bin           : 0/1 (Sin exceso / Exceso)
#  - Estado_Nutricional_3 : Sin exceso / Sobrepeso / Obesidad
datos_cat <- datos_cat %>%
  mutate(
    Target_bin = as.integer(Target_Exceso_Peso == "Exceso de peso"),
    Estado_Nutricional_3 = factor(
      Cat_IMC,
      levels = c("Delgadez", "Peso normal", "Sobrepeso", "Obesidad")
    ),
    # Para prevalencias de sobrepeso/obesidad agrupamos delgadez+normal en "Sin exceso"
    Estado_Nut_report = fct_collapse(
      Estado_Nutricional_3,
      "Sin exceso de peso" = c("Delgadez", "Peso normal")
    ),
    Estado_Nut_report = factor(
      Estado_Nut_report,
      levels = c("Sin exceso de peso", "Sobrepeso", "Obesidad")
    ),
    # Diseño muestral
    Estrato    = factor(Estrato),
    Ponderador = as.numeric(Ponderador),
    # Referencias coherentes con los modelos
    Sexo   = relevel(factor(Sexo),   ref = "Femenino"),
    Region = relevel(factor(Region), ref = "Patagonia"),
    Edad_f = relevel(factor(Edad),   ref = "13")
  )

# 2.b Terciles de las variables conductuales continuas -------------------------
a_terciles <- function(x) {
  cortes <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  cut(x, breaks = c(-Inf, cortes, Inf),
      labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE)
}

datos_cat <- datos_cat %>%
  mutate(
    Sedentarismo_t = relevel(a_terciles(HorasPorDia_Sedentarismo), ref = "Bajo"),
    ActFisica_t    = relevel(a_terciles(Horas_PorDia_ActFisica),   ref = "Bajo")
  )

cat("\n=== Cortes de terciles (percentiles 33 / 67) ===\n")
for (v in c("HorasPorDia_Sedentarismo", "Horas_PorDia_ActFisica")) {
  q <- quantile(datos_cat[[v]], probs = c(1/3, 2/3), na.rm = TRUE)
  cat(sprintf("  %-26s  p33 = %5.2f   p67 = %5.2f\n", v, q[1], q[2]))
}

# 2.c Recodificación de FCA a factor ordenado (ref = ocasional)
vars_fca <- c("Frec_Lacteos","Frec_Frutas","Frec_Verduras","Frec_Papas_Pastas",
              "Frec_Integrales_Legumbres","Frec_Fiambres_Embutidos","Frec_Carnes_Huevos",
              "Frec_Pescado","Frec_Aceites","Frec_FrutosSecos_Semillas",
              "Frec_Snacks_Copetin","Frec_Golosinas","Frec_Facturas_Pasteleria",
              "Frec_Preelaborados","Frec_Bebidas_Light","Frec_Bebidas_Azucaradas",
              "Frec_Agua")

relevel_si_existe <- function(x, ref) {
  x <- factor(x)
  if (ref %in% levels(x)) x <- relevel(x, ref = ref)
  x
}
datos_cat <- datos_cat %>%
  mutate(across(all_of(vars_fca), ~ relevel_si_existe(.x, "Consumo ocasional")))


# ──────────────────────────────────────────────────────────────────────────────
# 3. OBJETO DE DISEÑO MUESTRAL (para todas las estimaciones ponderadas)
# ──────────────────────────────────────────────────────────────────────────────
options(survey.lonely.psu = "adjust")
diseno <- svydesign(ids = ~1, strata = ~Estrato, weights = ~Ponderador,
                    data = datos_cat, nest = TRUE)


# ══════════════════════════════════════════════════════════════════════════════
# DISTRIBUCIÓN DE LA VARIABLE RESPUESTA — NO PONDERADA Y PONDERADA
# ══════════════════════════════════════════════════════════════════════════════

# 1.a Conteo muestral y población expandida ------------------------------------
n_muestral <- nrow(datos_cat)
n_poblacion <- sum(datos_cat$Ponderador)
cat("\n================ DISTRIBUCIÓN DEL TARGET ================\n")
cat(sprintf("N muestral (adolescentes 13-17)      : %d\n", n_muestral))
cat(sprintf("Población representada (Σ ponderador): %s\n",
            format(round(n_poblacion), big.mark = ".", decimal.mark = ",")))

# 1.b Distribución NO ponderada (frecuencias muestrales) -----------------------
#   Exceso de peso (binario)
tab_np_bin <- datos_cat %>%
  count(Target_Exceso_Peso) %>%
  mutate(porc = round(100 * n / sum(n), 1))
cat("\n--- Exceso de peso (NO ponderado) ---\n"); print(tab_np_bin)

#   Tres categorías (sin exceso / sobrepeso / obesidad)
tab_np_3 <- datos_cat %>%
  count(Estado_Nut_report) %>%
  mutate(porc = round(100 * n / sum(n), 1))
cat("\n--- Estado nutricional 3 categorías (NO ponderado) ---\n"); print(tab_np_3)

# 1.c Distribución PONDERADA (prevalencias poblacionales) con IC95% logit ------
#   svyciprop con method="logit" -> IC apropiado para proporciones
prev_exceso <- svyciprop(~ I(Target_bin == 1), diseno, method = "logit")
cat("\n--- Prevalencia ponderada de EXCESO DE PESO (IC95% logit) ---\n")
print(prev_exceso); print(attr(prev_exceso, "ci"))

#   Prevalencia ponderada de sobrepeso y de obesidad por separado
prev_sobre <- svyciprop(~ I(Estado_Nut_report == "Sobrepeso"), diseno, method = "logit")
prev_obes  <- svyciprop(~ I(Estado_Nut_report == "Obesidad"),  diseno, method = "logit")
cat("\n--- Prevalencia ponderada de SOBREPESO (IC95% logit) ---\n")
print(prev_sobre); print(attr(prev_sobre, "ci"))
cat("\n--- Prevalencia ponderada de OBESIDAD (IC95% logit) ---\n")
print(prev_obes);  print(attr(prev_obes, "ci"))

# 1.d Tabla comparativa muestra (no ponderada) vs población (ponderada) --------
#   Esta es la tabla/figura que abre el capítulo.
prop_np <- prop.table(table(datos_cat$Estado_Nut_report))
prop_w  <- svymean(~ Estado_Nut_report, diseno)
tab_target_compara <- tibble(
  Categoria   = levels(datos_cat$Estado_Nut_report),
  Muestral_pct   = round(100 * as.numeric(prop_np), 1),
  Poblacional_pct = round(100 * as.numeric(prop_w), 1)
)
cat("\n--- Comparación muestral vs poblacional (%) ---\n")
print(tab_target_compara)

# 1.e Figura: barras muestral vs poblacional (para el cuerpo) ------------------
fig_target <- tab_target_compara %>%
  pivot_longer(c(Muestral_pct, Poblacional_pct),
               names_to = "Tipo", values_to = "Porcentaje") %>%
  mutate(Tipo = recode(Tipo,
                       Muestral_pct = "Muestral (no ponderado)",
                       Poblacional_pct = "Poblacional (ponderado)"),
         Categoria = factor(Categoria,
                            levels = c("Sin exceso de peso","Sobrepeso","Obesidad"))) %>%
  ggplot(aes(x = Categoria, y = Porcentaje, fill = Tipo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", Porcentaje)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("Muestral (no ponderado)" = "grey65",
                               "Poblacional (ponderado)" = "#2c7fb8")) +
  labs(title = "Distribución del estado nutricional",
       subtitle = "Comparación muestral vs. poblacional (ponderada)",
       x = NULL, y = "Porcentaje (%)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(fig_target)

# ══════════════════════════════════════════════════════════════════════════════
# TABLA 1 — DISTRIBUCIÓN DEL EXCESO DE PESO POR CATEGORÍA
# ══════════════════════════════════════════════════════════════════════════════

# 2.a Variables por dimensión ---------
vars_demograficas <- c("Region", "Sexo", "Edad")

vars_conductuales <- c("HorasPorDia_Sedentarismo", "Horas_PorDia_ActFisica")

vars_entorno <- c("Hace_ActFisica_Escuela", "Escuela_Provee_Alimento",
                  "Escuela_Tiene_Kiosco", "Compro_En_Kiosco",
                  "Kiosco_Compro_No_Recomendados", "Kiosco_Compro_Recomendados")

vars_socio <- c("Asiste_Escuela", "Cobertura_Salud", "Nivel_Educacion_Jefe",
                "Quintil_Ingreso", "Tipo_Vivienda", "Material_Piso",
                "Material_Paredes", "Origen_Agua", "Tiene_Baño", "Desague_Baño",
                "Tiene_Electricidad", "Tiene_Heladera", "Combustible_Cocina",
                "Uso_Vivienda", "Indice_Hacinamiento", "Indice_NBI", "Indice_NSE",
                "Indice_Obesogenico_Cat", "Indice_Material_Cat")


vars_tabla1_cat <- c("Region", "Sexo", "Edad",
                     vars_fca,
                     "Sedentarismo_t", "ActFisica_t",
                     vars_entorno, vars_socio)

# 2.b Función: % por fila (ponderado) de Sin exceso / Exceso + N por categoría --
#   Para una variable, devuelve una fila por categoría con:
#     - % poblacional Sin exceso de peso
#     - % poblacional Con exceso de peso   (suman 100 dentro de la categoría)
#     - N muestral total de la categoría
tabla_var <- function(var, design, data) {
  # Tabla ponderada cruzada (estimación poblacional)
  tw <- svytable(as.formula(paste0("~ ", var, " + Target_bin")), design)
  if (ncol(tw) < 2) return(NULL)
  # Porcentajes POR FILA (dentro de cada categoría de la variable)
  pct <- prop.table(tw, margin = 1) * 100
  # N muestral total por categoría (no ponderado)
  n_cat <- as.numeric(table(data[[var]]))
  names(n_cat) <- names(table(data[[var]]))
  
  tibble(
    Variable  = var,
    Categoria = rownames(tw),
    SinExceso = round(pct[, "0"], 1),
    Exceso    = round(pct[, "1"], 1),
    Total     = n_cat[rownames(tw)]
  )
}

# 2.c Construcción de la tabla completa ----------------------------------------
tabla1_resumen <- map_dfr(vars_tabla1_cat,
                        ~ tabla_var(.x, diseno, datos_cat))

cat("\n=== TABLA 1 (% por fila ponderado + N) ===\n")
print(tabla1_resumen, n = Inf)

# 2.d Versión gtsummary -------------------
#   tbl_svysummary con percent="row": cada celda muestra el % de la categoría
#   que cae en esa columna (Sin exceso / Exceso). add_n() agrega el N total.
#   SIN add_p() -> sin warnings y sin columna de p-valores.
diseno_t1 <- svydesign(
  ids = ~1, strata = ~Estrato, weights = ~Ponderador,
  data = datos_cat %>%
    mutate(Target_lab = factor(
      ifelse(Target_bin == 1, "Exceso de peso", "Sin exceso de peso"),
      levels = c("Sin exceso de peso", "Exceso de peso"))),
  nest = TRUE)

tabla1 <- tbl_svysummary(
  diseno_t1,
  by = Target_lab,
  include = all_of(vars_tabla1_cat),
  #label = etiquetas,
  statistic = all_categorical() ~ "{p}%",   # solo el porcentaje
  percent = "row",                           # porcentajes POR FILA
  digits = all_categorical() ~ 1,
  missing = "no"
) %>%
  add_n(col_label = "**Total**") %>%          # N total por categoría
  modify_header(label ~ "**Categoría**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Exceso de peso**") %>%
  bold_labels()


# ══════════════════════════════════════════════════════════════════════════════
# COEFICIENTE V DE CRAMÉR PONDERADO
#   Mide la asociación de cada predictora categórica con Target_bin a nivel 
#   poblacional (frecuencias ponderadas). Se grafica ordenado de mayor a menor.
# ══════════════════════════════════════════════════════════════════════════════

# 3.a Función: V de Cramér a partir de una tabla de contingencia ponderada -----
cramer_v_ponderado <- function(var, design) {
  # Tabla de contingencia ponderada (estimación poblacional de frecuencias)
  tab <- svytable(as.formula(paste0("~ ", var, " + Target_bin")), design)
  if (any(dim(tab) < 2)) return(NA_real_)        # variable sin variabilidad
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  n   <- sum(tab)
  k   <- min(nrow(tab), ncol(tab))
  as.numeric(sqrt(chi$statistic / (n * (k - 1))))
}

# 3.b Variables categóricas candidatas -----------------------------------------
#   Incluye demográficas, FCA (3 niveles), conductuales en terciles,
#   entorno escolar y sociodemográficas categóricas.
vars_cramer <- c(
  "Region", "Sexo", "Edad_f",
  vars_fca,
  "Sedentarismo_t", "ActFisica_t",
  "Hace_ActFisica_Escuela", "Escuela_Provee_Alimento", "Escuela_Tiene_Kiosco",
  "Compro_En_Kiosco", "Kiosco_Compro_No_Recomendados", "Kiosco_Compro_Recomendados",
  "Asiste_Escuela", "Cobertura_Salud", "Nivel_Educacion_Jefe", "Quintil_Ingreso",
  "Tipo_Vivienda", "Indice_Hacinamiento", "Indice_NBI", "Indice_NSE",
  "Indice_Obesogenico_Cat", "Indice_Material_Cat"
)

# 3.c Cálculo del V de Cramér ponderado para cada variable ---------------------
cramer_tab <- tibble(
  variable = vars_cramer,
  cramer_v = map_dbl(vars_cramer, ~ cramer_v_ponderado(.x, diseno))
) %>%
  filter(!is.na(cramer_v)) %>%
  arrange(desc(cramer_v))

cat("\n=== Cramér's V ponderado (desc) ===\n")
print(cramer_tab, n = Inf)

# 3.d Figura: barras horizontales ordenadas -------------------
fig_cramer <- cramer_tab %>%
  mutate(variable = fct_reorder(variable, cramer_v)) %>%
  ggplot(aes(x = variable, y = cramer_v)) +
  geom_col(fill = "#2c7fb8") +
  geom_text(aes(label = sprintf("%.3f", cramer_v)), hjust = -0.15, size = 3) +
  coord_flip() +
  labs(title = "Coeficiente de Cramer’s V",
       x = NULL, y = "Cramér's V (asociación)") +
  ylim(0, max(cramer_tab$cramer_v) * 1.15) +
  theme_minimal(base_size = 11)

print(fig_cramer)

# ══════════════════════════════════════════════════════════════════════════════
# BLOQUES COMPLEMENTARIOS
# ══════════════════════════════════════════════════════════════════════════════

# ── Tabla de valores faltantes por variable ─────────────────────────

na_summary <- datos_cat %>%
  summarise(across(all_of(vars_tabla1_cat), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(porc_na = round(100 * n_na / nrow(datos_cat), 1)) %>%
  arrange(desc(n_na))
cat("\n=== Valores faltantes por variable (desc) ===\n")
print(na_summary, n = Inf)

# ── Descriptivos univariados de las continuas ───────────────────────
#   Utilidad: media/DE/mediana/percentiles de las continuas.
vars_continuas <- c("Edad", "IMC", "HorasPorDia_Sedentarismo",
                    "Horas_PorDia_ActFisica", "HorasPorDia_Sueño",
                    "Ingreso_Imputado_Por_UC", "Cant_Miembros", "Cant_Habitaciones")
desc_continuas <- datos_cat %>%
  select(any_of(vars_continuas)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarise(
    n      = sum(!is.na(valor)),
    media  = round(mean(valor, na.rm = TRUE), 2),
    de     = round(sd(valor,   na.rm = TRUE), 2),
    mediana= round(median(valor, na.rm = TRUE), 2),
    p25    = round(quantile(valor, 0.25, na.rm = TRUE), 2),
    p75    = round(quantile(valor, 0.75, na.rm = TRUE), 2),
    min    = round(min(valor, na.rm = TRUE), 2),
    max    = round(max(valor, na.rm = TRUE), 2),
    .groups = "drop"
  )
cat("\n=== Descriptivos de variables continuas ===\n")
print(desc_continuas)

# ── Histogramas/densidades de conductuales por exceso de peso ───────
#   Utilidad: muestra la forma de las distribuciones de sedentarismo y act. física
#   según condición de exceso de peso.
datos_long_cond <- datos_cat %>%
  select(Target_Exceso_Peso, HorasPorDia_Sedentarismo,
         Horas_PorDia_ActFisica) %>%
  pivot_longer(-Target_Exceso_Peso, names_to = "variable", values_to = "horas") %>%
  mutate(variable = recode(variable,
                           HorasPorDia_Sedentarismo = "Sedentarismo (h/día)",
                           Horas_PorDia_ActFisica   = "Actividad física (h/día)"))

fig_densidades <- ggplot(datos_long_cond,
                         aes(x = horas, fill = Target_Exceso_Peso)) +
  geom_density(alpha = 0.45) +
  facet_wrap(~ variable, scales = "free", ncol = 1) +
  labs(title = "Distribución de variables conductuales según exceso de peso",
       x = "Horas por día", y = "Densidad", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

print(fig_densidades)

# ── Prevalencia ponderada de exceso de peso por subgrupos ───────────
#   Gráficos de barras de prevalencia con IC95% logit por variable
#   sociodemográfica/demográfica. Función reutilizable; se grafican varias.
prevalencia_por_grupo <- function(var, design) {
  form <- as.formula(paste0("~ I(Target_bin == 1)"))
  by_f <- as.formula(paste0("~ ", var))
  est  <- svyby(form, by_f, design, svyciprop, vartype = "ci", method = "logit")
  names(est) <- c("nivel", "prev", "li", "ls")
  est$variable <- var
  est
}

vars_prev <- c("Edad_f", "Sexo", "Region", "Indice_NSE", "Indice_Material_Cat",
               "Indice_NBI", "Indice_Hacinamiento", "Cobertura_Salud",
               "Nivel_Educacion_Jefe")

prev_list <- map(vars_prev, ~ tryCatch(prevalencia_por_grupo(.x, diseno),
                                       error = function(e) NULL))
prev_all  <- bind_rows(prev_list)

# Figura combinada (todas las variables).
fig_prev <- prev_all %>%
  ggplot(aes(x = nivel, y = prev)) +
  geom_col(fill = "#41b6c4") +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Prevalencia ponderada de exceso de peso por subgrupos (IC95%)",
       x = NULL, y = "Prevalencia") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

print(fig_prev)

#   Sub-figura: prevalencia por edad y sexo (la más relevante)
fig_prev_edad_sexo <- prev_all %>%
  filter(variable %in% c("Edad_f", "Sexo")) %>%
  mutate(variable = recode(variable, Edad_f = "Edad", Sexo = "Sexo")) %>%
  ggplot(aes(x = nivel, y = prev)) +
  geom_col(fill = "#2c7fb8") +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Prevalencia ponderada de exceso de peso por edad y sexo (IC95%)",
       x = NULL, y = "Prevalencia") +
  theme_minimal(base_size = 11)

print(fig_prev_edad_sexo)
