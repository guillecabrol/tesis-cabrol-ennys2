###############################################################################
# CLUSTERING DE ADOLESCENTES ARGENTINOS — ENNyS 2
# Método: PAM (k-medoids) con distancia de Gower
###############################################################################

# ============================================================================
# 0. LIBRERÍAS Y SEMILLA
# ============================================================================
library(cluster)      # daisy(), pam()
library(dplyr)
library(tidyr)
library(ggplot2)
library(survey)       # caracterización ponderada
library(factoextra)   # silueta

set.seed(1213)

# ============================================================================
# 1. CARGA Y PREPARACIÓN
# ============================================================================
df <- read.csv("dataset.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
cat("Dimensiones originales:", dim(df), "\n")

# ----- 1.1 Variables de comportamiento -----
vars_horas <- c("HorasPorDia_Sedentarismo", "Horas_PorDia_ActFisica")
vars_terciles <- c("Sedentarismo_t", "ActFisica_t")

# ----- 1.2 Variables alimentarias: 3 frecuencias individuales -----
niveles_frec <- c("Consumo ocasional", "Consumo semanal", "Consumo diario")
frec_a_num   <- function(x) as.integer(factor(x, levels = niveles_frec))  # 1-2-3

df$Frutas_n     <- frec_a_num(df$Frec_Frutas)
df$Verduras_n   <- frec_a_num(df$Frec_Verduras)
df$Azucaradas_n <- frec_a_num(df$Frec_Bebidas_Azucaradas)

vars_dieta <- c("Frutas_n", "Verduras_n", "Azucaradas_n")

cat("Resumen de las 3 frecuencias (1-3):\n")
print(summary(df[vars_dieta]))

# ----- 1.3 Estado_Peso: 3 categorías ordenadas (solo para caracterización) -----
df$Estado_Peso <- factor(
  dplyr::case_when(
    df$Cat_IMC %in% c("Peso normal", "Delgadez") ~ "Sin exceso de peso",
    df$Cat_IMC == "Sobrepeso"                     ~ "Sobrepeso",
    df$Cat_IMC == "Obesidad"                      ~ "Obesidad"
  ),
  levels = c("Sin exceso de peso", "Sobrepeso", "Obesidad"),
  ordered = TRUE
)

# ----- 1.4 Imputar HorasPorDia (mediana) y pasarlas a TERCILES -----
for (v in vars_horas) {
  na_count <- sum(is.na(df[[v]]))
  if (na_count > 0) {
    med <- median(df[[v]], na.rm = TRUE)
    df[[v]][is.na(df[[v]])] <- med
    cat("Imputados", na_count, "NAs en", v, "con mediana =", med, "\n")
  }
}

a_terciles <- function(x) {
  cortes <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  cut(x, breaks = c(-Inf, cortes, Inf),
      labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE, ordered_result = TRUE)
}
df$Sedentarismo_t <- a_terciles(df$HorasPorDia_Sedentarismo)
df$ActFisica_t    <- a_terciles(df$Horas_PorDia_ActFisica)
df$Sueño_t        <- a_terciles(df$HorasPorDia_Sueño)

# ----- 1.5 Subconjunto activo: 3 frecuencias + 3 terciles -----
vars_activas <- c(vars_dieta, "Sedentarismo_t", "ActFisica_t")
df_clust <- df[, c("id", vars_activas)]

# ----- 2. Gower -----
# Las 3 frecuencias son numéricas acotadas (1-3, sin outliers) → intervalares;
# los 3 terciles conductuales, ordinales. Cada variable aporta 1/6 a la
# distancia → dieta 3/6, comportamiento 3/6 (dominios balanceados).
tipos <- list(ordratio = c("Sedentarismo_t", "ActFisica_t"))
gower_dist <- daisy(df_clust[, vars_activas], metric = "gower", type = tipos)

# ============================================================================
# 3. NÚMERO ÓPTIMO DE CLUSTERS (silueta sobre PAM, k = 2..8)
# ============================================================================
k_range <- 2:8
sil_widths <- numeric(length(k_range))
cat("\n--- Silueta promedio por k ---\n")
for (i in seq_along(k_range)) {
  pam_fit <- pam(gower_dist, k = k_range[i], diss = TRUE)
  sil_widths[i] <- pam_fit$silinfo$avg.width
  cat("  k =", k_range[i], ": silueta =", round(sil_widths[i], 4), "\n")
}

df_sil <- data.frame(k = k_range, sil = sil_widths)
p_sil <- ggplot(df_sil, aes(k, sil)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = round(sil, 3)), vjust = -1, size = 3.5) +
  scale_x_continuous(breaks = k_range) +
  labs(title = "Silueta promedio vs k (PAM + Gower)",
       x = "Número de clusters (k)", y = "Ancho de silueta promedio") +
  theme_minimal(base_size = 12)
print(p_sil)

k_opt <- 3
cat("\n>>> k elegido:", k_opt, "<<<\n")

# ============================================================================
# 4. AJUSTE FINAL DE PAM
# ============================================================================
k_final <- k_opt

cat("\n--- Ajuste final con k =", k_final, "---\n")
pam_final <- pam(gower_dist, k = k_final, diss = TRUE)

df_clust$cluster <- factor(pam_final$clustering)
df$cluster <- df_clust$cluster

cat("\nTamaños de cluster:\n");      print(table(df_clust$cluster))
cat("\nProporciones:\n");            print(round(prop.table(table(df_clust$cluster)), 3))
cat("\n--- Medoides ---\n");         print(df_clust[pam_final$id.med, ])

sil_final <- silhouette(pam_final$clustering, gower_dist)
print(fviz_silhouette(sil_final, palette = "jco") +
        labs(title = paste("Silueta por individuo — PAM k =", k_final)))

# ============================================================================
# 5. TABLA ÚNICA DE CARACTERIZACIÓN POR CLUSTER (arsenal::tableby)
# ============================================================================
# install.packages("arsenal")
library(arsenal)

# --- 5.0 Preparar variables para la tabla --------------------------------
df$ind_obesidad <- as.integer(df$Cat_IMC == "Obesidad")
df$ind_exceso   <- as.integer(df$Cat_IMC %in% c("Sobrepeso", "Obesidad"))

# Limpiar "NS/NC" del entorno escolar
vars_a_limpiar <- c("Hace_ActFisica_Escuela", "Escuela_Tiene_Kiosco",
                    "Compro_En_Kiosco", "Kiosco_Compro_No_Recomendados",
                    "Indice_Obesogenico_Cat")
for (v in vars_a_limpiar) if (v %in% names(df)) df[[v]][df[[v]] == "NS/NC"] <- NA

# --- Arreglos para el Bloque B -------------------------------
# (a) Estado de peso SIN ordenar → arsenal usa chi-cuadrado
df$Estado_Peso_tab <- factor(as.character(df$Estado_Peso),
                             levels = c("Sin exceso de peso", "Sobrepeso", "Obesidad"))
# (b) Colapsar "Nunca asistió" (n=2) con "No asiste pero asistió" → "No asiste",
#     para no desestabilizar el chi-cuadrado con celdas casi vacías.
df$Asiste_Escuela_tab <- dplyr::case_when(
  df$Asiste_Escuela %in% c("No asiste pero asistió", "Nunca asistió") ~ "No asiste",
  TRUE ~ df$Asiste_Escuela)
# (c) Hacinamiento a 3 categorías (juntar "Holgado" con "Sin hacinamiento"),
#     para que sea consistente con las corridas ponderadas previas.
df$Indice_Hacinamiento_tab <- dplyr::case_when(
  df$Indice_Hacinamiento %in% c("Holgado", "Sin hacinamiento") ~ "Sin hacinamiento",
  TRUE ~ df$Indice_Hacinamiento)
# (d) Quintil de ingreso como factor para la caracterización
df$Quintil_Ingreso_f <- factor(df$Quintil_Ingreso, levels = 1:5,
                               labels = c("Q1 (Bajo)", "Q2", "Q3", "Q4", "Q5 (Alto)"))

# ---------------------------------------------------------------------------------------------
# BLOQUE A — VARIABLES ACTIVAS (las que formaron los clusters)        
# ---------------------------------------------------------------------------------------------
# Reporto las 3 frecuencias (media en escala 1-3) y las horas CONTINUAS
# de comportamiento (media), más interpretables que los terciles.
labels_activas <- list(
  Frutas_n                 = "Frutas (frec. media 1-3)",
  Verduras_n               = "Verduras (frec. media 1-3)",
  Azucaradas_n             = "Bebidas azucaradas (frec. media 1-3)",
  Horas_PorDia_ActFisica   = "Actividad física (h/día)",
  HorasPorDia_Sedentarismo = "Sedentarismo (h/día)"
)

tab_activas <- tableby(
  cluster ~ Frutas_n + Verduras_n + Azucaradas_n +
    Horas_PorDia_ActFisica + HorasPorDia_Sedentarismo,
  data = df, numeric.stats = c("meansd", "median"),
  digits = 3
)

cat("\n================ BLOQUE A: VARIABLES ACTIVAS ================\n")
print(summary(tab_activas, labelTranslations = labels_activas, text = TRUE))

# --------------------------------------------------------------------------------------------
# BLOQUE B — CARACTERIZACIÓN (variables externas al armado)                          
# --------------------------------------------------------------------------------------------
labels_caract <- list(
  Estado_Peso_tab          = "Estado nutricional",
  Edad                     = "Edad (años)",
  Quintil_Ingreso_f        = "Quintil de ingreso",
  Sexo                     = "Sexo",
  Region                   = "Región",
  HorasPorDia_Sueño        = "Sueño (h/día)",
  Cobertura_Salud          = "Cobertura de salud",
  Nivel_Educacion_Jefe     = "Educación del jefe/a",
  Indice_NSE               = "NSE",
  Indice_NBI               = "NBI",
  Indice_Hacinamiento_tab  = "Hacinamiento",
  Indice_Material_Cat      = "Índice material",
  Asiste_Escuela_tab       = "Asistencia escolar",
  Escuela_Provee_Alimento  = "Escuela provee alimento",
  Indice_Obesogenico_Cat   = "Índice obesogénico escolar"
)

tab_caract <- tableby(
  cluster ~ Estado_Peso_tab + Edad + Quintil_Ingreso_f + Sexo + Region  + HorasPorDia_Sueño +
    Cobertura_Salud + Nivel_Educacion_Jefe + Indice_NSE + Indice_NBI +
    Indice_Hacinamiento_tab + Indice_Material_Cat + Asiste_Escuela_tab +
    Escuela_Provee_Alimento + Indice_Obesogenico_Cat,
  data = df, numeric.stats = c("meansd"),
  cat.stats = c("countpct"), digits = 2
)

cat("\n================ BLOQUE B: CARACTERIZACIÓN ================\n")
print(summary(tab_caract, labelTranslations = labels_caract, text = TRUE))

# ============================================================================
# 6. PREVALENCIA PONDERADA DE PESO POR CLUSTER (survey)
# ============================================================================
# IMPORTANTE: arsenal NO pondera.
options(survey.lonely.psu = "adjust")
diseno <- svydesign(ids = ~1, strata = ~Estrato, weights = ~Ponderador,
                    data = df, nest = TRUE)

prev_ob <- svyby(~ind_obesidad, ~cluster, diseno, svyciprop, vartype = "ci", method = "logit")
prev_ex <- svyby(~ind_exceso,  ~cluster, diseno, svyciprop, vartype = "ci", method = "logit")

prev_cluster <- tibble(
  cluster  = prev_ob$cluster,
  obesidad = round(100 * prev_ob$ind_obesidad, 1),
  ob_lo    = round(100 * prev_ob$ci_l, 1),
  ob_hi    = round(100 * prev_ob$ci_u, 1),
  exceso   = round(100 * prev_ex$ind_exceso, 1),
  ex_lo    = round(100 * prev_ex$ci_l, 1),
  ex_hi    = round(100 * prev_ex$ci_u, 1)
)
cat("\n--- Prevalencia ponderada de peso por cluster (IC95%) ---\n")
print(prev_cluster)

# ============================================================================
# 7. GRÁFICOS SELECCIONADOS (complemento de la tabla)
# ============================================================================

# ── G1. Prevalencia ponderada de OBESIDAD y EXCESO por cluster (con IC) ────
ob_global <- 100 * as.numeric(svyciprop(~ind_obesidad, diseno, method = "logit"))
ex_global <- 100 * as.numeric(svyciprop(~ind_exceso,   diseno, method = "logit"))

d_prev <- bind_rows(
  prev_cluster %>% transmute(cluster, medida = "Obesidad",
                             pct = obesidad, lo = ob_lo, hi = ob_hi),
  prev_cluster %>% transmute(cluster, medida = "Exceso de peso",
                             pct = exceso, lo = ex_lo, hi = ex_hi)
) %>%
  mutate(medida = factor(medida, levels = c("Obesidad", "Exceso de peso")))

d_lineas <- data.frame(medida = factor(c("Obesidad", "Exceso de peso"),
                                       levels = c("Obesidad", "Exceso de peso")),
                       global = c(ob_global, ex_global))

p_g1 <- ggplot(d_prev, aes(cluster, pct, fill = cluster)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.7, size = 3.6) +
  # geom_hline(data = d_lineas, aes(yintercept = global),
  #            linetype = "dashed", color = "red") +
  facet_wrap(~medida) +
  labs(title = "Prevalencia ponderada de obesidad y exceso de peso por cluster",
       #subtitle = "IC95% (método logit) | línea punteada: prevalencia global",
       x = "Cluster", y = "%") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
print(p_g1)

# ── G2. Perfil conductual: % en cada tercil por cluster (variables activas) ─
# (No ponderado: describe la partición en sus propias variables)
perfil_terc <- df %>%
  pivot_longer(c(Sedentarismo_t, ActFisica_t),
               names_to = "variable", values_to = "nivel") %>%
  count(cluster, variable, nivel) %>%
  group_by(cluster, variable) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(variable = recode(variable,
                           Sedentarismo_t = "Sedentarismo",
                           ActFisica_t    = "Actividad física"),
         nivel = factor(nivel, levels = c("Bajo", "Medio", "Alto")))

p_g2 <- ggplot(perfil_terc, aes(cluster, nivel, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct), "%")), size = 3.2) +
  facet_wrap(~variable, ncol = 3) +
  scale_fill_gradient(low = "white", high = "#185FA5", name = "%") +
  labs(title = "Perfil conductual de los clusters",
       subtitle = "% de cada cluster en cada tercil",
       x = "Cluster", y = "Tercil") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"))
print(p_g2)

# ── G2b. Perfil alimentario: % "Consumo diario" de cada frecuencia por cluster ─
# (Las 3 frecuencias también son activas: este gráfico las describe.)
perfil_dieta <- df %>%
  transmute(cluster,
            Frutas    = Frec_Frutas,
            Verduras  = Frec_Verduras,
            Azucaradas = Frec_Bebidas_Azucaradas) %>%
  pivot_longer(-cluster, names_to = "alimento", values_to = "nivel") %>%
  count(cluster, alimento, nivel) %>%
  group_by(cluster, alimento) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  filter(nivel == "Consumo diario")

p_g2b <- ggplot(perfil_dieta, aes(cluster, alimento, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct), "%")), size = 3.2) +
  scale_fill_gradient(low = "white", high = "#185FA5", name = "% diario") +
  labs(title = "Perfil alimentario de los clusters",
       subtitle = "% de cada cluster con consumo diario",
       x = "Cluster", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())
print(p_g2b)

# ── G3. Socioeconómicas significativas por cluster (ponderado) ──────────────
comp_long <- function(var) {
  tab <- svytable(as.formula(paste0("~cluster + ", var)), design = diseno)
  d <- as.data.frame(prop.table(tab, margin = 1) * 100)
  names(d) <- c("cluster", "categoria", "pct")
  d$var_orig <- var
  d
}

vars_g3 <- c("Nivel_Educacion_Jefe", "Indice_NBI", "Cobertura_Salud", "Indice_NSE")
etiq_g3 <- c(Nivel_Educacion_Jefe = "Educación del jefe/a",
             Indice_NBI = "NBI",
             Cobertura_Salud = "Cobertura de salud",
             Indice_NSE = "NSE")
orden_g3 <- list(
  Nivel_Educacion_Jefe = c("Bajo", "Medio", "Alto"),
  Indice_NBI           = c("Con", "Sin"),
  Cobertura_Salud      = c("Obra", "Solo"),
  Indice_NSE           = c("D2E", "D1", "C3", "C2", "ABC1")
)

d_g3 <- bind_rows(lapply(vars_g3, comp_long))
niveles <- unlist(lapply(vars_g3, function(v) {
  cats <- unique(as.character(d_g3$categoria[d_g3$var_orig == v]))
  pats <- orden_g3[[v]]
  rk <- sapply(cats, function(cc) {
    hit <- which(sapply(pats, function(p) grepl(p, cc, fixed = TRUE)))
    if (length(hit)) hit[1] else Inf
  })
  cats[order(rk)]
}))
d_g3$categoria <- factor(d_g3$categoria, levels = unique(niveles))
d_g3$variable  <- factor(etiq_g3[d_g3$var_orig], levels = etiq_g3)

p_g3 <- ggplot(d_g3, aes(cluster, categoria, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct), "%")), size = 3) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_fill_gradient(low = "white", high = "#185FA5", name = "%") +
  labs(title = "Composición socioeconómica por cluster (ponderada)",
       subtitle = "Variables con asociación significativa (Rao-Scott)",
       x = "Cluster", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 8))
print(p_g3)

# ── G4. Entorno escolar por cluster (ponderado) ─────────────────────────────
# Reutiliza comp_long() definida en el bloque G3.
vars_g4 <- c("Asiste_Escuela", "Escuela_Provee_Alimento", "Escuela_Tiene_Kiosco",
             "Compro_En_Kiosco", "Kiosco_Compro_No_Recomendados",
             "Hace_ActFisica_Escuela", "Indice_Obesogenico_Cat")
etiq_g4 <- c(Asiste_Escuela = "Asistencia escolar",
             Escuela_Provee_Alimento = "Escuela provee alimento",
             Escuela_Tiene_Kiosco = "Escuela tiene kiosco",
             Compro_En_Kiosco = "Compró en kiosco",
             Kiosco_Compro_No_Recomendados = "Kiosco: compró no recomendados",
             Hace_ActFisica_Escuela = "Act. física en escuela",
             Indice_Obesogenico_Cat = "Índice obesogénico escolar")
orden_g4 <- list(
  Asiste_Escuela                = c("estatal", "privado", "No asiste"),
  Escuela_Provee_Alimento       = c("No", "Si"),
  Escuela_Tiene_Kiosco          = c("No", "Sí", "No aplica"),
  Compro_En_Kiosco              = c("No", "Sí", "No aplica"),
  Kiosco_Compro_No_Recomendados = c("No", "Sí", "No aplica"),
  Hace_ActFisica_Escuela        = c("No", "Sí", "No aplica"),
  Indice_Obesogenico_Cat        = c("Bajo", "Moderado", "Alto")
)

d_g4 <- bind_rows(lapply(vars_g4, comp_long))
niveles_g4 <- unlist(lapply(vars_g4, function(v) {
  cats <- unique(as.character(d_g4$categoria[d_g4$var_orig == v]))
  pats <- orden_g4[[v]]
  rk <- sapply(cats, function(cc) {
    hit <- which(sapply(pats, function(p) grepl(p, cc, fixed = TRUE)))
    if (length(hit)) hit[1] else Inf
  })
  cats[order(rk)]
}))
d_g4$categoria <- factor(d_g4$categoria, levels = unique(niveles_g4))
d_g4$variable  <- factor(etiq_g4[d_g4$var_orig], levels = etiq_g4)

p_g4 <- ggplot(d_g4, aes(cluster, categoria, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct), "%")), size = 3) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_fill_gradient(low = "white", high = "#185FA5", name = "%") +
  labs(title = "Entorno escolar por cluster (ponderado)",
       x = "Cluster", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"),
        axis.text.y = element_text(size = 8))
print(p_g4)