# ============================================================
# REGRESIÓN LOGÍSTICA PONDERADA
# Etapa exploratoria: TODAS las FCA + conductuales (terciles)
# Modelo múltiple final: FCA seleccionadas + conductuales (terciles)
# + edad, sexo y región
# ============================================================

library(tidyverse)
library(survey)
library(broom)
library(mice)
library(pROC)          # AUC no ponderado
library(WeightedROC)   # AUC ponderado por diseño

options(survey.lonely.psu = "adjust")

# ── 0. TOGGLES ──────────────────────────────────────────────
#   "obesidad" -> Cat_IMC == "Obesidad" vs resto
#   "exceso"   -> Target_Exceso_Peso == "Exceso de peso"
target_elegido <- "Exceso de peso"

#   "factor"   -> un OR por edad (14/15/16/17 vs 13), no asume linealidad
#   "numerica" -> un OR por año adicional (asume efecto lineal en el logit)
edad_como <- "factor"

# ── 1. DATOS ────────────────────────────────────
datos <- read.csv("dataset.csv", na.strings = c("", " ", "NA"))

datos <- datos %>%
  mutate(
    y = if (target_elegido == "obesidad")
      as.integer(Cat_IMC == "Obesidad")
    else
      as.integer(Target_Exceso_Peso == "Exceso de peso"),
    Sexo       = relevel(factor(Sexo),   ref = "Femenino"),
    Region     = relevel(factor(Region), ref = "Patagonia"),
    Estrato    = factor(Estrato),
    Ponderador = as.numeric(Ponderador)
  )

# Edad: factor (ref 13) o numérica, según el toggle
if (edad_como == "factor") {
  datos$Edad <- relevel(factor(datos$Edad), ref = "13")
} else {
  datos$Edad <- as.numeric(as.character(datos$Edad))
}

vars_continuas <- c("HorasPorDia_Sedentarismo",
                    "Horas_PorDia_ActFisica",
                    "HorasPorDia_Sueño")

# ── 1b. VARIABLES DE FRECUENCIA DE CONSUMO (FCA) ────────────
# Todas las FCA categóricas
vars_fca <- c(
  "Frec_Lacteos", "Frec_Frutas", "Frec_Verduras", "Frec_Papas_Pastas",
  "Frec_Integrales_Legumbres", "Frec_Fiambres_Embutidos", "Frec_Carnes_Huevos",
  "Frec_Pescado", "Frec_Aceites", "Frec_FrutosSecos_Semillas", "Frec_Snacks_Copetin",
  "Frec_Golosinas", "Frec_Facturas_Pasteleria", "Frec_Preelaborados",
  "Frec_Bebidas_Light", "Frec_Bebidas_Azucaradas", "Frec_Agua"
)
vars_fca <- intersect(vars_fca, names(datos))

relevel_si_existe <- function(x, ref) {
  x <- factor(x)
  if (ref %in% levels(x)) x <- relevel(x, ref = ref)
  x
}
datos <- datos %>%
  mutate(across(all_of(vars_fca),
                ~ relevel_si_existe(.x, "Consumo ocasional")))

# ── 2. IMPUTACIÓN MICE (solo continuas con faltantes) ───────
set.seed(1213)
aux <- datos %>% select(all_of(vars_continuas), y, Edad, Sexo, Region)
metodos <- make.method(aux)
metodos[vars_continuas] <- "pmm"
imp  <- mice(aux, m = 5, method = metodos, seed = 1213, printFlag = FALSE)
comp <- complete(imp, 1)
datos[, vars_continuas] <- comp[, vars_continuas]

# ── 3. TERCILES (Bajo / Medio / Alto) ───────────────────────
# Cortes en los percentiles 33 y 67 de cada variable (sin ponderar).
a_terciles <- function(x) {
  cortes <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  cut(x, breaks = c(-Inf, cortes, Inf),
      labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE)
}

datos <- datos %>%
  mutate(
    Sedentarismo_t = relevel(a_terciles(HorasPorDia_Sedentarismo), ref = "Bajo"),
    ActFisica_t    = relevel(a_terciles(Horas_PorDia_ActFisica),   ref = "Bajo"),
    Sueño_t        = relevel(a_terciles(HorasPorDia_Sueño),        ref = "Bajo")
    # Para la hipótesis en U del sueño: relevel(..., ref = "Medio")
  )

cat("\n=== Cortes de terciles (percentiles 33 / 67) ===\n")
for (v in vars_continuas) {
  q <- quantile(datos[[v]], probs = c(1/3, 2/3), na.rm = TRUE)
  cat(sprintf("%-26s %.2f | %.2f\n", v, q[1], q[2]))
}

# ── 4. DISEÑO MUESTRAL ──────────────────────────────────────
diseno <- svydesign(ids = ~1, strata = ~Estrato, weights = ~Ponderador,
                    data = datos, nest = TRUE)

# ============================================================
# ETAPA EXPLORATORIA
# Modelos simples y tests globales sobre TODAS las variables:
# demográficas + conductuales (terciles) + todas las FCA.
# ============================================================

# Variables que se exploran de a una (OR crudo)
vars_explora <- c("Edad", "Sexo", "Region",
                  "Sedentarismo_t", "ActFisica_t",
                  vars_fca)

modelo_simple <- function(v) {
  svyglm(as.formula(paste("y ~", v)), design = diseno, family = quasibinomial())
}
modelos_s <- set_names(map(vars_explora, modelo_simple), vars_explora)

res_simples <- imap_dfr(modelos_s, function(m, v) {
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    transmute(variable = v, term,
              OR   = round(estimate, 2),
              IC95 = paste0(round(conf.low, 2), " \u2013 ", round(conf.high, 2)),
              p    = round(p.value, 4))
})
cat("\n=== Modelos simples (OR crudo de cada variable) ===\n")
print(res_simples, n = Inf)

tests_s <- imap_dfr(modelos_s, function(m, v) {
  t <- regTermTest(m, as.formula(paste("~", v)))
  tibble(variable = v, F = round(as.numeric(t$Ftest), 3),
         p_global = round(as.numeric(t$p), 4))
}) %>% arrange(p_global)
cat("\n=== Tests globales (modelos simples, ordenados por p) ===\n")
print(tests_s, n = Inf)

# ============================================================
# MODELO MÚLTIPLE FINAL
# ============================================================
# Predictores del modelo múltiple:
#   - Demográficas: Edad, Sexo, Region
#   - Conductuales (terciles): Sedentarismo y Act. física
#   - FCA seleccionadas: Frutas, Verduras, Bebidas azucaradas

vars_mult <- c("Edad", "Sexo", "Region",
               "Sedentarismo_t", "ActFisica_t",
               "Frec_Frutas", "Frec_Verduras", "Frec_Bebidas_Azucaradas")

formula_mult <- as.formula(paste("y ~", paste(vars_mult, collapse = " + ")))
modelo_mult  <- svyglm(formula_mult, design = diseno, family = quasibinomial())

res_mult <- tidy(modelo_mult, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  transmute(term,
            OR   = round(estimate, 2),
            IC95 = paste0(round(conf.low, 2), " \u2013 ", round(conf.high, 2)),
            p    = round(p.value, 4))
cat("\n=== Modelo múltiple — OR ajustado (IC95%) ===\n")
print(res_mult, n = Inf)

tests_m <- map_dfr(vars_mult, function(v) {
  t <- regTermTest(modelo_mult, as.formula(paste("~", v)))
  tibble(variable = v, F = round(as.numeric(t$Ftest), 3),
         p_global = round(as.numeric(t$p), 4))
}) %>% arrange(p_global)
cat("\n=== Tests globales (modelo múltiple) ===\n")
print(tests_m)

# ── FOREST PLOT (modelo múltiple) ───────────────────────────
etiquetas <- c(
  "Sedentarismo_tMedio" = "Sedentarismo: medio vs bajo",
  "Sedentarismo_tAlto"  = "Sedentarismo: alto vs bajo",
  "ActFisica_tMedio"    = "Act. f\u00edsica: media vs baja",
  "ActFisica_tAlto"     = "Act. f\u00edsica: alta vs baja",
  "Edad14" = "Edad: 14 vs 13", "Edad15" = "Edad: 15 vs 13",
  "Edad16" = "Edad: 16 vs 13", "Edad17" = "Edad: 17 vs 13",
  "Edad"   = "Edad (por año)",
  "SexoMasculino"   = "Sexo: masculino vs femenino",
  "RegionNEA"       = "Regi\u00f3n: NEA vs Patagonia",
  "RegionCuyo"      = "Regi\u00f3n: Cuyo vs Patagonia",
  "RegionCentro"    = "Regi\u00f3n: Centro vs Patagonia",
  "RegionGBA"       = "Regi\u00f3n: GBA vs Patagonia",
  "RegionNOA"       = "Regi\u00f3n: NOA vs Patagonia",
  "Frec_FrutasConsumo diario"             = "Frutas: diario vs ocasional",
  "Frec_FrutasConsumo semanal"            = "Frutas: semanal vs ocasional",
  "Frec_VerdurasConsumo diario"           = "Verduras: diario vs ocasional",
  "Frec_VerdurasConsumo semanal"          = "Verduras: semanal vs ocasional",
  "Frec_Bebidas_AzucaradasConsumo diario" = "Bebidas azucaradas: diario vs ocasional",
  "Frec_Bebidas_AzucaradasConsumo semanal"= "Bebidas azucaradas: semanal vs ocasional"
)

df_forest <- tidy(modelo_mult, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    etiqueta_txt = ifelse(term %in% names(etiquetas), etiquetas[term], term),
    etiqueta = factor(etiqueta_txt, levels = rev(unique(etiqueta_txt))),
    sig = case_when(
      p.value < 0.05 ~ "Significativa (p < 0.05)",
      p.value < 0.10 ~ "Borderline (0.05\u20130.10)",
      TRUE           ~ "No significativa (p \u2265 0.10)"
    ),
    sig = factor(sig, levels = c("Significativa (p < 0.05)",
                                 "Borderline (0.05\u20130.10)",
                                 "No significativa (p \u2265 0.10)"))
  )

colores <- c("Significativa (p < 0.05)"        = "#185FA5",
             "Borderline (0.05\u20130.10)"      = "#BA7517",
             "No significativa (p \u2265 0.10)" = "#888780")

p_forest <- ggplot(df_forest, aes(x = estimate, y = etiqueta, color = sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.2, linewidth = 0.8, orientation = "y") +
  geom_point(size = 3) +
  scale_x_log10("Odds Ratio (IC95%, escala log.)") +
  scale_color_manual(name = NULL, values = colores) +
  labs(
    title    = "Regresi\u00f3n log\u00edstica m\u00faltiple ponderada",
    # subtitle = paste0("Desenlace: ", target_elegido,
    #                   " | terciles ref: Bajo | FCA ref: ocasional | edad: ", edad_como),
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5, size = 11.5),
    plot.subtitle      = element_text(color = "gray40", hjust = 0.5, size = 9),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p_forest)

# ============================================================
# EVALUACIÓN DEL MODELO MÚLTIPLE
# AUC ponderado + validación cruzada k = 10 + sensibilidad / F2
# ------------------------------------------------------------

# ── 1. AUC IN-SAMPLE (no ponderado y ponderado) ─────────────
datos_eval <- datos %>%
  mutate(prob_pred = as.numeric(predict(modelo_mult, type = "response")),
         peso_norm = Ponderador / mean(Ponderador))

roc_in <- roc(datos_eval$y, datos_eval$prob_pred, quiet = TRUE)
ci_in  <- ci.auc(roc_in)
cat(sprintf("\nAUC in-sample (no pond.)  = %.4f  [%.4f \u2013 %.4f]\n",
            as.numeric(auc(roc_in)), ci_in[1], ci_in[3]))

wauc_in <- WeightedAUC(WeightedROC(guess = datos_eval$prob_pred,
                                   label = datos_eval$y,
                                   weight = datos_eval$peso_norm))
cat(sprintf("AUC in-sample (ponderado) = %.4f\n", wauc_in))

# ── 2. VALIDACIÓN CRUZADA k = 10 ────────────────────────────
# En cada fold: recrea el diseño en train, ajusta formula_mult,
# predice en test. (Los terciles ya están fijados sobre toda la
# muestra.)
set.seed(1213)
k     <- 10
folds <- sample(rep(1:k, length.out = nrow(datos)))

res_cv <- map_dfr(1:k, function(i) {
  tr <- datos[folds != i, ]
  te <- datos[folds == i, ]
  dis_tr <- svydesign(ids = ~1, strata = ~Estrato, weights = ~Ponderador,
                      data = tr, nest = TRUE)
  m <- tryCatch(svyglm(formula_mult, design = dis_tr, family = quasibinomial()),
                error = function(e) NULL)
  if (is.null(m)) return(NULL)
  tibble(fold = i,
         obs  = te$y,
         prob = as.numeric(predict(m, newdata = te, type = "response")),
         peso = te$Ponderador)
})

roc_cv  <- roc(res_cv$obs, res_cv$prob, quiet = TRUE)
ci_cv   <- ci.auc(roc_cv)
peso_cv <- res_cv$peso / mean(res_cv$peso)
wauc_cv <- WeightedAUC(WeightedROC(guess = res_cv$prob, label = res_cv$obs,
                                   weight = peso_cv))

auc_por_fold <- res_cv %>%
  group_by(fold) %>%
  summarise(auc = as.numeric(auc(roc(obs, prob, quiet = TRUE))), .groups = "drop")

cat("\n=== AUC validación cruzada (k = 10) ===\n")
cat(sprintf("AUC CV (no pond.)  = %.4f  [%.4f \u2013 %.4f]\n",
            as.numeric(auc(roc_cv)), ci_cv[1], ci_cv[3]))
cat(sprintf("AUC CV (ponderado) = %.4f\n", wauc_cv))
cat(sprintf("Por fold \u2014 media %.4f | SD %.4f | rango %.4f\u2013%.4f\n",
            mean(auc_por_fold$auc), sd(auc_por_fold$auc),
            min(auc_por_fold$auc), max(auc_por_fold$auc)))

# ── 3. UMBRAL ÓPTIMO (Youden) Y MÉTRICAS ────────────────────
youden <- coords(roc_cv, x = "best", best.method = "youden",
                 ret = c("threshold", "sensitivity", "specificity"))
cat("\n=== Punto óptimo de Youden (CV) ===\n")
print(youden)

umbral <- 0.33  # <- fijalo a mano si querés

pred <- ifelse(res_cv$prob >= umbral, 1, 0)
obs  <- res_cv$obs
beta <- 2

# Métricas no ponderadas
vp <- sum(pred == 1 & obs == 1)
fp <- sum(pred == 1 & obs == 0)
fn <- sum(pred == 0 & obs == 1)
prec <- vp / (vp + fp)
rec  <- vp / (vp + fn)                       # sensibilidad
f2   <- (1 + beta^2) * prec * rec / (beta^2 * prec + rec)

# Métricas ponderadas
w   <- res_cv$peso / mean(res_cv$peso)
vpw <- sum(w * (pred == 1 & obs == 1))
fpw <- sum(w * (pred == 1 & obs == 0))
fnw <- sum(w * (pred == 0 & obs == 1))
precw <- vpw / (vpw + fpw)
recw  <- vpw / (vpw + fnw)
f2w   <- (1 + beta^2) * precw * recw / (beta^2 * precw + recw)

cat("\n=== Métricas en el umbral seleccionado ===\n")
cat(sprintf("Umbral:              %.3f\n", umbral))
cat(sprintf("Sensibilidad:        %.4f   (ponderada %.4f)\n", rec, recw))
cat(sprintf("F2-score (beta = 2): %.4f   (ponderado %.4f)\n", f2, f2w))

