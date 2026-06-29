# ============================================================
# RANDOM FOREST — EXCESO DE PESO / OBESIDAD EN ADOLESCENTES
# Solo variables de comportamiento (en terciles) + demográficas + FCA (SOLO frutas, verduras y beb. azucaradas).
# Framework: tidymodels | Motor: ranger
# ============================================================

library(tidymodels)
library(ranger)
library(vip)
library(pROC)
library(tidyverse)

# ── 0. TOGGLE ──────────────────────────────────
#   "exceso"   -> Target_bin (exceso de peso)  [como el script original]
#   "obesidad" -> Cat_IMC == "Obesidad" vs resto
target_elegido <- "exceso"

# ── 1. CARGA, TERCILES Y SELECCIÓN DE VARIABLES ─────────────
df <- read.csv("dataset.csv", stringsAsFactors = TRUE)

a_terciles <- function(x) {
  cortes <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  cut(x,
      breaks = c(-Inf, cortes, Inf),
      labels = c("Bajo", "Medio", "Alto"),
      include.lowest = TRUE)
}

df <- df %>%
  mutate(
    Sedentarismo_t = a_terciles(HorasPorDia_Sedentarismo),
    ActFisica_t    = a_terciles(Horas_PorDia_ActFisica),
    Sueño_t        = a_terciles(HorasPorDia_Sueño),
    
    y_bin = if (target_elegido == "obesidad")
      as.integer(Cat_IMC == "Obesidad")
    else
      as.integer(Target_bin)
  )

# ------------------------------------------------------------
# Variables del modelo
# ------------------------------------------------------------

vars_modelo <- c(
  "Edad",
  "Sexo",
  "Region",
  
  "Sedentarismo_t",
  "ActFisica_t",
  
  "Frec_Frutas",
  "Frec_Verduras",
  "Frec_Bebidas_Azucaradas"
)

df_modelo <- df %>%
  select(all_of(vars_modelo), y_bin) %>%
  mutate(
    Target_bin = factor(
      y_bin,
      levels = c(1, 0),
      labels = c("Si", "No")
    )
  ) %>%
  select(-y_bin) %>%
  filter(!is.na(Target_bin))

cat("Desenlace:", target_elegido, "\n")
cat("Distribución del target:\n")
print(prop.table(table(df_modelo$Target_bin)))
cat("Dimensiones:", dim(df_modelo), "\n")

# ── 2. RECETA ───────────────────────────────────────────────
# Imputación dentro de cada fold (evita leakage):
#   - moda para los factores (incluye los NAs de los terciles)
#   - mediana para numéricas (solo Edad, sin NAs → inocuo)
receta <- recipe(Target_bin ~ ., data = df_modelo) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# ── 3. MODELO ───────────────────────────────────────────────
p <- ncol(df_modelo) - 1
cat("\nPredictores:", p, "| sqrt(p):", round(sqrt(p)), "\n")

rf_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
  set_engine("ranger", importance = "permutation", seed = 1213) %>%
  set_mode("classification")

wf <- workflow() %>% add_recipe(receta) %>% add_model(rf_spec)

# ── 4. CV ESTRATIFICADO (10 folds, comparable con RegLog) ───
set.seed(1213)
folds_rf <- vfold_cv(df_modelo, v = 10, strata = Target_bin)

# ── 5. GRID DE HIPERPARÁMETROS ──────────────────────────────
# Con 6 predictoras, mtry no puede pasar de 6 → rango 2–5.
n_pred <- ncol(df_modelo) - 1

rf_grid <- grid_regular(
  mtry(range  = c(2, max(2, n_pred - 1))),
  min_n(range = c(5, 40)),
  levels = 4
)
cat("Combinaciones a explorar:", nrow(rf_grid), "\n")

# ── 6. GRID SEARCH ──────────────────────────────────────────
cat("\nEntrenando grid search...\n")
set.seed(1213)
rf_tune <- tune_grid(
  wf,
  resamples = folds_rf,
  grid      = rf_grid,
  metrics   = metric_set(roc_auc, accuracy,
                         yardstick::sensitivity, yardstick::specificity),
  control   = control_grid(save_pred = TRUE, verbose = TRUE)
)

# ── 7. MEJORES HIPERPARÁMETROS ──────────────────────────────
cat("\n--- Top combinaciones por AUC ---\n")
show_best(rf_tune, metric = "roc_auc", n = 8) %>% print()

autoplot(rf_tune, metric = "roc_auc") +
  labs(title = "AUC según hiperparámetros (CV 10 folds)") +
  theme_minimal(base_size = 12)

mejor_params <- select_best(rf_tune, metric = "roc_auc")
cat("\nMejores hiperparámetros:\n"); print(mejor_params)

# ── 8. AUC CV GLOBAL (OOF) ──────────────────────────────────
pred_cv_rf <- collect_predictions(rf_tune, parameters = mejor_params)

roc_cv_rf <- roc(pred_cv_rf$Target_bin, pred_cv_rf$.pred_Si,
                 levels = c("No", "Si"), direction = "<", quiet = TRUE)
auc_cv_rf <- auc(roc_cv_rf)
ci_cv_rf  <- ci.auc(roc_cv_rf)
cat(sprintf("\nAUC CV global (OOF): %.4f [IC 95%%: %.4f\u2013%.4f]\n",
            auc_cv_rf, ci_cv_rf[1], ci_cv_rf[3]))

# ── 9. MODELO FINAL (para importancia) ──────────────────────
wf_final <- finalize_workflow(wf, mejor_params)
set.seed(1213)
modelo_final <- fit(wf_final, data = df_modelo)
rf_fit <- extract_fit_parsnip(modelo_final)

# ── 10. IMPORTANCIA DE VARIABLES (incluye demográficas) ─────
# Ahora Edad/Sexo/Región SÍ se grafican: son predictoras.
imp_df <- vi(rf_fit$fit, scale = TRUE) %>%
  mutate(
    Variable = fct_reorder(Variable, Importance),
    
    grupo = case_when(
      
      str_detect(
        as.character(Variable),
        "Sedentarismo|ActFisica|Sue"
      ) ~ "Comportamiento",
      
      str_detect(
        as.character(Variable),
        "Frec_Frutas|Frec_Verduras|Frec_Bebidas_Azucaradas"
      ) ~ "Alimentación",
      
      TRUE ~ "Demográficas"
    )
  )

cat("\n--- Importancia de variables ---\n")
print(imp_df)

colores_imp <- c(
  "Comportamiento" = "#185FA5",
  "Alimentación"   = "#5B8C5A",
  "Demográficas"   = "#BA7517"
)

p_imp <- ggplot(imp_df, aes(x = Importance, y = Variable, fill = grupo)) +
  geom_col(width = 0.7) +
  scale_fill_manual(name = NULL, values = colores_imp) +
  scale_x_continuous(name = "Importancia relativa (escala 0\u2013100)",
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Importancia de variables \u2014 Random Forest",
       # subtitle = paste0("Permutaci\u00f3n | ranger | ntree = 500 | desenlace: ",
       #                   target_elegido),
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(color = "gray40", size = 9, hjust = 0.5),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
print(p_imp)

# ── 11. UMBRAL ÓPTIMO (Youden) Y MÉTRICAS ───────────────────
youden_rf <- coords(roc_cv_rf, x = "best", best.method = "youden",
                    ret = c("threshold", "sensitivity", "specificity"))
cat("\n--- Umbral óptimo de Youden (RF, CV) ---\n"); print(youden_rf)

umbral <- 0.28  

pred_binaria <- ifelse(pred_cv_rf$.pred_Si >= umbral, "Si", "No")
obs_real     <- as.character(pred_cv_rf$Target_bin)

vp <- sum(pred_binaria == "Si" & obs_real == "Si")
fp <- sum(pred_binaria == "Si" & obs_real == "No")
fn <- sum(pred_binaria == "No" & obs_real == "Si")
vn <- sum(pred_binaria == "No" & obs_real == "No")  

precision     <- vp / (vp + fp)
recall        <- vp / (vp + fn)
especificidad <- vn / (vn + fp)
beta <- 2
f2 <- (1 + beta^2) * precision * recall / ((beta^2 * precision) + recall)

cat("\n========================================\n")
cat("MÉTRICAS EN EL UMBRAL (RF)\n")
cat("========================================\n")
cat(sprintf("Umbral:        %.3f\n", umbral))
cat(sprintf("Sensibilidad:  %.4f\n", recall))
cat(sprintf("Especificidad: %.4f\n", especificidad))
cat(sprintf("F2-score:      %.4f\n", f2))
cat("========================================\n")

# ── 12. CURVA ROC ───────────────────────────────────────────
p_roc <- ggplot(data.frame(fpr = 1 - roc_cv_rf$specificities,
                           tpr = roc_cv_rf$sensitivities),
                aes(x = fpr, y = tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray60", linewidth = 0.5) +
  geom_line(color = "#185FA5", linewidth = 1) +
  annotate("text", x = 0.65, y = 0.25,
           label = sprintf("AUC CV = %.3f", auc_cv_rf),
           size = 4, color = "#185FA5", fontface = "bold") +
  scale_x_continuous("1 \u2212 Especificidad", limits = c(0, 1), expand = c(0.01, 0)) +
  scale_y_continuous("Sensibilidad", limits = c(0, 1), expand = c(0.01, 0)) +
  labs(title    = "Curva ROC \u2014 Random Forest (CV 10 folds)",
       subtitle = "Predicciones out-of-fold del mejor modelo") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5),
        panel.grid.minor = element_blank())
print(p_roc)