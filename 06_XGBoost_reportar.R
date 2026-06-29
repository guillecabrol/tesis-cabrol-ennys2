# ============================================================
# XGBOOST — EXCESO DE PESO / OBESIDAD EN ADOLESCENTES
#  Variables de alimentación seleccionadas
# (Frutas, Verduras y Bebidas Azucaradas)
# + comportamiento (en terciles)
# + demográficas.
# Framework: tidymodels | Motor: xgboost
# ============================================================

library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(pROC)

# ── 0. TOGGLE ──────────────────────────────────
#   "exceso"   -> Target_bin (exceso de peso)
#   "obesidad" -> Cat_IMC == "Obesidad" vs resto
target_elegido <- "exceso"

# ── 1. CARGA, TERCILES Y SELECCIÓN DE VARIABLES ─────────────
df <- read.csv("dataset.csv", stringsAsFactors = FALSE)

a_terciles <- function(x) {
  cortes <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  cut(x, breaks = c(-Inf, cortes, Inf),
      labels = c("Bajo", "Medio", "Alto"), include.lowest = TRUE)
}

df <- df %>%
  mutate(
    Sedentarismo_t = a_terciles(HorasPorDia_Sedentarismo),
    ActFisica_t    = a_terciles(Horas_PorDia_ActFisica),
    Sueño_t   = a_terciles(HorasPorDia_Sueño),
    y_bin = if (target_elegido == "obesidad")
      as.integer(Cat_IMC == "Obesidad")
    else
      as.integer(Target_bin)
  )

df_modelo <- df %>%
  transmute(
    Edad,
    Sexo,
    Region,
    
    Sedentarismo_t,
    ActFisica_t,
    
    Frec_Frutas,
    Frec_Verduras,
    Frec_Bebidas_Azucaradas,
    
    Target_bin = factor(y_bin,
                        levels = c(1, 0),
                        labels = c("Si", "No"))
  ) %>%
  filter(!is.na(Target_bin))

cat("Desenlace:", target_elegido, "\n")
cat("Distribución del target:\n"); print(prop.table(table(df_modelo$Target_bin)))
cat("Dimensiones:", dim(df_modelo), "\n")

# ── 2. RECETA ───────────────────────────────────────────────
# XGBoost necesita matriz numérica → step_dummy. Imputación por fold.
receta <- recipe(Target_bin ~ ., data = df_modelo) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())

prep(receta) %>% bake(new_data = NULL) %>% ncol() %>%
  cat("Variables después de dummies:", ., "\n")

# ── 3. MODELO (7 hiperparámetros tuneados) ──────────────────
xgb_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  min_n          = tune(),
  loss_reduction = tune(),
  mtry           = tune(),
  sample_size    = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

wf <- workflow() %>% add_recipe(receta) %>% add_model(xgb_spec)

# ── 4. CV ESTRATIFICADO (10 folds) ──────────────────────────
set.seed(1213)
folds_xgb <- vfold_cv(df_modelo, v = 10, strata = Target_bin)

# ── 5. GRID (space-filling, 60 combinaciones) ───────────────
n_preds <- prep(receta) %>% bake(new_data = NULL) %>%
  select(-Target_bin) %>% ncol()

set.seed(1213)
grid_xgb <- grid_space_filling(
  trees(range          = c(200, 1500)),
  tree_depth(range     = c(2, 5)),
  learn_rate(range     = c(-2.3, -1)),    # log10: 0.005 a 0.1
  min_n(range          = c(10, 50)),
  loss_reduction(range = c(-5, 0.7)),     # log10
  mtry(range           = c(round(n_preds * 0.5), n_preds)),
  sample_size          = sample_prop(c(0.5, 1.0)),
  size = 60
)
cat("Combinaciones a explorar:", nrow(grid_xgb), "\n")

# ── 6. GRID SEARCH ──────────────────────────────────────────
metricas <- metric_set(roc_auc, accuracy,
                       yardstick::sensitivity, yardstick::specificity)

cat("\nCorriendo grid search XGBoost...\n")
set.seed(1213)
xgb_tune <- tune_grid(
  wf,
  resamples = folds_xgb,
  grid      = grid_xgb,
  metrics   = metricas,
  control   = control_grid(save_pred = TRUE, verbose = TRUE)
)

# ── 7. MEJORES HIPERPARÁMETROS ──────────────────────────────
cat("\n--- Top 10 combinaciones por AUC ---\n")
show_best(xgb_tune, metric = "roc_auc", n = 10) %>% print()

mejor_params <- select_best(xgb_tune, metric = "roc_auc")
cat("\nMejores hiperparámetros:\n"); print(mejor_params)

# ── 8. AUC CV GLOBAL (OOF) ──────────────────────────────────
pred_cv_xgb <- collect_predictions(xgb_tune, parameters = mejor_params)

roc_cv_xgb <- roc(pred_cv_xgb$Target_bin, pred_cv_xgb$.pred_Si,
                  levels = c("No", "Si"), direction = "<", quiet = TRUE)
auc_cv_xgb <- auc(roc_cv_xgb)
ci_cv_xgb  <- ci.auc(roc_cv_xgb)
cat(sprintf("\nAUC CV global (OOF): %.4f [IC 95%%: %.4f\u2013%.4f]\n",
            auc_cv_xgb, ci_cv_xgb[1], ci_cv_xgb[3]))

# ── 9. UMBRAL ÓPTIMO (Youden) Y MÉTRICAS ────────────────────
coords_xgb <- coords(roc_cv_xgb, x = "best", best.method = "youden",
                     ret = c("threshold", "sensitivity", "specificity"))
cat("\n--- Umbral óptimo de Youden ---\n"); print(coords_xgb)

umbral <- 0.31

pred_binaria <- ifelse(pred_cv_xgb$.pred_Si >= umbral, "Si", "No")
obs_real     <- as.character(pred_cv_xgb$Target_bin)

vp <- sum(pred_binaria == "Si" & obs_real == "Si")
vn <- sum(pred_binaria == "No" & obs_real == "No")
fp <- sum(pred_binaria == "Si" & obs_real == "No")
fn <- sum(pred_binaria == "No" & obs_real == "Si")

precision     <- vp / (vp + fp)
recall        <- vp / (vp + fn)
especificidad <- vn / (vn + fp)
beta <- 2
f2 <- (1 + beta^2) * precision * recall / ((beta^2 * precision) + recall)

cat("\n========================================\n")
cat("MÉTRICAS EN EL UMBRAL (XGBoost)\n")
cat("========================================\n")
cat(sprintf("Umbral:        %.3f\n", umbral))
cat(sprintf("Sensibilidad:  %.4f\n", recall))
cat(sprintf("Especificidad: %.4f\n", especificidad))
cat(sprintf("F2-score:      %.4f\n", f2))
cat("========================================\n")

# ── 10. MODELO FINAL (para importancia) ─────────────────────
wf_final <- finalize_workflow(wf, mejor_params)
set.seed(1213)
modelo_final <- fit(wf_final, data = df_modelo)
xgb_fit <- extract_fit_parsnip(modelo_final)

# ── 11. IMPORTANCIA DE VARIABLES (incluye demográficas) ─────
# step_dummy crea una columna por categoría, así que aparecen los
# niveles (p.ej. Sedentarismo_t_Alto, Region_NEA, etc.).
imp_df <- vi(xgb_fit$fit, scale = TRUE) %>%
  arrange(Importance) %>%
  mutate(
    Variable = factor(Variable, levels = Variable),
    grupo = case_when(
      str_detect(as.character(Variable),
                 "Frec_Frutas|Frec_Verduras|Frec_Bebidas_Azucaradas") ~ "Alimentación",
      str_detect(as.character(Variable),
                 "Sedentarismo|ActFisica|Sue") ~ "Comportamiento",
      TRUE ~ "Demográficas"
    )
  )
cat("\n--- Importancia de variables ---\n")
print(imp_df %>% arrange(desc(Importance)))

colores_imp <- c(
  "Alimentación"   = "#5E8F58",
  "Comportamiento" = "#185FA5",
  "Demográficas"   = "#BA7517"
)

p_imp <- ggplot(imp_df, aes(x = Importance, y = Variable, fill = grupo)) +
  geom_col(width = 0.7) +
  scale_fill_manual(name = NULL, values = colores_imp) +
  scale_x_continuous("Importancia relativa (escala 0\u2013100)",
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Importancia de variables \u2014 XGBoost",
       #subtitle = paste0("M\u00e9trica: gain | desenlace: ", target_elegido),
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5),
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
print(p_imp)

# ── 12. CURVA ROC ───────────────────────────────────────────
p_roc <- ggplot(data.frame(fpr = 1 - roc_cv_xgb$specificities,
                           tpr = roc_cv_xgb$sensitivities),
                aes(x = fpr, y = tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray60", linewidth = 0.5) +
  geom_line(color = "#185FA5", linewidth = 1) +
  annotate("text", x = 0.65, y = 0.25,
           label = sprintf("AUC CV = %.3f", auc_cv_xgb),
           size = 4, color = "#185FA5", fontface = "bold") +
  scale_x_continuous("1 \u2212 Especificidad", limits = c(0, 1), expand = c(0.01, 0)) +
  scale_y_continuous("Sensibilidad", limits = c(0, 1), expand = c(0.01, 0)) +
  labs(title    = "Curva ROC \u2014 XGBoost (CV 10 folds)",
       subtitle = "Predicciones out-of-fold del mejor modelo") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
        plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5),
        panel.grid.minor = element_blank())
print(p_roc)