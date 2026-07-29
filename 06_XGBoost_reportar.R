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

# ── 3. MODELO (6 hiperparámetros tuneados; gamma y L1/L2 en default) ──
xgb_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  min_n          = tune(),
  loss_reduction = 0,        # fijo: la poda ya la ejercen tree_depth y min_n
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
  trees(range      = c(30, 400)),
  tree_depth(range = c(1, 4)),          # incluye stumps (depth = 1)
  learn_rate(range = c(-1.5, -0.5)),    # log10: 0.03 a 0.3
  min_n(range      = c(10, 60)),
  mtry(range       = c(round(n_preds * 0.5), n_preds)),
  sample_size      = sample_prop(c(0.5, 1.0)),
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

metricas_en_umbral <- function(prob_si, obs, umbral) {
  pred <- ifelse(prob_si >= umbral, "Si", "No")
  obs  <- as.character(obs)
  vp <- sum(pred == "Si" & obs == "Si"); fp <- sum(pred == "Si" & obs == "No")
  fn <- sum(pred == "No" & obs == "Si"); vn <- sum(pred == "No" & obs == "No")
  sens <- vp / (vp + fn)
  esp  <- vn / (vn + fp)
  prec <- if ((vp + fp) > 0) vp / (vp + fp) else NA_real_
  acc  <- (vp + vn) / (vp + fp + fn + vn)
  f2   <- if (!is.na(prec) && (4 * prec + sens) > 0)
    5 * prec * sens / (4 * prec + sens) else NA_real_
  c(accuracy = acc, sensibilidad = sens, precision = prec,
    especificidad = esp, F2 = f2)
}

# ── 9. UMBRALES Y MÉTRICAS (OOF) ────────────────────────────
# (a) Umbral de Youden (referencia)
umbral_youden <- as.numeric(
  coords(roc_cv_xgb, x = "best", best.method = "youden",
         ret = "threshold", transpose = FALSE)$threshold
)

# (b) Umbral ADOPTADO: mayor umbral con sensibilidad >= 0.80
target_sens <- 0.80
co   <- coords(roc_cv_xgb, x = "all",
               ret = c("threshold", "sensitivity", "specificity"),
               transpose = FALSE)
cand <- co[co$sensitivity >= target_sens & is.finite(co$threshold), ]
umbral_sens <- max(cand$threshold)

cat(sprintf("\nUmbral Youden (referencia):     %.3f\n", umbral_youden))

cat("\n=== Comparación de umbrales (OOF, CV 10) — XGBoost ===\n")
comp_umbrales <- rbind(
  Youden           = c(umbral = umbral_youden,
                       metricas_en_umbral(pred_cv_xgb$.pred_Si, pred_cv_xgb$Target_bin, umbral_youden)),
  Adoptado_sens80  = c(umbral = umbral_sens,
                       metricas_en_umbral(pred_cv_xgb$.pred_Si, pred_cv_xgb$Target_bin, umbral_sens))
)
print(round(comp_umbrales, 4))

# ── 10. MODELO FINAL (para importancia) ─────────────────────
wf_final <- finalize_workflow(wf, mejor_params)
set.seed(1213)
modelo_final <- fit(wf_final, data = df_modelo)
xgb_fit <- extract_fit_parsnip(modelo_final)

# ── 11. IMPORTANCIA DE VARIABLES (agregada a nivel variable) ─────
# step_dummy crea una columna por categoría; sumamos el gain de todas
# las dummies de cada variable para obtener su importancia total,
# comparable con la del RF (una fila por variable).
vars_orig <- c("Edad", "Sexo", "Region", "Sedentarismo_t", "ActFisica_t",
               "Frec_Frutas", "Frec_Verduras", "Frec_Bebidas_Azucaradas")

imp_df <- vi(xgb_fit$fit, scale = FALSE) %>%                 # gain crudo (aditivo)
  mutate(Variable_orig = map_chr(Variable, function(v) {
    hit <- vars_orig[startsWith(v, vars_orig)]              # qué variable es prefijo del dummy
    if (length(hit)) hit[which.max(nchar(hit))] else v      # el prefijo más largo
  })) %>%
  group_by(Variable_orig) %>%
  summarise(Importance = sum(Importance), .groups = "drop") %>%
  mutate(
    Importance = 100 * Importance / max(Importance),
    Variable   = fct_reorder(Variable_orig, Importance),
    grupo = case_when(
      str_detect(Variable_orig, "Frec_Frutas|Frec_Verduras|Frec_Bebidas_Azucaradas") ~ "Alimentación",
      str_detect(Variable_orig, "Sedentarismo|ActFisica")                        ~ "Comportamiento",
      TRUE ~ "Demográficas"
    )
  )

cat("\n--- Importancia de variables ---\n")
print(imp_df %>% arrange(desc(Importance)), n = Inf)

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

#ggsave("importancia_xgb.pdf", plot = p_imp, width = 9, height = 5)

# ── 12. MÉTRICAS POR FOLD: ENTRENAMIENTO vs EVALUACIÓN + BOXPLOTS ──
umbral_reporte <- umbral_sens

eval_fold <- function(split) {
  tr <- analysis(split); te <- assessment(split)
  mod <- fit(wf_final, data = tr)
  prob_tr <- predict(mod, tr, type = "prob")$.pred_Si
  prob_te <- predict(mod, te, type = "prob")$.pred_Si
  auc_tr <- as.numeric(auc(roc(tr$Target_bin, prob_tr,
                               levels = c("No","Si"), direction = "<", quiet = TRUE)))
  auc_te <- as.numeric(auc(roc(te$Target_bin, prob_te,
                               levels = c("No","Si"), direction = "<", quiet = TRUE)))
  m_tr <- metricas_en_umbral(prob_tr, tr$Target_bin, umbral_reporte)
  m_te <- metricas_en_umbral(prob_te, te$Target_bin, umbral_reporte)
  bind_rows(
    tibble(particion = "Entrenamiento", metrica = c(names(m_tr), "AUC"),
           valor = c(as.numeric(m_tr), auc_tr)),
    tibble(particion = "Evaluación",    metrica = c(names(m_te), "AUC"),
           valor = c(as.numeric(m_te), auc_te))
  )
}

set.seed(1213)
metricas_folds <- folds_xgb$splits %>%
  set_names(folds_xgb$id) %>%
  imap(~ eval_fold(.x) %>% mutate(fold = .y)) %>%
  bind_rows()

resumen_ft <- metricas_folds %>%
  group_by(particion, metrica) %>%
  summarise(media = mean(valor, na.rm = TRUE),
            sd    = sd(valor,   na.rm = TRUE), .groups = "drop") %>%
  arrange(metrica, particion)
cat("\n--- Métricas por fold: media ± sd (train vs test) — XGBoost ---\n")
print(resumen_ft, n = Inf)

# Boxplots
orden_m <- c("sensibilidad","precision","F2","accuracy","AUC","especificidad")
metricas_folds <- metricas_folds %>% mutate(metrica = factor(metrica, levels = orden_m))

span <- metricas_folds %>%
  group_by(metrica) %>%
  summarise(r = max(valor) - min(valor), .groups = "drop") %>%
  pull(r) %>% max() * 1.15

lims <- metricas_folds %>%
  group_by(metrica) %>%
  summarise(centro = (min(valor) + max(valor)) / 2, .groups = "drop") %>%
  mutate(lo = pmax(0, centro - span/2), hi = pmin(1, centro + span/2)) %>%
  pivot_longer(c(lo, hi), values_to = "valor") %>%
  transmute(metrica, particion = "Entrenamiento", valor)

p_box <- metricas_folds %>%
  filter(metrica %in% orden_m) %>%
  ggplot(aes(x = particion, y = valor, fill = particion)) +
  geom_boxplot(width = 0.6, alpha = 0.85, outlier.size = 0.8) +
  geom_blank(data = lims) +
  facet_wrap(~ metrica, nrow = 2, scales = "free_y") +
  scale_fill_manual(values = c("Entrenamiento" = "#BA7517", "Evaluación" = "#185FA5")) +
  labs(title = "Distribución de métricas por fold (CV 10) — XGBoost",
       subtitle = sprintf("Umbral adoptado = %.3f (sens >= %.2f)", umbral_reporte, target_sens),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        plot.title         = element_text(face = "bold", hjust = 0.5),
        plot.subtitle      = element_text(color = "gray40", hjust = 0.5),
        strip.text         = element_text(face = "bold"),
        panel.grid.minor.y = element_line(linewidth = 0.3, color = "gray92"))
print(p_box)

#ggsave("boxplot_xgb.pdf", plot = p_box, width = 9, height = 5)
