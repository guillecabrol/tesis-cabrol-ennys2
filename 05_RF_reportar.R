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

as.factor(df_modelo$Edad)

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

rf_spec <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "permutation", seed = 1213) %>%
  set_mode("classification")

wf <- workflow() %>% add_recipe(receta) %>% add_model(rf_spec)

# ── 4. CV ESTRATIFICADO (10 folds, comparable con RegLog) ───
set.seed(1213)
folds_rf <- vfold_cv(df_modelo, v = 10, strata = Target_bin)

# ── 5. GRID DE HIPERPARÁMETROS ──────────────────────────────
n_pred <- ncol(df_modelo) - 1   

trees_grid <- c(30, 50, 100, 200)   
mtry_grid  <- c(1, 2, 3, 4, 6, 8)  
min_n_grid <- c(100, 150, 250, 300, 350)   

rf_grid <- tidyr::crossing(
  trees = trees_grid,
  mtry  = mtry_grid,
  min_n = min_n_grid
)
cat("Combinaciones a explorar:", nrow(rf_grid), "\n")   # 4*4*4 = 64

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
metrica_sel <- "roc_auc"

cat("\n--- Top combinaciones por", metrica_sel, "---\n")
show_best(rf_tune, metric = metrica_sel, n = 8) %>% print()

autoplot(rf_tune, metric = "roc_auc") +
  labs(title = "AUC según hiperparámetros (CV 10 folds)") +
  theme_minimal(base_size = 12)

k <- 1
mejor_params <- show_best(rf_tune, metric = metrica_sel, n = 10) %>%
  slice(k) %>% select(mtry, trees, min_n, .config)
cat("\nMejores hiperparámetros (", metrica_sel, "):\n", sep = ""); print(mejor_params)

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
#ggsave("importancia_rf.pdf", plot = p_imp, width = 9, height = 5)

# ── 11. UMBRALES Y MÉTRICAS (OOF) ───────────────────────────
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
    5 * prec * sens / (4 * prec + sens) else NA_real_   # F-beta con beta=2
  c(accuracy = acc, sensibilidad = sens, precision = prec,
    especificidad = esp, F2 = f2)
}

# (a) Umbral de Youden (referencia)
umbral_youden <- as.numeric(
  coords(roc_cv_rf, x = "best", best.method = "youden",
         ret = "threshold", transpose = FALSE)$threshold
)

# (b) Umbral ADOPTADO: mayor umbral con sensibilidad >= 0.8
target_sens <- 0.80
co   <- coords(roc_cv_rf, x = "all",
               ret = c("threshold", "sensitivity", "specificity"),
               transpose = FALSE)
cand <- co[co$sensitivity >= target_sens & is.finite(co$threshold), ]
umbral_sens <- max(cand$threshold)

cat(sprintf("\nUmbral Youden (referencia):     %.3f\n", umbral_youden))

# Comparación de los dos umbrales
cat("\n=== Comparación de umbrales (OOF, CV 10) — RF ===\n")
comp_umbrales <- rbind(
  Youden           = c(umbral = umbral_youden,
                       metricas_en_umbral(pred_cv_rf$.pred_Si, pred_cv_rf$Target_bin, umbral_youden)),
  Adoptado_sens80  = c(umbral = umbral_sens,
                       metricas_en_umbral(pred_cv_rf$.pred_Si, pred_cv_rf$Target_bin, umbral_sens))
)
print(round(comp_umbrales, 4))

# ── 12. MÉTRICAS POR FOLD: ENTRENAMIENTO vs Evaluación + BOXPLOTS ──
umbral_reporte <- umbral_sens

eval_fold <- function(split) {
  tr <- analysis(split); te <- assessment(split)
  mod <- fit(wf_final, data = tr)   # la receta se re-prepara solo con tr (sin leakage)
  
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
    tibble(particion = "Evaluación",        metrica = c(names(m_te), "AUC"),
           valor = c(as.numeric(m_te), auc_te))
  )
}

set.seed(1213)
metricas_folds <- folds_rf$splits %>%
  set_names(folds_rf$id) %>%
  imap(~ eval_fold(.x) %>% mutate(fold = .y)) %>%
  bind_rows()

# Resumen media ± sd (train vs test)
resumen_ft <- metricas_folds %>%
  group_by(particion, metrica) %>%
  summarise(media = mean(valor, na.rm = TRUE),
            sd    = sd(valor,   na.rm = TRUE), .groups = "drop") %>%
  arrange(metrica, particion)
cat("\n--- Métricas por fold: media ± sd (train vs test) — RF ---\n")
print(resumen_ft, n = Inf)

# ── Boxplots: eje de IGUAL ALTO por panel (idéntico a RegLog) ──
orden_m <- c("sensibilidad","precision","F2","accuracy","AUC","especificidad")
metricas_folds <- metricas_folds %>% mutate(metrica = factor(metrica, levels = orden_m))

# Alto común = el mayor rango entre métricas, con 15% de margen
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
  labs(title = "Distribución de métricas por fold (CV 10) — Random Forest",
       subtitle = sprintf("Umbral adoptado = %.3f (sens >= %.2f)", umbral_reporte, target_sens),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "none",
        plot.title         = element_text(face = "bold", hjust = 0.5),
        plot.subtitle      = element_text(color = "gray40", hjust = 0.5),
        strip.text         = element_text(face = "bold"),
        panel.grid.minor.y = element_line(linewidth = 0.3, color = "gray92"))
print(p_box)

#ggsave("boxplot_rf.pdf", plot = p_box, width = 9, height = 5)
