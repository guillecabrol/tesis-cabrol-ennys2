# Instalar y cargar librerías necesarias
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rstatix)
library(car)
library(gtsummary)
library(gridExtra)
library(grid)
library(FactoMineR)
library(factoextra)

# Eleccion de variables

# 1. Cargar la base original y completar con NA las celdas vacias
dataset_original <- read.csv("ENNyS2_encuesta.csv", na.strings = c("", " ", "NA"))


# 2. Filtrado del dataset
dataset <- dataset_original %>%
  
  # 1. FILTRADO DE POBLACIÓN: Me quedo solo con los adolescentes de 13 a 17 años (Cuestionario 3)
  filter(E_CUEST == "13 a 17 años") %>%
  
  #. Filtro los datos faltantes en la variable respuesta y arreglo algunas edades que estan mal
  filter(!is.na(IMC_5a17cat_ex)) %>%
  
  mutate(
    SD_MIEMBRO_SORTEADO_SD_4 = ifelse(id == 83293373, 17, SD_MIEMBRO_SORTEADO_SD_4)
  ) %>%
  
  mutate(
    SD_MIEMBRO_SORTEADO_SD_4 = ifelse(SD_MIEMBRO_SORTEADO_SD_4 == "12", "13", SD_MIEMBRO_SORTEADO_SD_4)
  ) %>%
  
  # 3. SELECCIÓN DE COLUMNAS
  select(
    # Generales y Diseño Muestral
    id, 
    Aglomerado = UPM, 
    Estrato = EUPM, 
    Ponderador = F_STG_calib, 
    Region = region,
    Sexo = SD_MIEMBRO_SORTEADO_SD_3,
    Edad = SD_MIEMBRO_SORTEADO_SD_4,
    Asiste_Escuela = SD_MIEMBRO_SORTEADO_SD_15, # "¿Asiste a asistió a algún establecimiento educativo?"
    
    
    # Antropométricas (Variable Respuesta)
    Peso_kg = PESO,
    Talla_cm = TALLA,
    IMC = IMC,
    Z_IMC = ZIMCE,
    Cat_IMC = IMC_5a17cat,
    Target_Exceso_Peso = IMC_5a17cat_ex,
    
    # Frecuencia de Consumo al Mes (Cuestionario Crudo)
    Frec_Lacteos = T_C3_FCA_6_1_1,
    Frec_Frutas = T_C3_FCA_6_1_2,
    Frec_Verduras = T_C3_FCA_6_1_3,
    Frec_Papas_Pastas = T_C3_FCA_6_1_4,
    Frec_Integrales_Legumbres = T_C3_FCA_6_1_5,
    Frec_Fiambres_Embutidos = T_C3_FCA_6_1_6,
    Frec_Carnes_Huevos = T_C3_FCA_6_1_7,
    Frec_Pescado = T_C3_FCA_6_1_8,
    Frec_Aceites = T_C3_FCA_6_1_9,
    Frec_FrutosSecos_Semillas = T_C3_FCA_6_1_10,
    Frec_Snacks_Copetin = T_C3_FCA_6_1_11,
    Frec_Golosinas = T_C3_FCA_6_1_12,
    Frec_Facturas_Pasteleria = T_C3_FCA_6_1_13,
    Frec_Preelaborados = T_C3_FCA_6_1_14,
    Frec_Bebidas_Light = T_C3_FCA_6_1_15,
    Frec_Bebidas_Azucaradas = T_C3_FCA_6_1_16,
    Frec_Agua = T_C3_FCA_6_1_17,
    
    
    # Actividad Física, Sedentarismo y Sueño
    HorasPorDia_Sedentarismo = hs_CS_total_adol, 
    Cumple_OMS_ActFisica = activo_OMS_adol,
    HorasPorDia_Sueño = hs_sueño_ajustado_adol, 
    Cumple_OMS_Sueño = sueño_OMS_adol, 
    Hace_ActFisica_Escuela = C3_AF_4_17,
    TA_semanal_tot_min_adol,   
    HO_semanal_tot_min_adol,   
    HPO_semanal_tot_min_adol,  
    TL_semanal_tot_min_adol,   
    TOL_semanal_tot_min_adol,  
    ESC_semanal_tot_min_adol,
    
    # Entorno Escolar (Provisión y Kiosco)
    Escuela_Provee_Alimento = EE_7_1, 
    Esc_Provee_BebAzucaradas = EE_7_2_1,
    Esc_Provee_BebLight = EE_7_2_2,
    Esc_Provee_Infusiones = EE_7_2_3,
    Esc_Provee_Copetin = EE_7_2_4,
    Esc_Provee_Golosinas = EE_7_2_5,
    Esc_Provee_Facturas = EE_7_2_6,
    Esc_Provee_Frutas = EE_7_2_7,
    Esc_Provee_Agua = EE_7_2_8,
    Esc_Provee_Lacteos = EE_7_2_9,
    Esc_Provee_Sandwich = EE_7_2_10,
    
    Escuela_Tiene_Kiosco = C3_EE_7_3,
    Compro_En_Kiosco = C3_EE_7_4,
    starts_with("C3_EE_7_5_"),
    
    # Sociodemográficas y Vivienda
    Cobertura_Salud = Cobertura_salud, 
    Quintil_Ingreso = IngHog_UC_QUINT_imp, 
    Ingreso_Imputado_Por_UC = ingreso_uc_imputado, 
    
    Tipo_Vivienda = SD_3_1, 
    Cant_Miembros = SD_CANT_MIEMBROS, 
    Cant_Habitaciones = SD_3_14, 
    Material_Piso = SD_3_2, 
    Material_Paredes = SD_3_3, 
    Origen_Agua = SD_3_4, 
    Tiene_Baño = SD_3_6, 
    Desague_Baño = SD_3_7, 
    Tiene_Electricidad = SD_3_8, 
    Tiene_Heladera = SD_3_9, 
    Combustible_Cocina = SD_3_10, 
    Uso_Vivienda = SD_3_13,
    
    # Variables de miembros del hogar (para NBI 4 y NBI 5)
    # Edad, asistencia escolar y situación laboral de cada miembro (M01..M15)
    # + nivel educativo del jefe (M01). Se descartan luego de construir el NBI.
    matches("^M[0-9]{2}_SD_4$"),    # edad de cada miembro
    matches("^M[0-9]{2}_SD_15$"),   # asistencia escolar de cada miembro
    matches("^M[0-9]{2}_SD_19$"),   # ¿trabajó? (ocupado) de cada miembro
    M01_SD_16,                      # nivel educativo del jefe
    M01_SD_17,                      # ¿completó ese nivel? (jefe)
  ) %>%
  
  #agrupo a delgadez y peso normal en una categoria "sin exceso de peso" para que me quede binaria el target
  mutate(
    Target_Exceso_Peso = case_when(
      Target_Exceso_Peso == "Exceso de peso" ~ "Exceso de peso",
      Target_Exceso_Peso %in% c("Delgadez", "Peso normal") ~ "Sin exceso de peso",
      TRUE ~ NA_character_
    )
  ) %>%
  
  mutate(
    # 1. Sumamos los minutos diarios de todos los dominios de actividad física
    Minutos_PorDia_ActFisica = rowSums(select(., 
                                              TA_semanal_tot_min_adol,   
                                              HO_semanal_tot_min_adol,   
                                              HPO_semanal_tot_min_adol,  
                                              TL_semanal_tot_min_adol,   
                                              TOL_semanal_tot_min_adol,  
                                              ESC_semanal_tot_min_adol   
    ), na.rm = TRUE),
    
    # 2. Dividimos por 60 para crear tu variable final en horas
    Horas_PorDia_ActFisica = Minutos_PorDia_ActFisica / 60,
    .after = HorasPorDia_Sedentarismo 
  ) %>%
  
# ---------------------------------------------------------
# REDONDEO DE VARIABLES NUMÉRICAS
# ---------------------------------------------------------
mutate(
  across(
    c(
      Peso_kg,
      Talla_cm,
      IMC,
      HorasPorDia_Sedentarismo,
      HorasPorDia_Sueño,
      Horas_PorDia_ActFisica,
      Ingreso_Imputado_Por_UC
    ),
    ~ round(., 1)
  )
) %>%
  
  mutate(
    Nivel_Educacion_Jefe = case_when(
      M01_SD_16 %in% c("NS/NR", "99") ~ NA_character_,
      M01_SD_17 %in% c("NS/NR", "99", "Ignorado") ~ NA_character_,
      
      # Primario o menos
      M01_SD_16 %in% c("Inicial (jardín/ preescolar)", "Primario", "EGB") ~ "Bajo_Nivel_Educativo",
      
      # Secundario
      M01_SD_16 %in% c("Secundario", "Polimodal") & M01_SD_17 == "Si" ~ "Medio_Nivel_Educativo",
      M01_SD_16 %in% c("Secundario", "Polimodal") & M01_SD_17 == "No" ~ "Bajo_Nivel_Educativo",
      
      # Terciario o más
      M01_SD_16 %in% c("Superior no universitario", "Universitario", "Post Universitario") ~ "Alto_Nivel_Educativo",
      
      TRUE ~ NA_character_
    ), 
    Nivel_Educacion_Jefe = factor(
      Nivel_Educacion_Jefe,
      levels = c("Bajo_Nivel_Educativo", "Medio_Nivel_Educativo", "Alto_Nivel_Educativo")
    ),
    .after = Cobertura_Salud
  ) %>%
  
  mutate(
    across(
      c(Frec_Lacteos, Frec_Frutas, Frec_Verduras, Frec_Papas_Pastas, Frec_Integrales_Legumbres, 
        Frec_Fiambres_Embutidos, Frec_Carnes_Huevos, Frec_Pescado, Frec_Aceites, Frec_FrutosSecos_Semillas,
        Frec_Snacks_Copetin, Frec_Golosinas, Frec_Facturas_Pasteleria, Frec_Preelaborados,
        Frec_Bebidas_Light, Frec_Bebidas_Azucaradas, Frec_Agua),
      ~ factor(
        case_when(
          . %in% c("Nunca o menos de 1 vez al mes",
                   "Entre 1 y 3 veces al mes") ~ "Consumo ocasional",
          
          . %in% c("1 vez por semana",
                   "2 a 4 veces por semana",
                   "5 a 6 veces por semana") ~ "Consumo semanal",
          
          . %in% c("1 vez al día",
                   "Entre 2 y 3 veces al día",
                   "Entre 4 y 5 veces al día",
                   "6 veces o más por día") ~ "Consumo diario",
          
          TRUE ~ NA_character_
        ),
        levels = c(
          "Consumo ocasional",
          "Consumo semanal",
          "Consumo diario"
        )
      )
    )
  ) %>%
  
  # 4. FEATURE ENGINEERING (Creación de variables)
  mutate(
    # 1. Categoría de Riesgo (Alimentos No Recomendados / Ultraprocesados)
    Kiosco_Compro_No_Recomendados = case_when(
      is.na(Compro_En_Kiosco) ~ NA,
      if_any(starts_with("C3_EE_7_5_"), ~ grepl("Bebidas sin azúcar|Bebidas con azúcar|Productos de copetín|Golosinas|Facturas|Sándwich", ., ignore.case = TRUE)) ~ 1,
      TRUE ~ 0
    ),
    
    # 2. Categoría Protectora (Alimentos Recomendados / Saludables)
    Kiosco_Compro_Recomendados = case_when(
      is.na(Compro_En_Kiosco) ~ NA,
      if_any(starts_with("C3_EE_7_5_"), ~ grepl("Frutas frescas|Verduras frescas|Agua segura|Yogur|Infusiones", ., ignore.case = TRUE)) ~ 1,
      TRUE ~ 0
    ),
    .after = Compro_En_Kiosco
  ) %>%
  
  select(
    -starts_with("C3_EE_7_5_"),
    -starts_with("SD_6_3_")
  )


#---------------------INDICES--------------------------------------

# Cortes NSE según percentiles SAIMO
cortes_nse <- quantile(
  dataset$Ingreso_Imputado_Por_UC,
  probs = c(0.133, 0.458, 0.760, 0.942),
  na.rm = TRUE
)

dataset <- dataset %>%
  mutate(
    # ---------------------------------------------------------
    # 1. ÍNDICE DE HACINAMIENTO
    # ---------------------------------------------------------
    Ratio_Hacinamiento = Cant_Miembros / Cant_Habitaciones,
    
    Indice_Hacinamiento = case_when(
      is.na(Ratio_Hacinamiento) ~ NA_character_,
      Ratio_Hacinamiento < 1 ~ "Holgado",
      Ratio_Hacinamiento >= 1 & Ratio_Hacinamiento <= 2 ~ "Sin hacinamiento",
      Ratio_Hacinamiento > 2 & Ratio_Hacinamiento <= 3 ~ "Hacinamiento moderado",
      Ratio_Hacinamiento > 3 ~ "Hacinamiento crítico"
    ),
    
    # ---------------------------------------------------------
    # 2. ÍNDICE DE NECESIDADES BÁSICAS INSATISFECHAS (NBI)
    # ---------------------------------------------------------
    
    # NBI 1: Vivienda precaria
    NBI_1 = case_when(
      Tipo_Vivienda %in% c("Inquilinato/ conventillo",
                           "Pieza/s en hotel/ pensión",
                           "Casilla",
                           "Otros") ~ 1,
      is.na(Tipo_Vivienda) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # NBI 2: Condiciones sanitarias
    NBI_2 = case_when(
      Tiene_Baño %in% c("Letrina (sin arrastre de agua)",
                        "No tiene baño") ~ 1,
      is.na(Tiene_Baño) ~ NA_real_,
      TRUE ~ 0
    ),
    
    # NBI 3: Hacinamiento crítico
    NBI_3 = case_when(
      is.na(Ratio_Hacinamiento) ~ NA_real_,
      Ratio_Hacinamiento > 3 ~ 1,
      TRUE ~ 0
    ),
    
    # ---- NBI 4: asistencia escolar incompleta ----
    # Al menos un niño/a de 6 a 12 años en el hogar que no asiste a la escuela.
    NBI_4 = {
      hay_nino_noasiste <- rep(FALSE, n())
      for (i in 1:15) {
        col_edad <- get(sprintf("M%02d_SD_4",  i))
        col_asis <- get(sprintf("M%02d_SD_15", i))
        e <- suppressWarnings(as.numeric(col_edad))
        es_nino   <- !is.na(e) & e >= 6 & e <= 12
        no_asiste <- col_asis %in% c("No asiste pero asistió", "Nunca asistió")
        hay_nino_noasiste <- hay_nino_noasiste | (es_nino & no_asiste)
      }
      as.integer(hay_nino_noasiste)
    },
    
    # ---- NBI 5: capacidad de subsistencia reducida ----
    # (a) 4 o más personas por miembro ocupado  Y
    # (b) el jefe/a no completó la primaria (criterio amplio, ver metodología)
    n_miembros_hogar = rowSums(!is.na(across(matches("^M[0-9]{2}_SD_4$") &
                                               !matches("_SD_4[0-9]")))),
    n_ocupados = rowSums(across(matches("^M[0-9]{2}_SD_19$"),
                                ~ .x == "Sí"), na.rm = TRUE),
    ratio_dependencia = ifelse(n_ocupados == 0, Inf,
                               n_miembros_hogar / n_ocupados),
    jefe_sin_primaria = (M01_SD_16 == "Primario" & M01_SD_17 == "No"),
    NBI_5 = as.integer(ratio_dependencia >= 4 &
                         !is.na(jefe_sin_primaria) & jefe_sin_primaria),
    
    # Índice final NBI (5 dimensiones oficiales INDEC)
    Indice_NBI = case_when(
      NBI_1 == 1 | NBI_2 == 1 | NBI_3 == 1 | NBI_4 == 1 | NBI_5 == 1 ~ "Con NBI",
      NBI_1 == 0 & NBI_2 == 0 & NBI_3 == 0 & NBI_4 == 0 & NBI_5 == 0 ~ "Sin NBI",
      TRUE ~ NA_character_
    ),
    
    # ---------------------------------------------------------
    # 3. ÍNDICE DE NIVEL SOCIOECONÓMICO (NSE - SAIMO)
    # ---------------------------------------------------------
    Indice_NSE = case_when(
      is.na(Ingreso_Imputado_Por_UC) ~ NA_character_,
      Ingreso_Imputado_Por_UC <= cortes_nse[1] ~ "D2E (Clase baja)",
      Ingreso_Imputado_Por_UC > cortes_nse[1] & Ingreso_Imputado_Por_UC <= cortes_nse[2] ~ "D1 (Baja superior)",
      Ingreso_Imputado_Por_UC > cortes_nse[2] & Ingreso_Imputado_Por_UC <= cortes_nse[3] ~ "C3 (Media típica)",
      Ingreso_Imputado_Por_UC > cortes_nse[3] & Ingreso_Imputado_Por_UC <= cortes_nse[4] ~ "C2 (Media alta)",
      Ingreso_Imputado_Por_UC > cortes_nse[4] ~ "ABC1 (Clase alta)"
    ),
    
    # =============================================================
    # ÍNDICE DE ENTORNO ESCOLAR OBESOGÉNICO — versión revisada
    # =============================================================
    
    # ----------------------------------------------------------
    # Indicadores base
    # ----------------------------------------------------------
    provee_alimento    = Escuela_Provee_Alimento %in% c("Sí", "Si"),
    no_provee_alimento = Escuela_Provee_Alimento == "No",
    
    tiene_kiosco       = Escuela_Tiene_Kiosco %in% c("Sí", "Si"),
    no_tiene_kiosco    = Escuela_Tiene_Kiosco == "No",
    
    compro_kiosco      = Compro_En_Kiosco %in% c("Sí", "Si"),
    no_compro_kiosco   = Compro_En_Kiosco == "No",

    Riesgo_Provision = case_when(
      no_provee_alimento ~ 0,
      provee_alimento ~ rowSums(cbind(
        Esc_Provee_BebAzucaradas %in% c("Siempre", "A veces"),
        Esc_Provee_Copetin       %in% c("Siempre", "A veces"),
        Esc_Provee_Golosinas     %in% c("Siempre", "A veces"),
        Esc_Provee_Facturas      %in% c("Siempre", "A veces"),
        Esc_Provee_Sandwich      %in% c("Siempre", "A veces"),
        Esc_Provee_BebLight      %in% c("Siempre", "A veces")
      ), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    
    Prot_Provision = case_when(
      no_provee_alimento ~ 0,
      provee_alimento ~ rowSums(cbind(
        Esc_Provee_Frutas  %in% c("Siempre", "A veces"),
        Esc_Provee_Agua    %in% c("Siempre", "A veces"),
        Esc_Provee_Lacteos %in% c("Siempre", "A veces")
      ), na.rm = TRUE),
      TRUE ~ NA_real_
    ),
    
    Net_Provision = case_when(
      is.na(Riesgo_Provision) | is.na(Prot_Provision) ~ NA_real_,
      no_provee_alimento ~ 0,
      TRUE ~ Riesgo_Provision / 6 - Prot_Provision / 3
    ),

    Net_Kiosco = case_when(
      no_tiene_kiosco              ~ 0,
      tiene_kiosco & no_compro_kiosco ~ 0,
      tiene_kiosco & compro_kiosco ~
        as.numeric(Kiosco_Compro_No_Recomendados == 1) -
        as.numeric(Kiosco_Compro_Recomendados    == 1),
      TRUE ~ NA_real_
    ),
    
    Net_ActFisica = case_when(
      Hace_ActFisica_Escuela == "Sí" ~ -1,
      Hace_ActFisica_Escuela == "No" ~  0,
      TRUE ~ NA_real_
    ),

    Indice_Obesogenico = Net_Provision + Net_Kiosco + Net_ActFisica,
    
    # ----------------------------------------------------------
    # Categorización con cortes fijos en ±1/3
    #
    #   Bajo     < -1/3  →  entorno predominantemente protector
    #   Moderado  [-1/3, +1/3]  →  entorno neutro / mixto
    #   Alto     > +1/3  →  entorno predominantemente obesogénico
    # ----------------------------------------------------------
    Indice_Obesogenico_Cat = case_when(
      is.na(Indice_Obesogenico)        ~ NA_character_,
      Indice_Obesogenico < -1/3        ~ "Bajo",
      Indice_Obesogenico <=  1/3       ~ "Moderado",
      TRUE                             ~ "Alto"
    ),
    Indice_Obesogenico_Cat = factor(
      Indice_Obesogenico_Cat,
      levels = c("Bajo", "Moderado", "Alto")
    )
  ) 

# =============================================================================
# ÍNDICE MATERIAL - MCA
# =============================================================================

vars_material <- dataset %>%
  select(
    Material_Piso,
    Material_Paredes,
    Origen_Agua,
    Tiene_Baño,
    Desague_Baño,
    Tiene_Electricidad,
    Combustible_Cocina
  ) %>%
  mutate(
    Desague_Baño = case_when(
      is.na(Desague_Baño) &
        Tiene_Baño %in% c("Letrina (sin arrastre de agua)", "No tiene baño") ~ "Sin desagüe",
      TRUE ~ Desague_Baño
    ),
    across(everything(), ~ na_if(., "NS/NC")),
    across(everything(), ~ na_if(., "Ns/Nc")),
    across(everything(), ~ na_if(., "Otro")),
    across(everything(), ~ na_if(., "NA")),
    across(everything(), as.factor)
  )

# Filas completas para MCA
filas_mca <- complete.cases(vars_material)

mca_material <- MCA(
  vars_material[filas_mca, ],
  graph = FALSE
)

# Gráficos de diagnóstico
fviz_screeplot(mca_material, addlabels = TRUE) +
  labs(title = "Varianza explicada por dimensión - MCA Índice Material")

fviz_contrib(mca_material, choice = "var", axes = 1, top = 15)

# Scores dimensión 1
scores_dim1 <- mca_material$ind$coord[, 1]

# Reescalar a [0,1]
indice_material_raw <- (scores_dim1 - min(scores_dim1)) /
  (max(scores_dim1) - min(scores_dim1))

# Agregar al dataset
dataset$Indice_Material <- NA_real_
dataset$Indice_Material[filas_mca] <- indice_material_raw

# Categorizar en terciles
dataset <- dataset %>%
  mutate(
    Indice_Material_Cat = case_when(
      is.na(Indice_Material) ~ NA_character_,
      Indice_Material <= quantile(Indice_Material, 0.33, na.rm = TRUE) ~ "Bajo",
      Indice_Material <= quantile(Indice_Material, 0.66, na.rm = TRUE) ~ "Medio",
      TRUE ~ "Alto"
    ),
    Indice_Material_Cat = factor(
      Indice_Material_Cat,
      levels = c("Bajo", "Medio", "Alto")
    )
  )

dataset <- dataset %>%
  select(
    -NBI_1, -NBI_2, -NBI_3, -NBI_4, -NBI_5,
    -n_miembros_hogar, -n_ocupados, -ratio_dependencia, -jefe_sin_primaria,
    -matches("^M[0-9]{2}_SD_4$"),
    -matches("^M[0-9]{2}_SD_15$"),
    -matches("^M[0-9]{2}_SD_19$"),
    -M01_SD_16, -M01_SD_17,
    -Esc_Provee_BebAzucaradas, -Esc_Provee_BebLight, -Esc_Provee_Infusiones, -Esc_Provee_Copetin,
    -Esc_Provee_Golosinas, -Esc_Provee_Facturas, -Esc_Provee_Frutas, -Esc_Provee_Agua, -Esc_Provee_Lacteos,
    -Esc_Provee_Sandwich, -no_compro_kiosco, -Indice_Obesogenico, -no_provee_alimento, -no_tiene_kiosco,
    -provee_alimento, -tiene_kiosco, -compro_kiosco, -TA_semanal_tot_min_adol, -HO_semanal_tot_min_adol,   
    -HPO_semanal_tot_min_adol, -TL_semanal_tot_min_adol, -TOL_semanal_tot_min_adol, -ESC_semanal_tot_min_adol, 
    -Minutos_PorDia_ActFisica, -Ratio_Hacinamiento, -Indice_Material, -Riesgo_Provision, -Prot_Provision,
    -Net_Provision, -Net_Kiosco,-Net_ActFisica
  )

# =============================================================================
# TIPOS DE VARIABLES
# =============================================================================

# Funciones auxiliares
as_factor_ordenado <- function(x, niveles) {
  factor(x, levels = niveles)
}

as_factor_simple <- function(x) {
  factor(x)
}

as_numeric_safe <- function(x) {
  as.numeric(x)
}

dataset <- dataset %>%
  mutate(
    # ---------------------------------------------------------
    # Variable respuesta
    # ---------------------------------------------------------
    Target_Exceso_Peso = factor(
      Target_Exceso_Peso,
      levels = c("Sin exceso de peso", "Exceso de peso")
    ),
    
    Target_bin = case_when(
      Target_Exceso_Peso == "Exceso de peso" ~ 1,
      Target_Exceso_Peso == "Sin exceso de peso" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # ---------------------------------------------------------
    # Variables de diseño muestral 
    # ---------------------------------------------------------
    id = as.character(id),
    Aglomerado = as.factor(Aglomerado),
    Estrato = as.factor(Estrato),
    Ponderador = as.numeric(Ponderador),
    
    # ---------------------------------------------------------
    # Perfil sociodemográfico
    # ---------------------------------------------------------
    Region = as.factor(Region),
    Sexo = as.factor(Sexo),
    Edad = factor(Edad, levels = sort(unique(Edad))),
    
    Asiste_Escuela = as.factor(Asiste_Escuela),
    
    # ---------------------------------------------------------
    # Antropométricas
    # ---------------------------------------------------------
    Peso_kg = as.numeric(Peso_kg),
    Talla_cm = as.numeric(Talla_cm),
    IMC = as.numeric(IMC),
    
    # ---------------------------------------------------------
    # Alimentación
    # ---------------------------------------------------------
    across(
      c(
        Frec_Lacteos,
        Frec_Frutas,
        Frec_Verduras,
        Frec_Papas_Pastas,
        Frec_Carnes_Huevos,
        Frec_Pescado,
        Frec_Snacks_Copetin,
        Frec_Golosinas,
        Frec_Bebidas_Azucaradas,
        Frec_Agua
      ),
      ~ factor(
        .,
        levels = c(
          "Consumo ocasional",
          "Consumo semanal",
          "Consumo diario"
        ),
        ordered = TRUE
      )
    ),
    
    # ---------------------------------------------------------
    # Actividad física, sedentarismo y sueño
    # ---------------------------------------------------------
    HorasPorDia_Sedentarismo = as.numeric(HorasPorDia_Sedentarismo),
    HorasPorDia_Sueño = as.numeric(HorasPorDia_Sueño),
    Horas_PorDia_ActFisica = as.numeric(Horas_PorDia_ActFisica),
    
    Cumple_OMS_ActFisica = as.factor(Cumple_OMS_ActFisica),
    Cumple_OMS_Sueño = as.factor(Cumple_OMS_Sueño),
    Hace_ActFisica_Escuela = as.factor(Hace_ActFisica_Escuela),
    
    # ---------------------------------------------------------
    # Entorno escolar
    # ---------------------------------------------------------
    Escuela_Provee_Alimento = as.factor(Escuela_Provee_Alimento),
    Escuela_Tiene_Kiosco = as.factor(Escuela_Tiene_Kiosco),
    Compro_En_Kiosco = as.factor(Compro_En_Kiosco),
    
    Kiosco_Compro_No_Recomendados = factor(
      Kiosco_Compro_No_Recomendados,
      levels = c(0, 1),
      labels = c("No", "Sí")
    ),
    
    Kiosco_Compro_Recomendados = factor(
      Kiosco_Compro_Recomendados,
      levels = c(0, 1),
      labels = c("No", "Sí")
    ),
    
    Indice_Obesogenico_Cat = factor(
      Indice_Obesogenico_Cat,
      levels = c("Bajo", "Moderado", "Alto"),
      ordered = TRUE
    ),
    
    # ---------------------------------------------------------
    # Socioeconómicas
    # ---------------------------------------------------------
    Cobertura_Salud = as.factor(Cobertura_Salud),
    
    Nivel_Educacion_Jefe = factor(
      Nivel_Educacion_Jefe,
      levels = c(
        "Bajo_Nivel_Educativo",
        "Medio_Nivel_Educativo",
        "Alto_Nivel_Educativo"
      ),
      ordered = TRUE
    ),
    
    Quintil_Ingreso = factor(
      Quintil_Ingreso,
      levels = sort(unique(Quintil_Ingreso)),
      ordered = TRUE
    ),
    
    Ingreso_Imputado_Por_UC = as.numeric(Ingreso_Imputado_Por_UC),
    
    Indice_NSE = factor(
      Indice_NSE,
      levels = c(
        "D2E (Clase baja)",
        "D1 (Baja superior)",
        "C3 (Media típica)",
        "C2 (Media alta)",
        "ABC1 (Clase alta)"
      ),
      ordered = TRUE
    ),
    
    Indice_NBI = factor(
      Indice_NBI,
      levels = c("Sin NBI", "Con NBI")
    ),
    
    # ---------------------------------------------------------
    # Vivienda
    # ---------------------------------------------------------
    Tipo_Vivienda = as.factor(Tipo_Vivienda),
    
    Cant_Miembros = as.numeric(Cant_Miembros),
    Cant_Habitaciones = as.numeric(Cant_Habitaciones),
    
    Indice_Hacinamiento = factor(
      Indice_Hacinamiento,
      levels = c(
        "Holgado",
        "Sin hacinamiento",
        "Hacinamiento moderado",
        "Hacinamiento crítico"
      ),
      ordered = TRUE
    ),
    
    Material_Piso = as.factor(Material_Piso),
    Material_Paredes = as.factor(Material_Paredes),
    Origen_Agua = as.factor(Origen_Agua),
    Tiene_Baño = as.factor(Tiene_Baño),
    Desague_Baño = as.factor(Desague_Baño),
    Tiene_Electricidad = as.factor(Tiene_Electricidad),
    Tiene_Heladera = as.factor(Tiene_Heladera),
    Combustible_Cocina = as.factor(Combustible_Cocina),
    Uso_Vivienda = as.factor(Uso_Vivienda),
    
    Indice_Material_Cat = factor(
      Indice_Material_Cat,
      levels = c("Bajo", "Medio", "Alto"),
      ordered = TRUE
    )
  )

# TABLA NAs --------------------------------------------------------------------------------------------------

na_summary <- dataset %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_na") %>%
  mutate(
    porcentaje_na = n_na / nrow(dataset) * 100
  ) %>%
  arrange(desc(porcentaje_na))

#write.csv(na_summary, "NA_summary.csv", row.names = FALSE)


#Ver como controlar las categorias desbalanceadas----------------------------------------------------

# ============================================================
# 2. Función para revisar proporciones por categoría
# ============================================================

# tabla <- function(data, vars = NULL) {
# 
#   if (is.null(vars)) {
#     vars <- data %>%
#       select(where(is.factor)) %>%
#       names()
#   }
# 
#   map_dfr(vars, function(v) {
#     data %>%
#       count(categoria = .data[[v]], name = "n") %>%
#       mutate(
#         variable = v,
#         categoria = as.character(categoria),
#         categoria = replace_na(categoria, "<NA>"),
#         prop = n / sum(n),
#         prop_pct = round(prop * 100, 2)
#       ) %>%
#       select(variable, categoria, n, prop_pct) %>%
#       arrange(variable, desc(n))
#   })
# }
# 
# # Diagnóstico inicial solo para variables factor
# diagnostico_antes <- tabla(dataset)
# 
# diagnostico_antes %>%
#   filter(prop_pct < 0.10) %>%
#   arrange(variable, prop_pct)

dataset <- dataset %>%
  
  mutate(
    Asiste_Escuela = case_when(
      Asiste_Escuela %in% c("No asiste pero asistió", "Nunca asistió") ~ "No asiste",
      TRUE ~ Asiste_Escuela
    )
  ) %>%
  
  mutate(
    no_asiste = Asiste_Escuela == "No asiste",
    
    Hace_ActFisica_Escuela = case_when(
      no_asiste ~ "No aplica",
      TRUE ~ Hace_ActFisica_Escuela
    ),
    
    Escuela_Provee_Alimento = case_when(
      no_asiste ~ "No aplica",
      TRUE ~ Escuela_Provee_Alimento
    ),
    
    Escuela_Tiene_Kiosco = case_when(
      no_asiste ~ "No aplica",
      TRUE ~ Escuela_Tiene_Kiosco
    ),
    
    Compro_En_Kiosco = case_when(
      no_asiste ~ "No aplica",
      Escuela_Tiene_Kiosco %in% c("No", "No aplica") ~ "No aplica",
      TRUE ~ Compro_En_Kiosco
    ),
    
    Kiosco_Compro_No_Recomendados = case_when(
      no_asiste ~ "No aplica",
      Escuela_Tiene_Kiosco %in% c("No", "No aplica") ~ "No aplica",
      TRUE ~ as.character(Kiosco_Compro_No_Recomendados)
    ),
    
    Kiosco_Compro_Recomendados = case_when(
      no_asiste ~ "No aplica",
      Escuela_Tiene_Kiosco %in% c("No", "No aplica") ~ "No aplica",
      TRUE ~ as.character(Kiosco_Compro_Recomendados)
    )
  ) %>%
  
  select(-no_asiste) %>%
  
  mutate(
    Tipo_Vivienda = case_when(
      Tipo_Vivienda %in% c("Casa", "Departamento") ~ Tipo_Vivienda,
      Tipo_Vivienda %in% c("Casilla", "Rancho", "Inquilinato/ conventillo", "Otros") ~
        "Vivienda precaria / otra",
      TRUE ~ Tipo_Vivienda
    ),
    
    Material_Piso = case_when(
      Material_Piso %in% c("Ladrillo suelto o tierra", "Otro") ~ "Piso precario / otro",
      TRUE ~ Material_Piso
    ),
    
    Material_Paredes = case_when(
      Material_Paredes == "Ladrillo, piedra, bloque u hormigón?" ~
        "Ladrillo/piedra/bloque/hormigón",
      TRUE ~ "Otros materiales"
    ),
    
    Origen_Agua = case_when(
      Origen_Agua == "Por cañería dentro de la vivienda" ~
        "Dentro de la vivienda",
      Origen_Agua %in% c(
        "Fuera de la vivienda, dentro del terreno",
        "Fuera del terreno"
      ) ~ "Fuera de la vivienda",
      TRUE ~ Origen_Agua
    ),
    
    Tiene_Baño = case_when(
      Tiene_Baño == "Inodoro con botón o cadena y arrastre de agua" ~
        "Inodoro con arrastre",
      Tiene_Baño == "Inodoro sin botón o cadena y arrastre de agua" ~
        "Inodoro sin botón/cadena",
      Tiene_Baño %in% c(
        "Letrina (sin arrastre de agua)",
        "No tiene baño"
      ) ~ "Letrina o sin baño",
      TRUE ~ Tiene_Baño
    ),
    
    Desague_Baño = case_when(
      Tiene_Baño == "Letrina o sin baño" & is.na(Desague_Baño) ~ "No aplica",
      is.na(Desague_Baño) ~ "NS/NC",
      Desague_Baño %in% c(
        "Solamente a pozo ciego",
        "A hoyo / excavación en la tierra"
      ) ~ "Pozo ciego / hoyo",
      TRUE ~ Desague_Baño
    ),
    
    Tiene_Electricidad = case_when(
      Tiene_Electricidad == "... por red?" ~ "Por red",
      TRUE ~ "Sin red / otra fuente"
    ),
    
    Combustible_Cocina = case_when(
      Combustible_Cocina %in% c("Gas de red", "Gas en garrafa") ~ Combustible_Cocina,
      TRUE ~ "Otro combustible"
    ),
    
    Indice_Hacinamiento = case_when(
      Indice_Hacinamiento %in% c("Holgado", "Sin hacinamiento") ~
        "Sin hacinamiento / holgado",
      TRUE ~ Indice_Hacinamiento
    ),
    
    Indice_Obesogenico_Cat = case_when(
      is.na(Indice_Obesogenico_Cat) ~ "NS/NC",
      Indice_Obesogenico_Cat %in% c("Moderado", "Alto") ~ "Moderado/Alto",
      TRUE ~ Indice_Obesogenico_Cat
    )
  )

na_summary_2 <- dataset %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_na") %>%
  mutate(
    porcentaje_na = n_na / nrow(dataset) * 100
  ) %>%
  arrange(desc(porcentaje_na))

#write.csv(na_summary_2, "NA_summary2.csv", row.names = FALSE)

write.csv(dataset, "dataset.csv", row.names = FALSE)
