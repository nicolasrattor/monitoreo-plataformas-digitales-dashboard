# ============================================================================
# Reproduce desde los microdatos todos los gráficos y cifras de la ponencia
# "¿Transición a la formalidad en los trabajadores de plataformas digitales?
#  Discutiendo los efectos de la Ley 21.431 de Chile"
#
# Ubicación sugerida: monitoreo_plataformas_digitales-main/scripts_R/
# Entradas (relativas a la raíz del proyecto, PROY_DIR o getwd()):
#   input/data/ano=YYYY/mes_central=M/part-0.parquet   microdatos ENE (hive)
#   input/esi_2020-personas.rdata, esi_2021.rdata, ... esi_2025_personas.rdata
#   input/sii/salida.xlsx                              BTE entregado por el SII (AE006W50032085)
#   input/isl/AL006T0007789.xlsx                       respuesta del ISL
#
# Salidas:
#   output/graficos/fig1_informalidad.png
#   output/graficos/fig2_bte_sii_vs_ene_formales_ic.png
#   output/graficos/fig3_ingreso_mensual_esi.png
#   output/tablas/cifras_ponencia.xlsx   (cada cifra del texto vs. valor calculado)
#   output/tablas/series_ene_ley21431.csv, ic_ene_formales.csv, esi_2020_2025.csv
#
# Metodología clave (la misma de scripts_python/compute_series.py):
#   * Universo Ley 21.431: plataformas_digitales == 1 y al menos una marca
#     declarada no excluida (redes sociales, streaming, alojamiento, etc.).
#   * Informalidad: ocup_form (1 formal, 2 informal) entre ese universo.
#   * IC 95% de totales y razones: linealización de Taylor con estrato,
#     conglomerado y fact_cal; estratos con un solo conglomerado aportan 0
#     (survey.lonely.psu = "certainty").
# ============================================================================

suppressPackageStartupMessages({
  library(arrow);   library(dplyr);  library(tidyr);   library(purrr)
  library(stringr); library(ggplot2); library(scales); library(readxl)
  library(openxlsx); library(survey); library(srvyr);  library(matrixStats)
})
options(survey.lonely.psu = "certainty")

for (d in c("output/graficos", "output/tablas", "output/data"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------------------------------
# 0. Parámetros
# ----------------------------------------------------------------------------
PERIODO_MIN <- as.Date("2020-02-01")     # primer trimestre móvil (centrado en feb-2020)
Z95         <- 1.96                      # IC normal, como en el gráfico BTE vs ENE
# Universo para el bloque de ingresos (ESI): "nuevo" = marcas Ley 21.431
# (marcas.py); "antiguo" = listas de exclusión por string de scripts_R/salarios_*.R.
# Cambia la mediana y la media citadas ($528.399 / $668.045 vs $532.238 / $666.133).
UNIVERSO_ESI <- "nuevo"
RUTA_BTE <- "input/sii/salida.xlsx"
RUTA_ISL <- "input/isl/AL006T0007789.xlsx"

AZUL <- "#1F4E79"; NARANJA <- "#D97706"; GRIS <- "#6B7280"

fmt_n <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)

# ----------------------------------------------------------------------------
# 1. Identificación de marcas (port de scripts_python/marcas.py)
# ----------------------------------------------------------------------------
MULTI <- c("UBER EATS" = "UBER EATS", "PEDIDOS YA" = "PEDIDOS YA", "PEDIDOSYA" = "PEDIDOS YA",
           "MERCADO LIBRE" = "MERCADOLIBRE", "PICKER UP" = "PICKER UP", "TIME JOBS" = "TIME JOBS",
           "JUMBO SHOPPERS" = "JUMBO SHOPPERS", "META ADS" = "META ADS", "EASY PROP" = "EASY PROP",
           "EASY TAXI" = "EASY TAXI", "CAMION GO" = "CAMION GO", "URBAN WRITERS" = "URBAN WRITERS",
           "SEX PATAGONIA" = "SEX PATAGONIA", "SAWU FIVER" = "SAWU",
           "MUNICIPALIDAD DE CONDES" = "MUNICIPALIDAD DE LAS CONDES")
MAX_N  <- max(lengths(strsplit(names(MULTI), " ")))
ALIAS  <- c("WATTASPP" = "WHATSAPP", "WSP" = "WHATSAPP", "INSTAGRAMWSP" = "INSTAGRAM",
            "FIVER" = "FIVERR", "YAPO" = "YAPO.CL", "MERCADOLIBRE" = "MERCADOLIBRE")
EXPAND <- list("INSTAGRAMWSP" = c("INSTAGRAM", "WHATSAPP"))
EXCLUIR <- c("INSTAGRAM", "FACEBOOK", "WHATSAPP", "MARKETPLACE", "TIKTOK", "TELEGRAM", "TWITTER",
             "YOUTUBE", "TWITCH", "SPOTIFY", "ONLYFANS", "CHATURBATE", "CAM4", "ARSMATE",
             "SEX PATAGONIA", "VGEN", "AIRBNB", "BOOKING", "DOCTORALIA", "PREPLY", "SUPERPROF",
             "CLASSGAP", "ENCUADRADO", "FIVERR", "URBAN WRITERS", "META ADS")

marcas <- function(texto) {
  if (is.na(texto) || !nzchar(texto)) return(character(0))
  toks <- strsplit(trimws(gsub("[,/]", " ", toupper(as.character(texto)))), " +")[[1]]
  toks <- toks[nzchar(toks)]
  out <- character(0); i <- 1
  while (i <= length(toks)) {
    hit <- NULL
    nmax <- min(MAX_N, length(toks) - i + 1)
    if (nmax >= 2) for (n in nmax:2) {
      fr <- paste(toks[i:(i + n - 1)], collapse = " ")
      if (fr %in% names(MULTI)) { hit <- list(MULTI[[fr]], n); break }
    }
    if (is.null(hit)) {
      t <- toks[i]
      if (t %in% names(EXPAND)) {
        for (m in EXPAND[[t]]) if (!(m %in% out)) out <- c(out, m)
        i <- i + 1; next
      }
      hit <- list(if (t %in% names(ALIAS)) ALIAS[[t]] else t, 1)
    }
    if (!(hit[[1]] %in% out)) out <- c(out, hit[[1]])
    i <- i + hit[[2]]
  }
  # regla acordada: si menciona MARKETPLACE, FACEBOOK no se cuenta aparte
  if ("MARKETPLACE" %in% out && "FACEBOOK" %in% out) out <- out[out != "FACEBOOK"]
  out
}
es_ley_uno <- function(x) any(!(marcas(x) %in% EXCLUIR))

# se evalúa solo sobre los valores distintos (unos 200 strings), no sobre millones de filas
es_ley <- function(v) {
  v <- as.character(v); v[is.na(v)] <- ""
  u <- unique(v)
  r <- vapply(u, es_ley_uno, logical(1), USE.NAMES = FALSE)
  r[match(v, u)]
}

# Universo "antiguo" (listas por string de scripts_R/salarios_plataformas_2020_2025.R)
EXCL_A <- c("INSTAGRAM","FACEBOOK","WHATSAPP","FACEBOOK INSTAGRAM","FACEBOOK MARKETPLACE",
  "INSTAGRAM FACEBOOK","FACEBOOK WHATSAPP","WHATSAPP FACEBOOK","WHATSAPP FACEBOOK INSTAGRAM",
  "INSTAGRAM FACEBOOK WHATSAPP","INSTAGRAM WHATSAPP","MARKETPLACE","FACEBOOK INSTAGRAM WHATSAPP",
  "WHATSAPP INSTAGRAM","WHATSAPP INSTAGRAM FACEBOOK","FACEBOOK MARKETPLACE INSTAGRAM","TIKTOK",
  "FACEBOOK WHATSAPP INSTAGRAM","INSTAGRAM TIKTOK","FACEBOOK INSTAGRAM TIKTOK","MARKETPLACE FACEBOOK",
  "MARKETPLACE INSTAGRAM","INSTAGRAM WHATSAPP FACEBOOK","INSTAGRAM MARKETPLACE","TELEGRAM",
  "FACEBOOK MARKETPLACE WHATSAPP","FACEBOOK MARKETPLACE INSTAGRAM WHATSAPP","INSTAGRAM FACEBOOK TIKTOK",
  "WATTASPP INSTAGRAM","FACEBOOK WHATSAPP INSTAGRAM TIKTOK","TIKTOK FACEBOOK INSTAGRAM WHATSAPP",
  "FACEBOOK TIKTOK INSTAGRAM","FACEBOOK INSTAGRAM MARKETPLACE","FACEBOOK TIKTOK","WHATSAPP FACEBOOK YAPO",
  "INSTAGRAM FACEBOOK WHATSAPP TIKTOK","WHATSAPP TIKTOK FACEBOOK","TWITTER INSTAGRAM",
  "FACEBOOK MARKETPLACE INSTAGRAM WHATSAPP FACEBOOK","INSTAGRAM MUNICIPALIDAD DE CONDES","INSTAGRAMWSP")
EXCL_B <- c("YOUTUBE", "AIRBNB", "ONLYFANS", "BOOKING", "WHATSAPP YOUTUBE", "TWITCH")
es_ley_antiguo <- function(v) { v <- as.character(v); !is.na(v) & nzchar(v) & !(v %in% c(EXCL_A, EXCL_B)) }

# ----------------------------------------------------------------------------
# 2. Varianza por linealización de Taylor (estratos x conglomerados)
# ----------------------------------------------------------------------------
var_taylor <- function(estrato, psu, z) {
  tibble(estrato, psu, z) %>%
    group_by(estrato, psu) %>% summarise(z = sum(z), .groups = "drop") %>%
    group_by(estrato) %>%
    summarise(v = if (n() > 1) n() / (n() - 1) * sum((z - mean(z))^2) else 0, .groups = "drop") %>%
    summarise(v = sum(v)) %>% pull(v)
}

# ----------------------------------------------------------------------------
# 3. Recorrido de los microdatos ENE, un trimestre móvil a la vez
#    (~7 millones de filas: no se cargan todas juntas)
# ----------------------------------------------------------------------------
archivos <- list.files("input/data", pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
ano_f <- as.integer(str_match(archivos, "ano=(\\d{4})")[, 2])
archivos <- archivos[!is.na(ano_f) & ano_f >= 2020]

COLS <- c("periodo", "fact_cal", "activ", "estrato", "conglomerado", "idrph", "ocup_form",
          "plataformas_digitales", "pd_especifique", "sda_pd", "sda_pd_especifique")

procesar <- function(f) {
  d <- read_parquet(f, col_select = all_of(COLS))
  per <- as.Date(d$periodo[1])
  if (per < PERIODO_MIN) return(NULL)

  ocup <- d$activ %in% 1
  pd1  <- d$plataformas_digitales %in% 1
  prin <- pd1 & es_ley(d$pd_especifique)                          # 1ª ocupación en plataforma Ley 21.431
  seg  <- (d$sda_pd %in% 1) & !pd1 & es_ley(d$sda_pd_especifique) # solo 2ª ocupación
  form <- d$ocup_form %in% 1
  inf  <- d$ocup_form %in% 2
  w    <- d$fact_cal

  Y_for <- sum(w[prin & form]); Y_inf <- sum(w[prin & inf]); Y_ren <- Y_for + Y_inf
  y_for <- w * (prin & form);   y_inf <- w * (prin & inf)
  p_inf <- Y_inf / Y_ren
  z_p   <- (y_inf - p_inf * (y_for + y_inf)) / Y_ren              # linealización de la razón
  v_t   <- function(y) sqrt(var_taylor(d$estrato, d$conglomerado, y))

  res <- tibble(
    periodo = per, ocupados_nac = sum(w[ocup]), informales_nac = sum(w[ocup & inf]),
    prin = sum(w[prin]), prin_formal = Y_for, prin_informal = Y_inf, seg = sum(w[seg]),
    n_prin = sum(prin), n_formal = sum(prin & form),
    se_formal = v_t(y_for), se_informal = v_t(y_inf), se_tasa_inf = v_t(z_p))

  # filas de plataformas del trimestre OND (central = noviembre) para unir con la ESI
  filas_esi <- NULL
  if (format(per, "%m") == "11")
    filas_esi <- d[pd1, c("idrph", "fact_cal", "pd_especifique")] %>%
      mutate(ano = as.integer(format(per, "%Y")))
  list(res = res, esi = filas_esi)
}

message("Procesando ", length(archivos), " archivos parquet ...")
out <- map(archivos, procesar) %>% compact()
serie    <- bind_rows(map(out, "res")) %>% arrange(periodo)
plat_nov <- bind_rows(map(out, "esi"))

serie <- serie %>% mutate(
  tasa_informal = prin_informal / (prin_formal + prin_informal) * 100,
  ic_tasa_inf   = Z95 * se_tasa_inf * 100,
  ic_formal_inf = prin_formal - Z95 * se_formal,
  ic_formal_sup = prin_formal + Z95 * se_formal,
  universo_1y2  = prin + seg,                    # incluye quienes solo la tienen como 2ª ocupación
  pct_ocupados  = universo_1y2 / ocupados_nac * 100,
  pct_principal = prin / (prin + seg) * 100,     # plataforma como ocupación principal
  inf_nacional  = informales_nac / ocupados_nac * 100)
saveRDS(serie, "output/data/serie_ene_ley21431.rds")
write.csv(serie, "output/tablas/series_ene_ley21431.csv", row.names = FALSE)
write.csv(serie %>% transmute(periodo, formales = prin_formal, ee = se_formal, ic_inf = ic_formal_inf,
                              ic_sup = ic_formal_sup, n_muestral = n_formal),
          "output/tablas/ic_ene_formales.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# 4. Figura 1: tasa de informalidad (ENE, trimestres móviles)
# ----------------------------------------------------------------------------
i_min <- which.min(serie$tasa_informal); i_max <- which.max(serie$tasa_informal); ult <- nrow(serie)
etq <- function(d) { m <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
                     paste0(m[as.integer(format(d, "%m"))], ". ", format(d, "%Y")) }
lab <- function(i, pre = "") paste0(pre, format(round(serie$tasa_informal[i], 1), nsmall = 1), "%\n(", etq(serie$periodo[i]), ")")

g1 <- ggplot(serie, aes(periodo, tasa_informal)) +
  geom_ribbon(aes(ymin = tasa_informal, ymax = 100), fill = "#EEF2F6") +
  geom_line(color = AZUL, linewidth = 1.3) +
  geom_point(data = serie[c(i_max, i_min, ult), ], color = AZUL, size = 3) +
  annotate("text", x = serie$periodo[i_max], y = serie$tasa_informal[i_max] - 4, hjust = -0.1,
           label = lab(i_max), color = AZUL, fontface = "bold", size = 4) +
  annotate("text", x = serie$periodo[i_min], y = serie$tasa_informal[i_min] - 5,
           label = lab(i_min, "Mínimo: "), color = AZUL, fontface = "bold", size = 4) +
  annotate("text", x = serie$periodo[ult], y = serie$tasa_informal[ult] + 5, hjust = 0.7,
           label = lab(ult), color = AZUL, fontface = "bold", size = 4) +
  scale_y_continuous(limits = c(40, 100), labels = label_percent(scale = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "% de ocupados en plataformas con empleo informal",
       caption = "Fuente: elaboración propia a partir de la Encuesta Nacional de Empleo (INE). Trimestres móviles.") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, color = GRIS), axis.text = element_text(color = GRIS),
        axis.title.y = element_text(color = GRIS))
ggsave("output/graficos/fig1_informalidad.png", g1, width = 11, height = 5.8, dpi = 200, bg = "white")

# variante con IC 95% de la tasa: muestra qué variaciones son ruido muestral
g1b <- ggplot(serie, aes(periodo, tasa_informal)) +
  geom_ribbon(aes(ymin = pmax(tasa_informal - ic_tasa_inf, 0), ymax = pmin(tasa_informal + ic_tasa_inf, 100)),
              fill = AZUL, alpha = .18) +
  geom_line(color = AZUL, linewidth = 1.2) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(x = NULL, y = "% informal (IC 95%)") + theme_minimal(base_size = 13)
ggsave("output/graficos/fig1_informalidad_con_IC.png", g1b, width = 11, height = 5.8, dpi = 200, bg = "white")

# ----------------------------------------------------------------------------
# 5. Figura 2: receptores únicos de BTE (SII) vs. ocupados formales ENE (IC 95%)
# ----------------------------------------------------------------------------
bte <- read_excel(RUTA_BTE, sheet = "A)", skip = 1) %>%
  transmute(periodo = as.Date(sprintf("%d-%02d-01", as.integer(`Año`), as.integer(Mes))),
            bte_emitidas = `Cantidad de BTE emitidas`,
            receptores   = `Cantidad de receptores únicos de BTE`) %>% arrange(periodo)
ene_f <- serie %>% filter(periodo >= as.Date("2024-03-01"))
atip  <- bte %>% filter(periodo %in% as.Date(c("2024-03-01", "2025-02-01")))
ult_b <- slice_tail(bte, n = 1); ult_e <- slice_tail(ene_f, n = 1)

g2 <- ggplot() +
  geom_ribbon(data = ene_f, aes(periodo, ymin = ic_formal_inf, ymax = ic_formal_sup), fill = NARANJA, alpha = .18) +
  geom_line(data = bte,   aes(periodo, receptores),  color = AZUL,    linewidth = 1.3) +
  geom_line(data = ene_f, aes(periodo, prin_formal), color = NARANJA, linewidth = 1.3) +
  geom_point(data = atip, aes(periodo, receptores), shape = 21, fill = "white", color = AZUL, size = 3, stroke = 1.2) +
  geom_point(data = ult_b, aes(periodo, receptores),  color = AZUL, size = 3) +
  geom_point(data = ult_e, aes(periodo, prin_formal), color = NARANJA, size = 3) +
  geom_text(data = ult_b, aes(periodo, receptores,  label = fmt_n(receptores)),  color = AZUL,    hjust = -0.2, fontface = "bold") +
  geom_text(data = ult_e, aes(periodo, prin_formal, label = fmt_n(prin_formal)), color = NARANJA, hjust = -0.2, fontface = "bold") +
  annotate("text", x = ult_e$periodo, y = ult_e$ic_formal_sup + 4000, hjust = 0.4, size = 3.3, color = NARANJA,
           label = paste0("IC95%: ", fmt_n(ult_e$ic_formal_inf), "–", fmt_n(ult_e$ic_formal_sup))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ","), limits = c(0, 130000)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               limits = c(as.Date("2024-02-01"), as.Date("2026-10-15"))) +
  labs(x = NULL, y = "Número de personas",
       caption = paste0("Fuente: SII (Ley 20.285, folio AE006W50032085) y ENE (INE, trimestres móviles). Azul: receptores únicos de BTE; ",
                        "naranja: ocupados formales en plataformas Ley 21.431 con IC 95%.\n",
                        "Círculos vacíos: meses atípicos del SII (mar. 2024; feb. 2025).")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, color = GRIS, size = 9), axis.text = element_text(color = GRIS))
ggsave("output/graficos/fig2_bte_sii_vs_ene_formales_ic.png", g2, width = 13, height = 6.5, dpi = 200, bg = "white")

# chequeo de la frase "las boletas superan a los formales durante todo el periodo"
cmp <- inner_join(bte, ene_f, by = "periodo") %>%
  transmute(periodo, receptores, formales = prin_formal, ic_sup = ic_formal_sup,
            bte_sobre_formales = receptores > formales, bte_sobre_ic_sup = receptores > ic_sup)

# ----------------------------------------------------------------------------
# 6. ESI 2020-2025: ingresos y horas (trimestre OND de cada año)
# ----------------------------------------------------------------------------
params <- tibble::tribble(
  ~ano, ~archivo_esi,                          ~imm_bruto,
  2020, "input/esi_2020-personas.rdata",         326500,
  2021, "input/esi_2021.rdata",                  337000,
  2022, "input/esi_2022.rdata",                  400000,
  2023, "input/esi_2023.rdata",                  460000,
  2024, "input/esi_2024.rdata",                  500000,
  2025, "input/esi_2025_personas.rdata",         529000) %>%
  mutate(imm_neto = round(imm_bruto * 430077 / 529000))

num <- function(x) as.numeric(unclass(x))

merge_esi <- function(ano_i, archivo) {
  plat <- plat_nov %>% filter(ano == ano_i)
  plat <- if (UNIVERSO_ESI == "nuevo") filter(plat, es_ley(pd_especifique)) else filter(plat, es_ley_antiguo(pd_especifique))
  e <- new.env(); load(archivo, envir = e)
  base <- get("base", envir = e) %>%
    transmute(idrph, estrato = as.character(unclass(estrato)),
              conglomerado = as.character(unclass(conglomerado)),  # el tipo cambia entre años (chr/dbl)
              fact_cal_y = num(fact_cal),
              d4_dias = num(d4_dias), d4_horas = num(d4_horas),
              fact_cal_esi = num(fact_cal_esi), ing_t_p = num(ing_t_p))
  plat %>% rename(fact_cal_ene = fact_cal) %>% left_join(base, by = "idrph") %>%
    mutate(ano = ano_i, horas_mes = d4_horas * d4_dias, itp_hora = ing_t_p / horas_mes,
           itp_hora = ifelse(is.finite(itp_hora), itp_hora, NA_real_))
}
esi_all <- map2_dfr(params$ano, params$archivo_esi, merge_esi)

estimar <- function(d) {
  v   <- d %>% filter(!is.na(fact_cal_esi), fact_cal_esi > 0)
  dis <- v %>% as_survey_design(ids = conglomerado, strata = estrato, weights = fact_cal_esi)
  ic_m  <- dis %>% summarise(x = survey_mean(ing_t_p,   na.rm = TRUE, vartype = "ci"))
  ic_md <- dis %>% summarise(x = survey_median(ing_t_p, na.rm = TRUE, vartype = "ci"))
  ic_h  <- dis %>% summarise(x = survey_mean(itp_hora,  na.rm = TRUE, vartype = "ci"))
  tibble(
    n_muestra = nrow(d), n_valido_ingreso = sum(!is.na(v$ing_t_p)), n_valido_hora = sum(!is.na(v$itp_hora)),
    ing_media = weighted.mean(v$ing_t_p, v$fact_cal_esi, na.rm = TRUE),
    ing_media_inf = ic_m$x_low, ing_media_sup = ic_m$x_upp,
    ing_mediana = matrixStats::weightedMedian(v$ing_t_p, v$fact_cal_esi, na.rm = TRUE),
    ing_mediana_inf = as.numeric(ic_md$x_low), ing_mediana_sup = as.numeric(ic_md$x_upp),
    hora_media = weighted.mean(v$itp_hora, v$fact_cal_esi, na.rm = TRUE),
    hora_media_inf = ic_h$x_low, hora_media_sup = ic_h$x_upp,
    horas_mes = weighted.mean(d$horas_mes, d$fact_cal_y, na.rm = TRUE))
}
esi_res <- esi_all %>% group_by(ano) %>% group_modify(~ estimar(.x)) %>% ungroup() %>%
  left_join(params, by = "ano") %>%
  mutate(razon_mediana_imm_neto = ing_mediana / imm_neto, razon_media_imm_neto = ing_media / imm_neto)
write.csv(esi_res, "output/tablas/esi_2020_2025.csv", row.names = FALSE)

# Figura 3: ingreso mensual (media y mediana, IC 95%) vs ingreso mínimo neto
g3_df <- esi_res %>%
  transmute(ano = factor(ano), Media = ing_media, Mediana = ing_mediana,
            Media_i = ing_media_inf, Media_s = ing_media_sup,
            Mediana_i = ing_mediana_inf, Mediana_s = ing_mediana_sup, imm_neto) %>%
  pivot_longer(c(Media, Mediana), names_to = "estadistico", values_to = "valor") %>%
  mutate(ic_inf = ifelse(estadistico == "Media", Media_i, Mediana_i),
         ic_sup = ifelse(estadistico == "Media", Media_s, Mediana_s))
g3 <- ggplot(g3_df, aes(ano, valor, fill = estadistico)) +
  geom_col(position = position_dodge(.75), width = .68) +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), position = position_dodge(.75), width = .16,
                linewidth = .6, color = "gray25", na.rm = TRUE) +
  geom_segment(data = distinct(g3_df, ano, imm_neto),
               aes(x = as.numeric(ano) - .45, xend = as.numeric(ano) + .45, y = imm_neto, yend = imm_neto),
               inherit.aes = FALSE, linetype = "dashed", linewidth = .9, color = "#C0392B") +
  scale_fill_manual(values = c(Media = "#2C3E50", Mediana = "#5DADE2")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = "Pesos del mes", fill = NULL,
       caption = paste0("Línea roja: ingreso mínimo mensual neto del año. Barras de error: IC 95% (diseño complejo ESI). Pesos nominales.\n",
                        "Fuente: elaboración propia en base a ENE y ESI (INE), trimestre OND.")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8.5, color = "gray50"))
ggsave("output/graficos/fig3_ingreso_mensual_esi.png", g3, width = 11, height = 6.2, dpi = 200, bg = "white")

# Horas: promedio 2020-2025 y percentil 80 (quintil superior); 4 semanas por mes, como en el texto
wq <- function(x, w, p) { k <- !is.na(x) & !is.na(w) & w > 0; x <- x[k]; w <- w[k]
                          o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w); x[which(cw >= p)[1]] }
hh <- esi_all %>% filter(!is.na(horas_mes), !is.na(fact_cal_y), fact_cal_y > 0)
horas_prom_mes <- weighted.mean(hh$horas_mes, hh$fact_cal_y)
horas_p80_sem  <- wq(hh$horas_mes, hh$fact_cal_y, .8) / 4

# ----------------------------------------------------------------------------
# 7. Registro administrativo (no es microdato ENE): ISL
# ----------------------------------------------------------------------------
isl_res <- read_excel(RUTA_ISL, sheet = "00_Resumen", skip = 3)
isl_n   <- as.numeric(isl_res$Resultado[isl_res$Indicador == "Denuncias identificadas"])

# ----------------------------------------------------------------------------
# 8. Cifras del texto: lo que dice la ponencia vs. lo que calcula el código
# ----------------------------------------------------------------------------
anual   <- function(v, a) mean(serie[[v]][format(serie$periodo, "%Y") == a])
desde24 <- serie %>% filter(periodo >= as.Date("2024-01-01"))
e25 <- esi_res %>% filter(ano == 2025); e20 <- esi_res %>% filter(ano == 2020)
r2  <- function(x) as.character(round(x, 2))

cifras <- tibble(
  cifra = c("N° de trimestres móviles ENE",
            "Universo Ley 21.431 (1ª + 2ª ocupación), último trimestre",
            "Universo como % de ocupados, último trimestre",
            "Informalidad nacional (todos los ocupados), último trimestre",
            "Tasa de informalidad en plataformas, primera observación",
            "Tasa de informalidad, máximo",
            "Tasa de informalidad, promedio 2020", "Tasa de informalidad, promedio 2022",
            "Tasa de informalidad, promedio 2024", "Tasa de informalidad, promedio 2025",
            "Tasa de informalidad, mínimo", "Período del mínimo", "Tasa de informalidad, último",
            "Informales, último trimestre (1ª ocupación)", "Formales, último trimestre (1ª ocupación)",
            "% con plataforma como ocupación principal, 2024 a la fecha (promedio mensual)",
            "Denuncias ISL (2022 a ago. 2026)",
            "Ingreso mensual mediano 2025 ($)", "Ingreso mensual medio 2025 ($)",
            "Mediana / ingreso mínimo neto, 2025", "Mediana / ingreso mínimo neto, 2020",
            "Crecimiento del mínimo neto 2020-2025 (%)",
            "Horas mensuales promedio 2020-2025", "Horas semanales promedio 2020-2025",
            "Horas semanales, percentil 80 (quintil superior)",
            "Horas mensuales promedio, solo 2025", "N° casos con ingreso/hora válido, 2025",
            "Meses (de 28) con BTE <= formales ENE"),
  calculado = c(r2(nrow(serie)), r2(serie$universo_1y2[ult]), r2(serie$pct_ocupados[ult]),
                r2(serie$inf_nacional[ult]), r2(serie$tasa_informal[1]), r2(serie$tasa_informal[i_max]),
                r2(anual("tasa_informal", "2020")), r2(anual("tasa_informal", "2022")),
                r2(anual("tasa_informal", "2024")), r2(anual("tasa_informal", "2025")),
                r2(serie$tasa_informal[i_min]), as.character(serie$periodo[i_min]), r2(serie$tasa_informal[ult]),
                r2(serie$prin_informal[ult]), r2(serie$prin_formal[ult]), r2(mean(desde24$pct_principal)),
                r2(isl_n), r2(e25$ing_mediana), r2(e25$ing_media), r2(e25$razon_mediana_imm_neto),
                r2(e20$razon_mediana_imm_neto), r2((e25$imm_neto / e20$imm_neto - 1) * 100),
                r2(horas_prom_mes), r2(horas_prom_mes / 4), r2(horas_p80_sem), r2(e25$horas_mes),
                r2(e25$n_valido_hora), r2(sum(!cmp$bte_sobre_formales))),
  en_ponencia = c("78", "105.594", "1,02%", "27%", "91,8% (mar-2020)", "91,8%",
                  "80,90%", "80,03%", "66,58%", "61,37%", "57,0%", "jul-2025", "61,2%",
                  "57.992", "36.764", "90,2%", "82", "$528.399", "$668.045", "1,23", "1,13",
                  "cerca de 62%", "184", "46", "62", "(no citado; ver si < 176)", "28",
                  "0 (el texto dice 'todo el período')"))

print(as.data.frame(cifras), right = FALSE)
write.xlsx(list(cifras = cifras, bte_vs_ene = cmp, esi = esi_res, serie_ene = serie),
           "output/tablas/cifras_ponencia.xlsx", overwrite = TRUE)
message("Listo. Gráficos en output/graficos y tablas en output/tablas.")
