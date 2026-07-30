# --------- Paquetes ---------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  shiny, bslib, dplyr, purrr, tibble, lubridate, scales,
  siebanxicor, fredr, quantmod, httr, jsonlite, glue,
  ggplot2, ggthemes, plotly, zoo
)

# --------- Configuración ---------
APP_TZ        <- "America/Mexico_City"  # zona horaria de referencia para el refresco
REFRESH_HOUR  <- 6L                     # hora del refresco diario (06:00)
CACHE_DIR     <- "cache"
CACHE_FILE    <- file.path(CACHE_DIR, "snapshot.rds")

# --------- Helpers de datos ---------
# Devuelve el último y el penúltimo valor no-NA de una serie (para calcular el cambio)
last_two <- function(df) {
  df <- df[!is.na(df$value), , drop = FALSE]
  if (!nrow(df)) return(list(value = NA_real_, prev = NA_real_, date = as.Date(NA)))
  df <- df[order(as.Date(df$date)), , drop = FALSE]
  n  <- nrow(df)
  list(
    value = suppressWarnings(as.numeric(df$value[n])),
    prev  = if (n >= 2) suppressWarnings(as.numeric(df$value[n - 1])) else NA_real_,
    date  = as.Date(df$date[n])
  )
}

last_val <- function(df) {
  df <- df[!is.na(df$value), ]
  if (!nrow(df)) return(tibble(date = as.Date(NA), value = NA_real_))
  df[which.max(as.Date(df$date)), , drop = FALSE]
}

fmt_val <- function(x, style = c("plain","pct","currency","index")) {
  style <- match.arg(style)
  if (is.na(x)) return("—")
  switch(
    style,
    pct      = paste0(number(x, accuracy = 0.01), " %"),
    currency = paste0("$", number(x, accuracy = 0.01)),
    index    = number(x, accuracy = 0.01, big.mark = ","),
    plain    = number(x, accuracy = 0.01)
  )
}

fmt_hover <- function(x, style) {
  if (identical(style, "pct")) {
    out <- paste0(scales::number(x, accuracy = 0.01), " %")
  } else if (identical(style, "currency")) {
    out <- scales::label_dollar()(x)
  } else {
    out <- scales::number(x, accuracy = 0.01, big.mark = ",")
  }
  out[is.na(x)] <- "—"
  out
}

nice_date <- function(d) ifelse(is.na(d), "—", format(as.Date(d), "%d %b %Y"))
safe_try  <- function(expr) tryCatch(expr, error = function(e) NULL)

# --------- Lógica de refresco diario (06:00 hora de México) ---------
# Frontera de refresco más reciente (<= now): el último 06:00 en APP_TZ
last_refresh_boundary <- function(now = Sys.time()) {
  lt <- as.POSIXlt(now, tz = APP_TZ)
  six_today <- as.POSIXct(
    sprintf("%04d-%02d-%02d %02d:00:00",
            lt$year + 1900L, lt$mon + 1L, lt$mday, REFRESH_HOUR),
    tz = APP_TZ
  )
  if (now < six_today) six_today - 24 * 3600 else six_today
}

# Próxima frontera de refresco (> now)
next_refresh_boundary <- function(now = Sys.time()) last_refresh_boundary(now) + 24 * 3600

# Devuelve el snapshot de datos, re-consultando las APIs solo si el caché es
# anterior a la última frontera de las 06:00. Ante un fallo de red conserva el
# último caché válido en lugar de mostrar la app vacía.
get_data_cached <- function(force = FALSE) {
  boundary <- last_refresh_boundary()
  cached   <- if (file.exists(CACHE_FILE)) safe_try(readRDS(CACHE_FILE)) else NULL
  fresh    <- !is.null(cached) && !is.null(cached$fetched_at) &&
              cached$fetched_at >= boundary && NROW(cached$data) > 0

  if (!force && fresh) return(cached)

  data <- safe_try(fetch_all())
  if (is.null(data) || !nrow(data)) {
    if (!is.null(cached)) return(cached)                 # fallback al caché previo
    return(list(data = tibble(), fetched_at = Sys.time()))
  }

  snapshot <- list(data = data, fetched_at = Sys.time())
  safe_try({
    dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
    saveRDS(snapshot, CACHE_FILE)
  })
  snapshot
}

# --------- Keys / Tokens ---------
if (file.exists(".Renviron")) readRenviron(".Renviron")
banxico_token <- Sys.getenv("BANXICO_TOKEN")
fred_key      <- Sys.getenv("FRED_API_KEY")
if (nzchar(banxico_token)) setToken(banxico_token) else message("BANXICO_TOKEN no encontrado")
if (nzchar(fred_key))      fredr_set_key(fred_key) else message("FRED_API_KEY no encontrado")

# --------- Cargar extracción ---------
source("data_fetch.R")

# --- Helpers de UI compartidos entre ui y server ---
# Etiqueta de cambio (▲/▼) vs. el dato previo, coloreada según dirección.
delta_badge <- function(valor, prev, formato) {
  if (is.na(valor) || is.na(prev)) return(NULL)
  d <- valor - prev
  if (identical(formato, "pct")) {
    txt  <- paste0(ifelse(d >= 0, "+", ""), scales::number(d, accuracy = 0.01), " pp")
  } else {
    txt  <- paste0(ifelse(d >= 0, "+", ""), scales::number(d, accuracy = 0.01))
  }
  cls <- if (d > 0) "delta-up" else if (d < 0) "delta-down" else "delta-flat"
  arrow <- if (d > 0) "▲" else if (d < 0) "▼" else "▬"
  tags$span(class = paste("delta-badge", cls), paste0(arrow, " ", txt))
}

card_metric <- function(title, value, date, delta = NULL, icon = NULL) {
  bslib::value_box(
    title = title,
    value = value,
    showcase = if (!is.null(icon)) icon else shiny::icon("chart-line"),
    theme_color = "primary",
    full_screen = FALSE,
    delta,
    p(class = "vb-date", tags$small(glue("Último dato: {date}")))
  )
}

wrap_click <- function(tag, code, etiqueta) {
  tags$div(
    class = "metric-click",
    title = paste("Ver histórico de", etiqueta),
    onclick = sprintf("Shiny.setInputValue('series_clicked','%s',{priority:'event'})", code),
    tag
  )
}
