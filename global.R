# --------- Paquetes ---------
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  shiny, bslib, dplyr, purrr, tibble, lubridate, scales,
  siebanxicor, fredr, quantmod, httr, jsonlite, glue,
  ggplot2, ggthemes, plotly, zoo
)

# --------- Helpers ---------
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
    index    = number(x, accuracy = 0.01),
    plain    = number(x, accuracy = 0.01)
  )
}

fmt_hover <- function(x, style) {
  if (identical(style, "pct")) {
    out <- paste0(scales::number(x, accuracy = 0.01), " %")
  } else if (identical(style, "currency")) {
    out <- scales::label_dollar()(x)
  } else {
    out <- scales::number(x, accuracy = 0.01)
  }
  out[is.na(x)] <- "—"
  out
}

nice_date <- function(d) ifelse(is.na(d), "—", format(as.Date(d), "%d %b %Y"))
safe_try  <- function(expr) tryCatch(expr, error = function(e) NULL)

# --------- Keys / Tokens ---------
if (file.exists(".Renviron")) readRenviron(".Renviron")
banxico_token <- Sys.getenv("BANXICO_TOKEN")
fred_key      <- Sys.getenv("FRED_API_KEY")
if (nzchar(banxico_token)) setToken(banxico_token) else message("BANXICO_TOKEN no encontrado")
if (nzchar(fred_key))      fredr_set_key(fred_key) else message("FRED_API_KEY no encontrado")

# --------- Cargar extracción ---------
source("data_fetch.R")

# --- Helpers de UI compartidos entre ui y server ---
card_metric <- function(title, value, date, icon = NULL) {
  bslib::value_box(
    title = title,
    value = value,
    showcase = if (!is.null(icon)) icon else shiny::icon("chart-line"),
    theme_color = "primary",
    full_screen = FALSE,
    p(tags$small(glue("Último dato: {date}")))
  )
}

wrap_click <- function(tag, code, etiqueta) {
  tags$div(
    style = "cursor:pointer;",
    title = paste("Ver histórico de", etiqueta),
    onclick = sprintf("Shiny.setInputValue('series_clicked','%s',{priority:'event'})", code),
    tag
  )
}
