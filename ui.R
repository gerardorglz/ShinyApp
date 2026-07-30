library(shiny)

# --------- Tema ---------
theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Poppins"),
  primary = "#2f8f8b"
)

# --------- Sección reutilizable ---------
seccion <- function(titulo, icono, output_id) {
  card(
    class = "seccion-card",
    card_header(
      class = "seccion-header",
      tags$span(icon(icono), tags$span(class = "seccion-titulo", titulo))
    ),
    card_body(uiOutput(output_id))
  )
}

# --------- UI ---------
ui <- page_fixed(
  theme = theme,
  title = "Indicadores MX & USA",
  lang = "es",
  tags$head(
    tags$style(HTML("
      body { background-color: #f6f8f8; }
      .bslib-page-fixed { padding-top: 1.25rem; padding-bottom: 2rem; max-width: 1180px; }

      /* Encabezado */
      .app-hero {
        background: linear-gradient(135deg, #2f8f8b 0%, #3fb0ac 100%);
        color: #fff; border-radius: 1rem; padding: 1.5rem 1.75rem;
        margin-bottom: 1.25rem; box-shadow: 0 8px 24px rgba(47,143,139,.18);
      }
      .app-hero h1 { font-weight: 700; font-size: 1.75rem; margin: 0; letter-spacing: -.5px; }
      .app-hero .subtitle { opacity: .92; margin: .35rem 0 .9rem; font-size: 1rem; }
      .refresh-badge {
        display: inline-block; background: rgba(255,255,255,.18);
        padding: .35rem .75rem; border-radius: 999px; font-size: .85rem;
      }
      .refresh-badge .fa { margin-right: .25rem; }

      /* Secciones */
      .seccion-card { border: none; border-radius: 1rem; margin-bottom: 1.25rem;
        box-shadow: 0 2px 10px rgba(0,0,0,.05); }
      .seccion-header { background: transparent; border-bottom: 1px solid #eceff1;
        font-weight: 600; }
      .seccion-titulo { margin-left: .5rem; font-size: 1.1rem; }

      /* Tarjetas / value boxes */
      .value-box { border-radius: .9rem; box-shadow: none; border: 1px solid #eceff1;
        transition: transform .12s ease, box-shadow .12s ease; }
      .value-box .value-box-value, .value-box .value { font-size: 1.7rem; font-weight: 700; }
      .value-box .value-box-title, .value-box .title { font-size: .82rem; opacity: .9; }
      .vb-date { margin: .25rem 0 0; opacity: .8; }
      .metric-click { cursor: pointer; height: 100%; }
      .metric-click:hover .value-box { transform: translateY(-3px);
        box-shadow: 0 10px 22px rgba(47,143,139,.20); }

      /* Indicador de cambio */
      .delta-badge { display: inline-block; font-size: .8rem; font-weight: 600;
        padding: .1rem .45rem; border-radius: 6px; margin-top: .2rem; }
      .delta-up   { color: #1a7f4b; background: rgba(26,127,75,.12); }
      .delta-down { color: #c0392b; background: rgba(192,57,43,.12); }
      .delta-flat { color: #6c757d; background: rgba(108,117,125,.12); }

      .empty-note { color: #6c757d; padding: .5rem; }

      /* Footer */
      .app-footer { color:#8a9498; font-size:.85rem; border-top:1px solid #e6eaea;
        margin-top: .5rem; padding-top: 1rem; }
      .app-footer a { color:#2f8f8b; text-decoration: none; }
      .app-footer a:hover { text-decoration: underline; }
    "))
  ),

  # Encabezado
  div(
    class = "app-hero",
    h1("Indicadores Económicos"),
    div(class = "subtitle", "Tablero de datos clave de México y Estados Unidos — Banxico · FRED · Yahoo Finance"),
    uiOutput("last_refresh")
  ),

  # Secciones
  seccion("México", "flag", "cards_mx"),
  seccion("Estados Unidos", "landmark", "cards_us"),
  seccion("Mercados", "arrow-trend-up", "cards_mkts"),

  # Pie
  div(
    class = "app-footer",
    p(tags$em("Haz clic en cualquier tarjeta para ver el histórico interactivo (desde el año 2000).")),
    p("Proyecto realizado para el curso “Mercado de Títulos de Deuda”. Autor: Gerardo Ruiz."),
    p("Fuentes: APIs de Banxico SIE, St. Louis FRED y Yahoo Finance."),
    p(
      "Código fuente: ",
      tags$a("GitHub", href = "https://github.com/gerardorglz",
             target = "_blank", rel = "noopener noreferrer")
    )
  )
)
