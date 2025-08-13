library(shiny)

# --------- UI ---------
theme <- bs_theme(
  version = 5, bootswatch = "minty",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Poppins")
)

ui <- page_fixed(
  theme = theme,
  title = "Indicadores MX & USA",
  tags$head(
    tags$style(HTML("
      .value-box { border-radius: 1rem; }
      .value-box .value { font-size: 1.8rem; }
      .bslib-page-fixed { padding-top: 1rem; padding-bottom: 2rem; }
      .section-title { margin-top: 1rem; margin-bottom: .5rem; }
      .modal-body .form-group { margin-bottom: .5rem; }
    "))
  ),
  h2(class = "section-title", "Indicadores Económicos"),
  htmlOutput("last_refresh"),
  hr(),
  h4(class = "section-title", "México"),
  uiOutput("cards_mx"),
  h4(class = "section-title", "Estados Unidos"),
  uiOutput("cards_us"),
  h4(class = "section-title", "Mercados"),
  uiOutput("cards_mkts"),
  hr(),
  div(
    style = "color:#6c757d; font-size:0.9rem;",
    p("Proyecto realizado para el curso 'Mercado de Títulos de Deuda'."),
    p("Autor: Gerardo Ruiz."),
    p("Fuentes: Se utilizan las APIs de Banxico SIE, St. Louis FRED, Yahoo Finance."),
    p(
      "Código fuente: ",
      tags$a(
        "GitHub",
        href = "https://github.com/gerardorglz",
        target = "_blank", rel = "noopener noreferrer"
      )
    )
  )
)
