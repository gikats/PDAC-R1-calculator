# =============================================================
#  Preoperative R1 Resection Risk Calculator  --  DEMO BUILD
#  Pancreatic ductal adenocarcinoma (PDAC)
#  1st Department of Surgery, School of Medicine, NKUA
#
#  >>> THIS IS A DEPLOYMENT TEST <<<
#  All coefficients are ARBITRARY placeholder values.
#  The output is NOT a valid risk estimate.
#  Set DEMO_MODE <- FALSE only after entering the real
#  coefficients from the final multivariable model.
# =============================================================

library(shiny)

DEMO_MODE <- TRUE          # <- FALSE when the real numbers are in

# =============================================================
#  BLOCK 1 — MODEL COEFFICIENTS   (placeholders)
#  Source: SPSS -> "Variables in the Equation" -> column B
# =============================================================

INTERCEPT     <- -3.20     # Constant
B_SIZE        <-  0.0350   # per mm
B_CA199       <-  0.0009   # per U/mL
B_VASC_LT180  <-  0.9200   # contact < 180 deg   (ref: no contact)
B_VASC_GE180  <-  1.7400   # contact >= 180 deg  (ref: no contact)
B_NEOADJ      <- -0.7400   # neoadjuvant therapy

CUTOFF  <- 0.42
AUC_TXT <- "0.78 (95% CI 0.70-0.86)"
N_TXT   <- "n = 100"

HEADER_IMAGE <- NULL       # e.g. "header.jpg" placed in app/www/
REPO_URL     <- "https://github.com/USERNAME/PDAC-R1-calculator"

# =============================================================
#  Model
# =============================================================

risk <- function(size, ca199, vasc, neoadj) {
  b_v <- switch(as.character(vasc),
                "0" = 0, "1" = B_VASC_LT180, "2" = B_VASC_GE180, 0)
  lp <- INTERCEPT + B_SIZE * size + B_CA199 * ca199 + b_v + B_NEOADJ * neoadj
  1 / (1 + exp(-lp))
}

# =============================================================
#  UI
# =============================================================

ui <- fluidPage(
  title = "PDAC R1 Risk Calculator (demo)",

  tags$head(
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Source+Serif+4:opsz,wght@8..60,600&family=Inter:wght@400;500;600&display=swap"),
    tags$style(HTML("
      :root {
        --ink:#16232E; --muted:#71828F; --line:#E1E8ED;
        --paper:#fff; --wash:#F3F6F8;
        --safe:#0F766E; --alert:#A32E2A; --accent:#2F6F9F;
        --warn-bg:#FFF4E0; --warn-line:#E3B04B; --warn-ink:#7A5410;
      }
      body { font-family:'Inter',sans-serif; color:var(--ink); background:var(--wash); }
      .wrap { max-width:1120px; margin:0 auto; padding:0 16px 44px; }

      .demo-banner {
        background:var(--warn-bg); border:1px solid var(--warn-line);
        border-radius:6px; padding:12px 18px; margin-top:22px;
        font-size:13px; color:var(--warn-ink); line-height:1.55;
      }
      .demo-banner strong { font-weight:600; }

      .hero { margin-top:16px; }
      .hero img { width:100%; height:170px; object-fit:cover;
                  border-radius:8px 8px 0 0; display:block; }
      .hero-bar { background:var(--ink); color:#fff; padding:20px 26px;
                  border-radius:0 0 8px 8px; }
      .hero-bar.solo { border-radius:8px; }
      .hero-bar h1 { font-family:'Source Serif 4',Georgia,serif;
                     font-size:25px; font-weight:600; margin:0; }
      .hero-bar p  { margin:6px 0 0; font-size:13px; opacity:.72; }

      .card { background:var(--paper); border:1px solid var(--line);
              border-radius:8px; padding:22px 24px; margin-top:16px; }
      .eyebrow { font-size:11px; font-weight:600; letter-spacing:.09em;
                 text-transform:uppercase; color:var(--muted);
                 margin:0 0 18px; padding-bottom:9px;
                 border-bottom:1px solid var(--line); }

      label { font-size:13px; font-weight:500; }
      .irs-bar, .irs-bar-edge { background:var(--accent) !important;
                                border-color:var(--accent) !important; }
      .irs-single, .irs-from, .irs-to { background:var(--ink) !important; }
      .irs-line { background:#E4EAEE !important; }
      .form-control, .selectize-input { border-radius:4px !important;
                                        border-color:var(--line) !important; }

      .verdict-box { background:var(--wash); border-radius:6px;
                     padding:18px 20px; margin-top:6px; }
      .pct { font-family:'Source Serif 4',Georgia,serif; font-size:56px;
             font-weight:600; line-height:1; letter-spacing:-.02em;
             transition:color 200ms ease; }
      .verdict { font-size:11.5px; font-weight:600; letter-spacing:.1em;
                 text-transform:uppercase; margin-top:8px; }
      .track { position:relative; height:8px; background:#E4EAEE;
               border-radius:4px; margin:18px 0 0; }
      .fill  { height:100%; border-radius:4px;
               transition:width 240ms ease, background 240ms ease; }
      .marker{ position:absolute; top:-5px; width:2px; height:18px;
               background:var(--ink); opacity:.6; }
      .marker span { position:absolute; top:21px; left:50%;
                     transform:translateX(-50%); white-space:nowrap;
                     font-size:9.5px; font-weight:600; color:var(--muted); }
      .scale { display:flex; justify-content:space-between; font-size:9.5px;
               color:var(--muted); margin-top:20px; }

      .meta { font-size:12px; color:var(--muted); line-height:1.75; }
      .meta strong { color:var(--ink); font-weight:600; }
      .meta a { color:var(--accent); }
      .disclaimer { margin-top:16px; font-size:11.5px; color:var(--muted);
                    line-height:1.6; text-align:center; }
    "))
  ),

  div(class = "wrap",

    if (DEMO_MODE) div(class = "demo-banner",
      strong("Demonstration build \u2014 not a validated tool. "),
      "The regression coefficients below are arbitrary placeholders used to ",
      "test deployment. Any probability shown is meaningless and must not be ",
      "used for clinical or research purposes."
    ),

    div(class = "hero",
      if (!is.null(HEADER_IMAGE)) tags$img(src = HEADER_IMAGE, alt = ""),
      div(class = if (is.null(HEADER_IMAGE)) "hero-bar solo" else "hero-bar",
        h1("Preoperative prediction of R1 resection"),
        tags$p("Pancreatic ductal adenocarcinoma \u00b7 1st Department of Surgery, School of Medicine, NKUA")
      )
    ),

    fluidRow(
      column(6,
        div(class = "card",
          p(class = "eyebrow", "Preoperative variables"),
          sliderInput("size", "Tumour size on CT (mm)",
                      min = 5, max = 60, value = 28, step = 1, ticks = FALSE),
          sliderInput("ca199", "CA 19-9 (U/mL)",
                      min = 0, max = 2000, value = 180, step = 10, ticks = FALSE),
          selectInput("vasc", "Vascular contact (SMV / SMA)",
                      choices = c("None" = 0, "< 180\u00b0" = 1, "\u2265 180\u00b0" = 2),
                      selected = 1),
          checkboxInput("neoadj", "Neoadjuvant therapy", value = FALSE),
          div(class = "verdict-box",
            div(style = "font-size:11px;font-weight:600;letter-spacing:.09em;
                         text-transform:uppercase;color:var(--muted);",
                "Probability of R1"),
            uiOutput("readout")
          )
        )
      ),

      column(6,
        div(class = "card",
          p(class = "eyebrow", "Tumour\u2013vessel relationship"),
          uiOutput("anatomy")
        ),
        div(class = "card",
          p(class = "eyebrow", "Risk across tumour size"),
          plotOutput("curve", height = "215px"),
          div(style = "font-size:11px;color:var(--muted);margin-top:8px;",
              "All other variables held at their current values.")
        )
      )
    ),

    div(class = "card", uiOutput("meta")),

    div(class = "disclaimer",
        "Research tool based on a multivariable logistic regression model. ",
        "Not a substitute for clinical judgement.")
  )
)

# =============================================================
#  SERVER
# =============================================================

server <- function(input, output) {

  p_now <- reactive({
    risk(input$size, input$ca199, input$vasc, as.numeric(input$neoadj))
  })

  output$readout <- renderUI({
    p <- p_now(); high <- p >= CUTOFF
    col <- if (high) "var(--alert)" else "var(--safe)"
    tagList(
      div(class = "pct", style = paste0("color:", col, ";"),
          sprintf("%.0f%%", p * 100)),
      div(class = "verdict", style = paste0("color:", col, ";"),
          if (high) "High risk" else "Low risk"),
      div(class = "track",
        div(class = "fill",
            style = sprintf("width:%.1f%%;background:%s;", p * 100, col)),
        div(class = "marker", style = sprintf("left:%.1f%%;", CUTOFF * 100),
            tags$span(sprintf("cut-off %.0f%%", CUTOFF * 100)))
      ),
      div(class = "scale", tags$span("0%"), tags$span("50%"), tags$span("100%"))
    )
  })

  output$anatomy <- renderUI({
    r    <- 10 + input$size * 0.62
    v    <- as.numeric(input$vasc)
    vcol <- c("#8FA3B0", "#C87F5A", "#A32E2A")[v + 1]
    vlab <- c("no contact", "contact < 180\u00b0", "contact \u2265 180\u00b0")[v + 1]
    gap  <- c(16, 4, -6)[v + 1]
    tx   <- 300 - r - gap

    HTML(sprintf('
      <svg viewBox="0 0 460 200" width="100%%" style="display:block">
        <ellipse cx="150" cy="105" rx="92" ry="40" fill="#E9EEF2"
                 stroke="#CBD6DE" stroke-width="1.5"/>
        <text x="118" y="110" font-family="Inter,sans-serif" font-size="13"
              fill="#71828F">pancreas</text>
        <rect x="300" y="42" width="11" height="126" rx="5" fill="%s"/>
        <rect x="330" y="42" width="11" height="126" rx="5" fill="%s"/>
        <text x="296" y="186" font-family="Inter,sans-serif" font-size="11.5"
              font-weight="600" fill="#71828F">SMV / SMA</text>
        <circle cx="%.0f" cy="105" r="%.0f" fill="#16232E" opacity="0.88"/>
        <text x="%.0f" y="%.0f" text-anchor="middle"
              font-family="Inter,sans-serif" font-size="11.5" fill="#71828F">
          tumour %d mm</text>
        <text x="230" y="24" text-anchor="middle" font-family="Inter,sans-serif"
              font-size="11.5" font-weight="600" fill="%s">%s</text>
      </svg>',
      vcol, vcol, tx, r, tx, 105 + r + 18, input$size, vcol, vlab))
  })

  output$curve <- renderPlot({
    sizes <- seq(5, 60, by = 1)
    ps <- vapply(sizes, function(s)
      risk(s, input$ca199, input$vasc, as.numeric(input$neoadj)), numeric(1))
    p <- p_now()

    par(mar = c(3.4, 4.2, 0.6, 0.6), family = "sans", las = 1)
    plot(sizes, ps, type = "n", ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE)
    abline(h = seq(0, 1, 0.2), col = "#EDF1F4", lwd = 1)
    abline(h = CUTOFF, lty = 2, lwd = 1.4, col = "#B4C0C9")
    lines(sizes, ps, lwd = 2.6, col = "#2F6F9F")
    points(input$size, p, pch = 21, cex = 1.7, lwd = 2,
           bg = if (p >= CUTOFF) "#A32E2A" else "#0F766E", col = "white")
    axis(1, at = seq(5, 60, 5), cex.axis = 0.8, col = "#D6DEE4",
         col.axis = "#71828F", tck = -0.02)
    axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"),
         cex.axis = 0.8, col = "#D6DEE4", col.axis = "#71828F", tck = -0.02)
    mtext("Tumour size (mm)", side = 1, line = 2.2, cex = 0.82, col = "#71828F")
  })

  output$meta <- renderUI({
    div(class = "meta",
      tags$div(strong("Discrimination: "),
               if (DEMO_MODE) "not applicable (demo build)"
               else paste0("C-index ", AUC_TXT, " \u00b7 ", N_TXT)),
      tags$div(strong("Cut-off: "), sprintf("%.2f (Youden index)", CUTOFF)),
      tags$div(strong("Model variables: "),
               "tumour size on CT, CA 19-9, degree of vascular contact, neoadjuvant therapy"),
      tags$div(strong("Source code: "),
               tags$a(href = REPO_URL, target = "_blank", REPO_URL))
    )
  })
}

shinyApp(ui, server)
