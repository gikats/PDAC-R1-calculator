# =============================================================
#  PDAC Preoperative prediction of R1 resection  --  DEMO BUILD
#
#  Vascular involvement is entered vessel by vessel and the
#  NCCN resectability category is derived automatically.
#  The MODEL uses the derived category, not the raw angles.
# =============================================================

library(shiny)

DEMO_MODE <- TRUE

# =============================================================
#  BLOCK 1 — MODEL COEFFICIENTS   (placeholders)
# =============================================================

INTERCEPT   <- -3.20
B_SIZE      <-  0.0350   # per mm
B_CA199     <-  0.0009   # per U/mL
B_BR        <-  1.0500   # borderline resectable   (ref: resectable)
B_LA        <-  1.9000   # locally advanced        (ref: resectable)
B_NEOADJ    <- -0.7400   # neoadjuvant therapy

CUTOFF  <- 0.42
AUC_TXT <- "0.78 (95% CI 0.70-0.86)"
N_TXT   <- "n = 100"

HEADER_IMAGE <- NULL
REPO_URL <- "https://github.com/gikats/PDAC-R1-calculator"

# =============================================================
#  NCCN resectability logic
#  Returns "R", "BR" or "LA".
#  Venous and arterial criteria are deliberately asymmetric.
# =============================================================

nccn_status <- function(loc, smv, sma, ca, cha) {

  # --- Locally advanced ---------------------------------------
  if (smv == "unrec")            return("LA")   # unreconstructible SMV/PV
  if (sma == "gt180")            return("LA")   # SMA > 180 deg
  if (ca  == "aorta")            return("LA")   # CA with aortic involvement
  if (loc == "head" && ca == "gt180") return("LA")
  if (cha == "extend")           return("LA")   # CHA into CA / bifurcation

  # --- Borderline resectable ----------------------------------
  if (sma == "le180")            return("BR")   # ANY SMA contact
  if (ca  %in% c("le180", "gt180")) return("BR")
  if (cha == "contact")          return("BR")   # CHA, no extension
  if (smv %in% c("gt180", "irreg")) return("BR")

  # --- Resectable ---------------------------------------------
  "R"
}

risk <- function(size, ca199, status, neoadj) {
  b_s <- switch(status, "R" = 0, "BR" = B_BR, "LA" = B_LA, 0)
  lp <- INTERCEPT + B_SIZE * size + B_CA199 * ca199 + b_s + B_NEOADJ * neoadj
  1 / (1 + exp(-lp))
}

STATUS_LABEL <- c(R  = "Resectable",
                  BR = "Borderline resectable",
                  LA = "Locally advanced")
STATUS_COL   <- c(R = "#0F766E", BR = "#C87F5A", LA = "#A32E2A")

# =============================================================
#  Vessel colour coding
#    Arteries are always red, veins always blue.
#    A single amber highlight marks any vessel the tumour touches;
#    the degree of contact is not graded by colour.
# =============================================================

ART   <- "#C0392B"     # every artery
VEIN  <- "#3A6EA5"     # every vein

HALO_NONE <- "none"
HALO      <- "#F0A830"   # tumour contact, whatever the degree

# each returns list(col = vessel colour, halo = highlight colour)
v_smv <- function(x) switch(x,
  none  = list(col = VEIN, halo = HALO_NONE),
  le180 = list(col = VEIN, halo = HALO),
  irreg = list(col = VEIN, halo = HALO),
  gt180 = list(col = VEIN, halo = HALO),
  unrec = list(col = VEIN, halo = HALO))

v_sma <- function(x) switch(x,
  none  = list(col = ART, halo = HALO_NONE),
  le180 = list(col = ART, halo = HALO),
  gt180 = list(col = ART, halo = HALO))

v_cha <- function(x) switch(x,
  none    = list(col = ART, halo = HALO_NONE),
  contact = list(col = ART, halo = HALO),
  extend  = list(col = ART, halo = HALO))

v_ca <- function(x, loc) switch(x,
  none  = list(col = ART, halo = HALO_NONE),
  le180 = list(col = ART, halo = HALO),
  gt180 = list(col = ART, halo = HALO),
  aorta = list(col = ART, halo = HALO))

# draws a vessel: the highlight underneath, then the vessel itself.
# Where two vessels cross, the one drawn later lies in front.
vessel <- function(d, v, w) {
  halo <- if (v$halo == "none") "" else paste0(
    '<path d="', d, '" stroke="', v$halo, '" stroke-width="', w + 9,
    '" stroke-linecap="round" fill="none" opacity="0.5"/>')
  paste0(halo,
    '<path d="', d, '" stroke="', v$col, '" stroke-width="', w,
    '" stroke-linecap="round" fill="none"/>')
}

# =============================================================
#  Anatomical schematic — differs by tumour location
#  Anterior view: patient's right on the viewer's left.
# =============================================================

draw_anatomy <- function(loc, size, smv, sma, ca, cha) {

  s_smv <- v_smv(smv); s_sma <- v_sma(sma)
  s_cha <- v_cha(cha); s_ca  <- v_ca(ca, loc)
  ao <- identical(ca, "aorta")
  plain_a <- list(col = ART,  halo = HALO_NONE)   # not assessed
  plain_v <- list(col = VEIN, halo = HALO_NONE)

  r <- 12 + size * 0.55
  if (loc == "head") { tx <- 168; ty <- 160 } else { tx <- 310; ty <- 114 }

  paste0(
  '<svg viewBox="0 0 460 250" width="100%" style="display:block">',

  # ---- inferior vena cava, immediately right of the aorta ----
  '<rect x="218" y="12" width="18" height="228" rx="9" fill="', VEIN,
  '" opacity="0.45"/>',

  # ---- aorta: highlighted only when the tumour involves it ----
  if (ao) paste0(
    '<rect x="244" y="8" width="32" height="236" rx="15" fill="', HALO,
    '" opacity="0.5"/>') else "",
  '<rect x="252" y="12" width="16" height="228" rx="8" fill="', ART,
  '" opacity="', if (ao) "1" else "0.45", '"/>',

  # ---- SMV / portal vein: ascends in front of the IVC, immediately
  #      to the patient right of the SMA, then passes posterior to
  #      the hepatic artery on its way to the porta
  vessel("M224,238 L224,150 Q220,96 138,52", s_smv, 11),

  # ---- splenic vein: joins the SMV at the confluence ----
  vessel("M374,120 Q300,138 230,152", plain_v, 9),

  # ---- SMA: arises from the front of the aorta and descends
  #      immediately to the patient left of the SMV
  vessel("M252,104 L244,104 L244,238", s_sma, 9),

  # ---- coeliac trunk and its three branches ----
  vessel("M252,68 L236,64", s_ca, 9),
  vessel("M240,64 Q234,42 246,22", plain_a, 5),      # left gastric
  vessel("M236,64 Q300,50 374,72", plain_a, 7),      # splenic artery
  vessel("M236,64 Q216,58 196,54", s_cha, 8),        # common hepatic

  # ---- CHA divides into PHA and GDA ----
  vessel("M196,54 Q174,44 150,34", s_cha, 7),
  # GDA leaves the CHA at the upper border of the duodenum, then
  # descends in the groove between the head and the neck, anterior
  # to the gland and lateral to the portal vein
  vessel("M196,54 Q196,90 186,124 Q180,150 184,176", plain_a, 6),

  # ---- pancreas: translucent, drawn over the vessels ----
  '<g fill="#F2CFC9" opacity="0.55" stroke="none">',
  '<path d="M200,126 Q262,102 322,96 Q364,92 392,84 L396,110
            Q364,120 322,132 Q262,138 204,170 Z"/>',
  '<ellipse cx="168" cy="160" rx="46" ry="38"/>',
  '</g>',

  # ---- tumour ----
  '<circle cx="', tx, '" cy="', ty, '" r="', round(r, 1),
  '" fill="#16232E" opacity="0.82"/>',

  '</svg>')
}

# =============================================================
#  UI
# =============================================================

ui <- fluidPage(
  title = "Preoperative prediction of R1 resection in Pancreatic Ductal Adenocarcinoma",

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
      .demo-banner { background:var(--warn-bg); border:1px solid var(--warn-line);
        border-radius:6px; padding:12px 18px; margin-top:22px;
        font-size:13px; color:var(--warn-ink); line-height:1.55; }
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
      .subhead { font-size:11px; font-weight:600; letter-spacing:.06em;
                 text-transform:uppercase; color:var(--muted);
                 margin:20px 0 10px; }
      label { font-size:13px; font-weight:500; }
      .irs-bar, .irs-bar-edge { background:var(--accent) !important;
                                border-color:var(--accent) !important; }
      .irs-single { background:var(--ink) !important; }
      .irs-line { background:#E4EAEE !important; }
      .form-control, .selectize-input { border-radius:4px !important;
                                        border-color:var(--line) !important; }
      .status-chip { display:inline-block; padding:5px 13px; border-radius:14px;
                     font-size:11.5px; font-weight:600; letter-spacing:.05em;
                     text-transform:uppercase; color:#fff; }
      .legend { display:flex; flex-wrap:wrap; gap:14px; margin-top:12px;
                font-size:10.5px; color:var(--muted); }
      .legend span { display:flex; align-items:center; gap:5px; }
      .legend i { width:11px; height:11px; border-radius:3px; display:block; }
      .verdict-box { background:var(--wash); border-radius:6px;
                     padding:18px 20px; margin-top:14px; }
      .pct { font-family:'Source Serif 4',Georgia,serif; font-size:56px;
             font-weight:600; line-height:1; letter-spacing:-.02em; }
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
      tags$strong("Demonstration build \u2014 not a validated tool. "),
      "Coefficients are arbitrary placeholders used to test deployment. ",
      "Any probability shown is meaningless."
    ),

    div(class = "hero",
      if (!is.null(HEADER_IMAGE)) tags$img(src = HEADER_IMAGE, alt = ""),
      div(class = if (is.null(HEADER_IMAGE)) "hero-bar solo" else "hero-bar",
        h1("Preoperative prediction of R1 resection in Pancreatic Ductal Adenocarcinoma.")
      )
    ),

    fluidRow(
      column(6,
        div(class = "card",
          p(class = "eyebrow", "Tumour and laboratory"),

          selectInput("loc", "Location",
                      choices = c("Head / uncinate process" = "head",
                                  "Body / tail" = "body")),
          sliderInput("size", "Size on CT (mm)",
                      min = 5, max = 60, value = 28, step = 1, ticks = FALSE),
          sliderInput("ca199", "CA 19-9 (U/mL)",
                      min = 0, max = 2000, value = 180, step = 10, ticks = FALSE),
          checkboxInput("neoadj", "Neoadjuvant therapy", value = FALSE),

          p(class = "subhead", "Venous involvement"),
          selectInput("smv", "SMV / portal vein",
            choices = c("No contact"                              = "none",
                        "\u2264180\u00b0, smooth contour"          = "le180",
                        "\u2264180\u00b0 with contour irregularity or thrombus" = "irreg",
                        ">180\u00b0, reconstructible"              = "gt180",
                        "Unreconstructible / occluded"             = "unrec")),

          p(class = "subhead", "Arterial involvement"),
          selectInput("sma", "Superior mesenteric artery",
            choices = c("No contact" = "none",
                        "\u2264180\u00b0" = "le180",
                        ">180\u00b0"      = "gt180")),
          selectInput("ca", "Coeliac axis",
            choices = c("No contact" = "none",
                        "\u2264180\u00b0" = "le180",
                        ">180\u00b0, aorta not involved, GDA intact" = "gt180",
                        "With aortic involvement" = "aorta")),
          selectInput("cha", "Common hepatic artery",
            choices = c("No contact" = "none",
                        "Contact, no extension to CA or bifurcation" = "contact",
                        "Extension to CA or hepatic artery bifurcation" = "extend"))
        )
      ),

      column(6,
        div(class = "card",
          p(class = "eyebrow", "Probability of R1"),
          uiOutput("readout")
        ),
        div(class = "card",
          p(class = "eyebrow", "Tumour\u2013vessel relationship"),
          uiOutput("anatomy")
        ),
        div(class = "card",
          p(class = "eyebrow", "NCCN resectability status"),
          uiOutput("status")
        )
      )
    ),

    # ---- full-width chart ----
    div(class = "card",
      p(class = "eyebrow", "Risk across tumour size"),
      plotOutput("curve", height = "330px")
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

  status <- reactive(
    nccn_status(input$loc, input$smv, input$sma, input$ca, input$cha)
  )

  p_now <- reactive(
    risk(input$size, input$ca199, status(), as.numeric(input$neoadj))
  )

  output$anatomy <- renderUI({
    HTML(draw_anatomy(input$loc, input$size,
                      input$smv, input$sma, input$ca, input$cha))
  })

  output$status <- renderUI({
    s <- status()
    div(class = "status-chip",
        style = paste0("background:", STATUS_COL[[s]], ";"),
        STATUS_LABEL[[s]])
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
            tags$span(sprintf("cut-off %.0f%%", CUTOFF * 100)))),
      div(class = "scale", tags$span("0%"), tags$span("50%"), tags$span("100%"))
    )
  })

  output$curve <- renderPlot({
    sizes <- seq(5, 60, by = 1)
    cur   <- status()
    p     <- p_now()

    ps <- vapply(sizes, function(s)
      risk(s, input$ca199, cur, as.numeric(input$neoadj)), numeric(1))

    par(mar = c(4.2, 5, 1.2, 1.6), family = "sans", las = 1)
    plot(sizes, ps, type = "n", ylim = c(0, 1),
         xlab = "", ylab = "", axes = FALSE)

    abline(h = seq(0, 1, 0.2), col = "#EDF1F4", lwd = 1)
    abline(h = CUTOFF, lty = 2, lwd = 1.4, col = "#B4C0C9")

    lines(sizes, ps, lwd = 3, col = "#2F6F9F")

    points(input$size, p, pch = 21, cex = 2.1, lwd = 2.4,
           bg = if (p >= CUTOFF) "#A32E2A" else "#0F766E", col = "white")

    axis(1, at = seq(5, 60, 5), cex.axis = 0.9, col = "#D6DEE4",
         col.axis = "#71828F", tck = -0.018)
    axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"),
         cex.axis = 0.9, col = "#D6DEE4", col.axis = "#71828F", tck = -0.018)
    mtext("Tumour size (mm)", side = 1, line = 2.7, cex = 0.9, col = "#71828F")
    mtext("Probability of R1", side = 2, line = 3.6, cex = 0.9,
          col = "#71828F", las = 0)
  })

  output$meta <- renderUI({
    div(class = "meta",
      tags$div(tags$strong("Model variables: "),
               "tumour size on CT, CA 19-9, NCCN resectability status ",
               "(3 levels, reference: resectable), neoadjuvant therapy"),
      tags$div(tags$strong("Discrimination: "),
               if (DEMO_MODE) "not applicable (demo build)"
               else paste0("C-index ", AUC_TXT, " \u00b7 ", N_TXT)),
      tags$div(tags$strong("Cut-off: "), sprintf("%.2f (Youden index)", CUTOFF)),
      tags$div(tags$strong("Resectability criteria: "),
               "NCCN Guidelines, Pancreatic Adenocarcinoma."),
      tags$div(tags$strong("Source code: "),
               tags$a(href = REPO_URL, target = "_blank", REPO_URL))
    )
  })
}

shinyApp(ui, server)
