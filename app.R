library(shiny)
library(shinyWidgets)   # sliderTextInput
library(DT)             # sortable table
library(tidyverse)
library(leaflet)
library(lubridate)
library(nationalparkcolors)   # remotes::install_github("katiejolly/nationalparkcolors")

set.seed(42)

# TODO: When you get new data, change this to the updated CSVs and the app will be rebuilt!
DATA_PATH <- "edmr_csvs/combined_data.csv"   

# ---- Settings ---------------------------------------------------------------
B         <- 10000   # importance-sampling draws per "<X"
MIN_POOL  <- 5       # min measured values <= X needed to estimate a distribution
FLAT_TOL  <- log2(1.10)  # changes smaller than +/-10% get no arrow
ARROW_CAP <- 3           # |log2 change| at which arrow length maxes out (8x)
PH_EPS    <- 0.1         # offset so |pH - 7| = 0 doesn't break ratios
ARROW_ANGLE <- 45        # degrees from vertical: increases lean right, decreases lean left

APP_TITLE <- "3 Rivers Waterkeeper: Allegheny County EDMR Data Explorer"
LOGO_FILE <- "3rwk.avif"
LINK_URL    <- "https://www.threeriverswaterkeeper.org/become-a-member"
LINK_TEXT   <- "Support 3RWK!"        
LOGO_URL    <- "https://3rwk.org"     
LINK_URL_2  <- "https://www.threeriverswaterkeeper.org/become-a-member"
LINK_TEXT_2 <- "Enjoyed our work? Support us!"      # <- text for the small button

# Map palette: "Zion" from nationalparkcolors (low -> high percentile)
PALETTE_REVERSE <- FALSE
zion <- as.character(nationalparkcolors::park_palette("Zion"))
if (PALETTE_REVERSE) zion <- rev(zion)

contaminants <- c(
  "Total Suspended Solids", "Total Phosphorus", "Total Nitrogen",
  "Total Dissolved Solids", "Oil and Grease", "pH",
  "Chemical Oxygen Demand (COD)", "Aluminum, Total", "Iron, Total",
  "Zinc, Total", "Lead, Total", "Copper, Total", "Nitrate-Nitrite as N",
  "Ammonia-Nitrogen", "Biochemical Oxygen Demand (BOD5)"
)

# ---- Load -------------------------------------------------------------------
all_rows <- read_csv(DATA_PATH, show_col_types = FALSE,
                     col_types = cols(.default = col_character(),
                                      Latitude = col_double(),
                                      Longitude = col_double())) |>
  filter(Parameter_Name %in% contaminants) |>
  mutate(DMR_Value = str_trim(DMR_Value))

# E / FF / GG rows
flag_rows <- all_rows |>
  filter(DMR_Value %in% c("E", "FF", "GG")) |>
  transmute(Primary_Facility_ID, Primary_Facility_Name, Client_Name,
            Parameter_Name, flag = DMR_Value)

# ---- Clean for mapping ------------------------------------------------------
raw <- all_rows |>
  mutate(
    censored   = str_starts(DMR_Value, "<"),
    num_value  = suppressWarnings(as.numeric(str_trim(str_remove(DMR_Value, "<")))),
    begin_date = as.Date(Monitoring_Period_Begin_Date, format = "%m/%d/%Y"),
    end_date   = as.Date(Monitoring_Period_End_Date,   format = "%m/%d/%Y")
  ) |>
  filter(!DMR_Value %in% c("E", "FF", "GG"),
         !is.na(num_value), num_value > 0,
         !is.na(begin_date), !is.na(end_date),
         !is.na(Latitude), !is.na(Longitude),
         Longitude < -76, Longitude > -81, Latitude > 39, Latitude < 43) # TODO: If your data is outside this bounding box, change these numbers to include it.

# ---- Importance-sampling imputation for "<X" values -------------------------
# For each (parameter, X): fit a kernel density (log10 scale, like the ridgeline
# plots) to the measured values <= X. The target is that density truncated at X.
# Draw B points from a Uniform proposal over [lo, log10(X)], weight each draw by
# the target density at that point (the proposal is constant, so the weight is
# just f(y)), and use the self-normalised importance-sampling estimate of
# E[value | value < X], computed on the original concentration scale.
measured <- raw |> filter(!censored)

impute_one <- function(param, X) {
  pool <- measured$num_value[measured$Parameter_Name == param &
                               measured$num_value <= X]
  if (length(pool) < MIN_POOL) {
    return(tibble(imputed = X / 2, pool_n = length(pool), method = "X/2 fallback"))
  }
  lp <- log10(pool)
  bw <- bw.nrd0(lp)
  lo <- min(lp) - 3 * bw
  hi <- log10(X)
  f  <- approxfun(density(lp, bw = bw, from = lo, to = hi, n = 1024), rule = 2)
  y  <- runif(B, lo, hi)          # proposal draws
  w  <- f(y)                      # importance weights (target / constant proposal)
  tibble(imputed = sum(w * 10^y) / sum(w),
         pool_n = length(pool), method = "Importance sampling")
}

lookup <- raw |>
  filter(censored) |>
  distinct(Parameter_Name, num_value) |>
  mutate(res = map2(Parameter_Name, num_value, impute_one)) |>
  unnest(res)

# metric = what is percentile-ranked and compared over time.
# For pH it is distance from neutral (|pH - 7|); otherwise the value itself.
# One record per facility x contaminant x reporting period (most extreme one).
dat <- raw |>
  left_join(lookup, by = c("Parameter_Name", "num_value")) |>
  mutate(value  = if_else(censored, imputed, num_value),
         metric = if_else(Parameter_Name == "pH", abs(value - 7), value),
         label  = if_else(censored, paste0("<", num_value), as.character(num_value))) |>
  group_by(Parameter_Name, Primary_Facility_ID, Latitude, Longitude,
           begin_date, end_date) |>
  slice_max(metric, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(Parameter_Name) |>
  mutate(pctl = ecdf(metric)(metric) * 100) |>
  ungroup() |>
  mutate(metric_adj = if_else(Parameter_Name == "pH", metric + PH_EPS, metric)) |>
  # period-over-period and since-first changes (log2 ratio; + = increase)
  arrange(Parameter_Name, Primary_Facility_ID, Latitude, Longitude, begin_date) |>
  group_by(Parameter_Name, Primary_Facility_ID, Latitude, Longitude) |>
  mutate(lr_prev  = log2(metric_adj / lag(metric_adj)),
         lr_first = if_else(row_number() == 1, NA_real_,
                            log2(metric_adj / first(metric_adj)))) |>
  ungroup()

# n = number of observations (individual DMR rows after cleaning)
counts  <- raw |> count(Parameter_Name, sort = TRUE)
choices <- setNames(counts$Parameter_Name,
                    paste0(counts$Parameter_Name, " (n = ",
                           format(counts$n, big.mark = ","), ")"))

# Monthly timeline: a record is shown for every month inside its reporting period
months       <- seq(floor_date(min(dat$begin_date), "month"),
                    floor_date(max(dat$end_date), "month"), by = "month")
month_labels <- format(months, "%b %Y")

# ---- Marker helpers (inline SVG icons so shape can vary) --------------------
svg_url <- function(svg) paste0("data:image/svg+xml;charset=utf-8,",
                                URLencode(svg, reserved = TRUE))

point_svg <- function(fill, censored) {
  shape <- if_else(censored,
    sprintf("<polygon points='10,1.5 18.5,10 10,18.5 1.5,10' fill='%s' stroke='#222' stroke-width='1.6'/>", fill),
    sprintf("<circle cx='10' cy='10' r='7.5' fill='%s' stroke='white' stroke-width='1.5'/>", fill))
  svg_url(paste0("<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'>", shape, "</svg>"))
}

# Arrow with its tail at the point, tilted like an election-shift map:
# increases lean up-right, decreases lean up-left. Rotated about the tail.
arrow_svg <- function(up, len, col) {
  map_chr(seq_along(up), function(i) {
    L   <- len[i]
    S   <- 2 * L + 20           # square canvas, tail at the centre
    cx  <- S / 2
    hl  <- min(14, L * 0.5)     # head length
    ang <- if (up[i]) ARROW_ANGLE else -ARROW_ANGLE
    body <- sprintf(
      "<g transform='rotate(%g %g %g)'><rect x='%g' y='%g' width='5' height='%g' fill='%s' stroke='white' stroke-width='0.8'/><polygon points='%g,%g %g,%g %g,%g' fill='%s' stroke='white' stroke-width='0.8'/></g>",
      ang, cx, cx, cx - 2.5, cx - L + hl, L - hl, col[i],
      cx, cx - L, cx - 8, cx - L + hl, cx + 8, cx - L + hl, col[i])
    svg_url(sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='%g' height='%g'>%s</svg>", S, S, body))
  })
}

fmt_change <- function(lr) ifelse(is.na(lr), "n/a", sprintf("%+.0f%%", (2^lr - 1) * 100))

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$title(APP_TITLE)),
  div(style = "display:flex; justify-content:space-between; align-items:center; gap:16px; margin:12px 0;",
      div(
        tags$a(href = LOGO_URL, target = "_blank", style = "display:inline-block;",
               tags$img(src = LOGO_FILE, onerror = "this.style.display='none'",
                        style = "max-height:90px; display:block; margin-bottom:8px;")),
        h2(APP_TITLE, style = "margin:0;")),
      tags$a(href = LINK_URL, target = "_blank", class = "btn btn-primary btn-lg",
             style = "padding:14px 28px; font-size:20px; font-weight:600;", LINK_TEXT)),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons("param", "Contaminant (ordered by # observations)",
                   choices = choices, selected = counts$Parameter_Name[1]),
      sliderTextInput("month", "Month", choices = month_labels,
                      selected = month_labels[length(month_labels)], grid = FALSE,
                      animate = animationOptions(interval = 1200)),
      radioButtons("trend", "Trend arrows",
                   choices = c("Off" = "off",
                               "Change vs. previous reporting period" = "prev",
                               "Change since first reported" = "first"),
                   selected = "off"),
      helpText("Point color = percentile of the value within the contaminant",
               "(all dates).",
               "Circles are measured values; diamonds are '<X' values, replaced",
               "by the expected value (importance sampling) of the smoothed",
               "distribution of measured values below X. Each month shows the",
               "reporting period covering it; the most extreme record per",
               "facility is used. Arrows start at the facility: red leans",
               "up-right = increase, blue leans up-left = decrease; length",
               "scales with size of change (none if within \u00b110%)."),
      conditionalPanel("input.param == 'pH'",
        helpText("For pH, percentile and arrows are based on distance from 7",
                 "(|pH - 7|), not the pH value itself.")),
      textOutput("summary"),
      tags$a(href = LINK_URL_2, target = "_blank", class = "btn btn-primary btn-xs",
             style = "margin-top:14px;", LINK_TEXT_2)
    ),
    mainPanel(
      width = 9,
      leafletOutput("map", height = "70vh"),
      tags$hr(),
      h4("Records without data (NODI codes)"),
      tags$details(
        style = "margin-bottom:10px;",
        tags$summary(style = "font-size:14px; font-style:italic; cursor:pointer;",
                     "What do these codes mean? (Click me!)"),
        p("Where results are not available, a NO Data Indicator (NODI) Code instead of a numerical value is entered into EDMR system. E and FF NODI codes are red flags:"),
        tags$ol(
          tags$li(tags$strong("E \u2013 Analysis Not Conducted/No Sample."),
                  " All samples or results are not available due to analytical equipment failure, because a sample collection was overlooked, or samples could not be collected for the parameter during the reporting period. Use of this NODI code results in a violation."),
          tags$li(tags$strong("FF \u2013 Other (See Comments)."),
                  " No Data, not covered by NODI codes \u201cE\u201d or \u201cGG.\u201d Use only in extenuating circumstances where the reason for the absence of data is not covered by NODI codes \u201cE\u201d or \u201cGG.\u201d Use of this NODI code also results in a violation."),
          tags$li(tags$strong("GG \u2013 Conditional Monitoring \u2013 Not Required."),
                  " If the permit requires sample collection and analysis only under certain conditions and those conditions were not met during the monitoring period. This includes non-representative outfalls. Use of this NODI code does not result in a violation."))),
      checkboxInput("flag_only_selected",
                    "Only the selected contaminant (otherwise all 15)", FALSE),
      DTOutput("flag_table")
    )
  ),
  # footer: horizontal bar, credit on the left, GitHub icon on the right
  tags$hr(style = "margin-top:30px; border-top:2px solid #ccc;"),
  div(style = "display:flex; justify-content:space-between; align-items:center; padding-bottom:20px;",
      tags$em("Made in collaboration with ",
              tags$a(href = "https://cmusuds.org", target = "_blank",
                     "Carnegie Mellon University Students Using Data for Social Good (SUDS)")),
      tags$a(href = "https://github.com/ArchithSharma/SUDS_3rwk", target = "_blank",
             title = "View the code on GitHub", `aria-label` = "GitHub repository",
             style = "color:#333;",
             HTML("<svg width='28' height='28' viewBox='0 0 16 16' fill='currentColor' xmlns='http://www.w3.org/2000/svg'><path d='M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z'/></svg>")))
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  pal <- colorNumeric(zion, domain = c(0, 100))

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles("https://tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") |>
      fitBounds(min(dat$Longitude), min(dat$Latitude),
                max(dat$Longitude), max(dat$Latitude)) |>
      addControl(
        html = paste0("<div style='background:white;padding:6px 8px;border-radius:4px;font-size:12px;line-height:1.5'>",
                      "&#9679; measured value<br>&#9670; below detection (&lt;X, imputed)<br>",
                      "<span style='color:#c0392b'>&#8599;</span> increase &nbsp;",
                      "<span style='color:#2471a3'>&#8598;</span> decrease</div>"),
        position = "bottomleft")
  })

  # percentile legend; only mentions distance from 7 when pH is selected
  observe({
    title <- if (input$param == "pH") "Percentile<br><small>(distance from 7)</small>" else "Percentile"
    leafletProxy("map") |>
      addLegend("bottomright", pal = pal, values = c(0, 100), title = title,
                opacity = 0.9, layerId = "pctl_legend")
  })

  current <- reactive({
    d <- months[match(input$month, month_labels)]
    dat |>
      filter(Parameter_Name == input$param, begin_date <= d, end_date >= d)
  })

  output$summary <- renderText({
    d <- current()
    sprintf("%d facilities shown (%d below detection)", nrow(d), sum(d$censored))
  })

  observe({
    d <- current()
    proxy <- leafletProxy("map") |> clearGroup("pts") |> clearGroup("arrows")
    if (nrow(d) == 0) return()

    d <- d |> mutate(lr = switch(input$trend, prev = lr_prev, first = lr_first, off = NA_real_))
    is_ph <- d$Parameter_Name == "pH"

    detect_txt <- if_else(d$censored,
      if_else(d$method == "Importance sampling",
              sprintf("<br><i>Imputed: expected value (importance sampling) from %d measured values up to X</i>",
                      d$pool_n),
              "<br><i>Too few measured values below X; used X/2</i>"),
      "")

    popup <- sprintf(
      "<b>%s</b><br>%s<br><small>Facility ID %s &middot; %s Co. &middot; %s</small><hr style='margin:4px 0'>
       <b>%s</b> (%s to %s)<br>Reported: %s %s<br>Value used: %s<br>%s: %.0f<br>
       Change vs. previous%s: %s<br>Change since first%s: %s%s",
      d$Primary_Facility_Name, d$Client_Name, d$Primary_Facility_ID, d$County,
      d$Municipality,
      d$Parameter_Name, format(d$begin_date, "%m/%d/%Y"), format(d$end_date, "%m/%d/%Y"),
      d$label, d$Units, signif(d$value, 4),
      if_else(is_ph, "Percentile of |pH - 7|", "Percentile"), d$pctl,
      if_else(is_ph, " in |pH - 7|", ""), fmt_change(d$lr_prev),
      if_else(is_ph, " in |pH - 7|", ""), fmt_change(d$lr_first), detect_txt)

    proxy |>
      addMarkers(data = d, lng = ~Longitude, lat = ~Latitude, group = "pts",
                 icon = icons(iconUrl = point_svg(pal(d$pctl), d$censored),
                              iconWidth = 20, iconHeight = 20,
                              iconAnchorX = 10, iconAnchorY = 10),
                 popup = popup, label = d$Primary_Facility_Name)

    if (input$trend != "off") {
      a <- d |> filter(!is.na(lr), abs(lr) >= FLAT_TOL)
      if (nrow(a) > 0) {
        len <- 12 + 14 * pmin(abs(a$lr), ARROW_CAP)
        up  <- a$lr > 0
        col <- if_else(up, "#c0392b", "#2471a3")
        S   <- 2 * len + 20
        proxy |>
          addMarkers(data = a, lng = ~Longitude, lat = ~Latitude, group = "arrows",
                     icon = icons(iconUrl = arrow_svg(up, len, col),
                                  iconWidth = S, iconHeight = S,
                                  iconAnchorX = S / 2, iconAnchorY = S / 2),
                     options = markerOptions(interactive = FALSE, zIndexOffset = 1000))
      }
    }
  })

  # ---- E / FF / GG summary table ----
  output$flag_table <- renderDT({
    f <- flag_rows
    if (isTRUE(input$flag_only_selected)) f <- f |> filter(Parameter_Name == input$param)

    tbl <- f |>
      group_by(`Facility ID` = Primary_Facility_ID) |>
      summarise(`Facility Name` = first(Primary_Facility_Name),
                `Client Name`   = first(Client_Name),
                `E`      = sum(flag == "E"),
                `FF`     = sum(flag == "FF"),
                `E + FF` = `E` + `FF`,
                `GG`     = sum(flag == "GG"),
                `Total`  = n(),
                .groups = "drop") |>
      arrange(desc(`Total`))

    datatable(tbl, rownames = FALSE, filter = "top", extensions = "Buttons",
              options = list(dom = "Bfrtip",
                             buttons = list(list(extend = "csv",
                                                 text = "Download CSV",
                                                 filename = "efgg_by_facility",
                                                 exportOptions = list(
                                                   modifier = list(page = "all",
                                                                   search = "none")))),
                             pageLength = 10, order = list(list(7, "desc"))))
  }, server = FALSE)   # client-side processing so the CSV button sees every row
}

shinyApp(ui, server)