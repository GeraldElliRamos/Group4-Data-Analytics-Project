# ============================================================
#  QuakeGuard – Earthquake Vulnerability Dashboard
#  Marikina City & San Mateo, Rizal  |  2000-2024
#  IT 030 Data Analytics | Group 4
# ============================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# ── Custom CSS ───────────────────────────────────────────────
quakeguard_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Rajdhani:wght@400;500;600;700&family=Inter:wght@300;400;500&display=swap');

  :root {
    --bg-deep:    #f0f4f8;
    --bg-panel:   #ffffff;
    --bg-card:    #ffffff;
    --border:     #dde3ed;
    --accent:     #c0392b;
    --accent2:    #e67e22;
    --accent3:    #16a085;
    --text-main:  #1a2236;
    --text-muted: #6b7a99;
    --glow:       0 2px 8px rgba(192,57,43,0.12);
  }

  body, .wrapper { background: var(--bg-deep) !important; font-family: 'Inter', sans-serif; }

  .main-sidebar, .left-side { background: var(--bg-panel) !important; border-right: 1px solid var(--border) !important; box-shadow: 2px 0 8px rgba(0,0,0,0.06) !important; }
  .sidebar-menu > li > a { color: var(--text-muted) !important; font-size: 13px; letter-spacing: 0.4px; transition: all 0.2s; }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover { color: var(--accent) !important; background: rgba(192,57,43,0.07) !important; border-left: 3px solid var(--accent) !important; }
  .sidebar-menu > li > a .fa { color: var(--accent) !important; }

  .main-header .logo {
    background: var(--bg-panel) !important;
    border-bottom: 1px solid var(--border) !important;
    font-family: 'Rajdhani', sans-serif !important;
    font-size: 20px !important;
    font-weight: 700 !important;
    letter-spacing: 2px !important;
    color: var(--accent) !important;
  }
  .main-header .navbar { background: var(--bg-panel) !important; border-bottom: 1px solid var(--border) !important; box-shadow: none !important; }
  .main-header .logo span.logo-mini { color: var(--accent); }

  .content-wrapper, .right-side { background: var(--bg-deep) !important; }
  .content { padding: 16px !important; }

  .small-box {
    background: var(--bg-card) !important;
    border: 1px solid var(--border) !important;
    border-radius: 8px !important;
    box-shadow: 0 2px 6px rgba(0,0,0,0.05) !important;
  }
  .small-box h3 { font-family: 'Rajdhani', sans-serif; font-size: 2rem; color: var(--text-main) !important; }
  .small-box p  { font-size: 12px; color: var(--text-muted) !important; text-transform: uppercase; letter-spacing: 0.8px; }
  .small-box .icon { color: rgba(0,0,0,0.06) !important; }
  .small-box.red-box   { border-left: 4px solid var(--accent)  !important; }
  .small-box.orange-box{ border-left: 4px solid var(--accent2) !important; }
  .small-box.teal-box  { border-left: 4px solid var(--accent3) !important; }
  .small-box.blue-box  { border-left: 4px solid #2980b9 !important; }

  .box {
    background: var(--bg-card) !important;
    border: 1px solid var(--border) !important;
    border-top: none !important;
    border-radius: 8px !important;
    box-shadow: 0 2px 6px rgba(0,0,0,0.05) !important;
  }
  .box-header {
    background: var(--bg-card) !important;
    border-bottom: 1px solid var(--border) !important;
    border-radius: 8px 8px 0 0 !important;
    padding: 10px 14px !important;
  }
  .box-header .box-title {
    font-family: 'Rajdhani', sans-serif;
    font-size: 14px;
    font-weight: 600;
    letter-spacing: 0.8px;
    text-transform: uppercase;
    color: var(--text-main) !important;
  }
  .box-header .box-title::before {
    content: '▐ ';
    color: var(--accent);
    font-size: 10px;
  }

  .sidebar-filter-title {
    font-family: 'Rajdhani', sans-serif;
    font-size: 11px;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    color: var(--accent);
    padding: 14px 14px 4px;
    border-top: 1px solid var(--border);
    margin-top: 8px;
  }
  .form-group label { color: var(--text-muted) !important; font-size: 12px !important; }
  .irs--shiny .irs-bar { background: var(--accent) !important; border-top: 1px solid var(--accent) !important; border-bottom: 1px solid var(--accent) !important; }
  .irs--shiny .irs-handle { background: var(--accent) !important; border-color: var(--accent) !important; }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: var(--accent) !important; }
  .irs--shiny .irs-line { background: var(--border) !important; }
  .irs--shiny .irs-grid-text { color: var(--text-muted) !important; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { color: var(--text-muted) !important; }

  .checkbox label { color: var(--text-muted) !important; font-size: 12px !important; }
  input[type='checkbox']:checked { accent-color: var(--accent); }
  .shiny-input-checkboxgroup .checkbox { margin: 2px 0 !important; }

  .selectize-input { background: #f8fafc !important; border-color: var(--border) !important; color: var(--text-main) !important; }
  .selectize-dropdown { background: var(--bg-panel) !important; border-color: var(--border) !important; color: var(--text-main) !important; }

  .nav-tabs-custom { background: var(--bg-card) !important; border: 1px solid var(--border) !important; border-radius: 8px !important; }
  .nav-tabs-custom > .nav-tabs { border-bottom: 1px solid var(--border) !important; background: transparent !important; }
  .nav-tabs-custom > .nav-tabs > li > a { color: var(--text-muted) !important; background: transparent !important; border: none !important; font-size: 12px; letter-spacing: 0.5px; }
  .nav-tabs-custom > .nav-tabs > li.active > a { color: var(--accent) !important; background: transparent !important; border-bottom: 2px solid var(--accent) !important; }
  .nav-tabs-custom > .tab-content { background: transparent !important; }

  #record_badge {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 6px;
    padding: 6px 12px;
    font-size: 12px;
    color: var(--text-muted);
    margin-bottom: 12px;
    display: inline-block;
  }
  #record_badge span { color: var(--accent); font-family: 'Rajdhani', sans-serif; font-size: 16px; font-weight: 700; }

  .plotly .bg { fill: transparent !important; }

  ::-webkit-scrollbar { width: 6px; height: 6px; }
  ::-webkit-scrollbar-track { background: var(--bg-deep); }
  ::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }

  .page-header-bar {
    background: linear-gradient(90deg, rgba(192,57,43,0.08) 0%, transparent 100%);
    border-left: 4px solid var(--accent);
    border-radius: 4px;
    padding: 10px 16px;
    margin-bottom: 16px;
  }
  .page-header-bar h4 {
    font-family: 'Rajdhani', sans-serif;
    font-size: 18px;
    font-weight: 700;
    letter-spacing: 1px;
    color: var(--text-main);
    margin: 0;
  }
  .page-header-bar p {
    font-size: 12px;
    color: var(--text-muted);
    margin: 2px 0 0;
  }
"

# ── Plot theme ───────────────────────────────────────────────
theme_quake <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_line(color = "#243050", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color = "#7a8aaa", size = 10),
      axis.title       = element_text(color = "#7a8aaa", size = 11),
      plot.title       = element_text(color = "#e8edf5", size = 13, family = "sans", face = "bold"),
      plot.subtitle    = element_text(color = "#7a8aaa", size = 10),
      legend.background= element_rect(fill = "transparent", color = NA),
      legend.text      = element_text(color = "#7a8aaa", size = 10),
      legend.title     = element_text(color = "#e8edf5", size = 10),
      strip.text       = element_text(color = "#e8edf5")
    )
}

pal_mag <- c(
  "Minor (<3.0)"       = "#4895ef",
  "Light (3.0-3.9)"    = "#f4a261",
  "Moderate (4.0-4.9)" = "#e67e22",
  "Strong (5.0+)"      = "#e63946"
)

pal_depth <- c(
  "Shallow (70 km or less)"  = "#e74c3c",
  "Intermediate (71-300 km)" = "#9b59b6",
  "Deep (more than 300 km)"  = "#2ecc71"
)

pal_src <- c("PHIVOLCS" = "#4895ef", "USGS" = "#2a9d8f")

# ── Fault line coordinates ───────────────────────────────────
west_valley_fault <- data.frame(
  lat = c(14.404, 14.450, 14.500, 14.540, 14.580, 14.620, 14.660, 14.700, 14.740, 14.780),
  lng = c(121.040, 121.055, 121.065, 121.075, 121.085, 121.095, 121.105, 121.115, 121.125, 121.135)
)

east_valley_fault <- data.frame(
  lat = c(14.500, 14.540, 14.580, 14.620, 14.660, 14.700, 14.740, 14.780),
  lng = c(121.110, 121.120, 121.130, 121.145, 121.160, 121.175, 121.185, 121.200)
)

# ── Population data (PSA 2020 Census) ───────────────────────
pop_data <- data.frame(
  city       = c("Marikina City", "San Mateo, Rizal", "Quezon City",
                 "Antipolo City", "Pasig City", "Cainta, Rizal",
                 "Taytay, Rizal", "Rodriguez (Montalban)"),
  population = c(512428, 267661, 2960048, 887399, 803159,
                 362380, 363788, 460286)
)

# ── UI ──────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span("QUAKEGUARD"),
    titleWidth = 220
  ),
  
  dashboardSidebar(
    width = 220,
    tags$style(HTML(quakeguard_css)),
    
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview",            tabName = "overview",  icon = icon("gauge-high")),
      menuItem("Temporal Trends",     tabName = "temporal",  icon = icon("chart-line")),
      menuItem("Magnitude & Depth",   tabName = "magdepth",  icon = icon("mountain")),
      menuItem("Epicenter Map",       tabName = "map",       icon = icon("map-location-dot")),
      menuItem("Data Sources",        tabName = "sources",   icon = icon("database")),
      menuItem("Risk & Preparedness", tabName = "risk",      icon = icon("shield-halved")),
      menuItem("Population Exposure", tabName = "exposure",  icon = icon("people-group")),
      menuItem("Smart City Response", tabName = "smartcity", icon = icon("city"))
    ),
    
    tags$div(class = "sidebar-filter-title", "FILTERS"),
    
    sliderInput("yr_range", "Year Range",
                min = 2000, max = 2024,
                value = c(2000, 2024), sep = "", ticks = FALSE),
    
    tags$div(style = "padding: 0 14px;",
             checkboxGroupInput("mag_filter", "Magnitude Class",
                                choices  = c("Minor (<3.0)", "Light (3.0-3.9)",
                                             "Moderate (4.0-4.9)", "Strong (5.0+)"),
                                selected = c("Minor (<3.0)", "Light (3.0-3.9)",
                                             "Moderate (4.0-4.9)", "Strong (5.0+)")
             )
    ),
    
    tags$div(style = "padding: 0 14px;",
             checkboxGroupInput("src_filter", "Data Source",
                                choices  = c("PHIVOLCS", "USGS"),
                                selected = c("PHIVOLCS", "USGS")
             )
    ),
    
    tags$div(class = "sidebar-filter-title", "AREA / CITY"),
    
    tags$div(style = "padding: 0 14px;",
             checkboxGroupInput("city_filter", NULL,
                                choices = c(
                                  "Marikina City", "San Mateo, Rizal", "Antipolo City",
                                  "Cainta, Rizal", "Taytay, Rizal", "Pasig City",
                                  "Quezon City", "Rodriguez (Montalban)", "Other"
                                ),
                                selected = c(
                                  "Marikina City", "San Mateo, Rizal", "Antipolo City",
                                  "Cainta, Rizal", "Taytay, Rizal", "Pasig City",
                                  "Quezon City", "Rodriguez (Montalban)", "Other"
                                )
             )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── Overview ────────────────────────────────────────
      tabItem(tabName = "overview",
              tags$div(class = "page-header-bar",
                       tags$h4("SEISMIC ACTIVITY OVERVIEW"),
                       tags$p("Marikina City & San Mateo, Rizal — 2000 to 2024")
              ),
              htmlOutput("record_badge"),
              fluidRow(
                valueBoxOutput("vbox_total",   width = 3),
                valueBoxOutput("vbox_strong",  width = 3),
                valueBoxOutput("vbox_shallow", width = 3),
                valueBoxOutput("vbox_max",     width = 3)
              ),
              fluidRow(
                box(title = "Earthquake Frequency Per Year", width = 8, solidHeader = FALSE,
                    plotlyOutput("freq_chart", height = "340px")),
                box(title = "Magnitude Class Breakdown", width = 4, solidHeader = FALSE,
                    plotlyOutput("mag_pie", height = "340px"))
              )
      ),
      
      # ── Temporal ────────────────────────────────────────
      tabItem(tabName = "temporal",
              tags$div(class = "page-header-bar",
                       tags$h4("TEMPORAL TRENDS"),
                       tags$p("Earthquake frequency and average magnitude over time")
              ),
              fluidRow(
                box(title = "Earthquake Frequency Per Year", width = 12, solidHeader = FALSE,
                    plotlyOutput("freq_bar_full", height = "320px"))
              ),
              fluidRow(
                box(title = "Average Magnitude Per Year", width = 12, solidHeader = FALSE,
                    plotlyOutput("avg_mag_line", height = "320px"))
              )
      ),
      
      # ── Magnitude & Depth ───────────────────────────────
      tabItem(tabName = "magdepth",
              tags$div(class = "page-header-bar",
                       tags$h4("MAGNITUDE & DEPTH DISTRIBUTION"),
                       tags$p("Understanding the character and impact potential of recorded earthquakes")
              ),
              fluidRow(
                box(title = "Magnitude Distribution", width = 6, solidHeader = FALSE,
                    plotlyOutput("mag_hist", height = "340px")),
                box(title = "Focal Depth Distribution", width = 6, solidHeader = FALSE,
                    plotlyOutput("depth_hist", height = "340px"))
              ),
              fluidRow(
                box(title = "Magnitude vs. Depth (Scatter)", width = 12, solidHeader = FALSE,
                    plotlyOutput("mag_depth_scatter", height = "340px"))
              )
      ),
      
      # ── Map ─────────────────────────────────────────────
      tabItem(tabName = "map",
              tags$div(class = "page-header-bar",
                       tags$h4("EPICENTER MAP"),
                       tags$p("Spatial distribution of recorded seismic events — click any marker for details")
              ),
              fluidRow(
                box(title = "Earthquake Epicenters", width = 12, solidHeader = FALSE,
                    leafletOutput("eq_map", height = "580px"))
              )
      ),
      
      # ── Sources ─────────────────────────────────────────
      tabItem(tabName = "sources",
              tags$div(class = "page-header-bar",
                       tags$h4("DATA SOURCE ANALYSIS"),
                       tags$p("Record coverage comparison between PHIVOLCS and USGS catalogs")
              ),
              fluidRow(
                box(title = "Records by Data Source", width = 6, solidHeader = FALSE,
                    plotlyOutput("src_bar", height = "320px")),
                box(title = "Annual Coverage by Source", width = 6, solidHeader = FALSE,
                    plotlyOutput("src_year_line", height = "320px"))
              ),
              fluidRow(
                box(title = "Source Summary", width = 12, solidHeader = FALSE,
                    tableOutput("src_table"))
              )
      ),
      
      # ── Risk & Preparedness ─────────────────────────────
      tabItem(tabName = "risk",
              tags$div(class = "page-header-bar",
                       tags$h4("RISK ZONES & PREPAREDNESS GUIDE"),
                       tags$p("Areas most at risk from the Marikina Valley Fault System and what communities can do")
              ),
              fluidRow(
                box(title = "Fault Rupture Impact Zones", width = 12, solidHeader = FALSE,
                    leafletOutput("risk_map", height = "500px"))
              ),
              fluidRow(
                box(title = "Marikina City — Risk Profile & Preparedness", width = 6, solidHeader = FALSE,
                    tags$div(style = "padding: 10px;",
                             tags$h4(style = "color:#c0392b; font-family:Rajdhani,sans-serif; letter-spacing:1px;",
                                     "WHY MARIKINA IS HIGH RISK"),
                             tags$p(style = "font-size:13px; color:#1a2236; line-height:1.7;",
                                    "Marikina City sits directly on top of the ", tags$b("West Valley Fault"),
                                    ", one of the most dangerous active faults in the Philippines. A major rupture
                  (estimated Magnitude 7.2) would cause severe ground shaking, liquefaction along
                  riverbanks, and surface rupture along the fault trace cutting through densely
                  populated barangays including Sto. Nino, Concepcion Uno, and Malanday."
                             ),
                             tags$hr(style = "border-color:#dde3ed;"),
                             tags$h4(style = "color:#c0392b; font-family:Rajdhani,sans-serif; letter-spacing:1px;",
                                     "MOST EXPOSED BARANGAYS"),
                             tags$p(style = "font-size:13px; color:#1a2236; line-height:1.7;",
                                    tags$b("Along the fault trace: "), "Sto. Nino, Concepcion Uno, Concepcion Dos, Malanday, Nangka, Parang",
                                    tags$br(),
                                    tags$b("Flood + liquefaction risk: "), "Tumana, Kalumpang, Industrial Valley, Barangka"
                             ),
                             tags$hr(style = "border-color:#dde3ed;"),
                             tags$h4(style = "color:#16a085; font-family:Rajdhani,sans-serif; letter-spacing:1px;",
                                     "WHAT MARIKINA CITY CAN DO"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Enforce strict building codes and retrofitting of old structures along the fault trace"),
                                     tags$li("Maintain and expand the Marikina River early warning and flood control system"),
                                     tags$li("Conduct regular earthquake drills in all barangays, schools, and markets"),
                                     tags$li("Map and communicate the exact fault trace to all residents through barangay assemblies"),
                                     tags$li("Pre-position emergency supplies and designate evacuation centers away from the fault zone"),
                                     tags$li("Partner with PHIVOLCS for continuous fault monitoring and community education"),
                                     tags$li("Restrict high-density construction within the 5-meter fault hazard zone on each side")
                             )
                    )
                ),
                box(title = "San Mateo, Rizal — Risk Profile & Preparedness", width = 6, solidHeader = FALSE,
                    tags$div(style = "padding: 10px;",
                             tags$h4(style = "color:#e67e22; font-family:Rajdhani,sans-serif; letter-spacing:1px;",
                                     "WHY SAN MATEO IS AT RISK"),
                             tags$p(style = "font-size:13px; color:#1a2236; line-height:1.7;",
                                    "San Mateo, Rizal lies adjacent to the northern segment of the ",
                                    tags$b("West Valley Fault"), " and is also exposed to the ",
                                    tags$b("East Valley Fault"),
                                    ". The town sits on alluvial soil deposited by the Marikina River,
                  which significantly amplifies ground shaking during an earthquake.
                  Its hilly eastern portions are also prone to earthquake-triggered landslides."
                             ),
                             tags$hr(style = "border-color:#dde3ed;"),
                             tags$h4(style = "color:#e67e22; font-family:Rajdhani,sans-serif; letter-spacing:1px;",
                                     "MOST EXPOSED AREAS"),
                             tags$p(style = "font-size:13px; color:#1a2236; line-height:1.7;",
                                    tags$b("Fault proximity: "), "Guitnang Bayan I & II, Poblacion, Dulong Bayan",
                                    tags$br(),
                                    tags$b("Landslide risk: "), "Ampid I & II, Guinayang, Banaba"
                             ),
                             tags$hr(style = "border-color:#dde3ed;"),
                             tags$h4(style = "color:#16a085; font-family:Rajdhani,sans-serif; letter-spacing:1px;",
                                     "WHAT SAN MATEO CAN DO"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Conduct soil liquefaction and landslide hazard mapping in coordination with MGB and PHIVOLCS"),
                                     tags$li("Establish a municipal earthquake contingency plan with clear roles for each barangay"),
                                     tags$li("Identify and reinforce critical infrastructure — bridges, roads, evacuation routes — that cross or run near the fault"),
                                     tags$li("Install community-level early warning systems and signage in high-risk barangays"),
                                     tags$li("Educate residents on the Drop, Cover, and Hold On protocol and post-quake procedures"),
                                     tags$li("Ensure all new construction follows NSCP seismic provisions"),
                                     tags$li("Coordinate with Marikina City and Rodriguez for cross-boundary evacuation and relief planning")
                             )
                    )
                )
              ),
              fluidRow(
                box(title = "Impact on Surrounding Cities", width = 12, solidHeader = FALSE,
                    tags$div(style = "padding: 10px;",
                             tags$p(style = "font-size:13px; color:#6b7a99; margin-bottom:12px;",
                                    "A major West Valley Fault rupture would not be limited to Marikina and San Mateo.
                PHIVOLCS estimates that a M7.2 event could affect over 30 cities and municipalities
                across Metro Manila and Rizal province."
                             ),
                             tags$table(
                               style = "width:100%; border-collapse:collapse; font-size:13px;",
                               tags$thead(
                                 tags$tr(style = "background:#f0f4f8; color:#1a2236;",
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "City / Municipality"),
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "Proximity to Fault"),
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "Primary Hazard"),
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "Risk Level")
                                 )
                               ),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("Marikina City")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Directly on fault"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Surface rupture, liquefaction, flooding"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                           tags$span(style = "color:#c0392b; font-weight:700;", "VERY HIGH"))
                                 ),
                                 tags$tr(style = "background:#fafbfc;",
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("San Mateo, Rizal")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Adjacent to fault"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Ground shaking, landslides, liquefaction"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                                 tags$span(style = "color:#c0392b; font-weight:700;", "VERY HIGH"))
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("Quezon City")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "1–5 km west of fault"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Severe ground shaking, structural damage"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                           tags$span(style = "color:#e67e22; font-weight:700;", "HIGH"))
                                 ),
                                 tags$tr(style = "background:#fafbfc;",
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("Antipolo City")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Near East Valley Fault"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Ground shaking, landslides on slopes"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                                 tags$span(style = "color:#e67e22; font-weight:700;", "HIGH"))
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("Pasig City")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "2–4 km from fault"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Strong shaking, liquefaction near river"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                           tags$span(style = "color:#e67e22; font-weight:700;", "HIGH"))
                                 ),
                                 tags$tr(style = "background:#fafbfc;",
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("Cainta, Rizal")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "3–5 km from fault"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Moderate to strong shaking, flooding"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                                 tags$span(style = "color:#f4a261; font-weight:700;", "MODERATE–HIGH"))
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("Taytay, Rizal")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "5–8 km from fault"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Moderate shaking, some liquefaction"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                           tags$span(style = "color:#f4a261; font-weight:700;", "MODERATE"))
                                 ),
                                 tags$tr(style = "background:#fafbfc;",
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$b("Rodriguez (Montalban)")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Near fault northern end"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Ground shaking, landslides, flooding"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;",
                                                 tags$span(style = "color:#e67e22; font-weight:700;", "HIGH"))
                                 )
                               )
                             )
                    )
                )
              )
      ),
      
      # ── Population Exposure ─────────────────────────────
      tabItem(tabName = "exposure",
              tags$div(class = "page-header-bar",
                       tags$h4("POPULATION EXPOSURE ANALYSIS"),
                       tags$p("Estimated population at risk based on earthquake epicenter distribution")
              ),
              fluidRow(
                valueBoxOutput("vbox_pop_marikina", width = 3),
                valueBoxOutput("vbox_pop_sanmateo", width = 3),
                valueBoxOutput("vbox_pop_qc",       width = 3),
                valueBoxOutput("vbox_pop_total",    width = 3)
              ),
              fluidRow(
                box(title = "Earthquake Events vs Population At Risk", width = 8, solidHeader = FALSE,
                    plotlyOutput("pop_exposure_chart", height = "380px")),
                box(title = "Population At Risk Summary", width = 4, solidHeader = FALSE,
                    tableOutput("pop_table"))
              ),
              fluidRow(
                box(title = "What Population Exposure Means for Smart Cities", width = 12, solidHeader = FALSE,
                    tags$div(style = "padding:10px;",
                             tags$p(style = "font-size:13px; color:#1a2236; line-height:1.8;",
                                    "Population exposure combines seismic hazard data with census population figures
                to estimate how many people are directly at risk during a fault rupture event.
                Smart cities use this metric to prioritize where to deploy sensors, emergency
                response teams, and automated alert systems."
                             ),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li(tags$b("Marikina City (Population ~512,000): "),
                                             "Highest priority for fault-line sensor deployment and automated evacuation routing systems."),
                                     tags$li(tags$b("San Mateo, Rizal (Population ~267,000): "),
                                             "Priority for landslide early warning systems and cross-boundary evacuation coordination with Marikina."),
                                     tags$li(tags$b("Quezon City (Population ~2.96M): "),
                                             "Largest exposed population. Smart building codes and real-time structural health monitoring are critical."),
                                     tags$li(tags$b("Antipolo City (Population ~887,000): "),
                                             "Slope monitoring sensors needed given landslide risk combined with high population density.")
                             )
                    )
                )
              )
      ),
      
      # ── Smart City Response ─────────────────────────────
      tabItem(tabName = "smartcity",
              tags$div(class = "page-header-bar",
                       tags$h4("SMART CITY EARTHQUAKE RESPONSE FRAMEWORK"),
                       tags$p("Technology-driven interventions for seismic risk reduction in Marikina and San Mateo")
              ),
              fluidRow(
                box(title = "Smart City Preparedness Scorecard", width = 12, solidHeader = FALSE,
                    tags$div(style = "padding:10px;",
                             tags$table(
                               style = "width:100%; border-collapse:collapse; font-size:13px;",
                               tags$thead(
                                 tags$tr(style = "background:#f0f4f8;",
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "Smart City Indicator"),
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "Marikina City"),
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "San Mateo, Rizal"),
                                         tags$th(style = "padding:8px; border:1px solid #dde3ed;", "Recommended Action")
                                 )
                               ),
                               tags$tbody(
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Real-time seismic sensors"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#e67e22;", "Partial")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Deploy MEMS accelerometer network linked to PHIVOLCS")
                                 ),
                                 tags$tr(style = "background:#fafbfc;",
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Automated public alert system"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#e67e22;", "Partial")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Integrate SMS blast and siren network with NDRRMC")
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "GIS evacuation routing"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#16a085;", "Yes")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#e67e22;", "Partial")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Expand to cover all barangays with real-time road status updates")
                                 ),
                                 tags$tr(style = "background:#fafbfc;",
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Structural health monitoring"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Install IoT strain sensors on bridges, schools, and hospitals")
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Flood + fault early warning"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#16a085;", "Yes (flood only)")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Integrate seismic triggers into existing flood warning infrastructure")
                                 ),
                                 tags$tr(style = "background:#fafbfc;",
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Open disaster data portal"),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#e67e22;", "Partial")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                         tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Publish real-time earthquake and hazard data as open API")
                                 ),
                                 tags$tr(
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Community mobile alert app"),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", tags$span(style = "color:#c0392b;", "None")),
                                   tags$td(style = "padding:8px; border:1px solid #dde3ed;", "Develop barangay-level push notification app integrated with PHIVOLCS feed")
                                 )
                               )
                             )
                    )
                )
              ),
              fluidRow(
                box(title = "Marikina City — Smart City Roadmap", width = 6, solidHeader = FALSE,
                    tags$div(style = "padding:10px;",
                             tags$h4(style = "color:#c0392b; font-family:Rajdhani,sans-serif;", "SHORT TERM (0–2 YEARS)"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Deploy seismic sensors along the West Valley Fault trace through Marikina"),
                                     tags$li("Integrate PHIVOLCS earthquake alerts into the existing Marikina flood SMS system"),
                                     tags$li("Launch a public-facing earthquake risk map app for residents"),
                                     tags$li("Require seismic retrofit assessment for all buildings within 500m of the fault")
                             ),
                             tags$h4(style = "color:#e67e22; font-family:Rajdhani,sans-serif;", "MEDIUM TERM (2–5 YEARS)"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Install IoT structural health monitors on all public schools and hospitals"),
                                     tags$li("Build a city-level disaster data dashboard accessible to all barangay officials"),
                                     tags$li("Complete fault hazard zone clearance — relocate informal settlers on fault trace"),
                                     tags$li("Establish a joint Marikina–San Mateo earthquake response command center")
                             ),
                             tags$h4(style = "color:#16a085; font-family:Rajdhani,sans-serif;", "LONG TERM (5+ YEARS)"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Achieve full smart building compliance for all new construction"),
                                     tags$li("Operate a fully automated earthquake early warning system with sub-10-second alerts"),
                                     tags$li("Become a model Philippine smart city for seismic disaster risk reduction")
                             )
                    )
                ),
                box(title = "San Mateo, Rizal — Smart City Roadmap", width = 6, solidHeader = FALSE,
                    tags$div(style = "padding:10px;",
                             tags$h4(style = "color:#c0392b; font-family:Rajdhani,sans-serif;", "SHORT TERM (0–2 YEARS)"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Conduct LIDAR-based hazard mapping for landslide and liquefaction zones"),
                                     tags$li("Establish a municipal DRRM office with dedicated seismic risk function"),
                                     tags$li("Train all barangay officials on earthquake response protocols"),
                                     tags$li("Install rain and slope movement sensors in Ampid and Guinayang")
                             ),
                             tags$h4(style = "color:#e67e22; font-family:Rajdhani,sans-serif;", "MEDIUM TERM (2–5 YEARS)"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Build a municipal GIS hazard portal showing fault, flood, and landslide zones"),
                                     tags$li("Retrofit or replace all pre-1992 public school buildings"),
                                     tags$li("Coordinate with Marikina City on shared evacuation routes and supply depots"),
                                     tags$li("Connect to the national NDRRMC real-time alert network")
                             ),
                             tags$h4(style = "color:#16a085; font-family:Rajdhani,sans-serif;", "LONG TERM (5+ YEARS)"),
                             tags$ul(style = "font-size:13px; color:#1a2236; line-height:2;",
                                     tags$li("Fully integrate with Metro Manila earthquake early warning network"),
                                     tags$li("Achieve zero informal settlements within mapped high-risk hazard zones"),
                                     tags$li("Publish annual seismic risk report as open data for researchers and planners")
                             )
                    )
                )
              )
      )
      
    ) # end tabItems
  )
)

# ── Server ──────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Load data ──────────────────────────────────────────
  raw <- reactive({
    df <- read.csv("data/processed/earthquake_combined.csv", stringsAsFactors = FALSE)
    df$date_time <- ymd_hms(df$date_time, quiet = TRUE)
    df$year      <- year(df$date_time)
    df <- df %>%
      mutate(
        mag_class = case_when(
          magnitude < 3.0 ~ "Minor (<3.0)",
          magnitude < 4.0 ~ "Light (3.0-3.9)",
          magnitude < 5.0 ~ "Moderate (4.0-4.9)",
          TRUE            ~ "Strong (5.0+)"
        ),
        depth_class = case_when(
          depth_km <= 70  ~ "Shallow (70 km or less)",
          depth_km <= 300 ~ "Intermediate (71-300 km)",
          TRUE            ~ "Deep (more than 300 km)"
        )
      )
    df$mag_class <- factor(df$mag_class,
                           levels = c("Minor (<3.0)", "Light (3.0-3.9)",
                                      "Moderate (4.0-4.9)", "Strong (5.0+)"))
    df$depth_class <- factor(df$depth_class,
                             levels = c("Shallow (70 km or less)",
                                        "Intermediate (71-300 km)",
                                        "Deep (more than 300 km)"))
    df <- df %>%
      mutate(city = case_when(
        latitude >= 14.620 & latitude <= 14.680 & longitude >= 121.070 & longitude <= 121.130 ~ "Marikina City",
        latitude >= 14.680 & latitude <= 14.760 & longitude >= 121.080 & longitude <= 121.160 ~ "San Mateo, Rizal",
        latitude >= 14.710 & latitude <= 14.810 & longitude >= 121.140 & longitude <= 121.250 ~ "Rodriguez (Montalban)",
        latitude >= 14.620 & latitude <= 14.760 & longitude >= 121.130 & longitude <= 121.260 ~ "Antipolo City",
        latitude >= 14.560 & latitude <= 14.620 & longitude >= 121.120 & longitude <= 121.200 ~ "Cainta, Rizal",
        latitude >= 14.490 & latitude <= 14.570 & longitude >= 121.130 & longitude <= 121.230 ~ "Taytay, Rizal",
        latitude >= 14.530 & latitude <= 14.620 & longitude >= 121.040 & longitude <= 121.100 ~ "Pasig City",
        latitude >= 14.580 & latitude <= 14.780 & longitude >= 120.980 & longitude <= 121.080 ~ "Quezon City",
        TRUE ~ "Other"
      ))
    df
  })
  
  # ── Filtered data ──────────────────────────────────────
  filtered <- reactive({
    raw() %>%
      filter(
        !is.na(year),
        year      >= input$yr_range[1],
        year      <= input$yr_range[2],
        mag_class %in% input$mag_filter,
        source    %in% input$src_filter,
        city      %in% input$city_filter
      )
  })
  
  # ── Record badge ───────────────────────────────────────
  output$record_badge <- renderUI({
    n <- nrow(filtered())
    HTML(paste0('<div id="record_badge">Showing <span>', formatC(n, big.mark = ","),
                '</span> records matching current filters</div>'))
  })
  
  # ── Value boxes ────────────────────────────────────────
  output$vbox_total <- renderValueBox({
    valueBox(formatC(nrow(filtered()), big.mark = ","), "Total Events",
             icon = icon("circle-radiation"), color = "red")
  })
  
  output$vbox_strong <- renderValueBox({
    n <- filtered() %>% filter(mag_class == "Strong (5.0+)") %>% nrow()
    valueBox(formatC(n, big.mark = ","), "Strong Events (5.0+)",
             icon = icon("triangle-exclamation"), color = "orange")
  })
  
  output$vbox_shallow <- renderValueBox({
    n   <- filtered() %>% filter(depth_class == "Shallow (70 km or less)") %>% nrow()
    pct <- round(n / max(nrow(filtered()), 1) * 100)
    valueBox(paste0(pct, "%"), "Shallow Earthquakes",
             icon = icon("layer-group"), color = "teal")
  })
  
  output$vbox_max <- renderValueBox({
    m <- max(filtered()$magnitude, na.rm = TRUE)
    valueBox(round(m, 1), "Max Recorded Magnitude",
             icon = icon("bolt"), color = "blue")
  })
  
  # ── Plotly helper ──────────────────────────────────────
  plotly_dark <- function(p) {
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color = "#6b7a99", family = "Inter, sans-serif"),
      xaxis         = list(gridcolor = "#e8edf5", zerolinecolor = "#e8edf5", linecolor = "#dde3ed"),
      yaxis         = list(gridcolor = "#e8edf5", zerolinecolor = "#e8edf5", linecolor = "#dde3ed"),
      legend        = list(bgcolor = "rgba(0,0,0,0)", bordercolor = "rgba(0,0,0,0)"),
      margin        = list(l = 40, r = 20, t = 30, b = 40)
    ) %>% config(displayModeBar = FALSE)
  }
  
  # ── Frequency bar ──────────────────────────────────────
  freq_plot <- reactive({
    filtered() %>%
      group_by(year) %>%
      summarise(count = n(), .groups = "drop")
  })
  
  render_freq <- function(height = 320) {
    renderPlotly({
      df <- freq_plot()
      plot_ly(df, x = ~year, y = ~count, type = "bar",
              marker = list(color = "#e63946", line = list(color = "rgba(0,0,0,0)", width = 0)),
              hovertemplate = "<b>%{x}</b><br>Events: %{y}<extra></extra>") %>%
        plotly_dark()
    })
  }
  
  output$freq_chart    <- render_freq()
  output$freq_bar_full <- render_freq()
  
  # ── Magnitude pie ──────────────────────────────────────
  output$mag_pie <- renderPlotly({
    df <- filtered() %>%
      group_by(mag_class) %>%
      summarise(n = n(), .groups = "drop")
    plot_ly(df, labels = ~mag_class, values = ~n, type = "pie",
            textinfo = "label+percent",
            marker = list(
              colors = c("#4895ef", "#f4a261", "#e67e22", "#e63946"),
              line   = list(color = "#111827", width = 2)
            ),
            hovertemplate = "<b>%{label}</b><br>Count: %{value}<extra></extra>") %>%
      layout(showlegend = FALSE,
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             margin = list(l = 10, r = 10, t = 10, b = 10),
             font = list(color = "#6b7a99")) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Avg magnitude line ─────────────────────────────────
  output$avg_mag_line <- renderPlotly({
    df <- filtered() %>%
      group_by(year) %>%
      summarise(avg_mag = mean(magnitude, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~year, y = ~avg_mag, type = "scatter", mode = "lines+markers",
            line   = list(color = "#f4a261", width = 2),
            marker = list(color = "#f4a261", size = 7, line = list(color = "#ffffff", width = 2)),
            hovertemplate = "<b>%{x}</b><br>Avg Mag: %{y:.2f}<extra></extra>") %>%
      plotly_dark() %>%
      layout(yaxis = list(title = "Average Magnitude"))
  })
  
  # ── Magnitude histogram ────────────────────────────────
  output$mag_hist <- renderPlotly({
    df <- filtered()
    plot_ly(df, x = ~magnitude, color = ~mag_class, type = "histogram",
            xbins  = list(size = 0.2),
            colors = unname(pal_mag[levels(df$mag_class)]),
            hovertemplate = "Mag: %{x}<br>Count: %{y}<extra></extra>") %>%
      plotly_dark() %>%
      layout(barmode = "stack",
             xaxis = list(title = "Magnitude"),
             yaxis = list(title = "Count"),
             legend = list(title = list(text = "Class")))
  })
  
  # ── Depth histogram ────────────────────────────────────
  output$depth_hist <- renderPlotly({
    df <- filtered()
    plot_ly(df, x = ~depth_km, color = ~depth_class, type = "histogram",
            xbins  = list(size = 10),
            colors = c("#e74c3c", "#9b59b6", "#2ecc71"),
            hovertemplate = "Depth: %{x} km<br>Count: %{y}<extra></extra>") %>%
      plotly_dark() %>%
      layout(barmode = "stack",
             xaxis = list(title = "Depth (km)"),
             yaxis = list(title = "Count"),
             legend = list(title = list(text = "Depth Class")))
  })
  
  # ── Magnitude vs depth scatter ─────────────────────────
  output$mag_depth_scatter <- renderPlotly({
    df <- filtered() %>% sample_n(min(nrow(.), 3000))
    plot_ly(df, x = ~magnitude, y = ~depth_km, color = ~mag_class,
            colors = unname(pal_mag[levels(df$mag_class)]),
            type = "scatter", mode = "markers",
            marker = list(size = 5, opacity = 0.5),
            hovertemplate = "Mag: %{x}<br>Depth: %{y} km<extra></extra>") %>%
      plotly_dark() %>%
      layout(xaxis = list(title = "Magnitude"),
             yaxis = list(title = "Depth (km)", autorange = "reversed"),
             legend = list(title = list(text = "Class")))
  })
  
  # ── Epicenter map ──────────────────────────────────────
  output$eq_map <- renderLeaflet({
    df <- filtered()
    getColor  <- function(mag) case_when(
      mag >= 5.0 ~ "#c0392b",
      mag >= 4.0 ~ "#e67e22",
      TRUE       ~ "#2980b9"
    )
    getRadius <- function(mag) ifelse(mag >= 5.0, 7, ifelse(mag >= 4.0, 5, 3))
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = west_valley_fault, lng = ~lng, lat = ~lat,
                   color = "#e63946", weight = 2.5, opacity = 0.8,
                   label = "West Valley Fault", dashArray = "6,4") %>%
      addPolylines(data = east_valley_fault, lng = ~lng, lat = ~lat,
                   color = "#f4a261", weight = 2, opacity = 0.8,
                   label = "East Valley Fault", dashArray = "6,4") %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~getColor(magnitude), radius = ~getRadius(magnitude),
        stroke = FALSE, fillOpacity = 0.8,
        popup = ~paste0(
          "<div style='font-family:Inter,sans-serif;font-size:13px;color:#1a2236;",
          "background:#ffffff;padding:8px;border-radius:6px;border:1px solid #dde3ed'>",
          "<b style='color:#c0392b'>M", round(magnitude, 1), "</b><br>",
          "<b>", city, "</b><br>",
          "Depth: ", round(depth_km, 1), " km<br>",
          "Source: ", source, "<br>",
          "Date: ", format(date_time, "%b %d, %Y"), "</div>"
        )
      ) %>%
      addLegend(position = "bottomright",
                colors = c("#c0392b", "#e67e22", "#2980b9", "#e63946", "#f4a261"),
                labels = c("Strong (5.0+)", "Moderate (4.0–4.9)", "Minor / Light (<4.0)",
                           "West Valley Fault", "East Valley Fault"),
                title = "Legend", opacity = 0.9)
  })
  
  # ── Source bar ─────────────────────────────────────────
  output$src_bar <- renderPlotly({
    df <- filtered() %>%
      group_by(source) %>%
      summarise(n = n(), .groups = "drop")
    plot_ly(df, x = ~source, y = ~n, type = "bar", color = ~source,
            colors = unname(pal_src[df$source]),
            hovertemplate = "<b>%{x}</b><br>Records: %{y}<extra></extra>") %>%
      plotly_dark() %>%
      layout(showlegend = FALSE,
             xaxis = list(title = "Source"),
             yaxis = list(title = "Record Count"))
  })
  
  # ── Source annual line ─────────────────────────────────
  output$src_year_line <- renderPlotly({
    df <- filtered() %>%
      group_by(year, source) %>%
      summarise(n = n(), .groups = "drop")
    plot_ly(df, x = ~year, y = ~n, color = ~source,
            colors = unname(pal_src),
            type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{x} · %{fullData.name}</b><br>Count: %{y}<extra></extra>") %>%
      plotly_dark() %>%
      layout(legend = list(orientation = "h", y = -0.15),
             yaxis = list(title = "Count"))
  })
  
  # ── Source summary table ───────────────────────────────
  output$src_table <- renderTable({
    filtered() %>%
      group_by(Source = source) %>%
      summarise(
        Records         = formatC(n(), big.mark = ","),
        `Years Covered` = paste(min(year, na.rm = TRUE), "–", max(year, na.rm = TRUE)),
        `Avg Magnitude` = round(mean(magnitude, na.rm = TRUE), 2),
        `Max Magnitude` = round(max(magnitude, na.rm = TRUE), 2),
        `% Shallow`     = paste0(round(mean(depth_km <= 70, na.rm = TRUE) * 100), "%"),
        .groups = "drop"
      )
  }, striped = FALSE, hover = TRUE, bordered = TRUE, align = "c", width = "100%", rownames = FALSE)
  
  # ── Risk map ───────────────────────────────────────────
  output$risk_map <- renderLeaflet({
    risk_zones <- data.frame(
      city = c("Marikina City", "San Mateo, Rizal", "Quezon City",
               "Antipolo City", "Pasig City", "Cainta, Rizal",
               "Taytay, Rizal", "Rodriguez (Montalban)"),
      lat  = c(14.650, 14.720, 14.676, 14.625, 14.573, 14.595, 14.545, 14.745),
      lng  = c(121.100, 121.120, 121.043, 121.175, 121.077, 121.155, 121.175, 121.185),
      risk = c("Very High", "Very High", "High", "High", "High",
               "Moderate-High", "Moderate", "High"),
      info = c(
        "Directly on West Valley Fault. Risk: surface rupture, liquefaction, flooding.",
        "Adjacent to West Valley Fault. Risk: ground shaking, landslides, liquefaction.",
        "1-5 km west of fault. Risk: severe ground shaking, structural damage.",
        "Near East Valley Fault. Risk: ground shaking, landslides on slopes.",
        "2-4 km from fault. Risk: strong shaking, liquefaction near river.",
        "3-5 km from fault. Risk: moderate to strong shaking, flooding.",
        "5-8 km from fault. Risk: moderate shaking, some liquefaction.",
        "Near fault northern end. Risk: ground shaking, landslides, flooding."
      )
    )
    risk_colors <- c("Very High" = "#c0392b", "High" = "#e67e22",
                     "Moderate-High" = "#f4a261", "Moderate" = "#2980b9")
    
    leaflet(risk_zones) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolylines(data = west_valley_fault, lng = ~lng, lat = ~lat,
                   color = "#e63946", weight = 3, opacity = 0.9,
                   label = "West Valley Fault", dashArray = "6,4") %>%
      addPolylines(data = east_valley_fault, lng = ~lng, lat = ~lat,
                   color = "#f4a261", weight = 2.5, opacity = 0.9,
                   label = "East Valley Fault", dashArray = "6,4") %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat, radius = 18,
        color = ~risk_colors[risk], fillColor = ~risk_colors[risk],
        fillOpacity = 0.35, weight = 2, opacity = 0.8,
        popup = ~paste0(
          "<div style='font-family:Inter,sans-serif;font-size:13px;padding:8px;",
          "border-radius:6px;border:1px solid #dde3ed;'>",
          "<b style='color:#c0392b;font-size:14px;'>", city, "</b><br>",
          "<b>Risk Level: </b>", risk, "<br><br>", info, "</div>"
        ),
        label = ~city
      ) %>%
      addLegend(position = "bottomright",
                colors = c("#c0392b", "#e67e22", "#f4a261", "#2980b9", "#e63946", "#f4a261"),
                labels = c("Very High Risk", "High Risk", "Moderate-High Risk",
                           "Moderate Risk", "West Valley Fault", "East Valley Fault"),
                title = "Risk Level", opacity = 0.9)
  })
  
  # ── Population exposure value boxes ───────────────────
  output$vbox_pop_marikina <- renderValueBox({
    valueBox("512,428", "Marikina City Population At Risk", icon = icon("people-group"), color = "red")
  })
  output$vbox_pop_sanmateo <- renderValueBox({
    valueBox("267,661", "San Mateo Population At Risk", icon = icon("people-group"), color = "orange")
  })
  output$vbox_pop_qc <- renderValueBox({
    valueBox("2.96M", "Quezon City Population At Risk", icon = icon("people-group"), color = "blue")
  })
  output$vbox_pop_total <- renderValueBox({
    valueBox("5.6M+", "Total Population in Impact Zone", icon = icon("earth-asia"), color = "teal")
  })
  
  # ── Population exposure chart ──────────────────────────
  output$pop_exposure_chart <- renderPlotly({
    eq_counts <- filtered() %>%
      group_by(city) %>%
      summarise(events = n(), .groups = "drop")
    df <- left_join(pop_data, eq_counts, by = "city") %>%
      mutate(events = replace_na(events, 0),
             exposure_score = round(events / population * 10000, 2))
    plot_ly(df, x = ~reorder(city, -exposure_score), y = ~exposure_score, type = "bar",
            marker = list(color = "#e63946"),
            hovertemplate = "<b>%{x}</b><br>Exposure Score: %{y}<extra></extra>") %>%
      plotly_dark() %>%
      layout(xaxis = list(title = "City"),
             yaxis = list(title = "Events per 10,000 Population"))
  })
  
  # ── Population table ───────────────────────────────────
  output$pop_table <- renderTable({
    eq_counts <- filtered() %>%
      group_by(city) %>%
      summarise(Events = n(), .groups = "drop")
    left_join(pop_data, eq_counts, by = "city") %>%
      mutate(
        Events           = replace_na(Events, 0),
        `Exposure Score` = round(Events / population * 10000, 2)
      ) %>%
      rename(City = city, Population = population) %>%
      mutate(Population = format(Population, big.mark = ",", scientific = FALSE)) %>%
      arrange(desc(`Exposure Score`))
  }, striped = FALSE, hover = TRUE, bordered = TRUE, align = "c", width = "100%", rownames = FALSE)
  
}

# ── Run ─────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)