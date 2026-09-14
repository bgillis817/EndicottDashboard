# ============================================================================
#  ENDICOTT PITCHING DASHBOARD  -  Fall 2026
#  Rapsodo bullpen data  -  dark theme matching the NECBL dashboard
# ============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(MASS)
library(purrr)
library(openxlsx)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Data source ──────────────────────────────────────────────────────────────
DATA_URL <- "https://raw.githubusercontent.com/bgillis817/EndicottDashboard/refs/heads/main/ECFallRapsodo.csv"
LOGO_URL <- "https://raw.githubusercontent.com/bgillis817/EndicottDashboard/refs/heads/main/logo.jpg"

pitch_pal <- c(
  "4-Seam"="#ff4655", "2-Seam"="#ff8c42", Sinker="#ff8c42",
  Cutter="#ffd700", Slider="#00d4ff", Curveball="#a78bfa",
  Sweeper="#34d399", ChangeUp="#6ee7b7", Splitter="#f472b6", Other="#94a3b8"
)
heat_fills <- c("#141720","#1a3a5c","#1565c0","#ff4655","#ffeb3b")

# ── Load / clean ─────────────────────────────────────────────────────────────
# Rapsodo writes "-" for missing values. VB (spin)/HB (spin) are blank on the
# new unit, so IVB/HB come from the trajectory columns.
load_data <- function() {
  raw <- readr::read_csv(DATA_URL, na=c("","NA","-"), show_col_types=FALSE)
  raw <- raw[, !grepl("^\\.\\.\\.|^Unnamed", names(raw))]
  names(raw) <- gsub("\\s+","_", names(raw))
  names(raw) <- gsub("[()]","", names(raw))

  raw %>%
    rename_with(~case_when(
      grepl("^name$", ., ignore.case=TRUE)          ~ "Pitcher",
      grepl("^release_height", ., ignore.case=TRUE)  ~ "ReleaseHeight",
      grepl("^release_side", ., ignore.case=TRUE)    ~ "ReleaseSide",
      grepl("^release_ext", ., ignore.case=TRUE)     ~ "ReleaseExtension",
      grepl("^release_angle", ., ignore.case=TRUE)   ~ "ReleaseAngle",
      grepl("^horizontal_angle", ., ignore.case=TRUE)~ "HorizontalAngle",
      grepl("^horizontal_approach", ., ignore.case=TRUE) ~ "HAA",
      grepl("^vertical_approach", ., ignore.case=TRUE)   ~ "VAA",
      grepl("^total_spin", ., ignore.case=TRUE)      ~ "TotalSpin",
      grepl("^true_spin", ., ignore.case=TRUE)       ~ "TrueSpin",
      grepl("^spin_efficiency", ., ignore.case=TRUE) ~ "SpinEff",
      grepl("^spin_direction", ., ignore.case=TRUE)  ~ "SpinDir",
      grepl("^vb_traj", ., ignore.case=TRUE)         ~ "IVB",
      grepl("^hb_traj", ., ignore.case=TRUE)         ~ "HB",
      grepl("^vb_spin", ., ignore.case=TRUE)         ~ "VB_spin",
      grepl("^hb_spin", ., ignore.case=TRUE)         ~ "HB_spin",
      grepl("^strike_zone_height", ., ignore.case=TRUE) ~ "StrikeZoneHeight",
      grepl("^strike_zone_side", ., ignore.case=TRUE)   ~ "StrikeZoneSide",
      grepl("^pitch_type", ., ignore.case=TRUE)      ~ "PitchType",
      grepl("^is_strike", ., ignore.case=TRUE)       ~ "IsStrike",
      grepl("^gyro", ., ignore.case=TRUE)            ~ "Gyro",
      grepl("^session_name", ., ignore.case=TRUE)    ~ "SessionName",
      TRUE ~ .
    )) %>%
    mutate(across(c(ReleaseHeight, ReleaseSide, ReleaseExtension, ReleaseAngle,
                    HorizontalAngle, HAA, VAA, Velocity, TotalSpin, TrueSpin,
                    SpinEff, IVB, HB, VB_spin, HB_spin, StrikeZoneHeight,
                    StrikeZoneSide, Gyro, No),
                  ~suppressWarnings(as.numeric(.)))) %>%
    mutate(
      # fall back to spin-based movement if the unit ever writes it again
      IVB = ifelse(is.na(IVB), VB_spin, IVB),
      HB  = ifelse(is.na(HB),  HB_spin, HB),
      Date = as.Date(Date, format="%m/%d/%Y"),
      PlateLocSide   = StrikeZoneSide / 12,
      PlateLocHeight = StrikeZoneHeight / 12,
      IsStrike = toupper(trimws(IsStrike)) == "Y",
      ZoneCheck = dplyr::between(PlateLocHeight,1.59,3.41) &
                  dplyr::between(PlateLocSide,-1,1),
      PitchType = case_when(
        PitchType == "Fastball"        ~ "4-Seam",
        PitchType == "TwoSeamFastball" ~ "2-Seam",
        PitchType %in% c("Curve","CurveBall") ~ "Curveball",
        TRUE ~ PitchType
      )
    ) %>%
    filter(!is.na(Pitcher), !is.na(PitchType)) %>%
    arrange(Pitcher, Date, No) %>%
    group_by(Pitcher) %>%
    mutate(OverallPitchCount = row_number()) %>%
    ungroup()
}

# Back-to-back pairs within a session
make_seqs <- function(df) {
  df %>%
    arrange(Pitcher, Date, No) %>%
    group_by(Pitcher, Date) %>%
    mutate(prev_type = lag(PitchType),
           prev_strike = lag(IsStrike),
           prev_loc_side = lag(PlateLocSide),
           prev_loc_height = lag(PlateLocHeight)) %>%
    ungroup() %>%
    filter(!is.na(prev_type))
}

make_pairs <- function(seqs) {
  seqs %>%
    group_by(Pitcher, prev_type, PitchType) %>%
    summarise(
      n_pairs = n(),
      first_strike  = mean(prev_strike, na.rm=TRUE),
      second_strike = mean(IsStrike, na.rm=TRUE),
      both_strike   = mean(prev_strike & IsStrike, na.rm=TRUE),
      .groups="drop"
    ) %>%
    mutate(avg_strike = (first_strike + second_strike) / 2,
           strike_diff = second_strike - first_strike)
}

.cache <- new.env(parent=emptyenv())
load_and_cache <- function() {
  message("Loading Rapsodo data...")
  d <- load_data()
  .cache$data <- d
  .cache$last_loaded <- Sys.time()
  message("Loaded ", nrow(d), " pitches, ", n_distinct(d$Pitcher), " pitchers")
}
load_and_cache()

# ── Helpers ──────────────────────────────────────────────────────────────────
stat_card <- function(value, label) {
  tags$div(class="stat-card", tags$h2(value), tags$p(label))
}

pct <- function(x) paste0(round(x*100,1),"%")

dt_opts <- list(
  dom="t", pageLength=25, scrollX=TRUE,
  initComplete=JS("function(s,j){$(this.api().table().header()).css({'background':'#1e2235','color':'#b0b8d4'});}")
)

theme_navs <- function(base_size=12) {
  theme_minimal(base_size=base_size) %+replace% theme(
    plot.background  = element_rect(fill="#0f1117", color=NA),
    panel.background = element_rect(fill="#141720", color=NA),
    panel.grid.major = element_line(color="#2a2d3a", linewidth=.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color="#8892b0", size=9),
    axis.title       = element_text(color="#b0b8d4", size=10, face="bold"),
    plot.title       = element_text(color="#ffffff", size=14, face="bold", hjust=.5),
    plot.subtitle    = element_text(color="#8892b0", size=10, hjust=.5),
    legend.background= element_rect(fill="#1a1e2e", color=NA),
    legend.key       = element_rect(fill="#1a1e2e", color=NA),
    legend.text      = element_text(color="#b0b8d4", size=9),
    legend.title     = element_text(color="#e8eaf0", size=10, face="bold"),
    strip.background = element_rect(fill="#1e2235", color=NA),
    strip.text       = element_text(color="#e8eaf0", size=9, face="bold")
  )
}

pal_for <- function(types) {
  p <- pitch_pal[as.character(types)]
  p[is.na(p)] <- "#94a3b8"
  names(p) <- types
  p
}

loc_heatmap <- function(d, title_str, subtitle_str="") {
  if (is.null(d) || nrow(d) < 5) {
    return(ggplot() +
      annotate("text",x=0,y=2.5,label="Insufficient data",color="#8892b0",size=5) +
      theme_navs() +
      theme(axis.text=element_blank(),axis.title=element_blank(),panel.grid=element_blank()))
  }
  ggplot(d, aes(x=PlateLocSide, y=PlateLocHeight)) +
    stat_density_2d(aes(fill=after_stat(density)), geom="raster", contour=FALSE) +
    scale_fill_gradientn(colours=heat_fills, guide="none") +
    annotate("rect", xmin=-1,xmax=1,ymin=1.6,ymax=3.4, fill=NA, color="#ffffff", linewidth=.7) +
    geom_vline(xintercept=0, linewidth=.3, color="#2a2d3a", linetype="dashed") +
    geom_hline(yintercept=2.5, linewidth=.3, color="#2a2d3a", linetype="dashed") +
    ylim(1,4) + xlim(-2,2) +
    labs(title=title_str, subtitle=subtitle_str,
         x="Horizontal (Pitcher's View)", y="Vertical") +
    theme_navs()
}

render_table_png <- function(tbl, title_str="") {
  tbl[] <- lapply(tbl, as.character)
  n_cols <- ncol(tbl); n_rows <- nrow(tbl)
  tbl_long <- data.frame(
    x=rep(seq_len(n_cols), each=n_rows+1),
    y=rep(c(n_rows+1, seq(n_rows,1)), n_cols),
    label=c(rbind(names(tbl), do.call(cbind, lapply(tbl, as.character)))),
    is_header=rep(c(TRUE, rep(FALSE,n_rows)), n_cols),
    stringsAsFactors=FALSE
  )
  p <- ggplot(tbl_long, aes(x=x,y=y,label=label)) +
    geom_tile(aes(fill=is_header), color="grey70", linewidth=0.3) +
    geom_text(aes(fontface=ifelse(is_header,"bold","plain")), size=3.4, color="#222222") +
    scale_fill_manual(values=c("FALSE"="white","TRUE"="#e8e8e8"), guide="none") +
    theme_void(base_size=10) +
    theme(plot.margin=margin(14,14,14,14), plot.background=element_rect(fill="white",color=NA))
  if (nchar(title_str) > 0)
    p <- p + labs(title=title_str) +
      theme(plot.title=element_text(face="bold",size=12,hjust=0,margin=margin(b=8)))
  p
}
save_table_png <- function(file, tbl, title_str="") {
  w <- min(max(ncol(tbl)*1.3, 4), 20); h <- min(max((nrow(tbl)+2)*0.35, 2), 24)
  ggsave(file, render_table_png(tbl, title_str), width=w, height=h, dpi=200, bg="white", limitsize=FALSE)
}
save_plot_png <- function(file, p, width=10, height=7) {
  ggsave(file, p, width=width, height=height, dpi=200, bg="#0f1117", limitsize=FALSE)
}
save_table_xlsx <- function(file, tbl, sheet_name="Data") {
  wb <- openxlsx::createWorkbook()
  sheet_name <- substr(gsub("[^A-Za-z0-9 _-]","",sheet_name), 1, 31)
  if (nchar(sheet_name)==0) sheet_name <- "Data"
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, tbl)
  hdr <- openxlsx::createStyle(textDecoration="bold", fgFill="#1e2235", fontColour="#FFFFFF",
                               halign="center", border="TopBottomLeftRight")
  openxlsx::addStyle(wb, sheet_name, hdr, rows=1, cols=seq_len(ncol(tbl)), gridExpand=TRUE)
  openxlsx::setColWidths(wb, sheet_name, cols=seq_len(ncol(tbl)), widths="auto")
  openxlsx::freezePane(wb, sheet_name, firstRow=TRUE)
  openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
}
png_dl_btn <- function(id) {
  tags$div(style="text-align:right;margin-bottom:4px;",
    downloadLink(id, label=tagList(icon("download")," PNG"),
                 style="color:#8892b0;font-size:11px;text-decoration:none;"))
}
table_dl_btns <- function(id) {
  tags$div(style="text-align:right;margin-bottom:4px;",
    downloadLink(paste0(id,"_png"), label=tagList(icon("image")," PNG"),
                 style="color:#8892b0;font-size:11px;text-decoration:none;margin-right:14px;"),
    downloadLink(paste0(id,"_xlsx"), label=tagList(icon("file-excel")," XLSX"),
                 style="color:#8892b0;font-size:11px;text-decoration:none;"))
}

# ── Dark CSS ─────────────────────────────────────────────────────────────────
dark_css <- "
body{background:#0f1117!important;color:#e8eaf0!important;
     font-family:'Segoe UI',-apple-system,BlinkMacSystemFont,sans-serif;}
.navbar{background:#141720!important;border-bottom:1px solid #2a2d3a!important;}
.navbar-brand{color:#fff!important;font-weight:700;font-size:18px;display:flex;align-items:center;gap:10px;}
.navbar-brand img{height:34px;width:auto;}
.navbar-nav>li>a{color:#b0b8d4!important;font-weight:500;padding:14px 18px!important;}
.navbar-nav>li.active>a,.navbar-nav>li>a:hover{color:#ff4655!important;
  border-bottom:2px solid #ff4655!important;}
.well,.panel,.sidebar-panel,.main-panel{background:#141720!important;border:none!important;}
.form-control,select.form-control{background:#1e2235!important;color:#e8eaf0!important;
  border:1px solid #2e3350!important;border-radius:6px!important;}
.form-control:focus{border-color:#ff4655!important;box-shadow:0 0 0 2px rgba(255,70,85,.25)!important;}
label{color:#b0b8d4!important;font-size:12px;font-weight:600;letter-spacing:.5px;text-transform:uppercase;}
.btn-default{background:#1e2235!important;color:#e8eaf0!important;
  border:1px solid #2e3350!important;border-radius:6px!important;}
.btn-default:hover{background:#ff4655!important;border-color:#ff4655!important;color:#fff!important;}
.nav-tabs{border-bottom:1px solid #2a2d3a!important;}
.nav-tabs>li>a{background:#1a1e2e!important;color:#8892b0!important;
  border-color:#2a2d3a!important;border-radius:6px 6px 0 0!important;}
.nav-tabs>li.active>a,.nav-tabs>li>a:hover{background:#ff4655!important;
  color:#fff!important;border-color:#ff4655!important;}
.tab-content{background:#141720!important;border:1px solid #2a2d3a!important;
  border-top:none;padding:20px;border-radius:0 0 8px 8px;}
.dataTables_wrapper{color:#e8eaf0!important;}
table.dataTable thead th{background:#1e2235!important;color:#b0b8d4!important;
  border-bottom:1px solid #2a2d3a!important;}
table.dataTable tbody tr{background:#141720!important;color:#e8eaf0!important;}
table.dataTable tbody tr:nth-child(even){background:#1a1e2e!important;}
table.dataTable tbody tr:hover{background:#1e2235!important;}
.stat-card{background:#1a1e2e;border:1px solid #2a2d3a;border-radius:10px;
  padding:18px 20px;margin-bottom:14px;}
.stat-card h2{color:#ff4655;margin:0 0 4px;font-size:28px;font-weight:700;}
.stat-card p{color:#8892b0;margin:0;font-size:12px;text-transform:uppercase;letter-spacing:.5px;}
.section-header{color:#fff;font-size:18px;font-weight:700;
  border-left:3px solid #ff4655;padding-left:12px;margin:20px 0 4px;}
.section-sub{color:#8892b0;font-size:12px;margin:0 0 16px 15px;}
.filter-bar{background:#1a1e2e;border:1px solid #2a2d3a;border-radius:8px;
  padding:14px 18px;margin-bottom:18px;}
.checkbox label,.radio label{color:#b0b8d4!important;text-transform:none;font-weight:400;}
input[type=checkbox],input[type=radio]{accent-color:#ff4655;}
hr{border-color:#2a2d3a!important;}
h4,h5{color:#e8eaf0!important;}
.col-sm-3{background:#0f1117!important;}
.refresh-bar{background:#141720;border-top:1px solid #2a2d3a;padding:6px 20px;
  display:flex;align-items:center;gap:16px;}
.refresh-bar .btn{background:#1e2235;color:#e8eaf0;border:1px solid #2e3350;
  border-radius:6px;padding:4px 14px;font-size:12px;font-weight:600;}
.refresh-bar .btn:hover{background:#ff4655;border-color:#ff4655;color:#fff;}
.last-updated{color:#6b7280;font-size:12px;}
::-webkit-scrollbar{width:6px;height:6px;}
::-webkit-scrollbar-track{background:#0f1117;}
::-webkit-scrollbar-thumb{background:#2a2d3a;border-radius:3px;}
"

# ============================================================================
#  UI
# ============================================================================
ui <- navbarPage(
  title = tags$span(tags$img(src=LOGO_URL, alt="Endicott"), "Endicott Pitching"),
  id="mainNav", collapsible=TRUE,
  header = tags$head(tags$style(HTML(dark_css))),
  footer = tags$div(class="refresh-bar",
    actionButton("refresh_data","Refresh Data", icon=icon("rotate"), class="btn"),
    tags$span(class="last-updated", textOutput("last_updated_txt", inline=TRUE))
  ),

  tabPanel("Pitchers",
    sidebarLayout(
      sidebarPanel(width=3,
        tags$div(class="section-header","Fall 2026"),
        tags$hr(),
        uiOutput("p_playerSelect"),
        uiOutput("p_dateUI"),
        uiOutput("p_pitchTypeUI"),
        uiOutput("p_rangeUI"),
        tags$hr(),
        uiOutput("p_pitcherInfo")
      ),
      mainPanel(width=9,
        tabsetPanel(
          tabPanel("Summary",
            br(),
            fluidRow(
              column(4, uiOutput("p_card_n")),
              column(4, uiOutput("p_card_strike")),
              column(4, uiOutput("p_card_velo"))
            ),
            br(),
            tags$div(class="section-header","Pitch Arsenal"),
            tagList(table_dl_btns("p_arsenal"), dataTableOutput("p_arsenal")),
            br(),
            tags$div(class="section-header","Pitch Movement"),
            tags$div(class="section-sub","Shaded areas show how consistently the pitcher replicates pitch shape. Hover for details."),
            tagList(png_dl_btn("p_movement_png"), plotlyOutput("p_movement", height="500px")),
            br(),
            tags$div(class="section-header","Release Points"),
            tagList(png_dl_btn("p_release_png"), plotlyOutput("p_release", height="420px"))
          ),
          tabPanel("Heat Maps",
            br(),
            tags$div(class="section-header","Pitch Location Heat Maps"),
            tags$div(class="filter-bar",
              fluidRow(column(4, uiOutput("p_hm_pitch_ui")))
            ),
            tagList(png_dl_btn("p_heatmap_png"), plotOutput("p_heatmap", height="480px"))
          ),
          tabPanel("Pitch Locations",
            br(),
            tags$div(class="section-header","Individual Pitch Locations"),
            tags$div(class="section-sub","Every pitch plotted as a dot. Useful for small samples the heat map smooths over."),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("p_pl_pitch_ui")),
                column(4, radioButtons("p_pl_color","Color By",
                                       choices=c("Pitch Type"="pitch","Strike / Ball"="strike"),
                                       selected="pitch", inline=TRUE))
              )
            ),
            tagList(png_dl_btn("p_pitchloc_png"), plotOutput("p_pitchloc", height="480px"))
          ),
          tabPanel("Pitch Sequencing",
            br(),
            tags$div(class="section-header","Back-to-Back Pitch Matrix"),
            tags$div(class="section-sub","Consecutive pitches within a bullpen session. Respects the outing and pitch-number filters."),
            tags$div(class="filter-bar",
              fluidRow(column(4,
                selectInput("p_seq_metric","Matrix Metric",
                  choices=c("Usage%"="usage","Avg Strike%"="avg_strike",
                            "2nd Pitch Strike%"="second_strike","Both Strikes%"="both_strike"))))
            ),
            fluidRow(
              column(7, tagList(png_dl_btn("p_seqMatrix_png"),
                                plotOutput("p_seqMatrix", height="520px", click="p_mat_click"))),
              column(5,
                tags$div(class="section-header", style="font-size:14px;","Pair Locations"),
                fluidRow(
                  column(6,
                    tags$p(textOutput("p_seq_lbl1"),
                           style="color:#b0b8d4;font-size:11px;font-weight:600;text-align:center;"),
                    plotOutput("p_seq_loc1", height="230px")),
                  column(6,
                    tags$p(textOutput("p_seq_lbl2"),
                           style="color:#b0b8d4;font-size:11px;font-weight:600;text-align:center;"),
                    plotOutput("p_seq_loc2", height="230px"))
                ),
                br(),
                uiOutput("p_seq_stats")
              )
            ),
            br(),
            tags$div(class="section-header","Sequencing Usage"),
            tags$div(class="section-sub","Row = first pitch; each cell = % of the time it is followed by the column pitch."),
            tagList(table_dl_btns("p_seq_usage"), dataTableOutput("p_seq_usage"))
          ),
          tabPanel("Velocity & Spin",
            br(),
            tags$div(class="filter-bar",
              fluidRow(column(6,
                radioButtons("p_vs_order","Order",
                             choices=c("Over Time"="time","High \u2192 Low"="desc"),
                             selected="time", inline=TRUE)))
            ),
            tags$div(class="section-header","Velocity"),
            tagList(png_dl_btn("p_velo_png"), plotlyOutput("p_velo", height="360px")),
            br(),
            tags$div(class="section-header","Spin Rate"),
            tagList(png_dl_btn("p_spin_png"), plotlyOutput("p_spin", height="360px")),
            br(),
            tags$div(class="section-header","By Outing"),
            tags$div(class="section-sub","Session-by-session averages for the selected pitch type(s)."),
            tagList(table_dl_btns("p_outing"), dataTableOutput("p_outing"))
          )
        )
      )
    )
  ),

  tabPanel("Whole Staff",
    br(),
    tags$div(class="section-header","Staff Overview"),
    tags$div(class="filter-bar",
      fluidRow(column(4, uiOutput("s_pitch_ui")))
    ),
    tagList(table_dl_btns("s_table"), dataTableOutput("s_table"))
  )
)

# ============================================================================
#  SERVER
# ============================================================================
server <- function(input, output, session) {

  refresh_trigger <- reactiveVal(0)
  observeEvent(input$refresh_data, {
    showNotification("Refreshing data from GitHub...", type="message", duration=NULL, id="refreshing")
    load_and_cache()
    removeNotification("refreshing")
    showNotification("Data refreshed.", type="message", duration=3)
    refresh_trigger(refresh_trigger() + 1)
  })
  output$last_updated_txt <- renderText({
    refresh_trigger()
    if (!is.null(.cache$last_loaded))
      paste("Last updated:", format(.cache$last_loaded, "%m/%d/%Y %I:%M %p"))
    else "Not yet loaded"
  })

  all_data <- reactive({ refresh_trigger(); .cache$data })

  # ── Sidebar ────────────────────────────────────────────────────────────────
  output$p_playerSelect <- renderUI({
    d <- all_data(); req(d)
    players <- sort(unique(d$Pitcher))
    prev <- isolate(input$p_pitcher)
    sel <- if (!is.null(prev) && prev %in% players) prev else players[1]
    selectInput("p_pitcher","Select Pitcher", choices=players, selected=sel, selectize=TRUE)
  })

  p_pitcher_data <- reactive({
    req(input$p_pitcher)
    all_data() %>% filter(Pitcher == input$p_pitcher)
  })

  output$p_dateUI <- renderUI({
    d <- p_pitcher_data(); req(nrow(d) > 0)
    dates <- sort(unique(d$Date))
    counts <- d %>% count(Date)
    labels <- paste0(format(dates,"%m/%d/%Y"), " (", counts$n[match(dates, counts$Date)], ")")
    choices <- setNames(as.character(dates), labels)
    tagList(
      checkboxGroupInput("p_dates","Select Outing(s)", choices=choices, selected=as.character(dates)),
      fluidRow(
        column(6, actionButton("p_selAll","All", class="btn-default btn-sm")),
        column(6, actionButton("p_selNone","Clear", class="btn-default btn-sm"))
      )
    )
  })
  observeEvent(input$p_selAll, {
    d <- p_pitcher_data()
    updateCheckboxGroupInput(session,"p_dates", selected=as.character(sort(unique(d$Date))))
  })
  observeEvent(input$p_selNone, {
    updateCheckboxGroupInput(session,"p_dates", selected=character(0))
  })

  output$p_pitchTypeUI <- renderUI({
    d <- p_pitcher_data(); req(nrow(d) > 0)
    choices <- c("All", sort(unique(d$PitchType)))
    prev <- isolate(input$p_pitchType)
    sel <- if (!is.null(prev) && prev %in% choices) prev else "All"
    selectInput("p_pitchType","Pitch Type", choices=choices, selected=sel)
  })

  output$p_rangeUI <- renderUI({
    d <- p_pitcher_data(); req(nrow(d) > 0)
    mx <- max(d$OverallPitchCount, na.rm=TRUE)
    tagList(
      tags$hr(),
      fluidRow(
        column(6, numericInput("p_min","Min Pitch #", min=1, max=mx, value=1)),
        column(6, numericInput("p_max","Max Pitch #", min=1, max=mx, value=mx))
      )
    )
  })

  # Filtered by outing + pitch number only (used for tables that group by type)
  p_base <- reactive({
    d <- p_pitcher_data(); req(nrow(d) > 0)
    if (is.null(input$p_dates) || length(input$p_dates) == 0) return(NULL)
    d <- d %>% filter(Date %in% as.Date(input$p_dates))
    if (!is.null(input$p_min) && !is.null(input$p_max))
      d <- d %>% filter(OverallPitchCount >= input$p_min, OverallPitchCount <= input$p_max)
    d
  })

  # Full filter incl. pitch type
  p_filt <- reactive({
    d <- p_base(); if (is.null(d)) return(NULL)
    if (!is.null(input$p_pitchType) && input$p_pitchType != "All")
      d <- d %>% filter(PitchType == input$p_pitchType)
    d
  })

  output$p_pitcherInfo <- renderUI({
    d <- p_filt(); if (is.null(d) || nrow(d) == 0) return(NULL)
    tags$div(class="stat-card",
      tags$p(style="color:#8892b0;font-size:11px;","FALL 2026"),
      tags$h2(style="font-size:20px;", nrow(d), " pitches"),
      tags$p(n_distinct(d$Date), " outings")
    )
  })

  # ── Summary cards ──────────────────────────────────────────────────────────
  output$p_card_n <- renderUI({ d <- p_filt(); req(d); stat_card(nrow(d),"Pitches") })
  output$p_card_strike <- renderUI({ d <- p_filt(); req(d); stat_card(pct(mean(d$IsStrike,na.rm=TRUE)),"Strike%") })
  output$p_card_velo <- renderUI({ d <- p_filt(); req(d); stat_card(round(mean(d$Velocity,na.rm=TRUE),1),"Avg Velo") })

  # ── Arsenal ────────────────────────────────────────────────────────────────
  p_arsenal_df <- reactive({
    d <- p_filt(); req(d, nrow(d) > 0)
    d %>% group_by(Pitch=PitchType) %>%
      summarise(
        Pitches=n(),
        `Avg Velo`=round(mean(Velocity,na.rm=TRUE),1),
        `Max Velo`=round(max(Velocity,na.rm=TRUE),1),
        Spin=round(mean(TotalSpin,na.rm=TRUE),0),
        `Spin Eff`=pct(mean(SpinEff,na.rm=TRUE)/100),
        IVB=round(mean(IVB,na.rm=TRUE),1),
        HB=round(mean(HB,na.rm=TRUE),1),
        RelZ=round(mean(ReleaseHeight,na.rm=TRUE),2),
        RelX=round(mean(ReleaseSide,na.rm=TRUE),2),
        `Rel Angle`=round(mean(ReleaseAngle,na.rm=TRUE),1),
        HAA=round(mean(HAA,na.rm=TRUE),1),
        VAA=round(mean(VAA,na.rm=TRUE),1),
        `Strike%`=pct(mean(IsStrike,na.rm=TRUE)),
        .groups="drop"
      ) %>%
      mutate(Usage=scales::percent(Pitches/sum(Pitches), accuracy=0.1)) %>%
      dplyr::select(Pitch,Pitches,Usage,`Avg Velo`,`Max Velo`,Spin,`Spin Eff`,IVB,HB,
                    RelZ,RelX,`Rel Angle`,HAA,VAA,`Strike%`) %>%
      mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .))) %>%
      arrange(desc(Pitches))
  })
  output$p_arsenal <- renderDataTable({ datatable(p_arsenal_df(), options=dt_opts, rownames=FALSE) })
  output$p_arsenal_png <- downloadHandler(filename=function() "Arsenal.png",
    content=function(file) save_table_png(file, p_arsenal_df(), paste(input$p_pitcher,"- Arsenal")))
  output$p_arsenal_xlsx <- downloadHandler(filename=function() "Arsenal.xlsx",
    content=function(file) save_table_xlsx(file, p_arsenal_df(), "Arsenal"))

  # ── Movement (KDE shells + plotly) ─────────────────────────────────────────
  kde_shells <- function(d) {
    types <- unique(d$PitchType)
    n_shells <- 8
    prob_levels  <- seq(0.95, 0.10, length.out=n_shells)
    alpha_levels <- seq(0.05, 0.28, length.out=n_shells)
    purrr::map_dfr(types, function(pt) {
      sub <- d %>% filter(PitchType==pt, !is.na(HB), !is.na(IVB))
      if (nrow(sub) < 5) return(NULL)
      kde <- MASS::kde2d(sub$HB, sub$IVB, n=100, lims=c(-30,30,-30,30))
      z <- as.vector(kde$z); tot <- sum(z); sz <- sort(z, decreasing=TRUE); cz <- cumsum(sz)/tot
      purrr::map_dfr(seq_along(prob_levels), function(i) {
        ti <- which(cz >= (1-prob_levels[i]))[1]
        thr <- if (!is.na(ti)) sz[ti] else 0
        cl <- grDevices::contourLines(kde$x, kde$y, kde$z, levels=thr)
        if (length(cl)==0) return(NULL)
        purrr::map_dfr(seq_along(cl), function(j)
          data.frame(x=cl[[j]]$x, y=cl[[j]]$y, PitchType=pt, shell=i,
                     alpha_val=alpha_levels[i], group_id=paste(pt,i,j,sep="_")))
      })
    })
  }

  p_movement_gg <- reactive({
    d <- p_filt(); req(d, nrow(d) > 0)
    d <- d %>% filter(!is.na(HB), !is.na(IVB))
    req(nrow(d) > 0)
    types <- sort(unique(d$PitchType)); pal <- pal_for(types)
    d <- d %>% mutate(hover=paste0("<b>",PitchType,"</b><br>",
                                   "IVB: ",round(IVB,1)," in<br>HB: ",round(HB,1)," in<br>",
                                   "Velo: ",round(Velocity,1)," mph<br>Spin: ",round(TotalSpin,0)," rpm<br>",
                                   "Date: ",format(Date,"%m/%d")," #",No))
    shells <- kde_shells(d)
    p <- ggplot() +
      geom_hline(yintercept=0, color="#2a2d3a", linewidth=.8) +
      geom_vline(xintercept=0, color="#2a2d3a", linewidth=.8)
    if (!is.null(shells) && nrow(shells) > 0) {
      for (i in sort(unique(shells$shell))) for (pt in unique(shells$PitchType[shells$shell==i])) {
        sd <- shells %>% filter(shell==i, PitchType==pt)
        p <- p + geom_polygon(data=sd, aes(x=x,y=y,group=group_id),
                              fill=pal[pt], alpha=unique(sd$alpha_val), color=NA)
      }
    }
    p + geom_point(data=d, aes(x=HB,y=IVB,color=PitchType,text=hover), size=2.5, alpha=.75) +
      scale_color_manual(values=pal, name="Pitch Type") +
      xlim(-30,30) + ylim(-30,30) +
      labs(title=paste(input$p_pitcher,"- Movement"),
           x="Horizontal Break (in)", y="Induced Vertical Break (in)") +
      theme_navs()
  })
  output$p_movement <- renderPlotly({
    ggplotly(p_movement_gg(), tooltip="text") %>%
      layout(paper_bgcolor="#0f1117", plot_bgcolor="#141720",
             hoverlabel=list(bgcolor="#1a1e2e", font=list(color="#e8eaf0"), bordercolor="#ff4655")) %>%
      config(displaylogo=FALSE)
  })
  output$p_movement_png <- downloadHandler(filename=function() "Movement.png",
    content=function(file) save_plot_png(file, p_movement_gg(), width=8, height=7))

  # ── Release ────────────────────────────────────────────────────────────────
  p_release_gg <- reactive({
    d <- p_filt(); req(d, nrow(d) > 0)
    types <- sort(unique(d$PitchType)); pal <- pal_for(types)
    d <- d %>% mutate(hover=paste0("<b>",PitchType,"</b><br>",
                                   "Rel Side: ",round(ReleaseSide,2)," ft<br>",
                                   "Rel Height: ",round(ReleaseHeight,2)," ft<br>",
                                   "Rel Angle: ",round(ReleaseAngle,1),"\u00b0<br>",
                                   "Horiz Angle: ",round(HorizontalAngle,1),"\u00b0"))
    ggplot(d, aes(x=ReleaseSide, y=ReleaseHeight, color=PitchType, text=hover)) +
      geom_vline(xintercept=0, color="#2a2d3a", linewidth=.5) +
      geom_point(size=3, alpha=.7) +
      scale_color_manual(values=pal, name="Pitch Type") +
      xlim(-4,4) + ylim(2,8) +
      labs(title=paste(input$p_pitcher,"- Release Points"),
           x="Horizontal Release (ft)", y="Vertical Release (ft)") +
      theme_navs()
  })
  output$p_release <- renderPlotly({
    ggplotly(p_release_gg(), tooltip="text") %>%
      layout(paper_bgcolor="#0f1117", plot_bgcolor="#141720",
             hoverlabel=list(bgcolor="#1a1e2e", font=list(color="#e8eaf0"), bordercolor="#ff4655")) %>%
      config(displaylogo=FALSE)
  })
  output$p_release_png <- downloadHandler(filename=function() "ReleasePoints.png",
    content=function(file) save_plot_png(file, p_release_gg(), width=8, height=6.5))

  # ── Heat maps ──────────────────────────────────────────────────────────────
  output$p_hm_pitch_ui <- renderUI({
    d <- p_base(); req(d)
    selectInput("p_hm_pitch","Pitch Type", choices=c("All Pitches", sort(unique(d$PitchType))))
  })
  p_heatmap_plot <- reactive({
    d <- p_base(); req(d, nrow(d) > 0)
    d <- d %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    if (is.null(input$p_hm_pitch) || input$p_hm_pitch=="All Pitches") {
      if (nrow(d) < 5) return(ggplot()+annotate("text",x=0,y=0,label="Not enough data",color="#8892b0",size=6)+theme_navs())
      ggplot(d, aes(x=PlateLocSide, y=PlateLocHeight)) +
        stat_density_2d(aes(fill=after_stat(density)), geom="raster", contour=FALSE) +
        scale_fill_gradientn(colours=heat_fills, guide="none") +
        annotate("rect", xmin=-1,xmax=1,ymin=1.6,ymax=3.4, fill=NA, color="#ffffff", linewidth=.7) +
        ylim(1,4) + xlim(-1.8,1.8) +
        facet_wrap(~PitchType, ncol=3) +
        labs(title=paste(input$p_pitcher,"- Heat Maps"), subtitle="Pitcher's perspective",
             x="Horizontal", y="Vertical") +
        theme_navs()
    } else {
      loc_heatmap(d %>% filter(PitchType==input$p_hm_pitch),
                  paste(input$p_pitcher,"-",input$p_hm_pitch,"Heat Map"), "Pitcher's perspective")
    }
  })
  output$p_heatmap <- renderPlot({ p_heatmap_plot() }, bg="#0f1117")
  output$p_heatmap_png <- downloadHandler(filename=function() "HeatMap.png",
    content=function(file) save_plot_png(file, p_heatmap_plot(), width=10, height=8))

  # ── Pitch locations ────────────────────────────────────────────────────────
  output$p_pl_pitch_ui <- renderUI({
    d <- p_base(); req(d)
    selectInput("p_pl_pitch","Pitch Type", choices=c("All Pitches", sort(unique(d$PitchType))))
  })
  p_pitchloc_plot <- reactive({
    d <- p_base(); req(d, nrow(d) > 0)
    if (!is.null(input$p_pl_pitch) && input$p_pl_pitch!="All Pitches")
      d <- d %>% filter(PitchType==input$p_pl_pitch)
    d <- d %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    if (nrow(d)==0) return(ggplot()+annotate("text",x=0,y=0,label="No pitches",color="#8892b0",size=6)+theme_navs())
    base <- ggplot(d, aes(x=PlateLocSide, y=PlateLocHeight)) +
      annotate("rect", xmin=-1,xmax=1,ymin=1.6,ymax=3.4, fill=NA, color="#ffffff", linewidth=.7) +
      geom_vline(xintercept=0, linewidth=.3, color="#2a2d3a", linetype="dashed") +
      geom_hline(yintercept=2.5, linewidth=.3, color="#2a2d3a", linetype="dashed") +
      ylim(1,4) + xlim(-1.8,1.8) +
      labs(x="Horizontal (Pitcher's View)", y="Vertical", subtitle=paste0("n = ", nrow(d))) +
      theme_navs()
    if ((input$p_pl_color %||% "pitch")=="strike") {
      d <- d %>% mutate(Result=ifelse(IsStrike,"Strike","Ball"))
      p <- base + geom_point(data=d, aes(color=Result), size=2.6, alpha=.85) +
        scale_color_manual(values=c(Strike="#ff4655", Ball="#64748b"), name="Result")
    } else {
      p <- base + geom_point(aes(color=PitchType), size=2.6, alpha=.85) +
        scale_color_manual(values=pal_for(sort(unique(d$PitchType))), name="Pitch Type")
    }
    if (is.null(input$p_pl_pitch) || input$p_pl_pitch=="All Pitches")
      p + facet_wrap(~PitchType, ncol=3) + labs(title=paste(input$p_pitcher,"- Pitch Locations"))
    else
      p + labs(title=paste(input$p_pitcher,"-",input$p_pl_pitch,"Locations"))
  })
  output$p_pitchloc <- renderPlot({ p_pitchloc_plot() }, bg="#0f1117")
  output$p_pitchloc_png <- downloadHandler(filename=function() "PitchLocations.png",
    content=function(file) save_plot_png(file, p_pitchloc_plot(), width=10, height=8))

  # ── Sequencing ─────────────────────────────────────────────────────────────
  seq_clicked <- reactiveValues(first=NULL, second=NULL)

  p_seqs <- reactive({ d <- p_base(); req(d, nrow(d) > 1); make_seqs(d) })
  p_pairs <- reactive({ make_pairs(p_seqs()) })

  p_seqMatrix_plot <- reactive({
    d <- p_pairs(); req(d, nrow(d) > 0)
    metric <- input$p_seq_metric %||% "avg_strike"
    all_types <- sort(unique(c(d$prev_type, d$PitchType)))
    mat <- expand.grid(prev_type=all_types, PitchType=all_types, stringsAsFactors=FALSE) %>%
      left_join(d, by=c("prev_type","PitchType")) %>%
      mutate(n_pairs=ifelse(is.na(n_pairs),0L,n_pairs))
    if (metric=="usage") {
      mat <- mat %>% group_by(prev_type) %>%
        mutate(row_tot=sum(n_pairs), val=ifelse(row_tot>0, n_pairs/row_tot, NA_real_)) %>% ungroup()
    } else {
      mat <- mat %>% mutate(val=.data[[metric]])
    }
    mat <- mat %>% mutate(label=ifelse(n_pairs>0, paste0(round(val*100,0),"%\n(n=",n_pairs,")"), ""))
    lbl <- c(usage="Usage%", avg_strike="Avg Strike%", second_strike="2nd Strike%",
             both_strike="Both Strikes%")[metric]
    mid <- if (metric=="usage") 0.33 else if (metric=="both_strike") 0.35 else 0.55
    ggplot(mat, aes(x=prev_type, y=PitchType, fill=val)) +
      geom_tile(color="#0f1117", linewidth=1.2) +
      geom_text(aes(label=label), size=3.5, fontface="bold", color="#ffffff", lineheight=.85) +
      scale_fill_gradient2(low="#1565c0", mid="#1a1e2e", high="#ff4655", midpoint=mid,
                           na.value="#1a1e2e", limits=c(0,1),
                           labels=scales::percent_format(), name=lbl) +
      labs(x="First Pitch (Previous)", y="Second Pitch (Current)",
           title=paste(input$p_pitcher,"- Sequencing")) +
      theme_navs() +
      theme(panel.grid=element_blank(),
            axis.text.x=element_text(angle=30,hjust=1,size=10,face="bold"),
            axis.text.y=element_text(size=10,face="bold")) +
      coord_fixed()
  })
  output$p_seqMatrix <- renderPlot({ p_seqMatrix_plot() }, bg="#0f1117")
  output$p_seqMatrix_png <- downloadHandler(filename=function() "SequencingMatrix.png",
    content=function(file) save_plot_png(file, p_seqMatrix_plot(), width=9, height=9))

  observeEvent(input$p_mat_click, {
    d <- p_pairs(); req(d)
    all_types <- sort(unique(c(d$prev_type, d$PitchType)))
    xi <- round(input$p_mat_click$x); yi <- round(input$p_mat_click$y)
    if (xi>=1 && xi<=length(all_types) && yi>=1 && yi<=length(all_types)) {
      seq_clicked$first <- all_types[xi]; seq_clicked$second <- all_types[yi]
    }
  })
  output$p_seq_lbl1 <- renderText({ if (!is.null(seq_clicked$first)) paste("1st:", seq_clicked$first) else "1st pitch (click)" })
  output$p_seq_lbl2 <- renderText({ if (!is.null(seq_clicked$second)) paste("2nd:", seq_clicked$second) else "2nd pitch (click)" })

  output$p_seq_loc1 <- renderPlot({
    req(seq_clicked$first)
    loc <- p_seqs() %>% filter(prev_type==seq_clicked$first) %>%
      transmute(PlateLocSide=prev_loc_side, PlateLocHeight=prev_loc_height) %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    loc_heatmap(loc, paste0(seq_clicked$first,"\n(n=",nrow(loc),")"))
  }, bg="#0f1117")
  output$p_seq_loc2 <- renderPlot({
    req(seq_clicked$first, seq_clicked$second)
    loc <- p_seqs() %>% filter(prev_type==seq_clicked$first, PitchType==seq_clicked$second) %>%
      dplyr::select(PlateLocSide, PlateLocHeight) %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    loc_heatmap(loc, paste0(seq_clicked$second,"\n(n=",nrow(loc),")"))
  }, bg="#0f1117")

  output$p_seq_stats <- renderUI({
    req(seq_clicked$first, seq_clicked$second)
    pair <- p_pairs() %>% filter(prev_type==seq_clicked$first, PitchType==seq_clicked$second)
    if (nrow(pair)==0) return(tags$div(class="stat-card", tags$p(style="color:#8892b0;","No data.")))
    row <- function(l, v) tags$p(style="color:#8892b0;margin:4px 0;", l, strong(v))
    tags$div(class="stat-card",
      tags$p(style="color:#ff4655;font-weight:700;font-size:13px;",
             seq_clicked$first," \u2192 ",seq_clicked$second),
      row("", paste(pair$n_pairs, "sequences")),
      row("1st Strike%: ", pct(pair$first_strike)),
      row("2nd Strike%: ", pct(pair$second_strike)),
      row("Both Strikes: ", pct(pair$both_strike)),
      row("Change: ", paste0(ifelse(pair$strike_diff>=0,"+",""), round(pair$strike_diff*100,1)," pp"))
    )
  })

  p_seq_usage_df <- reactive({
    d <- p_seqs(); req(d, nrow(d) > 0)
    d %>% group_by(`First Pitch`=prev_type) %>% mutate(N=n()) %>%
      group_by(`First Pitch`, N, Next=PitchType) %>%
      summarise(np=n(), .groups="drop") %>%
      mutate(Usage=paste0(round(np/N*100,1),"%")) %>%
      dplyr::select(`First Pitch`, N, Next, Usage) %>%
      tidyr::pivot_wider(names_from=Next, values_from=Usage, values_fill="0%") %>%
      rename(`Total (1st)`=N) %>% arrange(desc(`Total (1st)`))
  })
  output$p_seq_usage <- renderDataTable({ datatable(p_seq_usage_df(), options=dt_opts, rownames=FALSE) })
  output$p_seq_usage_png <- downloadHandler(filename=function() "SequencingUsage.png",
    content=function(file) save_table_png(file, p_seq_usage_df(), "Sequencing Usage"))
  output$p_seq_usage_xlsx <- downloadHandler(filename=function() "SequencingUsage.xlsx",
    content=function(file) save_table_xlsx(file, p_seq_usage_df(), "Sequencing Usage"))

  # ── Velocity / Spin ────────────────────────────────────────────────────────
  trend_prep <- function(d, y_col, order_by) {
    d$Yval <- d[[y_col]]
    d <- d %>% filter(!is.na(Yval))
    if (identical(order_by,"desc")) {
      d <- d %>% arrange(PitchType, dplyr::desc(Yval)) %>% group_by(PitchType) %>%
        mutate(idx=row_number()-1) %>% ungroup()
      xlab <- "Rank (High \u2192 Low)"
    } else {
      d <- d %>% arrange(Date, No) %>% group_by(PitchType) %>%
        mutate(idx=row_number()-1) %>% ungroup()
      xlab <- "Pitch Count"
    }
    list(d=d, xlab=xlab)
  }
  trend_gg <- function(d, y_col, y_lab, title_str, order_by="time") {
    tp <- trend_prep(d, y_col, order_by); pd <- tp$d
    smry <- pd %>% group_by(PitchType) %>%
      summarise(avg=mean(Yval), mn=min(Yval), mx=max(Yval), max_idx=max(idx), last_v=last(Yval), .groups="drop") %>%
      mutate(lbl=paste0(round(avg,1), if (y_col=="Velocity") " mph\n(" else " rpm\n(", round(mn,0),"-",round(mx,0),")"))
    ggplot(pd, aes(x=idx, y=Yval, color=PitchType)) +
      geom_line(linewidth=1.1, alpha=.85) + geom_point(size=1.5, alpha=.5) +
      geom_text(data=smry, aes(x=max_idx, y=last_v, label=lbl, color=PitchType),
                hjust=-0.1, vjust=.5, size=2.8, lineheight=.85) +
      scale_color_manual(values=pal_for(sort(unique(pd$PitchType))), name="Pitch Type") +
      scale_x_continuous(expand=expansion(mult=c(.02,.18))) +
      labs(title=title_str, x=tp$xlab, y=y_lab) +
      theme_navs()
  }
  trend_plotly <- function(d, y_col, y_lab, title_str, order_by="time") {
    tp <- trend_prep(d, y_col, order_by); pd <- tp$d
    if (nrow(pd)==0) return(plotly::plotly_empty(type="scatter", mode="markers"))
    types <- sort(unique(pd$PitchType)); pal <- pal_for(types)
    plotly::plot_ly(pd, x=~idx, y=~Yval, color=~PitchType, colors=pal,
                    type="scatter", mode="lines+markers",
                    marker=list(size=7, opacity=.8), line=list(width=1),
                    text=~paste0(PitchType,"<br>Velo: ",round(Velocity,1)," mph<br>Spin: ",round(TotalSpin,0),
                                 " rpm<br>IVB: ",round(IVB,1)," in<br>HB: ",round(HB,1)," in<br>Date: ",
                                 format(Date,"%m/%d")," #",No),
                    hoverinfo="text") %>%
      plotly::layout(title=list(text=title_str, font=list(color="#ffffff")),
                     xaxis=list(title=tp$xlab, gridcolor="#2a2d3a", color="#b0b8d4", zerolinecolor="#2a2d3a"),
                     yaxis=list(title=y_lab, gridcolor="#2a2d3a", color="#b0b8d4", zerolinecolor="#2a2d3a"),
                     paper_bgcolor="#0f1117", plot_bgcolor="#141720",
                     legend=list(font=list(color="#b0b8d4"))) %>%
      plotly::config(displaylogo=FALSE)
  }
  output$p_velo <- renderPlotly({
    d <- p_filt(); req(d, nrow(d) > 0)
    trend_plotly(d, "Velocity", "Velocity (MPH)", paste(input$p_pitcher,"- Velocity"), input$p_vs_order %||% "time")
  })
  output$p_spin <- renderPlotly({
    d <- p_filt(); req(d, nrow(d) > 0)
    trend_plotly(d, "TotalSpin", "Spin Rate (RPM)", paste(input$p_pitcher,"- Spin Rate"), input$p_vs_order %||% "time")
  })
  output$p_velo_png <- downloadHandler(filename=function() "Velocity.png",
    content=function(file) { d <- p_filt(); save_plot_png(file,
      trend_gg(d,"Velocity","Velocity (MPH)",paste(input$p_pitcher,"- Velocity"),input$p_vs_order %||% "time"), 10, 6) })
  output$p_spin_png <- downloadHandler(filename=function() "SpinRate.png",
    content=function(file) { d <- p_filt(); save_plot_png(file,
      trend_gg(d,"TotalSpin","Spin Rate (RPM)",paste(input$p_pitcher,"- Spin Rate"),input$p_vs_order %||% "time"), 10, 6) })

  p_outing_df <- reactive({
    d <- p_filt(); req(d, nrow(d) > 0)
    d %>% group_by(Outing=format(Date,"%m/%d/%Y"), Pitch=PitchType) %>%
      summarise(Pitches=n(),
                `Avg Velo`=round(mean(Velocity,na.rm=TRUE),1),
                `Max Velo`=round(max(Velocity,na.rm=TRUE),1),
                Spin=round(mean(TotalSpin,na.rm=TRUE),0),
                IVB=round(mean(IVB,na.rm=TRUE),1),
                HB=round(mean(HB,na.rm=TRUE),1),
                `Strike%`=pct(mean(IsStrike,na.rm=TRUE)),
                .groups="drop") %>%
      arrange(Outing, desc(Pitches))
  })
  output$p_outing <- renderDataTable({ datatable(p_outing_df(), options=dt_opts, rownames=FALSE) })
  output$p_outing_png <- downloadHandler(filename=function() "ByOuting.png",
    content=function(file) save_table_png(file, p_outing_df(), paste(input$p_pitcher,"- By Outing")))
  output$p_outing_xlsx <- downloadHandler(filename=function() "ByOuting.xlsx",
    content=function(file) save_table_xlsx(file, p_outing_df(), "By Outing"))

  # ── Staff tab ──────────────────────────────────────────────────────────────
  output$s_pitch_ui <- renderUI({
    d <- all_data(); req(d)
    selectInput("s_pitch","Pitch Type", choices=c("All Pitches", sort(unique(d$PitchType))))
  })
  s_table_df <- reactive({
    d <- all_data(); req(d)
    if (!is.null(input$s_pitch) && input$s_pitch!="All Pitches") d <- d %>% filter(PitchType==input$s_pitch)
    d %>% group_by(Pitcher) %>%
      summarise(Outings=n_distinct(Date), Pitches=n(),
                `Avg Velo`=round(mean(Velocity,na.rm=TRUE),1),
                `Max Velo`=round(max(Velocity,na.rm=TRUE),1),
                Spin=round(mean(TotalSpin,na.rm=TRUE),0),
                IVB=round(mean(IVB,na.rm=TRUE),1),
                HB=round(mean(HB,na.rm=TRUE),1),
                RelZ=round(mean(ReleaseHeight,na.rm=TRUE),2),
                `Strike%`=round(mean(IsStrike,na.rm=TRUE)*100,1),
                .groups="drop") %>%
      arrange(desc(Pitches))
  })
  output$s_table <- renderDataTable({
    datatable(s_table_df(), options=c(dt_opts, list(dom="ft", pageLength=50)), rownames=FALSE)
  })
  output$s_table_png <- downloadHandler(filename=function() "StaffOverview.png",
    content=function(file) save_table_png(file, s_table_df(), "Staff Overview"))
  output$s_table_xlsx <- downloadHandler(filename=function() "StaffOverview.xlsx",
    content=function(file) save_table_xlsx(file, s_table_df(), "Staff Overview"))
}

shinyApp(ui=ui, server=server)



                                         
