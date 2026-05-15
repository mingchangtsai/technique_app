
# app_v36.R — CCBC Technique Checklist
# - Athlete names: prefer GAS doGet?action=athletes_sheet (reads "athletes" tab, Name column)
#                  fall back to doGet?action=athletes (unique names from main sheet)
# - Visible status during loading & saving (status pills at top)
# - Same rubric content as v35

library(shiny)
library(DT)
library(dplyr)
library(glue)
library(lubridate)
library(jsonlite)
suppressWarnings(suppressPackageStartupMessages(library(shinyWidgets)))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nzchar(as.character(a)[1])) a else b

# ===================== CONFIG LOAD =====================
# cfg_path <- "app_config.R"
# if (file.exists(cfg_path)) {
#   source(cfg_path, local = TRUE)
# }
# API_URL <- get0("API_URL", inherits = FALSE) %||% Sys.getenv("API_URL", "")
# API_KEY <- get0("API_KEY", inherits = FALSE) %||% Sys.getenv("API_KEY", "")
# if (!nzchar(API_URL) || !nzchar(API_KEY)) {
#   stop("Missing API_URL or API_KEY. Create app_config.R (git-ignored) or set environment variables.")
# }
# =======================================================

# ---- Config: read from environment (Posit Connect, .Renviron locally) ----
API_URL <- Sys.getenv("CCBC_API_URL", unset = "")
API_KEY <- Sys.getenv("CCBC_API_KEY", unset = "")

if (!nzchar(API_URL) || !nzchar(API_KEY)) {
  stop("Missing CCBC_API_URL or CCBC_API_KEY environment variables.")
}


safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
cap_first <- function(s) {
  if (is.null(s) || length(s) == 0) return(s)
  s <- trimws(s)
  ifelse(is.na(s) | nchar(s) == 0, s, paste0(toupper(substr(s,1,1)), substring(s,2)))
}

age_groups  <- c("BC Ski (T2T)", "BC Dev (L2C)")
sex_choices <- c("Male","Female")

# --------- Rubrics (same as v35) ----------
rubric <- list(
  "Offset" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are at appropriate angle to terrain and approximately the same angle",
      "Body weight is stacked on the front half of the foot while the knee drives the COM forward",
      "Pole are loaded appropriately with body mass as the pole tips pass the foot"
    ),
    "Power Line" = c(
      "Knee is driven forward on glide ski to initiate forward momentum from the kick ski",
      "Create a short amount of time between power lines",
      "Power line is achieved on each leg with knee aligned over ski"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 70-90 degrees and both arms are approximately the same height and",
      "Body maintains a slightly forward position with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is coming from high body position and getting the poles up to prepare for pole plant"
    ),
    "Leg Kick/Push" = c(
      "Different force is created throughout movement cycle (greater force during force creation phase)",
      "Creation of a power & quick impulse starts under COM",
      "Ground force from kick leg moves the body forward and not laterally onto the glide ski"
    ),
    "Reposition" = c(
      "Arms return fast enough to have time for pole force to be applied fully in direction of travel",
      "A preparation phase before each pole plant allowing shoulders to drop and set before pole plant"
    )
  ),
  "One-Skate" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are at appropriate angle to terrain and approximately the same angle",
      "Body weight is stacked on the front half of the foot",
      "Poles are loaded appropriately with body mass as the pole tips pass the foot"
    ),
    "Power Line" = c(
      "Knee is driven forward on glide ski to maintain forward momentum created from the kick ski",
      "The push off the leg is forward and off the forefoot to create forward propulsion",
      "Power line is maintained until skier chooses to switch skis in a purposeful manner"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 80-90 degrees",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is starting from a high body position"
    ),
    "Leg Kick/Push" = c(
      "Creation of a power & impulse starts under COM and leaves the ground in a timely manner for grade and snow condition",
      "Kick initiates forward acceleration by kicking back and off the forefoot while maintaining hip stability"
    ),
    "Reposition" = c(
      "Arms return fast enough to have time to place poles in a vertical angle if needed",
      "Arms return in a relaxed smooth motion before pole plant occurs"
    )
  ),
  "Diagonal Stride" = list(
    "Power Position" = c(
      "Upperbody and knee drive descends at the same time to load skis",
      "Body weight is stacked on the front half of the foot throughout PP",
      "Pole tips apply maximum power approximately next to the foot"
    ),
    "Power Line" = c(
      "No lateral flexion or torsion in the lower or upper body (ie. core stability, glu weakness)",
      "Power line is maintained throughout power transfer through hip, knee, and foot in a linear motion"
    ),
    "Pole Plant" = c(
      "Elbows flexed in a strong starting position at 70-90 degrees",
      "Body leans slightly forward as pole is placed on the ground and loaded with torso flexion",
      "Poles are in a position to apply appropriate angle and force for terrain"
    ),
    "Leg Kick/Push" = c(
      "Kick is initiated by the full body (back, legs, trunk, and arms)",
      "When the kick leaves the ground, the torso and leg are in straight line (180 degrees)",
      "Ground force from kick leg moves the body forward onto the glide ski"
    ),
    "Reposition Phase" = c(
      "Arms return fast enough to have time to place poles in a vertical angle and body to extend",
      "Torso maintains forward lean throughout reposition phase"
    )
  ),
  "Double Pole" = list(
    "Power Position" = c(
      "Pole tips apply maximum power approximately next to the foot and just after the stretch shortening cycle of the arms",
      "Knee and torso flexion initiates the drive forward to load the poles",
      "Poles are adequatly loaded to start the power position"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 80-100 degrees creating close to vertical pole plant",
      "Torso draws slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "High starting position to initiate pole plant with forefoot pressure"
    ),
    "Reposition Phase" = c(
      "Reposition motion is fast recovery motion with arms from hips to ready position. Body moves forward from a 70-75 degrees shin angle",
      "Shin angle is maintained at 70-75 degrees at end of pole plant"
    )
  )
)

rubric_u16 <- list(
  "Offset" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are approximately the same angle",
      "Body weight is stacked on the front half of the foot while the knee drives the COM forward over foot",
      "Poles are loaded maximally with COM rotating forward and across as the pole tips pass the foot"
    ),
    "Power Line" = c(
      "Knee is driven forward over glide ski to initiate forward momentum from the kick ski",
      "The ability to create a short amount of time between power lines with focus on power leg to power arm impulse",
      "Power line is maintained until force creation is started"
    ),
    "Pole Plant" = c(
      "Both elbows and shoulders are flexed in a strong starting position at 70-90 degrees creating similar power application on both poles",
      "Body maintains slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is coming from a high body position and getting the poles up to prepare for pole plant"
    ),
    "Leg Kick/Push" = c(
      "Kicking leg extends in direction of travel to match the forward torso angle in a straight line just after poles are maximally loaded",
      "As kick leg leaves the ground a straight line can be drawn through torso and leg in a 70 degrees angle to the ground",
      "Gound force from kick leg moves the body forward onto the glide ski in a forefoot pressure position with knee flexed in 60-80 degrees"
    ),
    "Reposition" = c(
      "Arms return fast and maintain pole angle to minimize cycle time",
      "A preparation phase before each pole plant allowing shoulders to drop and set before poles are loaded",
      "The cycle is smooth and efficient"
    )
  ),
  "One-Skate" = list(
    "Power Position" = c(
      "Shin and torso angle are approximately 70-75 degrees at peak of power position (torso can be flexed more)",
      "Body weight is balanced on the forefront of the foot during the power initiation off the kick ski",
      "Pole tips apply maximum power approximately next to the foot just after stretch shortening cycle of arms"
    ),
    "Power Line" = c(
      "Knee is driven forward on glide ski to initiate forward momentum from the kick ski while maintaining COM on the mid foot",
      "The ability to create a short amount of time between power lines",
      "Power line is maintained until skier chooses to switch skis in a purposeful manner"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 80-100 degrees creating close to vertical pole plan",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is starting from a high body position, getting the arms/poles quickly for pole plant"
    ),
    "Leg Kick/Push" = c(
      "Kick leg is momentarily left at full extension (straight line from shoulder to foot) before reposition phase",
      "As kick leg leaves the ground a torso and shin angle are maintained at 70 degrees angle to the ground",
      "Ground force from kick leg moves the body forward and up onto the glide ski while maintaining hip stability"
    ),
    "Reposition" = c(
      "Arms return slightly faster than the upper body bringing the poles to an angle that promotes forward momentum",
      "A preparation phase before each pole plant allowing muscles to relax and set before pole plant occurs",
      "High speed torso and arm recovery speed"
    )
  ),
  "Diagonal Stride" = list(
    "Power Position" = c(
      "Elbows and shoulders flexed in a strong starting position 70-90 degrees creating close to vertical pole plant",
      "Body weight is stacked on the forefront of the foot as knee preloads the legs before ground force is created",
      "Body maintains slight forward angle at pole plant and torso flexion is delayed until power position is reached"
    ),
    "Power Line" = c(
      "Knee is driven forward on glide ski to initiate forward momentum from the kick ski",
      "Both power creation and time between power lines can be created in a short amount of time (high impulse)",
      "Power line is maintained until force creation is started"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position 70-90 degrees creating close to vertical pole plant",
      "Body leans slightly forward as poles is placed on the ground and loaded with intentional torso flexion",
      "Skier is coming from an upright body position and the poles are in a position to apply necessary power"
    ),
    "Leg Kick/Push" = c(
      "Different force and limb speed is created throughout movement cycle",
      "Creation of a power & impulse under COM and leaves the ground in a timely manner for grade and snow condition",
      "Ground force from kick leg moves the body forward and up into a balanced position with foot under COM"
    ),
    "Reposition" = c(
      "Arm return slightly faster than the upper body bringing the poles to an angle that promotes forward momentum",
      "A preparation phase before each pole plant allowing muscles to relax and set before pole plant occurs",
      "Display a clear ability to change gear in diagonal stride"
    )
  ),
  "Double Pole" = list(
    "Power Position" = c(
      "Pole tips apply maximum power approximately next to the foot with highest pole power applied in front of the foot at high speed and before the foot at slower spped",
      "Knee and torso flexion initiates the drive forward to load the poles in high speed movement",
      "Poles are adequately loaded after the stretch shortening cycle of arms in direction of travel"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 70-100 degrees, creating close to a vertical pole plant",
      "Torso is drawn forward with abdominal muscles starting the initiation of the pole plant followed by the hip flexors",
      "High starting position with aggressive body angle of 70-75 degrees"
    ),
    "Reposition Phase" = c(
      "Body is balanced (COM) on mid foot at the end of pole push with upper body being used as a counterweight to hips to maintain position on the mid foot",
      "Shin angle is maintain at 70-75 degrees at the end of pole plant where the body is drawn forward and extended to high starting position to start cycle again",
      "High speed torso and arm recovery speed"
    )
  )
)

subtechniques <- names(rubric)

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      input[type=number]::-webkit-outer-spin-button,
      input[type=number]::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }
      input[type=number] { -moz-appearance: textfield; }
      .score-invalid { border-color: #dc3545 !important; box-shadow: 0 0 0 0.2rem rgba(220,53,69,.25); }
      .score-warn { margin-top: 4px; }
      .status-pill { display:inline-block; padding:2px 8px; border-radius:12px; font-size:12px; margin-left:6px;}
      .pill-loading { background:#fff3cd; color:#8a6d3b; border:1px solid #ffe8a1; }
      .pill-ok { background:#d4edda; color:#155724; border:1px solid #c3e6cb; }
      .pill-error { background:#f8d7da; color:#721c24; border:1px solid #f5c6cb; }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('gas_fetch', async (msg) => {
        const { method, url, body, inputId } = msg;
        try {
          const resp = await fetch(url, {
            method,
            headers: { 'Content-Type': 'text/plain;charset=utf-8' },
            body: body ? JSON.stringify(body) : undefined,
            mode: 'cors'
          });
          const text = await resp.text();
          let data;
          try { data = JSON.parse(text); } catch(e){ data = { ok:false, error:'bad_json', raw:text }; }
          Shiny.setInputValue(inputId, { ok:true, data }, { priority: 'event' });
        } catch (err) {
          Shiny.setInputValue(inputId, { ok:false, error: String(err) }, { priority: 'event' });
        }
      });
      document.addEventListener('input', function(e) {
        var el = e.target;
        if (!el || el.tagName !== 'INPUT') return;
        if (el.type !== 'number') return;
        if (!el.id || el.id.indexOf('score_') !== 0) return;
        var val = el.value;
        var grp = el.closest('.form-group') || el.parentElement;
        var warn = grp ? grp.querySelector('.score-warn') : null;
        if (val === '' || val === null) {
          el.classList.remove('score-invalid');
          if (warn) warn.remove();
          return;
        }
        var num = Number(val);
        var intOK = Number.isInteger(num);
        var inRange = num >= 0 && num <= 5;
        if (!intOK || !inRange) {
          el.classList.add('score-invalid');
          if (!warn) {
            warn = document.createElement('div');
            warn.className = 'score-warn text-danger small';
            warn.textContent = 'Score must be an integer 0–5.';
            grp.appendChild(warn);
          }
        } else {
          el.classList.remove('score-invalid');
          if (warn) warn.remove();
        }
      }, true);
    "))
  ),
  fluidRow(
    column(6, tags$img(src = "CSIP.jpg", height = "100px", style = "padding:10px;")),
    column(6, div(style = "text-align:right;", tags$img(src = "CCBC.jpg", height = "100px", style = "padding:10px;")))
  ),
  titlePanel("CCBC Technique Checklist"),
  # fluidRow(
  #   column(12, htmlOutput("global_status"))
  # ),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("athlete", "Athlete name",
        choices = NULL,
        options = list(placeholder = "Type a name or pick from list", create = TRUE)
      ),
      htmlOutput("athlete_hint"),
      shinyWidgets::prettyRadioButtons("sex_btn", "Sex", choices = sex_choices, inline = FALSE, status = "primary", animation = "jelly"),
      shinyWidgets::prettyRadioButtons("age_btn", "Team", choices = age_groups, inline = FALSE, status = "info", animation = "jelly"),
      dateInput("date", "Date", value = Sys.Date()),
      hr(),
      shinyWidgets::prettyRadioButtons("subtech", "Subtechnique", choices = subtechniques, inline = FALSE, status = "success", animation = "jelly", selected = subtechniques[1]),
      br(),
      actionButton("prefill_btn", "Load Previous Scores", class = "btn-outline-secondary", width = "100%"),
      br(), br(),
      actionButton("submit", "Submit & Save", class = "btn-primary", width = "100%"),
      br(), br(),
      verbatimTextOutput("status"),
      uiOutput("prefill_msg")
    ),
    mainPanel(
      uiOutput("ui_subtech"),
      hr(),
      h4("Last 10 Submissions"),
      DTOutput("tbl_recent")
    )
  )
)

# ---- helpers ----
df_to_rowlist <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

make_url <- function(params = list(), include_key = TRUE) {
  if (include_key) params$key <- API_KEY
  if (length(params)) {
    qs <- paste(paste0(URLencode(names(params)), "=", URLencode(as.character(params), reserved = TRUE)),
                collapse = "&")
    paste0(API_URL, "?", qs)
  } else API_URL
}

gas_get <- function(session, params = list(), inputId) {
  session$sendCustomMessage("gas_fetch", list(
    method  = "GET",
    url     = make_url(params, include_key = TRUE),
    body    = NULL,
    inputId = inputId
  ))
}

gas_post <- function(session, action, rows = NULL, record_key = NULL, inputId) {
  body <- list(action = action)
  if (!is.null(rows)) {
    if (is.data.frame(rows)) rows <- df_to_rowlist(rows)
    body$rows <- rows
  }
  if (!is.null(record_key)) body$key <- record_key
  session$sendCustomMessage("gas_fetch", list(
    method  = "POST",
    url     = make_url(list(), include_key = TRUE),
    body    = body,
    inputId = inputId
  ))
}

server <- function(input, output, session) {

  rv <- reactiveValues(loading_all = TRUE, loading_names = TRUE, saving = FALSE, load_error = NULL, names_error = NULL)

  master_df <- reactiveVal(
    tibble::tibble(
      Athlete = character(), Sex = character(), Age_Group = character(), Date = character(),
      Submitted_At = character(), Subtechnique = character(), Area = character(), Item = character(),
      Score = integer(), Comment = character()
    )
  )

  output$global_status <- renderUI({
    pills <- list()
    if (rv$loading_all) pills <- c(pills, span(class="status-pill pill-loading", "Loading data…"))
    if (!rv$loading_all && is.null(rv$load_error)) pills <- c(pills, span(class="status-pill pill-ok", "Data ready"))
    if (!is.null(rv$load_error)) pills <- c(pills, span(class="status-pill pill-error", paste("Load error:", rv$load_error)))
    if (rv$loading_names) pills <- c(pills, span(class="status-pill pill-loading", "Loading athlete names…"))
    if (!rv$loading_names && is.null(rv$names_error)) pills <- c(pills, span(class="status-pill pill-ok", "Names ready"))
    if (!is.null(rv$names_error)) pills <- c(pills, span(class="status-pill pill-error", paste("Names error:", rv$names_error)))
    if (rv$saving) pills <- c(pills, span(class="status-pill pill-loading", "Saving…"))
    do.call(tagList, pills)
  })

  # Load all rows (for prefill + recent)
  observeEvent(TRUE, {
    rv$loading_all <- TRUE
    rv$load_error <- NULL
    gas_get(session, list(), "gas_all")
  }, once = TRUE)

  observeEvent(input$gas_all, {
    rv$loading_all <- FALSE
    if (isTRUE(input$gas_all$ok) && isTRUE(input$gas_all$data$ok)) {
      d <- input$gas_all$data$data
      if (length(d) > 0) {
        master_df(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(d), simplifyVector = TRUE)))
      } else {
        master_df(master_df())
      }
    } else {
      rv$load_error <- input$gas_all$error %||% input$gas_all$data$error %||% "unknown"
    }
  }, ignoreInit = TRUE)

  # --------- Athlete names from Google Sheet 'athletes' (Name column) via GAS ---------
  # First try action=athletes_sheet, else fall back to action=athletes
  observeEvent(TRUE, {
    rv$loading_names <- TRUE
    rv$names_error <- NULL
    gas_get(session, list(action = "athletes_sheet"), "gas_athletes_sheet")
  }, once = TRUE)

  observeEvent(input$gas_athletes_sheet, {
    if (isTRUE(input$gas_athletes_sheet$ok) && isTRUE(input$gas_athletes_sheet$data$ok)) {
      choices <- unlist(input$gas_athletes_sheet$data$data, use.names = FALSE)
      updateSelectizeInput(session, "athlete", choices = choices, server = TRUE)
      output$athlete_hint <- renderUI(span(style="color:#888;", sprintf("Loaded %d names from database", length(choices))))
      rv$loading_names <- FALSE
    } else {
      # Fallback
      gas_get(session, list(action = "athletes"), "gas_athletes_fallback")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$gas_athletes_fallback, {
    rv$loading_names <- FALSE
    if (isTRUE(input$gas_athletes_fallback$ok) && isTRUE(input$gas_athletes_fallback$data$ok)) {
      choices <- unlist(input$gas_athletes_fallback$data$data, use.names = FALSE)
      updateSelectizeInput(session, "athlete", choices = choices, server = TRUE)
      output$athlete_hint <- renderUI(span(style="color:#888;", sprintf("Loaded %d names from database", length(choices))))
    } else {
      rv$names_error <- input$gas_athletes_fallback$error %||% input$gas_athletes_fallback$data$error %||% "unknown"
      output$athlete_hint <- renderUI(span(style="color:#c00;", "Could not load athlete names"))
    }
  }, ignoreInit = TRUE)

  # Map Team -> U16 rubric for "BC Dev"
  current_st_list <- reactive({
    req(input$subtech)
    st <- input$subtech
    is_u16 <- !is.null(input$age_btn) && (input$age_btn %in% c("BC Dev (L2C)"))
    if (is_u16 && !is.null(rubric_u16[[st]])) rubric_u16[[st]] else rubric[[st]]
  })

  output$ui_subtech <- renderUI({
    req(input$subtech, input$age_btn)
    st <- input$subtech
    st_list <- current_st_list()
    if (length(st_list) == 0) return(div(em("No items configured yet for this subtechnique.")))
    tagList(lapply(names(st_list), function(area) {
      items <- st_list[[area]]
      wellPanel(
        h4(strong(paste(st, "—", tools::toTitleCase(area)))),
        do.call(tagList, lapply(seq_along(items), function(i) {
          item <- items[[i]]
          sid <- paste0("score_", safe_id(st), "_", safe_id(area), "_", i)
          cid <- paste0("comment_", safe_id(st), "_", safe_id(area), "_", i)
          fluidRow(
            column(6, div(style="padding-top:6px;", cap_first(item))),
            column(2, numericInput(sid, "Score (0–5)", value = NA, min = 0, max = 5, step = 1, width = "100%")),
            column(4, textInput(cid, "Comment", placeholder = "Short note", width = "100%"))
          )
        }))
      )
    }))
  })

  output$prefill_msg <- renderUI(NULL)

  clear_current_subtech <- function() {
    req(input$subtech)
    st <- input$subtech
    st_list <- current_st_list()
    if (length(st_list) == 0) return(invisible(NULL))
    for (area in names(st_list)) {
      items <- st_list[[area]]
      for (i in seq_along(items)) {
        sid <- paste0("score_", safe_id(st), "_", safe_id(area), "_", i)
        cid <- paste0("comment_", safe_id(st), "_", safe_id(area), "_", i)
        updateNumericInput(session, sid, value = NA_real_)
        updateTextInput(session, cid, value = "")
      }
    }
    output$prefill_msg <- renderUI(NULL)
  }
  observeEvent(input$athlete, { clear_current_subtech() }, ignoreInit = TRUE)
  observeEvent(input$date,    { clear_current_subtech() }, ignoreInit = TRUE)
  observeEvent(input$subtech, { clear_current_subtech() }, ignoreInit = TRUE)
  observeEvent(input$age_btn, { clear_current_subtech() }, ignoreInit = TRUE)

  observeEvent(input$prefill_btn, {
    req(input$athlete, input$sex_btn, input$age_btn, input$subtech)
    df <- master_df()
    st <- input$subtech
    st_list <- current_st_list()
    if (length(st_list) == 0) return(invisible(NULL))

    df_match <- df %>%
      filter(Athlete == input$athlete, Sex == input$sex_btn, Age_Group == input$age_btn, Subtechnique == st) %>%
      mutate(
        Submitted_At_raw = Submitted_At,
        Date_raw = Date,
        Submitted_At = suppressWarnings(lubridate::ymd_hms(Submitted_At_raw)),
        Date = suppressWarnings(lubridate::ymd(Date_raw)),
        stamp = dplyr::coalesce(Date, as.Date(Submitted_At))
      ) %>%
      arrange(desc(stamp), desc(Submitted_At))

    if (nrow(df_match) == 0) {
      output$prefill_msg <- renderUI(div(style="color:#999;", "No previous matching entry to prefill."))
      return(invisible(NULL))
    }

    latest <- df_match %>% group_by(Area, Item) %>% slice(1) %>% ungroup()

    for (area in names(st_list)) {
      items <- st_list[[area]]
      for (i in seq_along(items)) {
        item <- items[[i]]
        sid <- paste0("score_", safe_id(st), "_", safe_id(area), "_", i)
        cid <- paste0("comment_", safe_id(st), "_", safe_id(area), "_", i)
        row <- latest %>% filter(Area == area, Item == item)
        if (nrow(row) > 0) {
          sc <- suppressWarnings(as.numeric(row$Score[1]))
          cm <- row$Comment[1]
          updateNumericInput(session, sid, value = ifelse(is.na(sc), NA_real_, sc))
          updateTextInput(session, cid, value = ifelse(is.na(cm), "", cm))
        } else {
          updateNumericInput(session, sid, value = NA_real_)
          updateTextInput(session, cid, value = "")
        }
      }
    }

    latest_label <- {
      d1 <- suppressWarnings(lubridate::ymd(df_match$Date_raw[1]))
      if (!is.na(d1)) {
        format(d1, "%Y-%m-%d")
      } else {
        ts1 <- suppressWarnings(lubridate::ymd_hms(df_match$Submitted_At_raw[1]))
        if (!is.na(ts1)) format(as.Date(ts1), "%Y-%m-%d") else {
          rawD <- as.character(df_match$Date_raw[1]); rawS <- as.character(df_match$Submitted_At_raw[1])
          if (!is.na(rawD) && nchar(rawD)) rawD else if (!is.na(rawS) && nchar(rawS)) rawS else "previous entry"
        }
      }
    }
    output$prefill_msg <- renderUI(div(style="color:#2b7a0b;", paste("Loaded previous scores from", latest_label)))
  })

  assembled <- reactive({
    req(input$athlete, input$sex_btn, input$age_btn, input$date, input$subtech)
    st <- input$subtech
    st_list <- current_st_list()
    rows <- list()
    for (area in names(st_list)) {
      items <- st_list[[area]]
      for (i in seq_along(items)) {
        item <- items[[i]]
        sid <- paste0("score_", safe_id(st), "_", safe_id(area), "_", i)
        cid <- paste0("comment_", safe_id(st), "_", safe_id(area), "_", i)
        rows[[length(rows)+1]] <- data.frame(
          Athlete      = trimws(input$athlete),
          Sex          = input$sex_btn,
          Age_Group    = input$age_btn,
          Date         = as.character(input$date),
          Submitted_At = as.character(Sys.time()),
          Subtechnique = st,
          Area         = area,
          Item         = item,
          Score        = input[[sid]],
          Comment      = ifelse(is.null(input[[cid]]) || input[[cid]] == "", NA, input[[cid]]),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
    }
    dplyr::bind_rows(rows)
  })

  validate_scores <- function(df) {
    bad <- df %>%
      mutate(score_num = suppressWarnings(as.numeric(Score)),
             score_int = suppressWarnings(as.integer(Score)),
             is_int = !is.na(score_num) & !is.na(score_int) & (score_num == score_int),
             in_range = !is.na(score_num) & score_num >= 0 & score_num <= 5) %>%
      filter(!(is_int & in_range))
    bad
  }

  output$status <- renderText("")

  pending_save <- reactiveVal(NULL)

  observeEvent(input$submit, {
    errs <- c()
    if (is.null(input$athlete) || !nzchar(trimws(input$athlete))) errs <- c(errs, "Athlete name is required.")
    if (is.null(input$sex_btn) || !nzchar(trimws(input$sex_btn))) errs <- c(errs, "Sex is required.")
    if (is.null(input$age_btn) || !nzchar(trimws(input$age_btn))) errs <- c(errs, "Team is required.")

    out <- assembled()

    if (any(is.na(out$Score))) {
      missing <- dplyr::filter(out, is.na(Score))
      errs <- c(errs, glue("Missing score(s) for {nrow(missing)} item(s). Please complete all scores."))
    } else {
      bad <- validate_scores(out)
      if (nrow(bad) > 0) {
        preview <- paste0(utils::head(glue("- {bad$Area}: '{bad$Item}' (got '{bad$Score}')"), 6), collapse = "\n")
        errs <- c(errs, "Scores must be whole numbers 0–5. Please fix:\n", preview)
      }
    }

    if (length(errs) > 0) {
      output$status <- renderText(paste(errs, collapse = "\n"))
      return(NULL)
    }

    key_vals <- list(
      Athlete = unique(out$Athlete)[1],
      Sex = unique(out$Sex)[1],
      Age_Group = unique(out$Age_Group)[1],
      Date = unique(out$Date)[1],
      Subtechnique = unique(out$Subtechnique)[1]
    )

    df <- master_df()
    dup_exists <- if (nrow(df) > 0) {
      nrow(dplyr::filter(df,
        Athlete == key_vals$Athlete,
        Sex == key_vals$Sex,
        Age_Group == key_vals$Age_Group,
        Date == key_vals$Date,
        Subtechnique == key_vals$Subtechnique
      )) > 0
    } else FALSE

    if (dup_exists) {
      pending_save(out)
      showModal(modalDialog(
        title = "Duplicate found",
        size = "m",
        easyClose = FALSE,
        footer = tagList(
          actionButton("confirm_replace", "Replace old data", class = "btn-danger"),
          modalButton("Cancel")
        ),
        div(
          p("An entry already exists for:"),
          tags$ul(
            tags$li(glue("Athlete: {key_vals$Athlete}")),
            tags$li(glue("Sex: {key_vals$Sex}")),
            tags$li(glue("Team: {key_vals$Age_Group}")),
            tags$li(glue("Date: {key_vals$Date}")),
            tags$li(glue("Subtechnique: {key_vals$Subtechnique}"))
          ),
          p("Do you want to replace the old data with your current scores/comments?")
        )
      ))
      return(invisible(NULL))
    }

    rv$saving <- TRUE
    output$status <- renderText("Saving…")
    gas_post(session, action = "append", rows = out, inputId = "gas_save")
  })

  render_save_error <- function(obj) {
    if (is.null(obj)) return("Error: could not save to database")
    if (isTRUE(obj$ok) && isTRUE(obj$data$ok)) return("Saved to database")
    parts <- c("Error: could not save to database")
    if (!is.null(obj$data$error)) parts <- c(parts, paste("API error:", as.character(obj$data$error)))
    if (!is.null(obj$error))      parts <- c(parts, paste("Bridge error:", as.character(obj$error)))
    if (!is.null(obj$data$code))  parts <- c(parts, paste("Code:", as.character(obj$data$code)))
    if (!is.null(obj$data$raw))   parts <- c(parts, paste("Raw:", substr(as.character(obj$data$raw), 1, 300)))
    paste(parts, collapse = " | ")
  }

  observeEvent(input$gas_save, {
    rv$saving <- FALSE
    ok <- isTRUE(input$gas_save$ok) && isTRUE(input$gas_save$data$ok)
    if (ok) {
      output$status <- renderText("Saved to database")
      gas_get(session, list(), "gas_all")
      if (!is.null(input$subtech)) {
        st <- input$subtech
        st_list <- current_st_list()
        if (length(st_list) > 0) {
          for (area in names(st_list)) {
            items <- st_list[[area]]
            for (i in seq_along(items)) {
              sid <- paste0("score_", safe_id(st), "_", safe_id(area), "_", i)
              cid <- paste0("comment_", safe_id(st), "_", safe_id(area), "_", i)
              updateNumericInput(session, sid, value = NA_real_)
              updateTextInput(session, cid, value = "")
            }
          }
        }
      }
      output$prefill_msg <- renderUI(NULL)
    } else {
      output$status <- renderText(render_save_error(input$gas_save))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_replace, {
    req(!is.null(pending_save()))
    out <- pending_save(); removeModal()

    key_vals <- list(
      Athlete = unique(out$Athlete)[1],
      Sex = unique(out$Sex)[1],
      Age_Group = unique(out$Age_Group)[1],
      Date = unique(out$Date)[1],
      Subtechnique = unique(out$Subtechnique)[1]
    )

    rv$saving <- TRUE
    output$status <- renderText("Saving…")
    gas_post(session, action = "replace", rows = out, record_key = key_vals, inputId = "gas_replace")
  })

  observeEvent(input$gas_replace, {
    rv$saving <- FALSE
    ok <- isTRUE(input$gas_replace$ok) && isTRUE(input$gas_replace$data$ok)
    if (ok) {
      output$status <- renderText("Saved to database")
      gas_get(session, list(), "gas_all")
      pending_save(NULL)
      output$prefill_msg <- renderUI(NULL)
    } else {
      output$status <- renderText(render_save_error(input$gas_replace))
    }
  }, ignoreInit = TRUE)

  output$tbl_recent <- renderDT({
    df <- master_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(
        Athlete = character(), Sex = character(), Team = character(), Date = character(), Subtechnique = character()
      ), options = list(dom = 't', pageLength = 10)))
    }
    recent <- df %>%
      mutate(
        Date_chr = as.character(Date),
        Date_iso = ifelse(!is.na(Date_chr) & grepl("T", Date_chr, fixed = TRUE), substr(Date_chr, 1, 10), Date_chr),
        Date_disp = suppressWarnings(lubridate::ymd(Date_iso)),
        Date_out  = ifelse(!is.na(Date_disp), format(Date_disp, "%Y-%m-%d"), Date_iso),
        ts = suppressWarnings(lubridate::ymd_hms(Submitted_At, quiet = TRUE)),
        key_ts = dplyr::coalesce(ts, as.POSIXct(Date_disp), as.POSIXct("1900-01-01", tz = "UTC"))
      ) %>%
      group_by(Athlete, Sex, Age_Group, Date_out, Subtechnique) %>%
      summarise(latest_key = max(key_ts, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(latest_key)) %>%
      rename(Date = Date_out, Team = Age_Group) %>%
      select(Athlete, Sex, Team, Date, Subtechnique) %>%
      head(10)
    datatable(recent, options = list(pageLength = 10, dom = 't'))
  })
}

shinyApp(ui, server)
