
# app_v34.R — CCBC Technique Checklist (GAS-served athletes; Shinylive-safe)
# - Reads API_URL/API_KEY from app_config.R or environment variables
# - Loads athlete list from Google Apps Script doGet?action=athletes (server holds secrets)
# - Keeps Google Apps Script backend for saving/replace

library(shiny)
library(DT)
library(dplyr)
library(glue)
library(lubridate)
library(jsonlite)
suppressWarnings(suppressPackageStartupMessages(library(shinyWidgets)))

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && nzchar(as.character(a)[1])) a else b

# ===================== CONFIG LOAD =====================
# Either provide app_config.R with API_URL, API_KEY, or set env vars API_URL, API_KEY
cfg_path <- "app_config.R"
if (file.exists(cfg_path)) {
  source(cfg_path, local = TRUE)
}
API_URL <- get0("API_URL", inherits = FALSE) %||% Sys.getenv("API_URL", "")
API_KEY <- get0("API_KEY", inherits = FALSE) %||% Sys.getenv("API_KEY", "")
if (!nzchar(API_URL) || !nzchar(API_KEY)) {
  stop("Missing API_URL or API_KEY. Create app_config.R (git-ignored) or set environment variables.")
}
# =======================================================

safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
cap_first <- function(s) {
  if (is.null(s) || length(s) == 0) return(s)
  s <- trimws(s)
  ifelse(is.na(s) | nchar(s) == 0, s, paste0(toupper(substr(s,1,1)), substring(s,2)))
}

age_groups  <- c("BC Ski (T2W)", "BC Dev (L2C)")   # "Team" labels
sex_choices <- c("Male","Female")

# --------- Rubrics (from your latest version) ----------
rubric <- list(
  "Offset" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are  at appropriate angle to terrain and approximately the same angle",
      "Body weight is stacked on the front half of the foot while driving the tip of ski in before or at a similar time as the rear of the ski",
      "Pole tips apply maximum power approximately next to the foot just after stretch shortening cycle of arms"
    ),
    "Power Line" = c(
      "Knee is driven forward on glide ski to initiate forward momentum from the kick ski",
      "Create a short amount of time between power lines",
      "power line is maintained until force creation is started"
    ),
    "Ski Lift" = c(
      "Skier is choosing to switch skis in purposeful manner and not falling onto other side",
      "The weight is fully over the glide leg and kick leg raises into the air (only one ski on ground at a time)"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 80-100 degrees creating  close to vertical pole plant",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "skier is coming from an almost straight body position and getting the poles up to prepare for pole plant"
    ),
    "Reposition Phase" = c(
      "arms return fast enough to have time to place poles in a vertical angle if needed",
      "a preparation phase before each pole plant allowing muscles to relax and set before pole plant occurs"
    )
  ),
  "One-Skate" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are  at appropriate angle to terrain and approximately the same angle",
      "Body weight is stacked on the front half of the foot while driving the tip of ski in before or at a similar time as the rear of the ski",
      "Pole tips apply maximum power approximately next to the foot just after stretch shortening cycle of arms"
    ),
    "Power Line" = c(
      "Knee is driven forward on glide ski to initiate forward momentum from the kick ski",
      "Create a short amount of time between power lines",
      "power line is maintained until force creation is started"
    ),
    "Ski Lift" = c(
      "Skier is choosing to switch skis in purposeful manner and not falling onto other side",
      "The weight is fully over the glide leg and kick leg raises into the air (only one ski on ground at a time)"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 80-100 degrees creating  close to vertical pole plant",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "skier is coming from an almost straight body position and getting the poles up to prepare for pole plant"
    ),
    "Leg Kick/Push" = c(
      "different force is created throughout movement cycle (greater force during force creation phase)",
      "creation of a power & impulse starts under  COM and leaves the ground in a timely manner for grade and snow condition",
      "ground force from kick leg moves the body forward onto the glide ski. (not up or to the side)"
    ),
    "Reposition Phase" = c(
      "arms return fast enough to have time to place poles in a vertical angle if needed",
      "a preparation phase before each pole plant allowing muscles to relax and set before pole plant occurs"
    )
  ),
  "Diagonal Stride" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are  at appropriate angle to terrain and approximately the same angle",
      "Body weight is stacked on the front half of the foot while driving the tip of ski in before or at a similar time as the rear of the ski",
      "Pole tips apply maximum power approximately next to the foot just after stretch shortening cycle of arms"
    ),
    "Power Line" = c(
      "Knee is driven forward on glide ski to initiate forward momentum from the kick ski",
      "Create a short amount of time between power lines",
      "power line is maintained until force creation is started"
    ),
    "Ski Lift" = c(
      "Skier is choosing to switch skis in purposeful manner and not falling onto other side",
      "The weight is fully over the glide leg and kick leg raises into the air (only one ski on ground at a time)"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 80-100 degrees creating  close to vertical pole plant",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "skier is coming from an almost straight body position and getting the poles up to prepare for pole plant"
    ),
    "Leg Kick/Push" = c(
      "different force is created throughout movement cycle (greater force during force creation phase)",
      "creation of a power & impulse starts under  COM and leaves the ground in a timely manner for grade and snow condition",
      "ground force from kick leg moves the body forward onto the glide ski. (not up or to the side)"
    ),
    "Reposition Phase" = c(
      "arms return fast enough to have time to place poles in a vertical angle if needed",
      "a preparation phase before each pole plant allowing muscles to relax and set before pole plant occurs"
    )
  ),
  "Double Pole" = list(
    "Power Position" = c(
      "high starting position with aggressive body angle of 70-75 degrees",
      "hips flex, knee drive  forward and torso goes down at same time  to load the poles",
      "high speed movement of downward and forward movemnt of upperbody"
    ),
    "Reposition Phase" = c(
      "body is balanced (COM) on mid foot at end of pole plant with upper body being used as a counterweight to hips to maintain position",
      "shin angle is maintained at 70-75 degrees at end of pole plant and the body is drawn forward and extended to high starting position to start cycle again"
    ),
    "Pole Plant" = c(
      "Elbows and shoulders flexed in a strong starting position at 80-100 degrees creating  close to vertical pole plant",
      "torso leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "Pole tips apply maximum power approximately next to the foot just after stretch shortening cycle of arms"
    )
  )
)

rubric_u16 <- list(
  "Offset" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are approximately the same angle",
      "Less joint angle at force generation",
      "Body weight is stacked on the front half of the foot",
      "Pole tips apply maximum power approximately next to the foot",
      "vertical movement of the COM is minimized throughout power position phase"
    ),
    "Power Line" = c(
      "Create a short amount of time between power lines",
      "Fully balanced on each leg",
      "No lateral flexion in the lower or upper body (ie. core stability,glut weakness)"
    ),
    "Pole Plant" = c(
      "Elbows flexed in a strong starting position at 80-100 degrees",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is coming from an almost straight body position and getting the poles up to prepare for pole plant",
      "Establish \"standing tall\" position to create optimal power transfer with COM in front of feet"
    ),
    "Leg Kick/Push" = c(
      "Upper body and leg flexes/decends at similar time (curtsy movement or commonly called preload)",
      "Different force is created throughout movement cycle (greater force during force creation phase)",
      "Create a short power impulse under COM for kick leg as weight is shifted to glide ski",
      "Kick initiation (impulse) finishes as toe passes heal of glide ski"
    )
  ),
  "One-Skate" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are approximately the same angle",
      "Less joint angle at force generation",
      "Body weight is stacked on the front half of the foot",
      "Pole tips apply maximum power approximately next to the foot",
      "vertical movement of the COM is minimized throughout power position phase"
    ),
    "Power Line" = c(
      "Create a short amount of time between power lines",
      "Fully balanced on each leg",
      "No lateral flexion in the lower or upper body (ie. core stability,glut weakness)"
    ),
    "Pole Plant" = c(
      "Elbows flexed in a strong starting position at 80-100 degrees",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is coming from an almost straight body position and getting the poles up to prepare for pole plant",
      "Establish \"standing tall\" position to create optimal power transfer with COM in front of feet"
    ),
    "Leg Kick/Push" = c(
      "Upper body and leg flexes/decends at similar time (curtsy movement or commonly called preload)",
      "Different force is created throughout movement cycle (greater force during force creation phase)",
      "Create a short power impulse under COM for kick leg as weight is shifted to glide ski",
      "Kick initiation (impulse) finishes as toe passes heal of glide ski"
    )
  ),
  "Diagonal Stride" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are approximately the same angle",
      "Less joint angle at force generation",
      "Body weight is stacked on the front half of the foot",
      "Pole tips apply maximum power approximately next to the foot",
      "vertical movement of the COM is minimized throughout power position phase"
    ),
    "Power Line" = c(
      "Create a short amount of time between power lines",
      "Fully balanced on each leg",
      "No lateral flexion in the lower or upper body (ie. core stability,glut weakness)"
    ),
    "Pole Plant" = c(
      "Elbows flexed in a strong starting position at 80-100 degrees",
      "Body leans slightly forward with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is coming from an almost straight body position and getting the poles up to prepare for pole plant",
      "Establish \"standing tall\" position to create optimal power transfer with COM in front of feet"
    ),
    "Leg Kick/Push" = c(
      "Upper body and leg flexes/decends at similar time (curtsy movement or commonly called preload)",
      "Different force is created throughout movement cycle (greater force during force creation phase)",
      "Create a short power impulse under COM for kick leg as weight is shifted to glide ski",
      "Kick initiation (impulse) finishes as toe passes heal of glide ski"
    )
  ),
  "Double Pole" = list(
    "Power Position" = c(
      "Shin, torso and forearm angle are approximately the same angle",
      "Body weight is stacked on the front half of the foot",
      "Maximum load on poles occurs under COM (beside foot)"
    ),
    "Power Line" = c(
      "Knee and torso are driven forward to initiate power position and maintain forward COM",
      "No lateral flexion in the lower or upper body (ie. core stability,glut weakness)"
    ),
    "Pole Plant" = c(
      "Elbows flexed in a strong starting position at 80-100 degrees",
      "Body leans forward with poles ready to be set down vertically (dependant on ground speed)",
      "Skier is coming from an almost straight body position and getting the poles up to prepare for pole plant"
    ),
    "Reposition Phase" = c(
      "Arms return fast enough to have time to place poles in a vertical angle if needed",
      "Arm is returned in a direct line to starting position"
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
    column(6, tags$img(src = "CSIP.jpg", height = "80px", style = "padding:10px;")),
    column(6, div(style = "text-align:right;", tags$img(src = "CCBC.jpg", height = "80px", style = "padding:10px;")))
  ),
  titlePanel("CCBC Technique Checklist"),
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
      h4("Recent submissions (latest 10)"),
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

  master_df <- reactiveVal(
    tibble::tibble(
      Athlete = character(), Sex = character(), Age_Group = character(), Date = character(),
      Submitted_At = character(), Subtechnique = character(), Area = character(), Item = character(),
      Score = integer(), Comment = character()
    )
  )

  # Load all rows (for prefill + recent) and the ATHLETES list from GAS
  observeEvent(TRUE, {
    gas_get(session, list(), "gas_all")
    gas_get(session, list(action = "athletes"), "gas_athletes")
  }, once = TRUE)

  observeEvent(input$gas_all, {
    if (isTRUE(input$gas_all$ok) && isTRUE(input$gas_all$data$ok)) {
      d <- input$gas_all$data$data
      if (length(d) > 0) {
        master_df(tibble::as_tibble(jsonlite::fromJSON(jsonlite::toJSON(d), simplifyVector = TRUE)))
      } else {
        master_df(master_df())
      }
    } else {
      err <- input$gas_all$error
      if (is.null(err)) err <- "unknown"
      showNotification(paste("Load failed:", err), type = "error")
    }
  }, ignoreInit = TRUE)

  output$athlete_hint <- renderUI(NULL)
  observeEvent(input$gas_athletes, {
    if (isTRUE(input$gas_athletes$ok) && isTRUE(input$gas_athletes$data$ok)) {
      choices <- unlist(input$gas_athletes$data$data, use.names = FALSE)
      updateSelectizeInput(session, "athlete", choices = choices, server = TRUE)
      output$athlete_hint <- renderUI(span(style="color:#888;", sprintf("Loaded %d names from server", length(choices))))
    } else {
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
          Age_Group    = input$age_btn,   # stored as chosen Team label
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

    gas_post(session, action = "replace", rows = out, record_key = key_vals, inputId = "gas_replace")
  })

  observeEvent(input$gas_replace, {
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
