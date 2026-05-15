
# app_rubric_v29.R
# CCBC Technique Checklist — Google Apps Script backend
# - Recent submissions shows YYYY-MM-DD (no time)
# - No stray trailing commas; compiles cleanly

library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(glue)
library(lubridate)
library(jsonlite)
library(httr)

# ====== Apps Script Web App URL + Key ======
API_URL <- "https://script.google.com/macros/s/AKfycbxDDx3iTpez8jz0AdkzXLzEgSGU2QvudO7cYxJqwDUHuiwFKPC2RHSL5sROOVjsZGyr/exec"
API_KEY <- "cCkbo3tpI9CQLTBTY8bkeWeHt-6oHlCsZ8O7YuQ4fClB64LG1z_nq-oUdyZ4KsAf"

# --------------- Utilities -----------------
safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

cap_first <- function(s) {
  if (is.null(s) || length(s) == 0) return(s)
  s <- trimws(s)
  ifelse(is.na(s) | nchar(s) == 0, s, paste0(toupper(substr(s,1,1)), substring(s,2)))
}

# --------------- API wrapper -----------------
api_get <- function(params = list()) {
  if (!is.null(API_KEY)) params$key <- API_KEY
  url <- API_URL
  if (length(params) > 0) {
    qs <- paste(paste0(URLencode(names(params)), "=", URLencode(as.character(params), reserved = TRUE)), collapse = "&")
    url <- paste0(API_URL, "?", qs)
  }
  resp <- httr::GET(url, timeout(30))
  httr::stop_for_status(resp, task = "GET Apps Script")
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  out <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  out
}

api_post <- function(body) {
  if (!is.null(API_KEY)) body$key <- API_KEY
  resp <- httr::POST(API_URL,
                     body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
                     httr::content_type_json(),
                     timeout(30))
  httr::stop_for_status(resp, task = "POST Apps Script")
  txt <- httr::content(resp, "text", encoding = "UTF-8")
  out <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  out
}

load_all <- function() {
  res <- api_get(list())  # default returns all data
  if (isTRUE(res$ok) && length(res$data) > 0) {
    df <- jsonlite::fromJSON(jsonlite::toJSON(res$data), simplifyVector = TRUE)
    tibble::as_tibble(df)
  } else {
    tibble::tibble(
      Athlete = character(), Sex = character(), Age_Group = character(), Date = character(),
      Submitted_At = character(), Subtechnique = character(), Area = character(), Item = character(),
      Score = integer(), Comment = character()
    )
  }
}

load_athletes <- function() {
  res <- api_get(list(action = "athletes"))
  if (isTRUE(res$ok) && length(res$data) > 0) unlist(res$data) else character()
}

append_rows <- function(df) {
  body <- list(action = "append", rows = df)
  res <- api_post(body)
  isTRUE(res$ok)
}

replace_rows_for_key <- function(key, df) {
  body <- list(action = "replace", key = key, rows = df)
  res <- api_post(body)
  isTRUE(res$ok)
}

# --------------- Choices -----------------
age_groups <- c("U18","U16")
sex_choices <- c("Male", "Female")

# --------------- Rubrics (abbrev for brevity; same as earlier builds) -----------------
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

# U16 overrides (same as previous build)
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

# --------------- UI -----------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      input[type=number]::-webkit-outer-spin-button,
      input[type=number]::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0; }
      input[type=number] { -moz-appearance: textfield; }
      .score-invalid { border-color: #dc3545 !important; box-shadow: 0 0 0 0.2rem rgba(220,53,69,.25); }
      .score-warn { margin-top: 4px; }
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
      prettyRadioButtons("sex_btn", "Sex", choices = sex_choices, inline = FALSE, status = "primary", animation = "jelly"),
      prettyRadioButtons("age_btn", "Age Group", choices = age_groups, inline = FALSE, status = "info", animation = "jelly"),
      dateInput("date", "Date", value = Sys.Date()),
      hr(),
      prettyRadioButtons("subtech", "Subtechnique", choices = subtechniques, inline = FALSE, status = "success", animation = "jelly", selected = subtechniques[1]),
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

# --------------- Server -----------------
server <- function(input, output, session) {

  master_df <- reactiveVal(load_all())

  observe({
    choices <- load_athletes()
    updateSelectizeInput(session, "athlete", choices = choices, server = TRUE)
  })

  current_st_list <- reactive({
    req(input$subtech)
    st <- input$subtech
    if (!is.null(input$age_btn) && input$age_btn == "U16" && !is.null(rubric_u16[[st]])) {
      rubric_u16[[st]]
    } else {
      rubric[[st]]
    }
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

  do_prefill <- function() {
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
  }
  observeEvent(input$prefill_btn, { do_prefill() })

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

  output$tbl_recent <- renderDT({
    df <- master_df()
    if (nrow(df) == 0) {
      return(datatable(data.frame(
        Athlete = character(), Sex = character(), Age_Group = character(), Date = character(), Subtechnique = character()
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
      rename(Date = Date_out) %>%
      select(Athlete, Sex, Age_Group, Date, Subtechnique) %>%
      head(10)
    datatable(recent, options = list(pageLength = 10, dom = 't'))
  })

  output$status <- renderText("")

  validate_scores <- function(df) {
    bad <- df %>%
      mutate(score_num = suppressWarnings(as.numeric(Score)),
             score_int = suppressWarnings(as.integer(Score)),
             is_int = !is.na(score_num) & !is.na(score_int) & (score_num == score_int),
             in_range = !is.na(score_num) & score_num >= 0 & score_num <= 5) %>%
      filter(!(is_int & in_range))
    bad
  }

  pending_save <- reactiveVal(NULL)

  observeEvent(input$submit, {
    errs <- c()
    if (is.null(input$athlete) || !nzchar(trimws(input$athlete))) errs <- c(errs, "Athlete name is required.")
    if (is.null(input$sex_btn) || !nzchar(trimws(input$sex_btn))) errs <- c(errs, "Sex is required.")
    if (is.null(input$age_btn) || !nzchar(trimws(input$age_btn))) errs <- c(errs, "Age Group is required.")

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
            tags$li(glue("Age Group: {key_vals$Age_Group}")),
            tags$li(glue("Date: {key_vals$Date}")),
            tags$li(glue("Subtechnique: {key_vals$Subtechnique}"))
          ),
          p("Do you want to replace the old data with your current scores/comments?")
        )
      ))
      return(invisible(NULL))
    }

    ok <- append_rows(out)
    if (ok) {
      output$status <- renderText("Saved to database")
      master_df(load_all())
      clear_current_subtech()
    } else {
      output$status <- renderText("Error: could not save to database")
    }
  })

  observeEvent(input$confirm_replace, {
    req(!is.null(pending_save()))
    out <- pending_save()
    removeModal()

    key_vals <- list(
      Athlete = unique(out$Athlete)[1],
      Sex = unique(out$Sex)[1],
      Age_Group = unique(out$Age_Group)[1],
      Date = unique(out$Date)[1],
      Subtechnique = unique(out$Subtechnique)[1]
    )

    ok <- replace_rows_for_key(key_vals, out)
    if (ok) {
      output$status <- renderText("Saved to database")
      master_df(load_all())
      clear_current_subtech()
      pending_save(NULL)
    } else {
      output$status <- renderText("Error: could not replace existing entry")
    }
  })
}

shinyApp(ui, server)
