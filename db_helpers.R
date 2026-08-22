# db_helpers.R ----------------------------------------------------------------
# Database access for the technique checklist app. Self-contained (no source()
# of ../database) so the app folder deploys as-is to Posit Connect Cloud.
#
# Objects used:
#   csi.mt_roster             view  - athlete names (setup_monthly_testing.R)
#   csi.technique_checklist   table - one row per rubric item scored
#                                     (created by database/setup_technique.R)
#
# Password: CCBC_DB_PWD env var (set this on the deployment server), falling
# back to the local keyring entry "ccbc_db". Never hardcoded.

library(DBI)
library(RPostgres)

TC_DB <- list(
  host   = "ccbclabs.duckdns.org",
  port   = 5432L,
  dbname = "bcskiteam",
  user   = "csi"
)

tc_password <- function() {
  pwd <- Sys.getenv("CCBC_DB_PWD", unset = "")
  if (!nzchar(pwd)) {
    pwd <- tryCatch(keyring::key_get("ccbc_db", username = TC_DB$user),
                    error = function(e) "")
  }
  if (!nzchar(pwd)) {
    stop("No database password. Set the CCBC_DB_PWD environment variable ",
         "(deployment) or store it with keyring::key_set('ccbc_db', ",
         "username = 'csi') (local).", call. = FALSE)
  }
  pwd
}

tc_connect <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = TC_DB$host, port = TC_DB$port, dbname = TC_DB$dbname,
    user = TC_DB$user, password = tc_password(),
    sslmode = "require",
    connect_timeout = 10,   # fail fast if a firewall drops the connection
    options = "-c search_path=csi,public"
  )
}

# Run a query/write with a connection that always gets closed. Each submit is
# its own short-lived connection, so the app never holds a stale one.
tc_with_con <- function(fn) {
  con <- tc_connect()
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)
  fn(con)
}

# Athlete names for the dropdown (production roster).
tc_load_athletes <- function(con) {
  DBI::dbGetQuery(con, "select name from csi.mt_roster order by name")$name
}

# All checklist rows, aliased to the column names the app uses internally
# (kept identical to the retired Google Sheet headers so the prefill and
# recent-submissions logic is unchanged).
tc_load_all <- function(con) {
  DBI::dbGetQuery(con, "
    select athlete                          as \"Athlete\",
           sex                              as \"Sex\",
           age_group                        as \"Age_Group\",
           to_char(test_date, 'YYYY-MM-DD') as \"Date\",
           to_char(submitted_at at time zone 'utc',
                   'YYYY-MM-DD HH24:MI:SS') as \"Submitted_At\",
           subtechnique                     as \"Subtechnique\",
           area                             as \"Area\",
           item                             as \"Item\",
           score                            as \"Score\",
           comment                          as \"Comment\"
      from csi.technique_checklist")
}

# Map the app's assembled data frame (sheet-style column names) onto the
# table's columns. submitted_at comes from the table default (now()).
tc_db_rows <- function(df) {
  data.frame(
    athlete      = trimws(df$Athlete),
    sex          = df$Sex,
    age_group    = df$Age_Group,
    test_date    = as.Date(df$Date),
    subtechnique = df$Subtechnique,
    area         = df$Area,
    item         = df$Item,
    score        = as.integer(df$Score),
    comment      = as.character(df$Comment),
    stringsAsFactors = FALSE
  )
}

tc_insert <- function(con, df) {
  DBI::dbAppendTable(con, DBI::Id(schema = "csi", table = "technique_checklist"),
                     tc_db_rows(df))
}

# Replace an existing submission (same athlete/sex/team/date/subtechnique):
# delete the old rows and insert the new ones in a single transaction.
tc_replace <- function(con, df) {
  rows <- tc_db_rows(df)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "
      delete from csi.technique_checklist
       where athlete = $1 and sex = $2 and age_group = $3
         and test_date = $4 and subtechnique = $5",
      params = list(rows$athlete[1], rows$sex[1], rows$age_group[1],
                    rows$test_date[1], rows$subtechnique[1]))
    DBI::dbAppendTable(con, DBI::Id(schema = "csi", table = "technique_checklist"),
                       rows)
  })
}
