
# ---- Robust athlete extraction from Airtable tibble df ----
extract_athletes <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  raw <- as.character(df[["fields.Name & Season"]])
  team <- df[["fields.Team"]]

  # Normalize team column to a single string per row
  team_norm <- if (is.list(team)) {
    vapply(team, function(x) paste(unique(unlist(x)), collapse=", "), "")
  } else as.character(team)

  ok <- grepl("2025/26", raw) & grepl("(Ski|Dev)", team_norm, ignore.case = TRUE)

  # Remove leading season like "2025/26 " if present
  names_only <- trimws(sub("^\\s*\\d{4}/\\d{2}\\s*", "", raw[ok]))
  names_only <- names_only[nzchar(names_only)]
  sort(unique(names_only))
}
