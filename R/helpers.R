META <- yaml::read_yaml(file = "meta.yaml")

META$first_class <- as.Date(META$first_class)

session_details <- function(type, note = "") {
  type_lower <- stringr::str_to_lower(type)
  if (type_lower %in% names(META)) {
    o <- glue::glue("### {type} \n\n")
    if (length(note) > 0) {
      o <- stringr::str_c(o, note, "\n\n")
    }
    for(i in META[[type_lower]]) {
      if (!("room" %in% names(i))) {
        i$room <- "Room TBD"
      }
      o <- stringr::str_c(
        o,
        glue::glue("+ {i$day}, {i$time}, {i$room}\n\n")
      )
    }
    cat(stringr::str_c(o, "\n\n"))
  }
}

teaching_team_details <- function(role) {
  role_lower <- stringr::str_to_lower(
    stringr::str_replace_all(
      role,
      c(" " = "_")
    )
  )
  if (role_lower %in% names(META$staff)) {
    o <- glue::glue("### {role} \n\n")
    for(i in META$staff[[role_lower]]) {
      o <- stringr::str_c(
        o,
        glue::glue("+ {i$role} ({i$pronouns}), {i$title}, {i$email} \ \n\n")
      )
      if ("office_hours" %in% names(i)) {
        o <- o |>
          stringr::str_c(
            glue::glue("*Office Hours: ")
          )
        for(h in i$office_hours) {
          o <- o |> stringr::str_c(
            glue::glue("{h$day}, {h$time} ({h$room}) ")
          )
        }
        o <- o |>
          stringr::str_c(
            glue::glue("*\n\n")
          )
      }
    }
    cat(stringr::str_c(o, "\n\n"))
  }
}

set_authors <- function(role = "Instructors") {
  authors <- c()
  for(i in META$staff[[stringr::str_to_lower(role)]]) {
    authors <- append(authors, i$name)
  }
  cat(glue::glue("---
  author: {authors}
  ---"))
}


get_zotero <- function(library, user = META$zotuser) {
  tmp_bib <- tempfile()
  url <- glue::glue("https://api.zotero.org/users/{user}/collections/{library}/items/top")
  httr::GET(
    url,
    query = list(
      format="biblatex",
      v="3"
    )
  ) |>
    httr::content("text", encoding="UTF-8") |>
    cat(file = tmp_bib)
  
  bib <- RefManageR::ReadBib(tmp_bib, check = FALSE)
  file.remove(tmp_bib)
  
  invisible(RefManageR::NoCite(bib, as.character(names(bib))))
  
  RefManageR::PrintBibliography(
    bib, 
    .opts = list(
      dashed = TRUE,
      first.inits = FALSE
    )
  )
}

format_date <- function(date) {
  format(
    date,
    format = "%m/%d"
  )
}

week_desc <- function(start, week) {
  mon <- format_date(
    lubridate::floor_date(start, "week", 1)
  )
  fri <- format_date(
    lubridate::ceiling_date(start, "week", week_start = 5)
  )
  paste("Week ", week, sep='', ", " , mon, "--", fri)
}

dow_convert <- function(dow) {
  if (dow=="Monday") {
    1
  } else if (dow=="Tuesday") {
    2
  } else if (dow=="Wednesday") {
    3
  } else if (dow=="Thursday") {
    4
  } else if (dow=="Friday") {
    5
  } else if (dow=="Saturday") {
    6
  } else if (dow=="Sunday") {
    7
  }
}

misses <- 0

lecture <- function(start = META$first_class, lec, offset = 0, title = "Lorem Ipsum") {
  lec_miss <- lec + misses
  lec_per_week <- length(META$lectures)
  week <- ceiling(lec_miss / lec_per_week)
  lec_in_week <- lec_miss - ((week - 1) * lec_per_week)
  day <- dow_convert(META$lectures[[lec_in_week]]$day)
  
  miss_days <- as.Date(META$miss_days)
  
  if(day == lubridate::wday(start, week_start = 1) + offset) {
    date <- lubridate::floor_date(
      start + offset + 7 * (week - 1),
      "week",
      week_start = day
    )
  } else {
    date <- lubridate::ceiling_date(
      start + offset + 7 * (week - 1),
      "week",
      week_start = day
    )
  }
  
  lec_day_text <- lubridate::wday(date, label = TRUE, abbr = FALSE)
  
  if (date %in% miss_days) {
    misses <<- misses + 1
  }
  cat(glue::glue("## Lecture {lec}: {lec_day_text} {format_date(date)}\n\n

               ### {title}\n\n

               "))
}

build_schedule <- function(schedule) {
  for (class in schedule) {
    lecture(META$first_class, lec = class$lec, title = class$title, offset = 0)
    if ("readings" %in% names(class)) {
      get_zotero(class$readings)
      cat("\n\n")
    }
  }
}
