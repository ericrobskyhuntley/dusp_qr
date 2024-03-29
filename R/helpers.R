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
        glue::glue("+ {i$day}, {i$time}, {i$room}")
      )
      if("instructor" %in% names(i)) {
        o <- stringr::str_c(
          o,
          glue::glue(", {i$instructor}")
        )
      }
      o <- stringr::str_c(
        o,
        "\n"
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



set_yaml <- function(named_list) {
  cat("---\n")
  for (key in names(named_list)) {
    cat(
      glue::glue("{key}: {named_list[[key]]}\n\n")
    )
  }
  cat("---\n")
}

set_thanks <- function() {
  thanks <- stringr::str_c(
    META$thanks,
    glue::glue("Course material is available [on Github here]({META$repo_url})."),
    sep = " ")
  set_yaml(list("thanks" = thanks))
}

set_authors <- function(role = "Instructors") {
  authors <- c()
  for(i in META$staff[[stringr::str_to_lower(role)]]) {
    if ("affiliation" %in% names(i)) {
      name_affil <- stringr::str_c(
        i$name,
        glue::glue("^[{i$title}, {i$affiliation}]")
      )
      authors <- append(authors, name_affil)
    } else {
      authors <- append(authors, i$name)
    }
  }
  set_yaml(list("author" = authors))
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
  
  opt <- c()
  for (i in bib) {
    if ("note" %in% names(i)) {
      if ("optional" %in% i$note) {
        opt <- append(opt, TRUE)
      } else {
        opt <- append(opt, FALSE)
      }
    } else {
      opt <- append(opt, FALSE)
    }
  }
  
  opt_bib <- base::suppressMessages(bib[opt,])
  bib <- base::suppressMessages(bib[!opt,])
  
  
  if (length(bib) > 0) {
    invisible(RefManageR::NoCite(bib, as.character(names(bib))))
    cat(
      "*Required Readings* \n\n"
    )
    RefManageR::PrintBibliography(
      bib, 
      .opts = list(
        dashed = TRUE,
        first.inits = FALSE
      )
    )
    cat(
      "\n\n"
    )
  }
  if (length(opt_bib) > 0) {
    for (i in 1:length(opt_bib)) {
      opt_bib[[i]]$note <- NULL
    }
    invisible(RefManageR::NoCite(opt_bib, as.character(names(opt_bib))))
    cat(
      "*Optional Readings* \n\n"
    )
    RefManageR::PrintBibliography(
      opt_bib, 
      .opts = list(
        dashed = TRUE,
        first.inits = FALSE
      )
    )
    cat(
      "\n\n"
    )
    
  }
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

MISSES <- 0

lecture_details <- function(start = META$first_class, class, offset = 0) {
  lec_miss <- class$lec + MISSES
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
  
  if(date %in% miss_days) {
    MISSES <<- MISSES + 1
    lec_miss <- class$lec + MISSES
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
  }
  
  lec_day_text <- lubridate::wday(date, label = TRUE, abbr = FALSE)
  
  topics <- stringr::str_c(stringr::str_c('`', class$topics, '`'), collapse = ',')
  cat(glue::glue("## Lecture {class$lec}: {lec_day_text} {format_date(date)}\n\n

               ### {class$title}\n\n
               
               *Topics: {topics}*\n\n

               "))
}

build_schedule <- function(schedule) {
  for (class in schedule) {
    lecture_details(META$first_class, class = class, offset = 0)
    if ("readings" %in% names(class)) {
      get_zotero(class$readings)
      cat("\n\n")
    }
  }
}
