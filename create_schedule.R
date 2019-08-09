library(lubridate)

class_days <- c("Mon", "Wed", "Fri")
semester <- "f"
year <- "2019"
start_day <- "9-4"
end_day <- "12-10"
start_finals_day <- "12-13"
end_finals_day <- "12-17"
start_midsemester_break <- "10-12"
end_midsemester_break <- "10-15"
start_thanksgiving_break <- "11-27"
end_thanksgiving_break <- "12-01"

have_final <- TRUE

start_date <- lubridate::mdy(paste0(start_day, "-", year))
end_date <- lubridate::mdy(paste0(end_day, "-", year))

start_midsemester_break_date <- lubridate::mdy(paste0(start_midsemester_break, "-", year))
end_midsemester_break_date <- lubridate::mdy(paste0(end_midsemester_break, "-", year))
start_thanksgiving_break_date <- lubridate::mdy(paste0(start_thanksgiving_break, "-", year))
end_thanksgiving_break_date <- lubridate::mdy(paste0(end_thanksgiving_break, "-", year))


days_of_week <- c("Mon", "Tue", "Wed", "Thu", "Fri")

week_mon <- lubridate::floor_date(start_date, unit = "weeks", week_start = 1)
week_ind <- 1



cat(
"---
  title: \"Schedule\"
---
  
  **Click on the text like \"Week 1: Sep 5 -- 7\" to expand or collapse the items we covered in that week.**
  
  I will fill in more detail and provide links to lecture notes and labs as we go along.  Items for future dates are tentative and subject to change.

```{r, echo = FALSE}
make_week_box <- function(id, open, title, contents) {
  cat('
<div class=\"panel panel-default\">
<div class=\"panel-heading\" role=\"tab\" id=\"headingOne\">
<h5 class=\"mb-0\">
<a data-toggle=\"collapse\" href=\"#',
id,
'\" aria-expanded=\"true\" aria-controls=\"collapseOne\">
',
title,
'</a>
</h5>
</div>

<div id=\"',
id,
'\" class=\"collapse',
ifelse(open, \" in\", \"\"),
'\">
<div class=\"panel-body\">
',
contents,
'
</div>
</div>
</div>',
      sep = \"\")
}
```

", file = "schedule.Rmd", append = FALSE)


while(week_mon <= end_date) {
  week_day_text <- paste(
    sapply(class_days, function(class_day) {
      class_date <- week_mon + which(days_of_week == class_day) - 1
      if(class_date < start_date || class_date > end_date) {
        return("")
      } else if(class_date >= start_midsemester_break_date && class_date <= end_midsemester_break_date) {
        return(paste0("\n#### ", format(class_date, "%a, %b %d"),
                      "\n * **No Class**: Midsemester Break.  Safe travels!\n"))
      } else if(class_date >= start_thanksgiving_break_date && class_date <= end_thanksgiving_break_date) {
        return(paste0("\n#### ", format(class_date, "%a, %b %d"),
                      "\n * **No Class**: Thanksgiving Break.  Safe travels!\n"))
      } else {
        return(paste0("\n#### ", format(class_date, "%a, %b %d"),
          "\n * **In class**, we will work on:\n * **After class**, please:\n"))
      }
    }),
    collapse = "\n"
  )
  
  cat(
    paste0(
      "```{r, echo = FALSE, results='asis'}\nmake_week_box(\"week", week_ind, "\", TRUE, \"Week ", week_ind, ": ", format(week_mon, "%b %d"), " -- ", format(week_mon + 4, "%b %d"),
      "\", \"", week_day_text, "\")\n```\n\n"),
    file = "schedule.Rmd",
    append = TRUE
  )
  
  week_ind <- week_ind + 1
  week_mon <- week_mon + 7
}


if(have_final) {
  finals_text <- "We will have a cumulative final exam."
} else {
  finals_text <- "We will not have a final exam in this class."
}

cat(
  paste0(
    "```{r, echo = FALSE, results='asis'}\nmake_week_box(\"finals\", TRUE, \"Final Exams: ", format(week_mon, "%b %d"), " -- ", format(week_mon + 4, "%b %d"),
    "\", \"", finals_text, "\")\n```\n\n"),
  file = "schedule.Rmd",
  append = TRUE
)
