process_demotions_and_promotions <- function(res, desc) {
  get <- function(kind) {
    text <- desc$get(paste0("Config/rcmdcheck/", kind))
    if (!is.na(text))
      trimws(strsplit(text, "\n")[[1]])
  }

  move <- function(patterns, from, to) {
    names <- c(errors = "ERROR",
               warnings = "WARNING",
               notes = "NOTE")
    if (length(patterns) && length(res[from])) {
      lapply(patterns,
             function(pattern) {
               hits <- grep(pattern, res[from], fixed = TRUE)
               if (length(hits)) {
                 if (!is.null(to))
                   res[[to]] <<- append(res[[to]],
                                      paste0("Converted from ", names[from], " to ", names[to], ":\n",
                                             res[[from]][hits]))
                 res[[from]] <<- res[[from]][-hits]
               }
             })
    }
  }

  move(get("demote/errors"),    "errors",   "warnings")
  move(get("demote/warnings"),  "warnings", "notes")
  move(get("demote/notes"),     "notes",    NULL)
  move(get("promote/notes"),    "notes",    "warnings")
  move(get("promote/warnings"), "warnings", "errors")

  res
}
