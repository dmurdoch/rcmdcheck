process_demotions_and_promotions <- function(res, demote, promote) {
  if (length(res$errors) && length(demote$errors)) {
    lapply(demote$errors,
           function(pattern) {
             hits <- grep(pattern, res$errors)
             if (length(hits)) {
               res$warnings <<- append(res$warnings,
                                       paste0("Demoted from ERROR to WARNING:\n",
                                              res$errors[hits]))
               res$errors <<- res$errors[-hits]
             }
           })
  }
  if (length(res$warnings) && length(demote$warnings)) {
    lapply(demote$warnings,
           function(pattern) {
             hits <- grep(pattern, res$warnings)
             if (length(hits)) {
               res$notes <<- append(res$notes,
                                    paste0("Demoted from WARNING to NOTE:\n",
                                           res$warnings[hits]))
               res$warnings <<- res$warnings[-hits]
             }
           })
  }
  if (length(res$notes) && length(demote$notes)) {
    lapply(demote$notes,
           function(pattern) {
             hits <- grep(pattern, res$notes)
             if (length(hits))
               res$notes <<- res$notes[-hits]
           })
  }
  if (length(res$notes) && length(promote$notes)) {
    lapply(promote$notes,
           function(pattern) {
             hits <- grep(pattern, res$notes)
             if (length(hits)) {
               res$warnings <<- append(res$warnings,
                                    paste0("Promoted from NOTE to WARNING:\n",
                                           res$notes[hits]))
               res$notes <<- res$notes[-hits]
             }
           })
  }
  if (length(res$warnings) && length(promote$warnings)) {
    lapply(promote$warnings,
           function(pattern) {
             hits <- grep(pattern, res$warnings)
             if (length(hits)) {
               res$errors <<- append(res$errors,
                                       paste0("Promoted from WARNING to ERROR:\n",
                                              res$warnings[hits]))
               res$warnings <<- res$warnings[-hits]
             }
           })
  }
  res
}
