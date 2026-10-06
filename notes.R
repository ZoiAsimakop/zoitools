#' Show my R notes
#'
#' Prints short notes on a topic. Call it without a topic to see which
#' topics are available.
#'
#' @param topic A topic name, e.g. "loops". Leave empty to list all topics.
#' @return The notes, printed to the console (invisibly returned as text).
#' @examples
#' notes()
#' notes("loops")
#' @export
notes <- function(topic = NULL) {
  all_notes <- list(
    loops = c(
      "Nested loops: the outer loop only moves on after the inner loops finish; the innermost changes fastest.",
      "Total rounds = sizes of all loops multiplied.",
      "Anything random that should change each round goes INSIDE the loop.",
      "Loops that build on earlier results: give starting values before the loop, start at the first position you can calculate.",
      "next skips one round, break ends the loop, stop() throws an error."
    ),
    containers = c(
      "Many values at the end: start with numeric() and store with x[i] <- value.",
      "One running total: start with 0 and update with total <- total + amount.",
      "i is a POSITION, x[i] is the VALUE stored there."
    ),
    conditions = c(
      "if() and while() need exactly ONE TRUE or FALSE.",
      "Inside a loop over rows, use data[i, \"column\"] to get one value, not data$column.",
      "&& = both must be TRUE; || = at least one must be TRUE.",
      "Conditions are checked top to bottom; only the first TRUE branch runs.",
      "A while loop needs something in its condition that changes, or it runs forever (Esc stops it)."
    ),
    functions = c(
      "Assignments inside a function are LOCAL; they never change global variables.",
      "Arguments get the name from the function definition, whatever the data is called outside.",
      "return() hands a value back; print() only shows it.",
      "stop() halts with an error; warning() alerts but keeps running."
    ),
    apply = c(
      "sapply(list, function(x) x$part) runs the function on each element and collects the results.",
      "lapply() always returns a list; sapply() simplifies to a vector or matrix if it can.",
      "sapply(list, `[[`, \"name\") pulls one named item from each element."
    ),
    text = c(
      "paste(x, collapse = \" \") glues the elements of ONE vector into one string; sep goes between separate arguments.",
      "grepl() gives TRUE/FALSE; grep() gives positions.",
      "sub() replaces the first match; gsub() replaces all matches.",
      "cat() only prints; it returns NULL."
    )
  )

  if (is.null(topic)) {
    cat("Available topics:", paste(names(all_notes), collapse = ", "), "\n")
    cat("Use notes(\"topic\") to see the notes for one topic.\n")
    return(invisible(names(all_notes)))
  }

  if (!topic %in% names(all_notes)) {
    stop("Unknown topic. Run notes() to see the available topics.")
  }

  cat(paste0("- ", all_notes[[topic]]), sep = "\n")
  invisible(all_notes[[topic]])
}
