# from here https://stackoverflow.com/questions/28166168/how-to-change-fontface-bold-italics-for-a-cell-in-a-kable-table-in-rmarkdown?noredirect=1&lq=1
format_cells <- function(df, rows ,cols,
                         value = c("italics", "bold", "strikethrough")){

  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]

  for (r in rows) {
    for (c in cols) {

      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])

      # Update formatting
      df[r, c] <- paste0(markup, df[r, c], markup)
    }
  }

  return(df)
}
