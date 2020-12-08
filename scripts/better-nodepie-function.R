
#try a better nodepie function with arguments for line color and fill separate

nodepie2 <- function (data, cols, fill, line_color, alpha = 1) 
{
  if (!"node" %in% colnames(data)) {
    stop("data should have a column 'node'...")
  }
  type <- value <- NULL
  if (missingArg(fill)) {
    fill <- NA
  }
  ldf <- gather(data, type, value, !!cols) %>% split(., .$node)
  lapply(ldf, function(df)
    ggpie(
      df,
      x = "value",
      fill = "type",
      color = line_color,
      label = NULL,
      tickslab = FALSE,
      legend = "none",
      size = 0.25,
      ggtheme = theme_void()
    ))
}

