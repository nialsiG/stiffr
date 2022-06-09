# lvd.plot----
#' @title lvd.plot
#' @description Plot load vs. displacement curve
#' @param x An object of class 'lvd'
#' @param auc A logical indicating whether the area under curve should be filled
#' @return A ggplot2 object
#' @examples
#'
#' @export
lvd.plot <- function(x, auc = TRUE) {
  if (class(x)[1] == "lvd") {
    Mydata <- data.frame(Load = x@load, Displacement = x@displacement)
    Myplot <- ggplot2::ggplot(data = Mydata, ggplot2::aes(x = Displacement, y = Load))
    if (auc == TRUE) {
      Myplot <- Myplot + ggplot2::geom_area(fill = "red", alpha = 0.2)
    }
    Myplot <- Myplot + ggplot2::geom_line(color = "red", size = 2)
    return(Myplot)
  }
}

# lvd.crop----
#' @title lvd.crop
#' @description Crop the range of lvd values to keep using an interactive plot
#' @param x An object of class 'lvd'
#' @return A new 'lvd' object
#' @examples
#'
#' @export
lvd.crop <- function(x) {
  #TO DO (see: plotly)
  #a <- plotly::ggplotly(lvd.plot(x), originalData = FALSE)
  #plotly::highlight(a, "plotly_selected", dynamic = TRUE, persistent = FALSE)
}
