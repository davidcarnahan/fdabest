#' Create a 3 x 7 Tiff Image
#'
#' This function has been designed to provide a standardized format for
#' all FDA BEST intiative graphs. It will generate a 3x7 Tiff image of
#' high resolution (300 dpi), and will save it to the working directory.
#'
#' @param df The dataframe loaded into the current environment to be used
#' for the image creation.
#' @param x The categorical variable to be used for the x-axis of the graph
#' @param y The discrete variable to be used for the y-axis of the graph
#' @param t The title desired for the graph (in quotes)
#' @param xlab The label for the x-axis if different from the variable name in
#' dataframe. By default, it will use the variable name.
#' @param ylab The label for the y-axis if different from the variable name in
#' dataframe. By default, it will use the variable name.
#'
#' @author David Carnahan <dcarnahan@@us.imshealth.com>
#' @export
#' @examples
#' @bar_tiff_3x7(mpg, cyl, hwy, "Mileage by Number of Cylinders")

bar_tiff_3x7 <- function(df, x, y, t, xlab = x_nm, ylab = y_nm) {

  require(ggplot2)
  require(stringi)
  require(Cairo)
  attach(df)

  # prepare variables for naming of file to be saved at the end
  dir <- getwd()
  nm <- colnames(df)
  x_nm <- nm[1]
  y_nm <- nm[2]

  # x and y labels
  labels <- function(xlab, ylab) {
    if (!exists(xlab)) {
      xlab <- x_nm
    } else {
      xlab <- xlab
    }

    if (!exists(ylab)) {
      ylab <- y_nm
    } else {
      ylab <- ylab
    }
  }

  # names for saving graph to directory
  xlab2 <- gsub(" ", "", xlab)
  ylab2 <- gsub(" ", "", ylab)
  tiff_nm <- paste0(ylab2, "-by-", xlab2, ".tiff")
  png_xd <- paste0(dir, "/", tiff_nm)

  # create tiff product
  tiff(tiff_nm, height = 3, width = 7, units = "in",
       family = "Helvetica", compression = "lzw", res = 300)

  # plot graph
  ggx <- ggplot(df, aes(x, y)) +
    geom_bar(fill=rgb(0.2,0.4,0.6,1) , width=.8, stat="identity") +
    geom_text(aes(label=y), vjust=0, size = 3) +
    scale_y_continuous() +
    scale_fill_manual(values = "darkblue") +
    ylab(stri_trans_totitle(ylab)) +
    xlab(stri_trans_totitle(xlab)) +
    theme_minimal() +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust=1, size = 10),
          axis.title.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 11, face = "bold"),
          strip.text.x = element_text(size = 9),
          legend.position = "none") +
    ggtitle(t)

  # save graphs and print out where it was saved
  ggsave(tiff_nm, type = "cairo")
  dev.off()
  print(sprintf("Your graph was saved in %s as %s", dir, tiff_nm))
}
