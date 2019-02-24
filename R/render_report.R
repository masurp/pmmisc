#' Creates an html report based on a .rmd file
#' 
#' This function simply allows to render RMarkdown files to produce html reports.
#' The report will be saved in the folder of the Rmarkdown file. 
#'
#' 
#' @param file path to the file that should be rendered. File needs to be an RMarkdown script. 
#' @param name name of the final report.
#' @examples 
#' # Produces an html-file called "01_analysis.html"
#' render_report("test.rmd", "01_analysis")
#' @export
render_report = function(file, name) {
  rmarkdown::render(
    file, params = list(
      name = name,
      numner = number
    ),
    output_file = paste0(name, ".html")
  )
}
