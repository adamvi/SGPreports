renderHTML <- function (
  rmd_input,
  number_sections = TRUE,
  number_section_depth=3,
  toc = TRUE,
  toc_depth = 2,
  self_contained = TRUE,
  html_template = "default",
  html_css = "default",
  pandoc_args = NULL) {
  
  ### Initial checks of alternative css and/or pandoc template
  
  ##  CSS check from Grmd::docx_document - credit to Max Gordon/Gforge https://github.com/gforge
  if (html_css != "default") {
    if (!all(sapply(html_css, file.exists))) {
      alt_html_css <- list.files(pattern = ".css$")
      if (length(alt_html_css) > 0) {
        alt_html_css <- paste0("\n You do have alternative file name(s) in current directory that you may intend to use.", 
                               " You may want to have a YAML section that looks something like:", 
                               "\n---", "\noutput:", "\n  SGPreports::multi_document:", 
                               "\n    html_css: \"", paste(alt_html_css, collapse = "\", \""), 
                               "\"", "\n---")
      } else {
        alt_html_css <- ""
      }
      stop("One or more of the css-file(s) that you've specified can't be identified.", 
           "The file(s) '", paste(html_css[!sapply(html_css, file.exists)], 
                                  collapse = "', '"), "'", " can't be found from the directory '", 
           getwd(), "'", " - i.e. the directory where you have your .Rmd-file", alt_html_css)
    }
  } else html_css <- system.file("rmarkdown", "templates", "multi_document", "resources", "report.css" , package = "SGPreports")

  ### Check pandoc template
  
  if (html_template != "default") {
    if (!file.exists(html_template)) {
      stop("The html_template file that you've specified can't be found in the file path provided.")
    }
  } else html_template <- system.file("rmarkdown", "templates", "multi_document", "resources", "report.html" , package = "SGPreports")

  
  ###
  ###  Render HTML (and master .md file)
  ###
  
  render(rmd_input, 
         multi_document(..., mathjax=NULL, theme=NULL, keep_md=TRUE, # passed and forced args to rmarkdown::html_document
                        css=html_css, template=html_template, number_sections=number_sections, number_section_depth=number_section_depth,
                        toc=toc, toc_depth=toc_depth, self_contained=self_contained, dev=dev, pandoc_args=pandoc_args), output_dir="./HTML")

  ### Move "master" .md file to HTML/markdown directory
  dir.create("HTML/markdown", showWarnings=FALSE)
  file.copy(gsub(".Rmd", ".md", rmd_input), "HTML/markdown", overwrite=TRUE)
  file.remove(gsub(".Rmd", ".md", rmd_input))
  
  return(NULL)
}### End renderMultiDocument

