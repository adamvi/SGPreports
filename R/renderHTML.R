renderHTML <- function (
  input,
  number_sections = TRUE,
  number_section_depth=3,
  toc = TRUE,
  toc_depth = 2,
  self_contained = TRUE,
  html_template = "default",
  html_css = "default",
  bibliography = "default",
  csl = "default",
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
					 											 collapse = "', '"), "'", " can't be found in the file path provided.")
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
  
	message("\n\t Rendering HTML with call to render(... multi_document):\n")
	
	render(rmd_input, 
  			 multi_document(..., # passed args to rmarkdown::html_document
  			 							 number_sections, number_section_depth, toc, toc_depth, self_contained, dev,
  			 							 template=html_template, css=html_css, bibliography, csl, pandoc_args),
  			     output_dir=file.path(".", "HTML"))
	
	### Move "master" .md file to HTML/markdown directory
	dir.create(file.path("HTML", "markdown"), showWarnings=FALSE)
	file.copy(file.path("HTML", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), file.path("HTML", "markdown"), overwrite=TRUE)
	file.remove(file.path("HTML", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)))
	
  return(NULL)
}### End renderMultiDocument

