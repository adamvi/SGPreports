renderMultiDocument <- function (
  rmd_input,
  output_format = c("HTML", "EPUB", "PDF"), # Can also produce "DOCX"
  cleanup_aux_files = TRUE,
  number_sections = TRUE,
  number_section_depth=3,
  convert_header_levels=c(5,6),
  toc = TRUE,
  toc_depth = 2,
  self_contained = TRUE,
  docx_self_contained = FALSE,
  dev="png",
  html_template = "default",
  epub_template = NULL, # Currently uses pandoc epub template.
  pdf_template = "default",
  html_css = "default",
  epub_css = "default",
  docx_css = "default",
  cover_img = NULL, 
  add_cover_title = FALSE,
  bibliography = "default",
  csl = "default",
  pandoc_args = NULL,
  ...) {

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
  
  if (epub_css != "default") {
    if (!all(sapply(epub_css, file.exists))) {
      alt_epub_css <- list.files(pattern = ".css$")
      if (length(alt_epub_css) > 0) {
        alt_epub_css <- paste0("\n You do have alternative file name(s) in current directory that you may intend to use.", 
                               " You may want to have a YAML section that looks something like:", 
                               "\n---", "\noutput:", "\n  SGPreports::multi_document:", 
                               "\n    epub_css: \"", paste(alt_epub_css, collapse = "\", \""), 
                               "\"", "\n---")
      } else {
        alt_epub_css <- ""
      }
      stop("One or more of the css-file(s) that you've specified can't be identified.", 
           "The file(s) '", paste(epub_css[!sapply(epub_css, file.exists)], 
                                  collapse = "', '"), "'", " can't be found in the file path provided.")
    }
  } else epub_css <- system.file("rmarkdown", "templates", "multi_document", "resources", "epub.css" , package = "SGPreports")
  
  ### Check pandoc templates
  
  if (html_template != "default") {
    if (!file.exists(html_template)) {
      stop("The html_template file that you've specified can't be found in the file path provided.")
    }
  } else html_template <- system.file("rmarkdown", "templates", "multi_document", "resources", "report.html" , package = "SGPreports")
  
  if (!is.null(epub_template)) {
    if (epub_template == "default") {
      epub_template <- NULL 
    } else {
      if (!file.exists(epub_template)) {
        stop("The epub_template file that you've specified can't be found in the file path provided.")
      }}
  }# EPUB default is pandoc template for now otherwise :: else epub_template <- system.file("rmarkdown", "templates", "multi_document", "resources", "epub.html" , package = "SGPreports")
  
  if (pdf_template != "default") {
    if (!file.exists(pdf_template)) {
      stop("The pdf_template file that you've specified can't be found in the file path provided.")
    }
  } else pdf_template <- system.file("rmarkdown", "templates", "multi_document", "resources", "damian.tex" , package = "SGPreports")

  ##  Check csl file  
  if (!is.null(csl)) {
    if (csl != "default") {
      if (!file.exists(csl)) {
        stop("The csl file that you've specified can't be found in the file path provided.")
      } else pandoc_args <- c(pandoc_args, "--csl", csl) # Use pandoc_args here since docx_document passes that to html_document
    } else pandoc_args <- c(pandoc_args, "--csl", system.file("rmarkdown", "templates", "multi_document", "resources", "apa.csl" , package = "SGPreports"))
  }
  
  ### Bibliography
  my.pandoc_citeproc <- rmarkdown:::find_program("pandoc-citeproc")
  
  if (!is.null(bibliography)) {
    if (bibliography == "default") {
      pandoc_args <-c(pandoc_args, "--filter", my.pandoc_citeproc, "--bibliography", 
                      system.file("rmarkdown", "templates", "multi_document", "resources", "educ.bib" , package = "SGPreports"))
    } else {
      if(file.exists(bibliography)) {
        pandoc_args <-c(pandoc_args, "--filter", my.pandoc_citeproc, "--bibliography", bibliography)
      } else stop("'bibliography' file not found.")
    }
  }
  
  ###  Check pandoc_args  
        # TBD
  
  if (!"HTML" %in% output_format & !file.exists(file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input)))) {
    if (!file.exists(file.path("HTML", gsub(".Rmd", ".html", rmd_input)))) {
      message("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input)), " was not found, but is required for 
        output_format ", paste(output_format, collapse=", "), ". The 'HTML' step will be added to 'output_format' and run.
        NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input)), " will be also created.\n")
      output_format <- c("HTML", output_format)
    } else stop("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input)), " was not found, but is required for 
        output_format ", paste(output_format, collapse=", "), ". Add 'HTML' the 'output_format' argument and re-run.
        NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input)), " will be overwritten when re-run.\n")
  }
  
  ###
  ###  Render HTML (and master .md file)
  ###

  if ("HTML" %in% output_format) {
    message("\n\t Rendering HTML with call to render(... Grmd::docx_document):\n")
    
    render(rmd_input, multi_document(..., # passed args to rmarkdown::html_document
      css=html_css, template=html_template, number_sections=number_sections, number_section_depth=number_section_depth,
      toc=toc, toc_depth=toc_depth, self_contained=self_contained, dev=dev, pandoc_args=pandoc_args), output_dir=file.path(".", "HTML"))
  
    ### Move "master" .md file to HTML/markdown directory
    dir.create(file.path("HTML", "markdown"), showWarnings=FALSE)
    file.copy(file.path("HTML", gsub(".Rmd", ".md", rmd_input)), file.path("HTML", "markdown"), overwrite=TRUE)
    file.remove(file.path("HTML", gsub(".Rmd", ".md", rmd_input)))
  }
  
  if ("EPUB" %in% output_format) {
    renderEPUB(input=rmd_input, cover_img, add_cover_title, number_sections, convert_header_levels,
               epub_template, epub_css, bibliography, csl, pandoc_args)
  }
    
  if ("PDF" %in% output_format) {
    renderPDF(input=rmd_input, keep_tex=cleanup_aux_files, number_sections, pdf_template, bibliography, csl, convert_header_levels, pandoc_args)
  }

  if ("DOCX" %in% output_format) {
    renderDOCX(input=rmd_input, self_contained=docx_self_contained, number_sections, number_section_depth, docx_css, bibliography, csl, pandoc_args)
    message("\n\n\tDOCX rendering is complete.  The output is the file \n\n\t", 
            file.path("DOCX", gsub(".Rmd", "-docx.html", rmd_input)), "\n\t
        In order to create a .docx file from it, you must serve the .html file
        and then copy and paste from the web page to a blank word document.
        see http://gforge.se/2013/12/fast-track-publishing-using-knitr-part-i/
        for more detail.
            
        One can now serve local web pages/sites using the R package 'servr'.
        For example, issue the command 'servr::httd(dir = \".\",port=4444)' 
        at the prompt (assuming you have 'servr' installed) and then go to\n", 
            paste("\n\thttp://localhost:4444/DOCX/", gsub(".Rmd", "-docx.html", rmd_input), sep=""),
            "\n\n\tin a web browser. Chrome seems to have the best results, at least in Mac OSX
        (Although Safari preserves math images created by --webtex in pandoc).
        This should preserve all images, formatting, etc. when you copy and paste in Word. 
        
        NOTE: You may need to change the port number in the call to httd(...).
        NOTE: Setting docx_self_contained = TRUE creates self contained documents that
              some browsers will retain images in the copy/paste to Word.")
  }
  
  if (cleanup_aux_files) {
    for(tmp.dir in output_format) {
      unlink(file.path(tmp.dir, "markdown"), recursive = TRUE)
    }
  }
}### End renderMultiDocument

