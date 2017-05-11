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
  docx_css = "default",
  epub_css = "default",
  cover_img = NULL, 
  add_cover_title = FALSE,
  bibliography = "default",
  csl = "default",
  literasee.repo=NULL,
  literasee.description=NULL,
  pandoc_args = NULL,
  ...) {

	###  Checks of alternative css and/or pandoc template done in externalized functions
  
  ###  Check for HTML routine request, or at least required master .md output 
  
  if (any(c("EPUB", "PDF", "DOCX") %in% toupper(output_format)) & !"HTML" %in% toupper(output_format) & !file.exists(file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)))) {
    if (!file.exists(file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)))) {
        message("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), " was not found, but is required for 
          output_format ", paste(output_format, collapse=", "), ". The 'HTML' step will be added to 'output_format' and run.
          NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)), " will be also created.\n")
        output_format <- c("HTML", output_format)
    } else stop("\n\tThe file ", file.path("HTML", "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), " was not found, but is required for 
        output_format ", paste(output_format, collapse=", "), ". Add 'HTML' the 'output_format' argument and re-run.
        NOTE: Document ", file.path("HTML", gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)), " will be overwritten when re-run.\n")
  }
  
  ###
  ###  Render HTML (and master .md file)
  ###

  if ("HTML" %in% toupper(output_format)) {
  	renderHTML(input=rmd_input, number_sections, number_section_depth, toc, toc_depth,
  						 self_contained, dev, html_template, html_css, bibliography, csl, pandoc_args)
  }
  
  if ("LITERASEE" %in% toupper(output_format)) {
    if (html_template != "default") {
      if (!file.exists(html_template)) {
        stop("The html_template file that you've specified can't be found in the file path provided.")
      } else tmp_html_template <- html_template
    } else tmp_html_template <- system.file("rmarkdown", "templates", "multi_document", "resources", "literasee.html" , package = "SGPreports")
    if (is.null(literasee.repo)) literasee.repo <- "LITERASEE"
    if (literasee.repo %in% list.dirs(full.names = FALSE, recursive = FALSE)) unlink(literasee.repo, recursive = TRUE)
    renderHTML(input=rmd_input, number_sections, number_section_depth, toc, toc_depth, self_contained, dev,
               html_template=tmp_html_template, html_css, bibliography, csl, pandoc_args, output_dir=literasee.repo)

    dir.create(file.path(literasee.repo, "markdown"), showWarnings=FALSE)
    file.rename(file.path(literasee.repo, gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)), file.path(literasee.repo, "markdown", gsub(".Rmd", ".md", rmd_input, ignore.case=TRUE)))
    file.rename(file.path(literasee.repo, gsub(".Rmd", ".html", rmd_input, ignore.case=TRUE)), file.path(literasee.repo, "report.md"))

    if (!self_contained) {
      #  Change relative file paths in report.md text
      html.text <- rmarkdown:::read_lines_utf8(file.path(literasee.repo, "report.md"), getOption("encoding"))
      for (f in file.path(gsub(".Rmd", "_files", rmd_input, ignore.case=TRUE), "..", grep(paste0("img", .Platform$file.sep), list.files(literasee.repo, recursive = TRUE), value = TRUE))) {
        html.text <- gsub(f, file.path("images", tail(strsplit(f, .Platform$file.sep)[[1]], 1)), html.text)
      }
      writeLines(html.text, file.path(literasee.repo, "report.md"))

      #  Move image files to /images directory
      file.rename(file.path(literasee.repo, gsub(".Rmd", "_files", rmd_input, ignore.case=TRUE)), file.path(literasee.repo, "images"))
      for (f in grep(paste0("img", .Platform$file.sep), list.files(literasee.repo, recursive = TRUE), value = TRUE)) {
        file.rename(file.path(literasee.repo, f), file.path(literasee.repo, "images", tail(strsplit(f, .Platform$file.sep)[[1]], 1)))
      }
      unlink(file.path(literasee.repo, "img"), recursive=TRUE)
    }
    if (!is.null(literasee.description)) cat(c("#", SGP::capwords(gsub(.Platform$file.sep, " ", literasee.repo)), "\n\n", literasee.description), file=file.path(literasee.repo, "README.md"))
    output_format <- literasee.repo
  }

  if ("EPUB" %in% toupper(output_format)) {
    renderEPUB(input=rmd_input, cover_img, add_cover_title, number_sections, convert_header_levels,
               epub_template, epub_css, bibliography, csl, pandoc_args)
  }
    
  if ("PDF" %in% toupper(output_format)) {
    renderPDF(input=rmd_input, keep_tex=cleanup_aux_files, number_sections, convert_header_levels,
    					pdf_template, bibliography, csl, pandoc_args)
  }

  if ("DOCX" %in% toupper(output_format)) {
    renderDOCX(input=rmd_input, self_contained=docx_self_contained, 
    					 number_sections, number_section_depth, docx_css, bibliography, csl, pandoc_args)
    message("\n\n\tDOCX rendering is complete.  The output is the file \n\n\t", 
            file.path("DOCX", gsub(".Rmd", "-docx.html", rmd_input, ignore.case=TRUE)), "\n\t
        In order to create a .docx file from it, you must serve the .html file
        and then copy and paste from the web page to a blank Word document.
        see http://gforge.se/2013/12/fast-track-publishing-using-knitr-part-i/
        for more detail.
            
        One can now serve local web pages/sites using the R package 'servr'.
        For example, issue the command 'servr::httd(dir = \".\",port=4444)' 
        at the prompt (assuming you have 'servr' installed) and then go to\n", 
            paste("\n\thttp://localhost:4444/DOCX/", gsub(".Rmd", "-docx.html", rmd_input, ignore.case=TRUE), sep=""),
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

