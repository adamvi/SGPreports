##  dualTable function for HTML and LaTeX output

dualTable <- function(my_tbl, where="H", align=paste(rep('c', ncol(my_tbl)), collapse=''), caption="", css.class = "gmisc_table breakboth", ...) { # , where="!htbp"
  tmp_caption <- tblCap(caption)
  if (all(nchar(align) > 1) & length(align)==1){
  	tex.align <- strsplit(align, "")[[1]]
  	html.align<- align
  } else {
  	tex.align <- align
  	html.align<- paste(align,collapse='')
  }
  cat("\n<!-- HTML_Start -->\n")
  html.text <- suppressWarnings(cat(htmlTable::htmlTable(my_tbl, title="", align=html.align, caption=tmp_caption, css.class=css.class,...)))
  cat("\n<!-- LaTeX_Start\n")
  latex.text<- print(Hmisc::latex(my_tbl, file="", where=where, col.just=tex.align, caption=tmp_caption, ...))
  cat("\nLaTeX_End -->\n")
}
