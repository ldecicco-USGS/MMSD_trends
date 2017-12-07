render_html <- function(output_file, filename_md) {
  
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

  outfile <- rmarkdown::render(input = filename_md, 
                    output_file = basename(output_file),
                    output_dir = dirname(output_file),
                    output_format = "html_document", 
                    quiet=TRUE)
  
}