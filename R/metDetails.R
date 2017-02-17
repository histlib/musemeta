# Modification of met.R to scrape additional details from metadata records

met_details <- function(id, ascii = FALSE, ...){
  out <- musemeta_GET(paste0(metbase(), id), config(followlocation = TRUE), ...)
  met_add(out, ascii, id)
  
met_add <- function(x, ascii, id){
  details <- xml2::xml_find_first(tmp, "//div[@class='collection-details__accordion-container']")
  title <- strw(strsplit(xml2::xml_text(xml2::xml_find_first(tmp, "//title")), "\\|")[[1]])[[2]]
  name <- c("title", gsub(":", "", xml2::xml_text(xml2::xml_find_all(details, "//header"))))
  tags <- c(title, xml2::xml_text(xml2::xml_find_all(details, "//br")))
  structure(nonascii(list(name = id, values = tags), ascii), class = "muse")
  }
  
  metbase <- function() "http://www.metmuseum.org/art/collection/search/"
