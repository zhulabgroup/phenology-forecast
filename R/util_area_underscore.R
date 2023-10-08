util_area_underscore <- function(area_full = "Appalachians / Cumberland Plateau") {
  area_conc <- area_full %>%
    # gsub(pattern = "\\s+|[^\\w\\s]+",replacement = "_", x = ., perl = TRUE)
    str_split(pattern = "/| ", simplify = T) %>%
    .[. != ""] %>%
    str_c(collapse = "_")

  return(area_conc)
}
