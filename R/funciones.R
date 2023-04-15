generar_radios <- function(objeto_espacial = NULL, columna_indice = NULL) {
  library(sf)
  library(tidyverse)
  objeto_interno <- objeto_espacial %>% st_geometry() %>% st_as_sf() %>% st_set_geometry('geometry')
  indice <- objeto_espacial[[columna_indice]]
  # attr(indice, 'class') <- NULL
  objeto_interno <- objeto_interno %>% mutate(indice = indice)
  radios_sf <- map_dfr(unique(objeto_interno$indice), function(x) {
    puntos <- objeto_interno[objeto_interno$indice == x,] %>%
      st_cast('POINT') %>%
      distinct() %>% 
      st_geometry()
    centroides <- objeto_interno[objeto_interno$indice == x,] %>% st_centroid()
    radios <- centroides %>% st_nearest_points(puntos) %>%
      st_as_sf() %>% 
      st_set_geometry('geometry')
    radios[, columna_indice] <- x
    id_radio_num <- 1:nrow(radios)
    id_radio_acolchado <- stri_pad_left(id_radio_num,
                                        max(nchar(id_radio_num)), 0)
    radios$id_radio <- paste0(radios[[columna_indice]], 'r', id_radio_acolchado)
    return(radios)
  })
  return(radios_sf)
}
