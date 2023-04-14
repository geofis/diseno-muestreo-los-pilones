library(sf)
library(tidyverse)
h3_res10 <- st_read('fuentes/h3-resolucion-10.gpkg')
h3_res10_c <- st_centroid(h3_res10)
h3_res10_v <- h3_res10 %>% st_cast('POINT')
h3_res10_
d[1,] %>% st_centroid %>% ggplot + geom_sf(size = 0.25) + geom_sf(data = d[1,] %>% st_cast('LINESTRING'))



# d[1,] %>% group_by(index) %>% summarize(m = mean(area), do_union=FALSE) %>% st_cast("MULTILINESTRING") %>% plot()

d[1,] %>% st_cast('POINT') %>% st_geometry() %>% st_nearest_points(d[1,] %>% st_centroid())

d[1:10,] %>% group_by(index) %>% st_cast('POINT') %>% st_geometry() %>% st_nearest_points(d[1:10, ] %>% st_centroid) %>% plot()

map(1:10, function(x) d[x,] %>% st_cast('POINT') %>%
      st_geometry() %>% st_nearest_points(d[x,] %>% st_centroid())) %>% do.call(rbind, .)

a <- map_dfr(d$index[1:10], function(x) {
  r <- d[d$index == x,] %>% st_cast('POINT') %>%
    st_geometry() %>%
    st_nearest_points(d[d$index == x,] %>% st_centroid()) %>%
    st_as_sf() %>% 
    st_set_geometry('geometry')
  r$index <- x
  return(r)
})


a <- map_dfr(ind_esp$h3_address, function(x) {
  r <- ind_esp[ind_esp$h3_address == x,] %>% st_cast('POINT') %>%
    st_geometry() %>%
    st_nearest_points(ind_esp[ind_esp$h3_address == x,] %>% st_centroid()) %>%
    st_as_sf() %>% 
    st_set_geometry('geometry')
  r$index <- x
  return(r)
})

# library(stplanr)
# line_segment(a, segment_length = units(10, 'm'))
st_line_sample(a %>% st_transform(32619), density = set_units(10, 'm')) %>% st_transform(4326) %>% st_write('sep-10-metros.kml')

generar_radiales <- function(objeto_espacial = NULL, columna_indice = NULL) {
  objeto_interno <- objeto_espacial %>% st_geometry()
  objeto_interno$indice <- objeto_interno[, columna_indice, drop=T] %>% st_drop_geometry()
  return(objeto_interno)
  r_sf <- map_dfr(unique(objeto_interno$indice), function(x) {
    r <- objeto_interno[objeto_interno$indice == x,] %>% st_cast('POINT') %>%
      st_geometry() %>%
      st_nearest_points(objeto_interno[objeto_interno$indice == x,] %>% st_centroid()) %>%
      st_as_sf() %>% 
      st_set_geometry('geometry')
    r$indice <- x
    return(r)
  })
  return(r_sf)
}
