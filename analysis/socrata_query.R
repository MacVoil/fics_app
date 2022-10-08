options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)
library(timetk)
library(quantmod)
library(plotly)

# get fics ----
# Trae toda la información relacionada con los fondos, esta base es para el analisis a realiar por parte de las empresas

get_fics <- function(
    empresa = NULL,
    patrimonio = NULL,
    tipo_empresa = NULL,
    subtipo_patrimonio = NULL,
    id_participacion = NULL,
    from = today() - years(2),
    to = today()) {
      
      # Transformanos str a date
      from <- ymd(from)
      to <- ymd(to)
      
      # Estructura url
      url_head <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT fecha_corte, nombre_entidad, nombre_tipo_entidad,  nombre_patrimonio, nombre_subtipo_patrimonio, tipo_participacion, numero_unidades_fondo_cierre, valor_unidad_operaciones, precierre_fondo_dia_t, valor_fondo_cierre_dia_t, numero_inversionistas, aportes_recibidos, retiros_redenciones, anulaciones, rendimientos_abonados, rentabilidad_diaria, rentabilidad_mensual, rentabilidad_semestral, rentabilidad_anual where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' ")
      
      if (!is.null(empresa)) {
          
          empresa <- str_c(empresa, collapse = "' ,'")
          
          empresa <- str_glue("and nombre_entidad in('{empresa}') ")
        }
      
      if (!is.null(tipo_empresa)) {
        
        empresa <- str_c(tipo_empresa, collapse = "' ,'")
        
        empresa <- str_glue("and nombre_tipo_entidad in('{tipo_empresa}') ")
      }
      
      if (!is.null(patrimonio)) {
          
          patrimonio <- str_c(patrimonio, collapse = "' ,'")
          
          patrimonio <- str_glue("and nombre_patrimonio in('{patrimonio}') ")
        }
      
      if (!is.null(subtipo_patrimonio)) {
          
          subtipo_patrimonio <- str_c(subtipo_patrimonio, collapse = "' ,'")
          
          subtipo_patrimonio <- str_glue("and nombre_subtipo_patrimonio in('{subtipo_patrimonio}') ")
      }
      
      if (!is.null(id_participacion)) {
        
        id_participacion <- str_c(id_participacion, collapse = "' ,'")
        
        id_participacion <- str_glue("and tipo_participacion in('{id_participacion}') ")
      }
      
      url_tail <- "LIMIT 100000000"
      url_total <- str_c(url_head,
                         empresa,
                         patrimonio,
                         subtipo_patrimonio, 
                         id_participacion,
                         url_tail) %>% 
          URLencode()
      
      # A data frame
      fromJSON(url_total) %>% 
          mutate(across(numero_unidades_fondo_cierre:rentabilidad_anual , as.numeric),
                 fecha_corte = ymd_hms(fecha_corte) %>% 
                     ymd()) %>%  
          rename_with(~ str_remove(.x, "^sum_")) %>% 
          arrange(fecha_corte,
                  nombre_entidad,
                  nombre_subtipo_patrimonio,
                  nombre_patrimonio
                  )
      } 

# Test
tictoc::tic()
test_get_fics <- get_fics(empresa   = "Alianza Fiduciaria S.A.",
                          #patrimonio =  "FONDO DE INVERSION COLECTIVA ABIERTO CON PACTO PERMANENCIA ALIANZA RENTA FIJA 90", 
                          id_participacion = "501",
                 from               = "2020-01-01", 
                 to                 = today())
tictoc::toc()
# get fics names ----
# Debido a que se necesitan los nombres exactos en socrata esa función ayuda a buscar los nombres exactos para usar en las demás funciones

get_fics_names <- function(
  .nombre_entidad = ".*",
  .nombre_tipo_entidad =  ".*",
  .nombre_subtipo_patrimonio =  ".*",
  .nombre_patrimonio =  ".*",
  .select = c("nombre_entidad", "nombre_tipo_entidad", "nombre_subtipo_patrimonio", "nombre_patrimonio", "max_fecha_corte")) {
  
    # url
    URLencode("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT nombre_entidad, nombre_tipo_entidad, nombre_subtipo_patrimonio, nombre_patrimonio, max(fecha_corte) group by nombre_entidad, nombre_tipo_entidad, nombre_subtipo_patrimonio, nombre_patrimonio LIMIT 100000000") %>% 
      fromJSON() %>% 
      mutate(max_fecha_corte = ymd_hms(max_fecha_corte) %>% 
               ymd()) %>% 
      arrange(nombre_entidad, 
              nombre_tipo_entidad,
              nombre_subtipo_patrimonio, 
              nombre_patrimonio) %>% 
    
      # Filtro
      filter(str_detect(str_to_lower(nombre_entidad), str_c(.nombre_entidad, collapse = "|") %>%
                                                      str_to_lower())) %>% 
      filter(str_detect(str_to_lower(nombre_tipo_entidad), str_c(.nombre_tipo_entidad, collapse = "|") %>%
                                                  str_to_lower())) %>% 
      filter(str_detect(str_to_lower(nombre_subtipo_patrimonio), str_c(.nombre_subtipo_patrimonio, collapse = "|") %>%
                                                                 str_to_lower())) %>% 
      filter(str_detect(str_to_lower(nombre_patrimonio), str_c(.nombre_patrimonio, collapse = "|") %>%
                                                         str_to_lower())) %>% 
      
      # seleccionar
      select(all_of(.select)) %>% 
      distinct() %>% 
      arrange_all()
    }

# Test
test_get_fics_names <- get_fics_names(.nombre_entidad = c("bancolombia", "alianza"), 
                                      .nombre_subtipo_patrimonio = "fic", 
                                      .nombre_patrimonio = "alternativo", 
                                      .select = c("nombre_entidad", "nombre_tipo_entidad","nombre_patrimonio", "max_fecha_corte"))


# get fics price----
# Con esta función traemos solo el precio de la unidad, esta pensada para los analisis realizados por los clientes 

get_fics_price <- function(
  from = today() - years(2),
  to = today(),
  empresa = NULL,
  patrimonio = NULL,
  tipo_empresa = NULL,
  subtipo_patrimonio = NULL,
  id_participacion = NULL) {
  
  # Transformanos str a date
  from <- ymd(from)
  to <- ymd(to)
  
  # Estructura url
  url_head <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT fecha_corte, nombre_entidad, nombre_tipo_entidad,  nombre_patrimonio, nombre_subtipo_patrimonio, tipo_participacion,valor_unidad_operaciones, numero_inversionistas where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' ") 
  
  if (!is.null(empresa)) {
    
    empresa <- str_c(empresa, collapse = "' ,'")
    
    empresa <- str_glue("and nombre_entidad in('{empresa}') ")
  }
  
  if (!is.null(tipo_empresa)) {
    
    empresa <- str_c(tipo_empresa, collapse = "' ,'")
    
    empresa <- str_glue("and nombre_tipo_entidad in('{tipo_empresa}') ")
  }
  
  if (!is.null(patrimonio)) {
    
    patrimonio <- str_c(patrimonio, collapse = "' ,'")
    
    patrimonio <- str_glue("and nombre_patrimonio in('{patrimonio}') ")
  }
  
  if (!is.null(subtipo_patrimonio)) {
    
    subtipo_patrimonio <- str_c(subtipo_patrimonio, collapse = "' ,'")
    
    subtipo_patrimonio <- str_glue("and nombre_subtipo_patrimonio in('{subtipo_patrimonio}') ")
  }
  
  if (!is.null(id_participacion)) {
    
    id_participacion <- str_c(id_participacion, collapse = "' ,'")
    
    id_participacion <- str_glue("and tipo_participacion in('{id_participacion}') ")
  }
  
  url_tail <- "LIMIT 100000000"
  url_total <- str_c(url_head,
                     empresa,
                     patrimonio,
                     subtipo_patrimonio,
                     id_participacion,
                     url_tail)
  
  url_total %>% 
    URLencode() %>% 
    fromJSON() %>% 
    mutate(across((valor_unidad_operaciones:numero_inversionistas), as.numeric),
           fecha_corte = ymd_hms(fecha_corte) %>% 
             ymd()) 
} 

# Test
tictoc::tic()
test_get_fics_price <- get_fics_price()
tictoc::toc()

# Test solo fics
tictoc::tic()
test_get_fics_price_2 <- get_fics_price(subtipo_patrimonio = c("FIC DE TIPO GENERAL", "FIC INMOBILIARIAS", "FIC DE MERCADO MONETARIO", "FIC BURSATILES"))
tictoc::toc()

# Test solo fics + solo fiduciarias
tictoc::tic()
test_get_fics_price_3 <- get_fics_price(subtipo_patrimonio = c("FIC DE TIPO GENERAL", "FIC INMOBILIARIAS", "FIC DE MERCADO MONETARIO", "FIC BURSATILES"),
                                        tipo_empresa = "SF-SOCIEDAD FIDUCIARIA")
tictoc::toc()

# Test solo fics + solo fiduciarias + alianza
tictoc::tic()
test_get_fics_price_4 <- get_fics_price(subtipo_patrimonio = c("FIC DE TIPO GENERAL", "FIC INMOBILIARIAS", "FIC DE MERCADO MONETARIO", "FIC BURSATILES"),
                                        tipo_empresa = "SF-SOCIEDAD FIDUCIARIA",
                                        empresa = "Alianza Fiduciaria S.A.")
tictoc::toc()

# Test solo fics + solo fiduciarias + alianza + sentencias I
tictoc::tic()
test_get_fics_price_5 <- get_fics_price(subtipo_patrimonio = c("FIC DE TIPO GENERAL", "FIC INMOBILIARIAS", "FIC DE MERCADO MONETARIO", "FIC BURSATILES"),
                                        tipo_empresa = "SF-SOCIEDAD FIDUCIARIA",
                                        empresa = "Alianza Fiduciaria S.A.",
                                        patrimonio = "FONDO DE INVERSIÓN COLECTIVA CERRADO SENTENCIAS NACIÓN ALIANZA")
tictoc::toc()

# Test fondo descontinuadoI
tictoc::tic()
test_get_fics_price_6 <- get_fics_price(patrimonio = "FIC SURA ACCIONES COLOMBIA")
tictoc::toc()




# fund performance ----
# Se le adiciona el valor de la volatilidad calculado para 365 días
# Notas, NO calcular la rentabilidad, utilizar la que trae los datos (modificar get fics price para incluir las rentabilidades)

fics_performance <- function(
  data){
  
  data %>% 
    group_by(nombre_patrimonio, tipo_participacion) %>% 
    arrange(nombre_patrimonio, fecha_corte) %>% 
    mutate(percent_dif = valor_unidad_operaciones/lag(valor_unidad_operaciones)-1) %>% 
    mutate(vol_mensual = slidify_vec(percent_dif, sd, .period = 30, .align = "right", .partial = TRUE)*sqrt(365),
           vol_semestral = slidify_vec(percent_dif, sd, .period = 180, .align = "right", .partial = TRUE)*sqrt(365),
           vol_anual = slidify_vec(percent_dif, sd, .period = 365, .align = "right", .partial = TRUE)*sqrt(365),
           rent_mesual = ((valor_unidad_operaciones/lag(valor_unidad_operaciones, n = 30))^(365/30))-1,
           rent_semestral = ((valor_unidad_operaciones/lag(valor_unidad_operaciones, n = 182))^(365/182))-1,
           rent_anual = valor_unidad_operaciones/lag(valor_unidad_operaciones, n = 365)-1) %>% 
    ungroup() 
  }

test_fics_performance <- fics_performance(test_get_fics_price_5)

# fics vigentes ----
# Trae aquellas participaciones por fondo que tubieron por lo menos un registro el mes inmediatamente anterior, el objetivo es poder filtra aquellas participaciones que por su inactuvidad posiblemente ya no se encuentren activas
# Nota, revisar si por demoras en la actualización de los datos es mejor usar los ultímos 2 meses mejor

fics_vigentes <- function(data){
  data %>%
    mutate(max_last_month = (today() %m-% months(1)) %>% 
             floor_date("month")) %>% 
    group_by(nombre_patrimonio, tipo_participacion) %>% 
    summarise(max_last_month = max(max_last_month),
              max_fecha_corte = max(fecha_corte)) %>% 
    ungroup() %>% 
    filter(max_fecha_corte >= max_last_month) %>% 
    select(nombre_patrimonio, tipo_participacion) %>% 
    inner_join(data, by = c("nombre_patrimonio", "tipo_participacion"))
}

test_fics_vigentes <- fics_vigentes(test_fics_performance)

# median_fics ----

median_fics <- function(data){
  data %>% 
    group_by(nombre_patrimonio, nombre_entidad, nombre_subtipo_patrimonio, nombre_tipo_entidad) %>% 
    filter(fecha_corte == max(fecha_corte)) %>% 
    filter(numero_inversionistas == max(numero_inversionistas)) %>% 
    distinct(numero_inversionistas, .keep_all = TRUE) %>% 
    ungroup() %>%  
    select(nombre_patrimonio, tipo_participacion)
}

test_median_fics <- median_fics(test_fics_vigentes)

# price plot ----
# Test de graficas, solo preliminar mientras se crean las definitivas

(test_fics_vigentes %>% 
   filter(tipo_participacion == "501") %>% 
   ggplot(aes(fecha_corte, valor_unidad_operaciones)) +
   geom_line()) %>%
  ggplotly()

(test_fics_vigentes %>% 
    filter(tipo_participacion == "501") %>% 
    ggplot(aes(fecha_corte, percent_dif)) +
    geom_line()) %>% 
  ggplotly()

g <- test_fics_performance %>% 
  filter(nombre_subtipo_patrimonio != "FONDOS DE CAPITAL PRIVADO") %>% 
  group_by(fecha_corte, nombre_entidad, nombre_tipo_entidad, nombre_patrimonio, nombre_subtipo_patrimonio) %>% 
  filter(numero_inversionistas == max(numero_inversionistas)) %>% 
  distinct(numero_inversionistas, .keep_all = TRUE) %>% 
  ungroup() %>% 
  filter(!is.na(rent_mesual)) %>% 
  group_by(nombre_patrimonio, tipo_participacion) %>% 
  filter(floor_date(max(test_fics_performance$fecha_corte), unit = "month") == floor_date(max(fecha_corte), unit = "month")) %>% 
  ungroup() %>% 
  filter(fecha_corte == max(fecha_corte)) %>% 
  filter(rent_anual > 0) %>% 
  filter((rent_anual-vol_anual) > 0.1043) %>% 
  filter(rent_anual > vol_anual) %>% 
  ggplot(aes(vol_anual, rent_anual, col = nombre_entidad, size = rent_anual-vol_anual, group = nombre_patrimonio)) +
  geom_point()

plotly::ggplotly(g, dynamicTicks = TRUE)  



