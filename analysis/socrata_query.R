options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)

# get fics ----

get_fics <- function(
    empresa = NULL,
    patrimonio = NULL,
    subtipo_patrimonio = NULL,
    from = today() - years(2),
    to = today()) {
      
      # Transformanos str a date
      from <- ymd(from)
      to <- ymd(to)
      
      # Estructura url
      url_head <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT fecha_corte, nombre_entidad, nombre_patrimonio, nombre_subtipo_patrimonio, sum(numero_unidades_fondo_cierre), sum(precierre_fondo_dia_t), sum(valor_fondo_cierre_dia_t), sum(numero_inversionistas), sum(aportes_recibidos), sum(retiros_redenciones), sum(anulaciones) where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' and tipo_entidad='5' ")
      
      if (!is.null(empresa)) {
          
          empresa <- str_c(empresa, collapse = "' ,'")
          
          empresa <- str_glue("and nombre_entidad in('{empresa}') ")
        }
      
      if (!is.null(patrimonio)) {
          
          patrimonio <- str_c(patrimonio, collapse = "' ,'")
          
          patrimonio <- str_glue("and nombre_patrimonio in('{patrimonio}') ")
        }
      
      if (!is.null(subtipo_patrimonio)) {
          
          subtipo_patrimonio <- str_c(subtipo_patrimonio, collapse = "' ,'")
          
          subtipo_patrimonio <- str_glue("and nombre_subtipo_patrimonio in('{subtipo_patrimonio}') ")
        }
      
      url_tail <- "group by fecha_corte, nombre_entidad, nombre_patrimonio, nombre_subtipo_patrimonio LIMIT 100000000"
      url_total <- str_c(url_head,
                         empresa,
                         patrimonio,
                         subtipo_patrimonio, 
                         url_tail) %>% 
          URLencode()
      
      # A data frame
      fromJSON(url_total) %>% 
          mutate(across(starts_with("sum_"), as.numeric),
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
test_get_fics <- get_fics(empresa            = "Alianza Fiduciaria S.A.", 
                 subtipo_patrimonio = c("FIC DE TIPO GENERAL", 
                                        "FIC INMOBILIARIAS"), 
                 from               = "2022-07-01", 
                 to                 = "2022-07-31")

# get fics names ----

get_fics_names <- function(
  .nombre_entidad = ".*",
  .nombre_subtipo_patrimonio =  ".*",
  .nombre_patrimonio =  ".*",
  .select = c("nombre_entidad", "nombre_subtipo_patrimonio", "nombre_patrimonio", "max_fecha_corte")) {
  
    # url
    URLencode("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT nombre_entidad, nombre_subtipo_patrimonio, nombre_patrimonio, max(fecha_corte) group by nombre_entidad, nombre_subtipo_patrimonio, nombre_patrimonio LIMIT 100000000") %>% 
      fromJSON() %>% 
      mutate(max_fecha_corte = ymd_hms(max_fecha_corte) %>% 
               ymd()) %>% 
      arrange(nombre_entidad, 
              nombre_subtipo_patrimonio, 
              nombre_patrimonio) %>% 
    
      # Filtro
      filter(str_detect(str_to_lower(nombre_entidad), str_c(.nombre_entidad, collapse = "|") %>%
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
                                      .select = c("nombre_entidad", "nombre_patrimonio", "max_fecha_corte"))

# get fpv ----

get_fpvs <- function(
  empresa = NULL,
  patrimonio = NULL,
  subtipo_patrimonio = NULL,
  from = today() - years(2),
  to = today()) {
  
  # Transformanos str a date
  from <- ymd(from)
  to <- ymd(to)
  
  # Nombres fondos vigentes
  fpv_names <- c(
    "FONDO DE PENSIONES DE JUBILACION E INVALIDEZ VISIÓN", #
    "FONDO VOLUNTARIO DE PENSION MULTIOPCION", 
    "FONDO DE PENSIONES VOLUNTARIAS MULTIACCION", 
    "FONDO VOLUNTARIO DE PENSIONES GNB", 
    "F.V.P. DAFUTURO", 
    "FONDO VOLUNTARIO PENSIONES JUBILACION INVALIDEZ CORREVAL",
    "FONDO VOLUNTARIO DE PENSIÓN BTG PACTUAL", 
    "FONDO DE PENSIONES VOLUNTARIAS PLATINO", 
    "FONDO VOLUNTARIO DE PENSIONES DE JUBILACION E INVALIDEZ RENTA4GLOBAL FIDUCIARIA", 
    "FDO VOLUNTARIO PENSIONES COLSEGUROS",
    "FDO.PENS.VOL.CLASS INVERSIONES",
    "FONDO PENSIONES ESMURFIT VOLUNTA",
    "FDO,  DE PENSIO. PROTECCI. VOLUNTAR", 
    "FONDO VOLUNTARIO DE PENSION PORVENIR",
    "FDO VOLUNT DE PENSIONES MULTIFIND"
  ) %>%
    str_c(collapse = "', '")
  
  # Estructura url
  url_head <- str_glue("https://www.datos.gov.co/resource/gpzw-wmxd.json?$query=SELECT fecha_corte, nombre_entidad, nombre_patrimonio, nombre_subtipo_patrimonio, sum(valor_unidad_operaciones), sum(precio_cierre_fondo_dia_t), sum(valor_fondo_cierre_dia_t_2) where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' and nombre_patrimonio in('{fpv_names}') ")
  
  if (!is.null(empresa)) {
    
    empresa <- str_c(empresa, collapse = "' ,'")
    
    empresa <- str_glue("and nombre_entidad in('{empresa}') ")
  }
  
  if (!is.null(patrimonio)) {
    
    patrimonio <- str_c(patrimonio, collapse = "' ,'")
    
    patrimonio <- str_glue("and nombre_patrimonio in('{patrimonio}') ")
  }
  
  if (!is.null(subtipo_patrimonio)) {
    
    subtipo_patrimonio <- str_c(subtipo_patrimonio, collapse = "' ,'")
    
    subtipo_patrimonio <- str_glue("and nombre_subtipo_patrimonio in('{subtipo_patrimonio}') ")
  }
  
  url_tail <- "group by fecha_corte, nombre_entidad, nombre_patrimonio, nombre_subtipo_patrimonio LIMIT 100000000"
  url_total <- str_c(url_head,
                     empresa,
                     patrimonio,
                     subtipo_patrimonio, 
                     url_tail) %>% 
    URLencode()
  
  # A data frame
  fromJSON(url_total) %>% 
    mutate(across(starts_with("sum_"), as.numeric),
           fecha_corte = ymd_hms(fecha_corte) %>% 
             ymd()) %>%  
    rename_with(~ str_remove(.x, "^sum_")) %>% 
    arrange(fecha_corte,
            nombre_entidad,
            nombre_subtipo_patrimonio,
            nombre_patrimonio
    )
}

test_get_fpvs <- get_fpvs(empresa            = "Alianza Fiduciaria S.A.",
                          from               = "2022-07-01", 
                          to                 = "2022-07-31")





