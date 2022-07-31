options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)

# get fics ----

get_fics <- function(
    empresa = NULL,
    patrimonio = NULL,
    tipo_empresa = NULL,
    subtipo_patrimonio = NULL,
    from = today() - years(2),
    to = today()) {
      
      # Transformanos str a date
      from <- ymd(from)
      to <- ymd(to)
      
      # Estructura url
      url_head <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT fecha_corte, nombre_entidad, nombre_tipo_entidad,  nombre_patrimonio, nombre_subtipo_patrimonio, sum(numero_unidades_fondo_cierre), sum(precierre_fondo_dia_t), sum(valor_fondo_cierre_dia_t), sum(numero_inversionistas), sum(aportes_recibidos), sum(retiros_redenciones), sum(anulaciones) where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' ")
      
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
      
      url_tail <- "group by fecha_corte, nombre_entidad, nombre_tipo_entidad, nombre_patrimonio, nombre_subtipo_patrimonio LIMIT 100000000"
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

# get fpvs ----

get_fpvs <- function(
  empresa = NULL,
  patrimonio = NULL,
  tipo_empresa = NULL,
  subtipo_patrimonio = NULL,
  from = today() - years(2),
  to = today()) {
  
  # Transformanos str a date
  from <- ymd(from)
  to <- ymd(to)
  
  # Nombres fondos vigentes
  fpv_names <- c(
    "F.V.P. DAFUTURO",                                                               
    "FDO VOLUNT DE PENSIONES MULTIFIND",                                            
    "FDO VOLUNTARIO PENSIONES COLSEGUROS",          
    "FDO,  DE PENSIO. PROTECCI. VOLUNTAR",             
    "FDO.PENS.VOL.CLASS INVERSIONES",                        
    "FONDO DE PENSIONES DE JUBILACION E INVALIDEZ VISIÓN",           
    "FONDO DE PENSIONES VOLUNTARIAS MULTIACCION",                               
    "FONDO DE PENSIONES VOLUNTARIAS PLATINO",                              
    "FONDO PENSIONES ESMURFIT VOLUNTA",                                     
    "FONDO VOLUNTARIO DE PENSIÓN BTG PACTUAL",                                 
    "FONDO VOLUNTARIO DE PENSION MULTIOPCION",                                   
    "FONDO VOLUNTARIO DE PENSION PORVENIR",                                    
    "FONDO VOLUNTARIO DE PENSIONES DE JUBILACION E INVALIDEZ RENTA4GLOBAL FIDUCIARIA",
    "FONDO VOLUNTARIO DE PENSIONES GNB",                                             
    "FONDO VOLUNTARIO PENSIONES JUBILACION INVALIDEZ CORREVAL",                       
    "FONDOS DE PENSIONES VOLUNTARIAS"
  ) %>%
    str_c(collapse = "', '")
  
  # Estructura url
  url_head <- str_glue("https://www.datos.gov.co/resource/gpzw-wmxd.json?$query=SELECT fecha_corte, nombre_entidad, nombre_tipo_entidad,  nombre_patrimonio, nombre_subtipo_patrimonio, valor_unidad_operaciones, precio_cierre_fondo_dia_t, valor_fondo_cierre_dia_t_2, aportes_traslados_recibidos,   traslados_aportes_valor_pesos, mesada_pensionales_valor, retiros_aportes_dif_mesada, otras_comisiones_valor_pesos, traslados_aseguradoras_rentas, otros_retiros_valor_pesos, anulaciones_valor_pesos where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' and nombre_patrimonio in('{fpv_names}') ")
  
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
  
  url_tail <- "LIMIT 100000000"
  url_total <- str_c(url_head,
                     empresa,
                     patrimonio,
                     subtipo_patrimonio, 
                     url_tail) %>% 
    URLencode()
  
  # A data frame
  fromJSON(url_total) %>% 
    mutate(across(valor_unidad_operaciones:anulaciones_valor_pesos, as.numeric),
           fecha_corte = ymd_hms(fecha_corte) %>% 
             ymd(),
           numero_inversionistas = as.numeric(NA),
           aportes_recibidos = aportes_traslados_recibidos,
           retiros_redenciones = traslados_aportes_valor_pesos + 
             mesada_pensionales_valor + 
             retiros_aportes_dif_mesada +
             otras_comisiones_valor_pesos +
             traslados_aseguradoras_rentas +
             otros_retiros_valor_pesos,
           anulaciones = -anulaciones_valor_pesos) %>% 
    select(fecha_corte,
           nombre_entidad,
           nombre_tipo_entidad,
           nombre_subtipo_patrimonio,
           nombre_patrimonio,
           valor_unidad_operaciones,
           precio_cierre_fondo_dia_t,
           valor_fondo_cierre_dia_t_2,
           numero_inversionistas,
           aportes_recibidos,
           retiros_redenciones,
           anulaciones) %>% 
    rename(numero_unidades_fondo_cierre = valor_unidad_operaciones,
           precierre_fondo_dia_t = precio_cierre_fondo_dia_t,
           valor_fondo_cierre_dia_t = valor_fondo_cierre_dia_t_2) %>% 
    arrange(fecha_corte,
            nombre_entidad,
            nombre_subtipo_patrimonio,
            nombre_patrimonio
    )
}

test_get_fpvs <- get_fpvs(empresa            = "Alianza Fiduciaria S.A.",
                          from               = "2022-07-01", 
                          to                 = "2022-07-31")

# get fpvs names ----

get_fpvs_names <- function(
  .nombre_entidad = ".*",
  .nombre_tipo_entidad = ".*", 
  .nombre_subtipo_patrimonio =  NA,
  .nombre_patrimonio =  ".*",
  .select = c("nombre_entidad", "nombre_tipo_entidad", "nombre_subtipo_patrimonio", "nombre_patrimonio", "max_fecha_corte")) {
  
  # Nombres fondos vigentes
  fpv_names <- c(
    "F.V.P. DAFUTURO",                                                               
    "FDO VOLUNT DE PENSIONES MULTIFIND",                                            
    "FDO VOLUNTARIO PENSIONES COLSEGUROS",          
    "FDO,  DE PENSIO. PROTECCI. VOLUNTAR",             
    "FDO.PENS.VOL.CLASS INVERSIONES",                        
    "FONDO DE PENSIONES DE JUBILACION E INVALIDEZ VISIÓN",           
    "FONDO DE PENSIONES VOLUNTARIAS MULTIACCION",                               
    "FONDO DE PENSIONES VOLUNTARIAS PLATINO",                              
    "FONDO PENSIONES ESMURFIT VOLUNTA",                                     
    "FONDO VOLUNTARIO DE PENSIÓN BTG PACTUAL",                                 
    "FONDO VOLUNTARIO DE PENSION MULTIOPCION",                                   
    "FONDO VOLUNTARIO DE PENSION PORVENIR",                                    
    "FONDO VOLUNTARIO DE PENSIONES DE JUBILACION E INVALIDEZ RENTA4GLOBAL FIDUCIARIA",
    "FONDO VOLUNTARIO DE PENSIONES GNB",                                             
    "FONDO VOLUNTARIO PENSIONES JUBILACION INVALIDEZ CORREVAL",                       
    "FONDOS DE PENSIONES VOLUNTARIAS"
  ) %>%
    str_c(collapse = "', '")
  
  # url
  names_tbl <- str_glue("https://www.datos.gov.co/resource/gpzw-wmxd.json?$query=SELECT nombre_entidad, nombre_tipo_entidad, nombre_subtipo_patrimonio, nombre_patrimonio, max(fecha_corte) where nombre_patrimonio in('{fpv_names}') group by nombre_entidad, nombre_tipo_entidad, nombre_subtipo_patrimonio, nombre_patrimonio LIMIT 100000000") %>% 
    URLencode() %>% 
    fromJSON() %>% 
    mutate(max_fecha_corte = ymd_hms(max_fecha_corte) %>% 
             ymd()) %>% 
    arrange(nombre_entidad, 
            nombre_subtipo_patrimonio, 
            nombre_patrimonio) %>% 
    
    # Filtro
    filter(str_detect(str_to_lower(nombre_entidad), str_c(.nombre_entidad, collapse = "|") %>%
                        str_to_lower())) %>% 
    filter(str_detect(str_to_lower(nombre_tipo_entidad), str_c(.nombre_tipo_entidad, collapse = "|") %>%
                        str_to_lower())) %>% 
    filter(str_detect(str_to_lower(nombre_patrimonio), str_c(.nombre_patrimonio, collapse = "|") %>%
                        str_to_lower())) 
  
  # Filtro patrimonio por aparte ya que algunos valores contienen caracteres que no se pueden escapar 
  if(all(!is.na(.nombre_subtipo_patrimonio))){
    names_tbl <- names_tbl %>% 
      filter(str_to_lower(nombre_subtipo_patrimonio) %in% str_to_lower(.nombre_subtipo_patrimonio))
    }
  
  
    # seleccionar
  names_tbl %>% 
    select(all_of(.select)) %>% 
    distinct() %>% 
    arrange_all()
  
}

test_get_fpvs_names <- get_fpvs_names(.nombre_entidad = c("Alianza Fiduciaria S.A.", "Allianz Seguros De Vida S.A."),
  .nombre_subtipo_patrimonio = c("FDOS DE PENSIONES(JUBIL-INVAL)","VOLUNTARIOS"),
  .nombre_patrimonio = c("FDO VOLUNTARIO PENSIONES COLSEGUROS", "FONDO DE PENSIONES DE JUBILACION E INVALIDEZ VISIÓN")
  )


# get fics fpv names ----

get_fics_fpvs_names <- function(
  .nombre_entidad = NA,
  .nombre_tipo_entidad = NA,
  .nombre_subtipo_patrimonio =  NA,
  .nombre_patrimonio =  NA,
  .select = c("nombre_entidad", "nombre_tipo_entidad", "nombre_subtipo_patrimonio", "nombre_patrimonio", "max_fecha_corte")) {
  
  fics_fpv_names <- get_fics_names() %>% 
    bind_rows(get_fpvs_names())
  
  if(all(!is.na(.nombre_entidad))){
    fics_fpv_names <- names_tbl %>% 
      filter(str_to_lower(nombre_entidad) %in% str_to_lower(.nombre_entidad))
    }
  
  if(all(!is.na(.nombre_tipo_entidad))){
    fics_fpv_names <- names_tbl %>% 
      filter(str_to_lower(nombre_tipo_entidad) %in% str_to_lower(.nombre_tipo_entidad))
    }
  
  if(all(!is.na(.nombre_subtipo_patrimonio))){
    fics_fpv_names <- names_tbl %>% 
      filter(str_to_lower(nombre_subtipo_patrimonio) %in% str_to_lower(.nombre_subtipo_patrimonio))
    }
  if(all(!is.na(.nombre_patrimonio))){
    fics_fpv_names <- names_tbl %>% 
      filter(str_to_lower(nombre_patrimonio) %in% str_to_lower(.nombre_patrimonio))
    }
  
  fics_fpv_names %>% 
    select(all_of(.select)) %>% 
    distinct() %>% 
    arrange_all()
  
  }

tictoc::tic()
get_fics_fpvs_names() 
tictoc::toc()
