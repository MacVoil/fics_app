options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)

get_fics <- function(
    empresa = NULL,
    patrimonio = NULL,
    subtipo_patrimonio = NULL,
    from = today() - years(2),
    to = today()
    ) {
      
      from <- ymd(from)
      
      to <- ymd(to)
      
      url_head <- str_glue("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT fecha_corte, nombre_entidad, nombre_patrimonio, nombre_subtipo_patrimonio, sum(numero_unidades_fondo_cierre), sum(valor_fondo_cierre_dia_t), sum(numero_inversionistas), sum(aportes_recibidos), sum(retiros_redenciones), sum(anulaciones) where fecha_corte between '{from}T00:00:00.000' and '{to}T00:00:00.000' and tipo_entidad='5' ")
      
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


test <- get_fics(empresa            = "Alianza Fiduciaria S.A.", 
                 subtipo_patrimonio = c("FIC DE TIPO GENERAL", 
                                        "FIC INMOBILIARIAS"), 
                 from               = "2022-07-01", 
                 to                 = "2022-07-31")
