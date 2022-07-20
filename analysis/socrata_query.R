options(scipen = 999)

library(tidyverse)
library(lubridate)
library(jsonlite)

# Socrata paths

url_fics <- "https://www.datos.gov.co/resource/qhpu-8ixx.json?$select="
campos <- "fecha_corte, tipo_entidad, codigo_entidad, nombre_entidad, codigo_negocio, nombre_patrimonio, nombre_subtipo_patrimonio, numero_unidades_fondo_cierre, valor_fondo_cierre_dia_t, numero_inversionistas "
rango_fechas <- str_glue("where fecha_corte between '2020-05-31T00:00:00.000' and '2022-05-31T00:00:00.000' ")
empresa <- "and nombre_entidad='Alianza Fiduciaria S.A.' "
agrupado <- "group by fecha_corte, tipo_entidad, codigo_entidad, nombre_entidad, codigo_negocio, nombre_patrimonio, nombre_subtipo_patrimonio"
path_socrata <- str_c(url_fics, campos, rango_fechas, empresa) %>% URLencode()

tictoc::tic()
fics_data_tbl <- jsonlite::fromJSON(URLencode("https://www.datos.gov.co/resource/qhpu-8ixx.json?$query=SELECT fecha_corte, tipo_entidad, codigo_entidad, nombre_entidad, codigo_negocio, nombre_patrimonio, nombre_subtipo_patrimonio, sum(numero_unidades_fondo_cierre), sum(valor_fondo_cierre_dia_t), sum(numero_inversionistas) where fecha_corte between '2020-05-31T00:00:00.000' and '2022-05-31T00:00:00.000' and nombre_entidad='Alianza Fiduciaria S.A.' group by fecha_corte, tipo_entidad, codigo_entidad, nombre_entidad, codigo_negocio, nombre_patrimonio, nombre_subtipo_patrimonio LIMIT 10000000"))%>% type_convert()
tictoc::toc()