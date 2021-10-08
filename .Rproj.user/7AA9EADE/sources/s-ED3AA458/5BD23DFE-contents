
# Código de procesamiento  ------------------------------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(tidyverse, #Muchas cosas
               haven, #Cargar datos.dta
               sjPlot, #Crear tablas y gráficos
               sjmisc,#Explorar datos
               dplyr, #Manipular datos
               sjlabelled, #Etiquetar datos
               car) #Recodificar


# 2. Cargar datos ---------------------------------------------------------

data <- read_dta("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/stata_esi/2020/esi-2020---personas.dta?sfvrsn=7a4b0e2b_4&amp;download=true")


# 3. Procesar datos -------------------------------------------------------


## a) Seleccionar columnas y filtrar filas ---------------------------------

data_proc <- data %>% 
  filter(b13_rev4cl_caenes != 1 | 
           b13_rev4cl_caenes != 2 |
           b13_rev4cl_caenes != 3 |
           b13_rev4cl_caenes != 6) %>% 
  select(exp = fact_cal_esi,
         id = idrph,
         contrato = b8,
         contrato_duracion = b9,
         contrato_sub = b12,
         cotiza_prev = b7a_1,
         cotiza_salud = b7a_2,
         cotiza_seguro = b7a_3,
         ingresos = ing_t_p,
         lugar_trab = b16,
         cond_vacaciones = b7b_1,
         cond_enfermedad = b7b_2,
         cond_maternidad = b7b_3,
         cond_guarderia = b7b_4,
         horas = c2_1_3,
         sexo,
         edad,
         educacion = cine)

## b) Recodificar datos ----------------------------------------------------

#Calcular cuantiles para recodificar ingresos
quantile(data_proc$ingresos, c(.2,.4,.6,.8))

data_proc <- data_proc %>% 
  mutate_at(vars(-c(exp, id, edad)), ~(as.numeric(.))) %>%  
  mutate(ingresos = case_when(ingresos <= 291974 ~ "$0 a $291.974",
                              ingresos > 291974 & ingresos <= 380500 ~ "$291.975 a $380.500",
                              ingresos > 380500 & ingresos <= 500000 ~ "$380.501 a $500.000",
                              ingresos > 500000 & ingresos <= 779787 ~ "$500.001 a $779.787",
                              ingresos > 779787 ~ "$779.788 o mas", 
                              TRUE ~ NA_character_),
         horas = case_when(horas <= 30 ~ "Igual o menor a 30 horas semanales",
                           horas > 30 & horas <= 45 ~ "Entre 31 y 45 horas semanales",
                           horas > 45 ~ "Mayor a 45 horas semanales", 
                           TRUE ~ NA_character_),
         contrato = car::recode(.$contrato, recodes = c("1 = 'Si'; 2 = 'No'"), 
                                as.factor = T, levels = c('Si', 'No')),
         contrato_duracion = car::recode(.$contrato_duracion, recodes = c("1 = 'Definido'; 2 = 'Indefinido'"),
                                         as.factor = T, levels = c('Definido', 'Indefinido')),
         contrato_sub = car::recode(.$contrato_sub, recodes = c("1 = 'Directamente con la empresa en donde trabaja'; 
                                                                2 = 'Con un contratista o subcontratista de bienes y servicios'; 
                                                                3 = 'Con una empresa de servicios temporales o suministradora de trabajadores'; 
                                                                4 = 'Con un enganchador (contratista agrícola)'"),
                                    as.factor = T, levels = c('Directamente con la empresa en donde trabaja', 
                                                              'Con un contratista o subcontratista de bienes y servicios', 
                                                              'Con una empresa de servicios temporales o suministradora de trabajadores', 
                                                              'Con un enganchador (contratista agrícola)')),
         lugar_trab = car::recode(.$lugar_trab, recodes = c("c(1,2) = 'En instalaciones, oficina o casa del cliente o empleador';
                                                            c(3,4,5) = 'En instalaciones u oficinas propias o arrendadas, en hogar propio o anexos';
                                                            c(6,9) = 'En la calle o via publica, u otro';
                                                            c(7,8) = 'En obras de construccion, mineras, predios agricolas o similares'"),
                                  as.factor = T, levels = c('En instalaciones, oficina o casa del cliente o empleador',
                                                            'En instalaciones u oficinas propias o arrendadas, en hogar propio o anexos', 
                                                            'En la calle o via publica, u otro', 
                                                            'En obras de construccion, mineras, predios agricolas o similares')),
         sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T,
                            levels = c('Hombre', 'Mujer')),
         educacion = car::recode(.$educacion, recodes = c("1 = 'Nunca estudio';
                                                               2 = 'Educacion preescolar';
                                                               3 = 'Educacion primaria (nivel 1)';
                                                               4 = 'Educacion primaria (nivel 2)';
                                                               5 = 'Educacion secundaria';
                                                               6 = 'Educacion tecnica (superior no universitaria)';
                                                               7 = 'Educacion universitaria';
                                                               8 = 'Postitulos y maestria';
                                                               9 = 'Doctorado'"),
                                 as.factor = T,
                                 levels = c('Nunca estudio',
                                            'Educacion preescolar',
                                            'Educacion primaria (nivel 1)',
                                            'Educacion primaria (nivel 2)',
                                            'Educacion secundaria',
                                            'Educacion tecnica (superior no universitaria)',
                                            'Educacion universitaria',
                                            'Postitulos y maestria',
                                            'Doctorado'))) %>% 
  mutate_at(vars("cond_vacaciones", "cond_enfermedad", "cond_maternidad", "cond_guarderia", "cotiza_prev", "cotiza_salud", "cotiza_seguro"), ~(car::recode(., recodes = c("1 = 'Si'; 2 = 'No'"), as.factor = T))) %>%
  mutate(ingresos = factor(.$ingresos, levels = c("$0 a $291.974",
                                                  "$291.975 a $380.500",
                                                  "$380.501 a $500.000",
                                                  "$500.001 a $779.787",
                                                  "$779.788 o más")),
         horas = factor(.$horas, levels = c("Igual o menor a 30 horas semanales",
                                            "Entre 31 y 45 horas semanales",
                                            "Mayor a 45 horas semanales"))) %>% 
  mutate_at(vars("contrato", "contrato_duracion", "contrato_sub", "cotiza_prev", "cotiza_salud", "cotiza_seguro", "lugar_trab", "cond_vacaciones", "cond_enfermedad", "cond_maternidad", "cond_guarderia"), ~(car::recode(., recodes = "c(77, 88, 99) = NA"))) %>%
  mutate_at(vars(c("horas", "educacion")), ~(car::recode(., recodes = "c(888, 999) = NA"))) 

# Revisar estructura
str(data_proc)


## c) Eliminar casos perdidos ----------------------------------------------

sum(is.na(data_proc))
dim(data_proc)

data_proc <-na.omit(data_proc)
dim(data_proc)


# 4. Etiquetar variables --------------------------------------------------

#(In)estabilidad
data_proc$contrato = set_label(data_proc$contrato, "Existencia de contrato")
data_proc$contrato_duracion = set_label(data_proc$contrato_duracion, "Duracion de contrato")
data_proc$contrato_sub = set_label(data_proc$contrato_sub, "Acuerdo de trabajo")

#(In)seguridad
data_proc$cotiza_prev = set_label(data_proc$cotiza_prev, "Cotizacion en prevision")
data_proc$cotiza_salud = set_label(data_proc$cotiza_salud, "Cotizacion en salud")
data_proc$cotiza_seguro = set_label(data_proc$cotiza_seguro, "Cotizacion en seguro de desempleo")

#(In)suficiencia
data_proc$ingresos = set_label(data_proc$ingresos, "Quintiles de ingreso")

#Condiciones de trabajo
data_proc$lugar_trab = set_label(data_proc$lugar_trab, "Lugar de trabajo")
data_proc$cond_vacaciones = set_label(data_proc$cond_vacaciones, "Derecho a vacaciones")
data_proc$cond_enfermedad = set_label(data_proc$cond_enfermedad, "Derecho a dias pagos por enfermedad")
data_proc$cond_maternidad = set_label(data_proc$cond_maternidad, "Derecho a dias pagos por pa/maternidad")
data_proc$cond_guarderia = set_label(data_proc$cond_guarderia, "Derecho a guarderia infantil")

#Cronopiedad
data_proc$horas <- set_label(data_proc$horas, 'Total horas semanales habituales en actividad principal')

#Diseño muestral
data_proc$exp <- set_label(data_proc$exp, 'Ponderador')
data_proc$id <- set_label(data_proc$id, 'Identificador')

#Socio-demográficas
data_proc$sexo <- set_label(data_proc$sexo, 'Sexo')
data_proc$edad <- set_label(data_proc$edad, 'Edad')
data_proc$educacion <- set_label(data_proc$educacion, 'Educacion')


# 5. Crear y exportar tablas resumen --------------------------------------

# (In)estabilidad
inestabilidad <- data_proc %>% 
  select(3:5)

sjPlot::view_df(inestabilidad,
                file = "output/tablas/inestabilidad.doc")
# (In)seguridad

inseguridad <- data_proc %>% 
  select(6:8)

sjPlot::view_df(inseguridad,
                file = "output/tablas/inseguridad.doc")

# (In)suficiencia

insuficiencia <- data_proc %>% 
  select(9)

sjPlot::view_df(insuficiencia,
                file = "output/tablas/insuficiencia.doc")

# Condiciones de trabajo

condiciones <- data_proc %>% 
  select(10:14)

sjPlot::view_df(condiciones,
                file = "output/tablas/condiciones.doc")

# Cronopiedad

cronopiedad <- data_proc %>% 
  select(15)

sjPlot::view_df(cronopiedad,
                file = "output/tablas/cronopiedad.doc")

#Socio-demograficas

demograficas <- data_proc %>% 
  select(16:18)

sjPlot::view_df(demograficas,
                file = "output/tablas/demograficas.doc")

# Limpiar entorno

rm(condiciones, cronopiedad, demograficas, inestabilidad, inseguridad, insuficiencia, data)


# 6. Exportar datos procesados --------------------------------------------

saveRDS(data_proc, file = "input/data/data_proc.rds")


