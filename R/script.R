
# 0. Cargar paquetes ------------------------------------------------------



pacman::p_load(tidyverse, #Muchas cosas
               haven, #Cargar datos 
               sjPlot, #Crear tablas y gráficos
               sjmisc,
               dplyr) #Explorar datos


# 1. Cargar datos ---------------------------------------------------------


datos <- read_dta("input/data/esi-2020---personas.dta")


# 2. Procesar datos -------------------------------------------------------


data_proc <- datos %>% 
  filter(b13_rev4cl_caenes != 1 | 
         b13_rev4cl_caenes != 2 |
         b13_rev4cl_caenes != 3 |
         b13_rev4cl_caenes != 6) %>% 
  select(exp = fact_cal_esi,
         contrato = b8,
         contrato_duracion = b9,
         contrato_sub = b12,
         prev_salud = d6_1_opcion,
         prev_pensiones = d6_2_opcion,
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
         educacion = cine)


# a) Transformar datatype -------------------------------------------------


data_proc <- data_proc %>% 
  mutate_at(vars(-c(exp, ingresos, horas)), ~(as_factor(.))) %>% 
  mutate(horas = as.numeric(.$horas))


# 3. Generar tablas descriptivas ------------------------------------------


sjPlot::view_df(data_proc)  
  
sexo_educ <- select(data_proc, sexo, educacion)

sjPlot::view_df(sexo_educ,
                encoding = "Latin-1",
                file = "output/tablas/sexo_educ.doc") 
