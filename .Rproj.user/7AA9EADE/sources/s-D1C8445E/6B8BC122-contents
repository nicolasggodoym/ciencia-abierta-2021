
# 0. Cargar paquetes ------------------------------------------------------



pacman::p_load(tidyverse, #Muchas cosas
               haven, #Cargar datos 
               sjPlot, #Crear tablas y gráficos
               sjmisc,
               dplyr,
               sjlabelled) #Explorar datos


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
  mutate(horas = as.numeric(.$horas)) %>% 
  mutate(ing_int = case_when(ingresos <= 291974 ~ "$0 a $291.974",
                             ingresos > 291974 & ingresos <= 380500 ~ "$291.975 a $380.500",
                             ingresos > 380500 & ingresos <= 500000 ~ "$380.501 a $500.000",
                             ingresos > 500000 & ingresos <= 779787 ~ "$500.001 a $779.787",
                             ingresos > 779787 ~ "$779.788 o más")) %>% 
  mutate(ing_int = factor(.$ing_int, levels = c("$0 a $291.974",
                                                "$291.975 a $380.500",
                                                "$380.501 a $500.000",
                                                "$500.001 a $779.787",
                                                "$779.788 o más")))


# 3. Generar tablas descriptivas ------------------------------------------


sjPlot::view_df(data_proc)  
  
sexo_educ <- select(data_proc, sexo, educacion)

sjPlot::view_df(sexo_educ,
                encoding = "Latin-1",
                file = "output/tablas/sexo_educ.doc") 

inseguridad <- select(data_proc, 
                      prev_salud,
                      prev_pensiones, 
                      cotiza_prev, 
                      cotiza_salud, 
                      cotiza_seguro)
sjPlot::view_df(inseguridad,
                encoding = "Latin-1",
                file = "output/tablas/inseguridad.doc")

insuficiencia <- select(data_proc, ing_int)

insuficiencia$ing_int <- sjlabelled::set_label(insuficiencia$ing_int, "Quintiles de ingreso")

sjPlot::view_df(insuficiencia,
                file = "output/tablas/insuficiencia.doc")
