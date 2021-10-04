
# Procesamiento de datos práctico 07 --------------------------------------

pacman::p_load(tidyverse,
               dplyr, 
               car,
               haven,
               sjmisc,
               sjlabelled)


# 1. Cargar datos ---------------------------------------------------------

casen <- read_dta("input/data/Casen en Pandemia 2020 STATA.dta")

# 2. Seleccionar y renombrar variables ------------------------------------

data <- casen %>% 
  select(exp = expr, #Ponderador regional
         varunit, #Conglomerado de varianza
         varstrat, #Estrato de varianza
         region, #Región
         pobreza, #Situación de pobreza por ingresos
         ing_tot_hog = ytoth, #Ingreso total del hogar
         sexo)
           

# 3. Recodificación de variables ------------------------------------------

data <- data %>% 
  mutate_all(~as.numeric(.)) %>% 
  mutate(pobreza = car::recode(.$pobreza,
                               recodes = c("1 = 'Pobres extremos';
                                           2 = 'Pobres no extremos';
                                           3 = 'No pobres'"),
                               as.factor = T, 
                               levels = c('Pobres extremos',
                                          'Pobres no extremos',
                                          'No pobres')),
         region = car::recode(.$region,
                              recodes = c("1 = 'Tarapacá';
                                          2 = 'Antofagasta';
                                          3 = 'Atacama';
                                          4 = 'Coquimbo';
                                          5 = 'Valparaíso';
                                          6 = 'OHiggins';
                                          7 = 'Maule';
                                          8 = 'Biobío';
                                          9 = 'La Araucanía';
                                          10 = 'Los Lagos';
                                          11 = 'Aysén';
                                          12 = 'Magallanes';
                                          13 = 'Metropolitana';
                                          14 = 'Los Ríos';
                                          15 = 'Arica y Parinacota';
                                          16 = 'Ñuble'"),
                              as.factor = T, 
                              levels = c('Tarapacá',
                                         'Antofagasta',
                                         'Atacama',
                                         'Coquimbo',
                                         'Valparaíso',
                                         'OHiggins',
                                         'Maule',
                                         'Biobío',
                                         'La Araucanía',
                                         'Los Lagos',
                                         'Aysén',
                                         'Magallanes',
                                         'Metropolitana',
                                         'Los Ríos',
                                         'Arica y Parinacota',
                                         'Ñuble')),
         sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"),
                            as.factor = T, levels = c("Hombre", "Mujer"))) %>% 
  na.omit()

# 4. Etiquetado de variables ----------------------------------------------

data$exp <- set_label(data$exp, 'Ponderador regional')
data$varunit <- set_label(data$varunit, 'Conglomerado de varianza')
data$varstrat <- set_label(data$varstrat, 'Estrato de varianza')
data$region <- set_label(data$region, 'Región')
data$pobreza <- set_label(data$pobreza, 'Situación de pobreza por ingresos')
data$ing_tot_hog <- set_label(data$ing_tot_hog, 'Ingreso total del hogar')
data$sexo <- set_label(data$sexo, 'Sexo')


# 5. Exportar datos -------------------------------------------------------

saveRDS(data, "input/data/casen_proc.rds")

