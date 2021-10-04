# Code 14 : Latinobarometro  --------------------------------------------------


# 1. Install packages -----------------------------------------------------
pacman::p_load(tidyverse, rvest, sjlabelled, Hmisc, countrycode, survey, srvyr, sjmisc, haven)

# 2. Load data ------------------------------------------------------------

## File names

file_names=as.list(dir(path ="input/data/lat/", pattern="*.sav"))

file_names = lapply(file_names, function(x) paste0('input/data/lat/', x))

## List of data frames
r <- lapply(file_names, haven::read_sav)

# 2. Proc -----------------------------------------------------------------
names(r) <- gsub("input/data/lat/latino|1995|1996|1997|1998|2000|2001|2002|
                 2003|2004|2005|2006|2007|2008|2009|2010|2011|2013|2015|2016|
                 2017|2018|.sav","", file_names)

r <- Map(function(x, y) {names(x) <- paste0(names(x), '_',  y); x}, r, names(r))

list_lat <- r

rm(list = ls(pattern = "r|file_names"))

# 3. Unlist ---------------------------------------------------------------
list2env(list_lat, envir = .GlobalEnv)


# 0. Apertura provisional de datos -------------------------------------------

latino1995 <- read_sav("input/data/lat/latino1995.sav")
latino1996 <- read_sav("input/data/lat/latino1996.sav")
latino1997 <- read_sav("input/data/lat/latino1997.sav")
latino1998 <- read_sav("input/data/lat/latino1998.sav")
latino2000 <- read_sav("input/data/lat/latino2000.sav")
latino2001 <- read_sav("input/data/lat/latino2001.sav")
latino2002 <- read_sav("input/data/lat/latino2002.sav")
latino2003 <- read_sav("input/data/lat/latino2003.sav")
latino2004 <- read_sav("input/data/lat/latino2004.sav")
latino2005 <- read_sav("input/data/lat/latino2005.sav")
latino2006 <- read_sav("input/data/lat/latino2005.sav")
latino2007 <- read_sav("input/data/lat/latino2006.sav")
latino2008 <- read_sav("input/data/lat/latino2008.sav")
latino2009 <- read_sav("input/data/lat/latino2009.sav")
latino2010 <- read_sav("input/data/lat/latino2010.sav")
latino2011 <- read_sav("input/data/lat/latino2011.sav")
latino2013 <- read_sav("input/data/lat/latino2013.sav")
latino2015 <- read_sav("input/data/lat/latino2015.sav")
latino2016 <- read_sav("input/data/lat/latino2016.sav")
latino2017 <- read_sav("input/data/lat/latino2017.sav")
latino2018 <- read_sav("input/data/lat/latino2018.sav")


# 4. Merge and recode d.f -----------------------------------------------------------
lat <- bind_rows(list(
  # 1995 ----------------------------------------------------------
  (`latino1995` %>% 
     select(iso3c = pais, distr_inc_fair = p19, most_power1 = p61a1, 
            id = enc, weight = wt) %>% 
     mutate(distr_inc_fair = as.numeric(distr_inc_fair),
            most_power1 = as.numeric(most_power1),
            iso3c = as.numeric(iso3c)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 76='BRA';
                                152='CHL'; 484='MEX'; 600='PRY'; 604='PER';
                                858='URY'; 862='VEN'")),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                      1 = 'Very Fair';
                                                                      2 = 'Fair';
                                                                      3 = 'Neither fair nor unfair';
                                                                      4 = 'Unfair';
                                                                      5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4, -3, -2, -1, 2, 7, 9)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 8 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies',
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
                                         
            year = 1995) %>% 
     mutate(iso3c = as.character(iso3c))),
   
  # 1996 ----------------------------------------------------------------
  (`latino1996` %>%
     select(iso3c = pais, trust_union = p33c, trust_private_comp = p33f, 
            trust_comp_asso = p33k, trust_gov = p33m, most_power1 = p63a,
            id = enc, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            most_power1 = as.numeric(most_power1)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>% 
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                            1 = 'A lot of confidence';
                                                            2 = 'Some confidence';
                                                            3 = 'Little confidence';
                                                            4 = 'No confidence at all'"),
                                         as.factor = T, 
                                         levels = c('A lot of confidence',
                                                    'Some confidence',
                                                    'Little confidence',
                                                    'No confidence at all')))) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 218='ECU'; 
                                222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 558='NIC';
                                591 = 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 858='URY'; 862='VEN'")),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1, 2,7,9)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 8 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies',
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
            year = 1996) %>% 
     mutate(iso3c = as.character(iso3c))),
   
  # 1997 ----------------------------------------------------------------
  (`latino1997` %>% select(iso3c = idenpa, distr_inc_fair = nsp20, labor_law_prot = sp83,
                     most_power1 = sp55a, id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            distr_inc_fair = as.numeric(distr_inc_fair),
            labor_law_prot = as.numeric(labor_law_prot),
            most_power1 = as.numeric(most_power1)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 218='ECU'; 
                                222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 558='NIC';
                                591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 858='URY'; 
                                862='VEN'")),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                      1 = 'Very Fair';
                                                                      2 = 'Fair';
                                                                      3 = 'Neither fair nor unfair';
                                                                      4 = 'Unfair';
                                                                      5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            labor_law_prot = car::recode(.$labor_law_prot, recodes = c("-4:-1=NA;
                                                                       1 = 'Very protected';
                                                                       2 = 'Fairly protected';
                                                                       3 = 'A little protected';
                                                                       4 = 'Not at all protected'"),
                                         as.factor = T,
                                         levels = c('Very protected', 'Fairly protected',
                                                    'A little protected', 'Not at all protected')),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1, 2,7,9)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 8 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies',
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
     year = 1997) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 1998 ----------------------------------------------------------------
  (`latino1998` %>% select(iso3c = idenpa, most_power1 = sp51a, 
                           id = numentre, weight = pondera) %>% 
     mutate(iso3c = as.numeric(iso3c),
            most_power1 = as.numeric(most_power1)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1,2,7,9)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 8 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies', 
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
            year = 1998) %>% 
     mutate(iso3c = as.character(iso3c))),
  # 2000 ----------------------------------------------------------------
  (`latino2000` %>% select(iso3c = IDENPA, labor_law_prot = P74ST, most_power1 = P51ST.A,
                     id = NUMENTRE, weight = WT) %>% 
     mutate(iso3c = as.numeric(iso3c),
            labor_law_prot = as.numeric(labor_law_prot),
            most_power1 = as.numeric(labor_law_prot)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
           labor_law_prot = car::recode(.$labor_law_prot, recodes = "c(-4, -3, -2, -1, 16)=NA;
                                        1 = 'Very protected';
                                        2 = 'Fairly protected';
                                        3 = 'A little protected';
                                        4 = 'No at all protected'",
                                          as.factor = T,
                                          levels = c('Very protected', 'Fairly protected',
                                                     'A little protected', 'Not at all protected')),
           most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1,2,7,8,9,10,11)=NA;
                                                                1 = 'Large companies';
                                                                3 = 'Trade unions';
                                                                4 = 'Mass media';
                                                                5 = 'Banks';
                                                                6 = 'The government'"),
                                       as.factor = T,
                                       levels = c('Large companies',
                                                  'Trade unions',
                                                  'Mass media',
                                                  'Banks',
                                                  'The government')),
             year = 2000) %>% 
     mutate(iso3c = as.character(iso3c))),
  # 2001 ----------------------------------------------------------------
  (`latino2001` %>%  
     select(iso3c = idenpa, distr_inc_fair = p11st,  
            trust_private_comp = p63std, most_power1 = p64sta, id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            distr_inc_fair = as.numeric(distr_inc_fair),
            most_power1 = as.numeric(most_power1)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>% 
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                           as.factor = T, 
                                                           levels = c('A lot of confidence',
                                                                      'Some confidence',
                                                                      'Little confidence',
                                                                      'No confidence at all')))) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                         as.factor = T, 
                                         levels = c('Very Fair', 'Fair',
                                                    'Neither fair nor unfair',
                                                    'Unfair', 'Very Unfair')),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1,2,4,7,9)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 8 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies', 
                                                 'Trade unions',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
            year = 2001) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2002 ----------------------------------------------------------------
  (`latino2002` %>% 
     select(iso3c = idenpa, distr_inc_fair = p16st, trust_banks = p34stb, 
            trust_gov = p34std, trust_private_comp = p34ste, 
            op_strong_tu = p22essh, id =numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            distr_inc_fair = as.numeric(distr_inc_fair),
            op_strong_tu = as.numeric(op_strong_tu)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>% 
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                         as.factor = T, 
                                         levels = c('Very Fair', 'Fair',
                                                    'Neither fair nor unfair',
                                                    'Unfair', 'Very Unfair')),
            op_strong_tu = car::recode(.$op_strong_tu, recodes = c("-4:-1=NA;
                                                                   1 = 'Strongly agree';
                                                                   2 = 'Agree';
                                                                   3 = 'Disagree';
                                                                   4 = 'Strongly disagree'"),
                                       as.factor = T,
                                       levels = c('Strongly agree', 'Agree',
                                                  'Disagree', 'Strongly disagree')),
            year = 2002) %>% 
     mutate(iso3c = as.character(iso3c))),
  # 2003 ----------------------------------------------------------------
  (`latino2003` %>% 
     select(iso3c = idenpa, trust_union = p23sta, trust_private_comp = p23stb, 
            trust_banks = p23std, trust_gov = p23stg, most_power1 = p10st.1,
            id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            most_power1 = as.numeric(most_power1)) %>%
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1,2,8)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 7 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies', 
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
            year = 2003) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2004 --------------------------------------------------------------------
  (`latino2004` %>% 
     select(iso3c = idenpa, gov_pow_people = p24wvs, most_power1 = p15st.1,
            trust_banks = p32sta, trust_gov = p32std, trust_union = p32ste,
            trust_private_comp = p34sta, id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people),
            most_power1 = as.numeric(most_power1)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1,2,8)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 7 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies', 
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
            year = 2004) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2005 --------------------------------------------------------------------
  (`latino2005` %>% 
     select(iso3c = idenpa, gov_pow_people = p22st, trust_union = p42stc, 
            trust_gov = p45stc, trust_private_comp = p45std, trust_banks = p45ste,
            trust_comp_asso = p47stg, most_power1 = p27st_1,
            id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people),
            most_power1 = as.numeric(most_power1)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1,0,2,8,9,10,11)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 7 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies',
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
            year = 2005) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2006 --------------------------------------------------------------------
  (`latino2006` %>% 
     select(iso3c = idenpa, gov_pow_people = p22st, most_power1 = p27st_1, 
            trust_union = p42stc, trust_gov = p45stc, trust_private_comp = p45std,
            trust_banks = p45ste, trust_comp_asso = p47stg, 
            id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people),
            most_power1 = as.numeric(most_power1)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            most_power1 = car::recode(.$most_power1, recodes = c("c(-4,-3,-2,-1,0,2,8,9,10,11,12)=NA;
                                                                 1 = 'Large companies';
                                                                 3 = 'Trade unions';
                                                                 4 = 'Mass media';
                                                                 5 = 'Banks';
                                                                 6 = 'Political parties';
                                                                 7 = 'The government'"),
                                      as.factor = T,
                                      levels = c('Large companies', 
                                                 'Trade unions',
                                                 'Mass media',
                                                 'Banks',
                                                 'Political parties',
                                                 'The government')),
            year = 2006) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2007 --------------------------------------------------------------------
  (`latino2007` %>% #Falta agregar MOST POWER 
     select(iso3c = idenpa, gov_pow_people = p20stm, trust_gov = p32st.a, 
            trust_private_comp = p32st.b, id = numentre, weight = wt,
            large.comp = P36ST.A, trade.un = P36ST.C, mass.media = P36ST.D,
            banks = P36ST.E, pol.parties = P36ST.F, gov = P36ST.G) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            most_power1 = case_when(large.comp == 1 ~ "Large companies",
                                    trade.un == 1 ~ "Trade unions",
                                    mass.media == 1 ~ "Mass media",
                                    banks == 1 ~ "Banks",
                                    pol.parties == 1 ~ "Political parties",
                                    gov == 1 ~ "The government"),
            year = 2007) %>% 
     mutate(iso3c = as.character(iso3c),
            most_power1 = factor(most_power1, levels = c('Large companies',
                                                         'Trade unions',
                                                         'Mass media',
                                                         'Banks',
                                                         'Political parties',
                                                         'The government'))) %>% 
     select(-c(large.comp, trade.un, mass.media, banks, pol.parties, gov))),
  
  # 2008 --------------------------------------------------------------------
  (`latino2008` %>% 
     select(iso3c = idenpa, gov_pow_people = p25st, conflict_rp = p69st.a, 
            conflict_man_workers = p69st.c, trust_gov = p31s.ta,  
            trust_banks = p31st.b, trust_union = p31st.g, trust_private_comp = p31st.h,
            id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people)) %>% 
     mutate_at(vars(starts_with('conflict')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate_at(vars(starts_with('conflict')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                             1 = 'Very strong';
                                                                             2 = 'Strong';
                                                                             3 = 'Weak';
                                                                             4 = 'It doesnt exist conflict'"),
                                                              as.factor = T,
                                                              levels = c('Very strong',
                                                                         'Strong',
                                                                         'Weak',
                                                                         "It doesnt exist conflict")))) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            year = 2008) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2009 --------------------------------------------------------------------
  (`latino2009` %>% 
     select(iso3c = idenpa, gov_pow_people = p22st, conflict_rp = p34stm.a, 
            conflict_man_workers = p34stm.b, trust_gov = p24st.a, trust_banks = p24st.b,
            trust_union = p24st.g, trust_private_comp = p24st.h,
            id = numentre, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people)) %>% 
     mutate_at(vars(starts_with('conflict')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate_at(vars(starts_with('conflict')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                             1 = 'Very strong';
                                                                             2 = 'Strong';
                                                                             3 = 'Weak';
                                                                             4 = 'It doesnt exist conflict'"),
                                                              as.factor = T,
                                                              levels = c('Very strong',
                                                                         'Strong',
                                                                         'Weak',
                                                                         "It doesn't exist conflict")))) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            year = 2009) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2010 --------------------------------------------------------------------
  (`latino2010` %>% # FALTA MOST POWER: Labour Unions y Big companies
     select(iso3c = IDENPA, gov_pow_people = P17ST, distr_inc_fair = P12ST, 
            conflict_rp = P27ST.A, conflict_man_workers = P27ST.B, trust_gov = P18ST.A,
            trust_banks = P18ST.B, trust_union = P18ST.G, trust_private_comp = P18ST.H,
            trust_state = P20ST.I, id = NUMENTRE, weight = wt) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people),
            distr_inc_fair = as.numeric(distr_inc_fair)) %>% 
     mutate_at(vars(starts_with('conflict')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate_at(vars(starts_with('conflict')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                             1 = 'Very strong';
                                                                             2 = 'Strong';
                                                                             3 = 'Weak';
                                                                             4 = 'It doesnt exist conflict'"),
                                                              as.factor = T,
                                                              levels = c('Very strong',
                                                                         'Strong',
                                                                         'Weak',
                                                                         "It doesn't exist conflict")))) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            year = 2010) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2011 --------------------------------------------------------------------
  (`latino2011` %>% 
     select(iso3c = IDENPA, gov_pow_people = P19ST, distr_inc_fair = P12ST, 
            id = NUMENTRE, weight = WT) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people),
            distr_inc_fair = as.numeric(distr_inc_fair)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            year = 2011) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2013 --------------------------------------------------------------------
  (`latino2013` %>% 
     select(iso3c = IDENPA, ssc = S8, distr_inc_fair = P27ST, gov_pow_people = P14ST,
            id = NUMENTRE, weight = WT) %>% 
     mutate(iso3c = as.numeric(iso3c),
            ssc = as.numeric(ssc),
            distr_inc_fair = as.numeric(distr_inc_fair),
            gov_pow_people = as.numeric(gov_pow_people)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            ssc = car::recode(.$ssc, recodes = c("-4:-1=NA;
                                                 1 = 'Upper';
                                                 2 = 'Upper middle';
                                                 3 = 'Middle';
                                                 4 = 'Lower middle';
                                                 5 = 'Lower'"),
                              as.factor = T,
                              levels = c('Upper', 'Upper middle', 'Middle', 
                                         'Lower middle', 'Lower')),
            year = 2013) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2015 --------------------------------------------------------------------
  (`latino2015` %>% 
     select(iso3c = IDENPA, ssc = S6, distr_inc_fair = P18ST, gov_pow_people = P14ST,
            trust_gov = P16ST.G, trust_union = P19ST.A, trust_private_comp = P19ST.E, 
            trust_state = P19ST.F, trust_banks = P19ST.G, id = NUMENTRE, weight = WT) %>% 
     mutate(iso3c = as.numeric(iso3c),
            ssc = as.numeric(ssc),
            distr_inc_fair = as.numeric(distr_inc_fair),
            gov_pow_people = as.numeric(gov_pow_people)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            ssc = car::recode(.$ssc, recodes = c("-4:-1=NA;
                                                 1 = 'Upper';
                                                 2 = 'Upper middle';
                                                 3 = 'Middle';
                                                 4 = 'Lower middle';
                                                 5 = 'Lower'"),
                              as.factor = T,
                              levels = c('Upper', 'Upper middle', 'Middle', 
                                         'Lower middle', 'Lower')),
            year = 2015) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2016 --------------------------------------------------------------------
  (`latino2016` %>% 
     select(iso3c = IDENPA, distr_inc_fair = P21ST, gov_pow_people = P10ST,
            trust_gov = P13STE, id = NUMENTRE, weight = WT) %>% 
     mutate(iso3c = as.numeric(iso3c),
            distr_inc_fair = as.numeric(distr_inc_fair),
            gov_pow_people = as.numeric(gov_pow_people),
            trust_gov = as.numeric(trust_gov)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            trust_gov = car::recode(.$trust_gov, recodes = c("-4:-1=NA;
                                                             1 = 'A lot of confidence';
                                                             2 = 'Some confidence';
                                                             3 = 'Little confidence';
                                                             4 = 'No confidence at all'"),
                                     as.factor = T, 
                                     levels = c('A lot of confidence',
                                                'Some confidence',
                                                'Little confidence',
                                                'No confidence at all')),
            year = 2016) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2017 --------------------------------------------------------------------
  (`latino2017` %>% 
     select(iso3c = IDENPA, ssc = S1, distr_inc_fair = P20ST, gov_pow_people = P10ST,
            trust_gov = P14ST.E, id = NUMENTRE, weight = WT) %>% 
     mutate(iso3c = as.numeric(iso3c),
            gov_pow_people = as.numeric(gov_pow_people),
            distr_inc_fair = as.numeric(distr_inc_fair),
            trust_gov = as.numeric(trust_gov),
            ssc = as.numeric(ssc)) %>% 
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            trust_gov = car::recode(.$trust_gov, recodes = c("-4:-1=NA;
                                                             1 = 'A lot of confidence';
                                                             2 = 'Some confidence';
                                                             3 = 'Little confidence';
                                                             4 = 'No confidence at all'"),
                                    as.factor = T, 
                                    levels = c('A lot of confidence',
                                               'Some confidence',
                                               'Little confidence',
                                               'No confidence at all')),
            ssc = car::recode(.$ssc, recodes = c("-4:-1=NA;
                                                 1 = 'Upper';
                                                 2 = 'Upper middle';
                                                 3 = 'Middle';
                                                 4 = 'Lower middle';
                                                 5 = 'Lower'"),
                              as.factor = T,
                              levels = c('Upper', 'Upper middle', 'Middle', 
                                         'Lower middle', 'Lower')),
            year = 2017) %>% 
     mutate(iso3c = as.character(iso3c))),
  
  # 2018 --------------------------------------------------------------------
  (`latino2018` %>% 
     select(iso3c = IDENPA, distr_inc_fair = P23ST, gov_pow_people = P14ST,
            trust_gov = P15STGBSC.E, trust_union = P16NC.C, trust_banks = P16NC.F,
            id = NUMENTRE, weight = WT) %>% 
     mutate(iso3c = as.numeric(iso3c),
            distr_inc_fair = as.numeric(distr_inc_fair),
            gov_pow_people = as.numeric(gov_pow_people)) %>% 
     mutate_at(vars(starts_with('trust')), ~as.numeric(.)) %>%
     mutate_at(vars(starts_with('trust')), ~(car::recode(., recodes = c("-4:-1=NA;
                                                                           1 = 'A lot of confidence';
                                                                           2 = 'Some confidence';
                                                                           3 = 'Little confidence';
                                                                           4 = 'No confidence at all'"),
                                                            as.factor = T, 
                                                            levels = c('A lot of confidence',
                                                                       'Some confidence',
                                                                       'Little confidence',
                                                                       'No confidence at all')))) %>%
     mutate(iso3c = car::recode(.$iso3c, recodes = c("-5:-1=NA; 32='ARG'; 68='BOL';
                                76='BRA'; 152='CHL'; 170='COL'; 188='CRI'; 214='DOM';
                                218='ECU'; 222='SLV'; 320='GTM'; 340='HND'; 484='MEX'; 
                                558='NIC'; 591= 'PAN'; 600='PRY'; 604='PER'; 724='ESP'; 
                                858='URY'; 862='VEN'")),
            gov_pow_people = car::recode(.$gov_pow_people, recodes = c("-4:-1=NA;
                                                                       1 = 'Benefit of powerful interests';
                                                                       2 = 'For the good of all'"),
                                         as.factor=T, 
                                         levels = c('Benefit of powerful interests',
                                                    'For the good of all')),
            distr_inc_fair = car::recode(.$distr_inc_fair, recodes = c("-4:-1=NA;
                                                                       1 = 'Very Fair';
                                                                       2 = 'Fair';
                                                                       3 = 'Neither fair nor unfair';
                                                                       4 = 'Unfair';
                                                                       5 = 'Very Unfair'"),
                                        as.factor = T, 
                                        levels = c('Very Fair', 'Fair',
                                                   'Neither fair nor unfair',
                                                   'Unfair', 'Very Unfair')),
            year = 2018) %>% 
     mutate(iso3c = as.character(iso3c))))) %>% 
  select(iso3c, year, id, weight, everything())

# 5. Group ----------------------------------------------------------------
rm(list = ls(pattern = "19|20|list")) #limpiar

lat <- lat %>%
  filter(!is.na(id)) %>% 
                 pivot_longer(cols = c(5:18), names_to = "variable", values_to = "value") %>% 
                 as_survey_design(ids = 3, weights = weight) %>%
                 group_by(iso3c, year, variable, value) %>%
                 filter(!is.na(value)) %>% 
                 summarise(prop = survey_mean(vartype = "ci",na.rm = TRUE)) %>% 
                 mutate(prop = prop*100) %>% select(1:prop) %>% 
                 mutate(value = str_replace_all(tolower(value), " ", "_")) %>%
                 mutate(value = str_replace_all(tolower(value), "_conflict_man_workers|
                                                _conflict_rp|_distr_inc_fair|_gov_pow_people|
                                                _labor_law_prot|_most_power1|_op_strong_tu|
                                                _ssc|_trust_banks|_ trust_comp_asso|
                                                _trust_gov|_trust_private_comp|
                                                _trust_state|_trust_union", ""),
                        value = str_replace_all(tolower(value), "_and_", "/")) %>% 
                 pivot_wider(names_from = c("variable", "value"),
                             values_from = "prop")



# 6. Label -------------------------------------------------------------------
# Llamar etiquetas (en slice se indican los tramos)
labels <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1aw_byhiC4b_0XPcTDtsCpCeJHabK38i4pCmkHshYMB8/edit#gid=0",
                                    range = c("B5:C900"), col_names = F) %>%
  select(variables = 1, etiquetas = 2) %>% 
  filter(grepl("_lat|year|iso3c", variables))

## Tranformar a vectornames
var.labels <- as.character(labels$etiquetas)
names(var.labels) <- labels$variables

## Etiquetar
Hmisc::label(lat) = as.list(var.labels[match(names(lat), names(lat))])

# 7. Save -----------------------------------------------------------------
saveRDS(lat, file="output/data/proc/latino.rds")
