# Computational and Translational Neuropsychiatry Lab (PSILANTRO Lab)
# Diego Angeles Valdez
# Alcohol Metrics

# load packages -----------------------------------------------------------
rm(list =ls())
set.seed(911) # Copito seed

pacman::p_load(tidyverse,
               plyr,
               hablar,
               data.table,
               ggpubr, 
               cowplot,
               lme4,
               performance,
               cluster,
               factoextra,
               lcmm)



# import data -------------------------------------------------------------

setwd("~/Documents/Projects/SUDMEX-Alc/Package_Alc/A_comparison/Data/Datasets/")

files <- list.files(pattern = ".csv")
data <- rbind.fill(lapply(files, fread, header=TRUE))%>%  convert(chr(Sex,RID,Phase),
                                                                  num(Age,Weight_kg,EtOH,H2O,Session))


# data curation -----------------------------------------------------------

data_all <- data %>%
  filter(Condition_day == "EtOH", Group == "Alc") %>% 
  dplyr::select(RID,Sex,Session,Weight_kg,Phase,Group,Condition, EtOH,H2O, Batch) %>%
  pivot_longer(cols=c(EtOH, H2O), values_to = "value", names_to = "Bottle") %>%
  pivot_wider(names_from = "Condition",values_from = "value") %>% 
  mutate(`24_hrs` = case_when(`24_hrs` >=45 ~ `24_hrs`))

data_curated <- data_all %>% 
  mutate(start_30mins = start - `30_mins`, 
         start_24_hrs = start - `24_hrs`,
         main_intake = (start_24_hrs * Weight_kg),
         basal_start = start - 27, 
         basal_30min = `30_mins` - 27,
         Basal_24hrs = `24_hrs` - 27,
         T_30min = basal_start - basal_30min,
         T_24hrs = basal_start - Basal_24hrs,
         Total_fluid_intake_24_ml = (T_24hrs * 100)/98.0608,
         Total_fluid_intake_30_ml = (T_30min * 100)/98.0608,
         Binge = (start_30mins* Weight_kg)) 

# Metrics alcohol ---------------------------------------------------------

data_alcohol <- data_curated %>% 
  filter(Bottle == "EtOH" ) %>% 
  mutate(Phase = case_when(Phase ==  "Model" ~ "Ethanol Exposure" ,
                           Phase == "Relapse" ~ "Relapse")) %>% 
  filter(Phase == "Ethanol Exposure" ) %>%  
  dplyr::select(RID,Sex,Session,Weight_kg,Phase,Group,main_intake) %>% 
  convert(fct(Sex,Session,Phase),
          num(RID))

# LCLMM esitmation 

data_alcohol$RID <-  as.numeric(data_alcohol$RID)
set.seed(42)
lcmm <- lcmm(main_intake ~ Session * Sex +Weight_kg ,
                       random = ~ 1, 
                       subject ="RID", data=data_alcohol, 
                       ng=1)


summary(lcmm) 
postprob(lcmm)


set.seed(42)
lcmm_alcohol_2_c <- gridsearch(rep = 30, maxiter = 100, minit = lcmm,
                               lcmm(main_intake ~ Session * Sex+Weight_kg ,
                                     random = ~ 1 , 
                                     mixture = ~Session, 
                                     subject="RID", data=data_alcohol, maxiter=200, 
                                     ng=2)) 



# demographic data  -------------------------------------------------------

Alcohol_intake <- ggplot(data = data_alcohol, aes(x = Session, y = main_intake, color = Sex)) + 
  geom_jitter(width = 0.25, height = 0, size = 2, alpha = 0.6) +  
  stat_summary(fun = mean, geom = "line", aes(group = Sex), size = 1.5) +  
  stat_summary(fun = mean, geom = "point", aes(group = Sex), size = 4) +  
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(group = Sex), width = 0.5, size = 2) +  
  scale_color_manual(values = c("#FF7F00", "#458B74")) +  
  labs(y = "EtOH Main intake g/Kg/24 hrs", x = "Sessions") +
  ylim(0, 8) +  
  theme_bw()  + theme(legend.title=element_blank())


Weight <- ggplot(data = data_alcohol, aes(x = Session, y = Weight_kg, color = Sex)) + 
  geom_jitter(width = 0.25, height = 0, size = 2, alpha = 0.6) +  
  stat_summary(fun = mean, geom = "line", aes(group = Sex), size = 1.5) +  
  stat_summary(fun = mean, geom = "point", aes(group = Sex), size = 4) +  
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(group = Sex), width = 0.5, size = 2) +  
  scale_color_manual(values = c("#FF7F00", "#458B74")) +  
  labs(y = "Weight Kg", x = "Sessions") +
  theme_bw() + theme(legend.title=element_blank())







