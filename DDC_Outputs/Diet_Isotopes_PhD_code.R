#Dietary Analyses for C&N - bone and dentine, and d13C x3
library(readxl) 
C_N_Database_bone <- read_excel("Dropbox/PhD_Cantab/C_N_Database_All_repaired.xlsx") #read in all europe bone data
View(C_N_Database_bone)
C_N_dentine<-read_excel("Dropbox/PhD_Cantab/C_N_Database_All_repaired.xlsx", 
                        sheet = "Humans Dentine")
View(C_N_dentine)
CN_fauna<-read_excel("Dropbox/PhD_Cantab/C_N_Database_All_repaired.xlsx", 
                     sheet = "Fauna")
View(CN_fauna)
CN_fauna$d13C<-as.numeric(CN_fauna$d13C)
CN_fauna$d15N<-as.numeric(CN_fauna$d15N)

#subset for only English material
England_CN_bone<- subset(C_N_Database_bone, Country=="England")
View(England_CN_bone)
summary(England_CN_bone)
England_CN_dentine<-subset(C_N_dentine, Country=="England")
England_CN_fauna<-subset(CN_fauna, Country=="England")

#new European Regions as discussed with Susanne - "Fennoscandia & Baltic", "Irish Sea", "England", "Scotland & Scottish Isles", "North Atlantic", "Po Valley", "Balearic & Tyrrhenian Seas", "Inland & Western Iberia", "Normandy/Neustria", "Austro-Hungary & Bavaria", "Frisia & Saxony", "Austrasia & Burgundy", "Croatia", "Greece"
library(tidyverse)
library(magrittr)
C_N_Database_bone$EuRegion<-C_N_Database_bone$County
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Split-Dalmatia', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Zadar', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vis', 'Croatia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vukovar-Srijem', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Šibenik-Knin', 'Croatia'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Roskilde', 'Skagerrak-Kattegat-Jutland Basin'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Zealand', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Funen', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ålborg', 'Skagerrak-Kattegat-Jutland Basin'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Saare', 'Baltic'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Turku and Pori', Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nord-Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nordland', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Troms', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rogaland', 'Atlantic & Arctic Norway'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sogn og Fjordane', 'Atlantic & Arctic Norway'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vest-Agder', 'Skagerrak-Kattegat-Jutland Basin'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vestfold', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hordaland', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hedmark', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Telemark', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Akershus', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oppland', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sør Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Uppland', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Uppsala', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kalmar', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Stockholm', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gotland', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Öland', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gävleborg', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Adelsö', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Leningrad Oblast', 'Baltic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nord Trøndelag', 'Atlantic & Arctic Norway'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Aarhus', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hjørring', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Skive', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oslo', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Isle of Man', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Pembrokeshire', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vale of Glamorgan', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Meath', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dublin', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ireland', 'Irish Sea'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Outer Hebrides', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Scotland', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rousay', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Highlands', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Orkney', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Mainland', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Highlands, Scotland', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sanday', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dumfries and Galloway', 'Scotland and Scottish Isles'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Bedfordshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Cambridgeshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dorset', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Derbyshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'East Sussex', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sussex', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kent', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hampshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Wiltshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Warwickshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Suffolk', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oxfordshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lincolnshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Tyne & Wear', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northumberland', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Yorkshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rutland', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nottinghamshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hertfordshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Buckinghamshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'North Lincolnshire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Somerset', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Norfolk', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gloucestershire', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Surrey', 'England'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northamptonshire', 'England'))
#C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kujalleq', 'North Atlantic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Mosfell', 'North Atlantic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lake Myvatn', 'North Atlantic'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Piemont', 'Po Valley'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Friuli-Venezia Giulia', 'Po Valley'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Piedmont', 'Po Valley'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ibiza', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rome', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Barcelona', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Valencia', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Languedoc', 'Balearic & Tyrrhenian Seas'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Normandy', 'Normandy/Neustria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Flanders', 'Normandy/Neustria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Wallonia', 'Normandy/Neustria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Aragon', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Pontevedra', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Madrid', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Setubal', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Alentejo', 'Inland & Western Iberia'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lower Saxony', 'Frisia & Saxony'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Friesland', 'Frisia & Saxony'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Saxony-Anhalt', 'Frisia & Saxony'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rhineland', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Grand Est', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rhein-Kreis Neuss, Nordrhine-Westphalia', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gem. Bedburg-Königshoven, Rhein-Erft-Kreis, Northrhine-Westphalia', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Baden-Württemberg', 'Austrasia & Burgundy'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Bavaria', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lower Austria', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northern Hungary', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Tyrol', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vienna', 'Austro-Hungary & Bavaria'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Crete', 'Greece'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Peloponnese', 'Greece'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Western Macedonia', 'Greece'))

C_N_Database_bone$EuRegion = factor(C_N_Database_bone$EuRegion,
                            levels=c("North Atlantic", "Atlantic & Arctic Norway","Skagerrak-Kattegat-Jutland Basin","Baltic", "Scotland and Scottish Isles", "Irish Sea", "England", "Frisia & Saxony", "Normandy/Neustria", "Austrasia & Burgundy","Austro-Hungary & Bavaria", "Po Valley", "Croatia", "Balearic & Tyrrhenian Seas","Inland & Western Iberia", "Greece"),ordered=TRUE)

#CUSTOM COLOUR PALETTES
discrainbowPaletteLARGE<-c("#E8ECFB","#D9CCE3", "#CAACCB","#BA8DB4","#AA6F9E","#994F88", "#882E72","#1965B0","#437DBF","#6195CF","#7BAFDE","#4EB265","#90C987","#CAE0AB","#F7F056","#F7CB45","#F4A736","#EE8026","#E65518","#DC050C","#A5170E","#72190E","#42150A","#777777")
discrainbowPalette<-c("#D1BBD7","#AE76A3","#882E72","#1965B0","#5289C7","#7BAFDE","#4EB265","#90C987","#CAE0AB","#F7F056","#F6C141","#F1932D","#E8601C","#DC050C","#777777")
qualcolourPalette<-c("black","#332288","#0077BB","#33BBEE","#44AA99","#117733","#999933","#DDCC77","#EE7733","#CC3311","#CC6677","#EE3377","#882255","#AA4499","#777777")

#qqplots and histograms for all variables
library(qqplotr)
library(ggplot2)
library(forcats)
#nitrogen
  #EMEU bone
bone_N_EMEUqq <- ggplot(data = C_N_Database_bone, mapping = aes(sample = d15N)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
bone_N_EMEUqq #bimodal 
  #EMEU dentine
dentine_N_EMEUqq <- ggplot(data = C_N_dentine, mapping = aes(sample = d15N)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
dentine_N_EMEUqq #also slightly bimodal
  #England bone
bone_N_Englandqq <- ggplot(data = England_CN_bone, mapping = aes(sample = d15N)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
bone_N_Englandqq #bimodal 
  #England dentine
dentine_N_Englandqq <- ggplot(data = England_CN_dentine, mapping = aes(sample = d15N)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
dentine_N_Englandqq #also slightly bimodal

#faunal nitrogen
ggplot(data = CN_fauna, mapping = aes(sample = CN_fauna$d15N)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()+facet_wrap(~CN_fauna$`Species Group`) 

#carbon
#EMEU bone
bone_C_EMEUqq <- ggplot(data = C_N_Database_bone, mapping = aes(sample = d13C)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
bone_C_EMEUqq # 
#EMEU dentine
dentine_C_EMEUqq <- ggplot(data = C_N_dentine, mapping = aes(sample = d13C)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
dentine_C_EMEUqq #
#England bone
bone_C_Englandqq <- ggplot(data = England_CN_bone, mapping = aes(sample = d13C)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
bone_C_Englandqq #
#England dentine
dentine_C_Englandqq <- ggplot(data = England_CN_dentine, mapping = aes(sample = d13C)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
dentine_C_Englandqq 

#faunal carbon
ggplot(data = CN_fauna, mapping = aes(sample = CN_fauna$d13C)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()+facet_wrap(~CN_fauna$`Species Group`) 

#histograms
#EMEU d13C bone
ggplot(C_N_Database_bone, aes(x=d13C)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=-19.4), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#EMEU d13C dentine
ggplot(C_N_dentine, aes(x=d13C)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=-19.61), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#England d13C bone
ggplot(England_CN_bone, aes(x=d13C)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=-20.03), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#England d13C dentine
ggplot(England_CN_dentine, aes(x=d13C)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=-19.80), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+ylab("Count")+
  theme_bw()

#EMEU d15N bone
ggplot(C_N_Database_bone, aes(x=d15N)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=10.29), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#EMEU d15N dentine
ggplot(C_N_dentine, aes(x=d15N)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=11.86), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#England d15N bone
ggplot(England_CN_bone, aes(x=d15N)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=9.973), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#England d15N dentine
ggplot(England_CN_dentine, aes(x=d15N)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=11.767), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#fauna nitrogen
ggplot(CN_fauna, aes(x=d15N)) + geom_histogram(binwidth=0.2)+
  xlab(expression(paste(delta^{15},N["collagen (AIR)"], " (\u2030)")))+ylab("Count")+
  theme_bw()+facet_wrap(~CN_fauna$`Species Group`)

#fauna carbon
ggplot(CN_fauna, aes(x=d13C)) + geom_histogram(binwidth=0.2)+
  xlab(expression(paste(delta^{13},C["collagen (PDB)"], " (\u2030)")))+ylab("Count")+
  theme_bw()+facet_wrap(~CN_fauna$`Species Group`)

#scatterplots and scatters with marginal plots
library(ggExtra)
library(viridis)
#scatter of just fauna - colour=country, shape = species
EMEU_CN_fauna_scatter<- ggplot(CN_fauna,aes(d13C,d15N))+
  theme_bw()+
  geom_point(aes(shape=CN_fauna$`Species Group`, color=CN_fauna$Country),size=4,)+
  scale_colour_viridis(discrete = TRUE)+
  scale_shape_manual(values=c(16, 8, 24, 15, 25, 17, 3, 4, 11, 13, 23, 10, 7))+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-28,-9),breaks=seq(-28,-9,1))+scale_y_continuous(limits=c(-1,18),breaks=seq(-1,18,1))
EMEU_CN_fauna_scatter #very little country variation visible so going to try just by species

EMEU_CN_fauna_scatter2<- ggplot(CN_fauna,aes(d13C,d15N))+
  theme_bw()+
  geom_point(aes(color=CN_fauna$`Species Group`),size=3,)+
  scale_colour_viridis(discrete = TRUE,name="Species Group")+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-28,-9),breaks=seq(-28,-9,1))+scale_y_continuous(limits=c(-1,18),breaks=seq(-1,18,1))+
  theme(legend.position ="bottom", legend.direction = "horizontal")
EMEU_CN_fauna_scatter2

EMEU_CN_fauna_scatter3<- ggplot(CN_fauna,aes(d13C,d15N, group=`Species Group`))+
  theme_bw()+
  geom_point(aes(shape=CN_fauna$`Species Group`,color=CN_fauna$`Species Group`),size=2,)+
  scale_shape_manual(values=c(16, 8, 24, 15, 25, 17, 3, 4, 11, 13, 23, 10, 7),name="Species Group")+
  scale_colour_manual(values=qualcolourPalette, name="Species Group")+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-28,-9),breaks=seq(-28,-9,1))+scale_y_continuous(limits=c(-1,18),breaks=seq(-1,18,1))+
  theme(legend.position ="bottom", legend.direction = "horizontal", legend.title = element_text(size=20), legend.text = element_text(siz=16), axis.title = element_text(size=20), axis.text = element_text(size=18))
EMEU_CN_fauna_scatter3

#England Faunal scatter
England_CN_fauna_scatter<- ggplot(England_CN_fauna,aes(d13C,d15N, group=`Species Group`))+
  theme_bw()+
  geom_point(aes(shape=England_CN_fauna$`Species Group`,color=England_CN_fauna$`Species Group`),size=2,)+
  scale_shape_manual(values=c(16, 8, 24, 15, 25, 3, 4, 11, 13, 23, 10, 7),name="Species Group")+#adjusted to match EMEU
  scale_colour_manual(values=c("black","#332288","#0077BB","#33BBEE","#44AA99","#999933","#DDCC77","#EE7733","#CC3311","#CC6677","#EE3377","#882255","#AA4499","#777777"), name="Species Group")+ #adjusted to match EMEU
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-28,-9),breaks=seq(-28,-9,1))+scale_y_continuous(limits=c(-1,18),breaks=seq(-1,18,1))+
  theme(legend.position ="bottom", legend.direction = "horizontal", legend.title = element_text(size=20), legend.text = element_text(siz=16), axis.title = element_text(size=20), axis.text = element_text(size=18))
England_CN_fauna_scatter

#humans
fct_reorder(C_N_Database_bone$Country, C_N_Database_bone$`Country Code`)

EMEU_CN_bone_scatter<- ggplot(C_N_Database_bone,aes(d13C,d15N))+
  theme_bw()+
  geom_point(aes(colour=Country),size=3,shape=16, show.legend = FALSE)+
  scale_color_manual("Country", values = c("black","black","black","black","orange","black","black","black","black","black","black","black","black","black","black","black","black","black","black","black", "black"))+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
EMEU_CN_bone_scatter


Eng_CN_bone_scatter<- ggplot(England_CN_bone,aes(d13C,d15N))+
  geom_point(size=3,shape=16)+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))
Eng_CN_bone_scatter

#grid.arrange(EMEU_CN_bone_scatter, Eng_CN_bone_scatter, ncol=1)

EMEU_CN_dentine_scatter<- ggplot(C_N_dentine,aes(d13C,d15N))+
  theme_bw()+
  geom_point(aes(colour=Country),size=3,shape=16, show.legend = FALSE)+
  scale_color_manual("Country", values = c("black","black","orange","black","black","black","black"))+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))
EMEU_CN_dentine_scatter


ggMarginal(EMEU_CN_bone_scatter ) #marginal distribution for all bone
ggMarginal(EMEU_CN_bone_scatter , type = "boxplot")
ggMarginal(EMEU_CN_dentine_scatter ) #marginal distribution for all dentine
ggMarginal(EMEU_CN_dentine_scatter , type = "boxplot")


#violin/ggridges
library(ggridges)
library(ggpubr)
#faunal plots 
CN_fauna$`Species Group` = factor(CN_fauna$`Species Group`,
                                   levels=c("Domestic Fowl","Other Bird", "Carnivore","Omnivore","Large Domestic Herbivore","Medium Domestic Herbivore","Wild Herbivore","Small mammal","Freshwater Fish","Other Fish","Marine Fish","Marine Mammal","Other"),ordered=TRUE)

#carbon
ggplot(CN_fauna, aes(x=d13C, y=CN_fauna$`Species Group`, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-28,-9),breaks=seq(-28,-9,1.0))+
  xlab(expression(paste(delta^{13},C["collagen (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

ggplot(data = CN_fauna, aes(y =d13C , x = CN_fauna$`Species Group`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-28,-9),breaks=seq(-28,-9,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
#nitrogen
ggplot(CN_fauna, aes(x=d15N, y=CN_fauna$`Species Group`, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-1,18),breaks=seq(-1,18,1.0))+
  xlab(expression(paste(delta^{15},N["collagen (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

ggplot(data = CN_fauna, aes(y =d15N , x = CN_fauna$`Species Group`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-1,18),breaks=seq(-1,18,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")

#England herbivores by region to look for variability
England_Herbivores<-subset(England_CN_fauna, `Species Group`!="Carnivore")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Domestic Fowl")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Freshwater Fish")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Marine Fish")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Omnivore")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Other")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Other Bird")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Other Fish")
England_Herbivores<-subset(England_Herbivores, `Species Group`!="Small mammal")

England_Herbivores$Region = factor(England_Herbivores$Region,
                                  levels=c("Northeast","Yorkshire and North Lincolnshire","Central","East","Upper Thames and Chilterns","Wessex", "Kent and East Sussex"),ordered=TRUE)

#carbon
ggplot(England_Herbivores, aes(x=d13C, y=England_Herbivores$Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-26,-18),breaks=seq(-26,-18,1.0))+
  xlab(expression(paste(delta^{13},C["collagen (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

ggplot(data = England_Herbivores, aes(x = Region, y = England_Herbivores$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-26,-18),breaks=seq(-26,-18,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")


#nitrogen
ggplot(England_Herbivores, aes(x=d15N, y=England_Herbivores$Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-1,14),breaks=seq(-1,14,1.0))+
  xlab(expression(paste(delta^{15},N["collagen (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

ggplot(data = England_Herbivores, aes(x = Region, y = England_Herbivores$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-1,14),breaks=seq(-1,14,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")

#EMEU violin and ridge plots of human bone and dentine
#by country & region
  #carbon
EMEU_d13Cbone_violin_boxjitter_country<-ggplot(data = C_N_Database_bone, aes(x = fct_reorder(C_N_Database_bone$Country, C_N_Database_bone$`Country Code`), y = C_N_Database_bone$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d13Cbone_violin_boxjitter_country

EMEU_d13Cbone_violin_boxjitter_region<-ggplot(data = C_N_Database_bone, aes(x =EuRegion, y = C_N_Database_bone$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d13Cbone_violin_boxjitter_region

EMEU_d13Cbone_ridges_country<-ggplot(C_N_Database_bone, aes(x=d13C, y=fct_reorder(C_N_Database_bone$Country, C_N_Database_bone$`Country Code`), fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d13Cbone_ridges_country

EMEU_d13Cbone_ridges_Region<-ggplot(C_N_Database_bone, aes(x=d13C, y=EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d13Cbone_ridges_Region

#dentine
C_N_dentine$EuRegion<-C_N_dentine$County
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Bedfordshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lincolnshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northumberland', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nottinghamshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kent', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Wiltshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Cambridgeshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rutland', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hertfordshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Yorkshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northamptonshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dorset', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Warwickshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Suffolk', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lincolnshire', 'England'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vukovar-Srijem', 'Croatia'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Funen', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Troms', 'Atlantic & Arctic Norway'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nordland', 'Atlantic & Arctic Norway'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oslo', 'Skagerrak-Kattegat-Jutland Basin'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Öland', 'Baltic'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Piedmont', 'Po Valley'))
C_N_dentine <- C_N_dentine %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Highlands, Scotland', 'Scotland and Scottish Isles'))

C_N_dentine$EuRegion = factor(C_N_dentine$EuRegion,
                                    levels=c("Atlantic & Arctic Norway","Skagerrak-Kattegat-Jutland Basin", "Baltic","Scotland and Scottish Isles", "England", "Po Valley", "Croatia"),ordered=TRUE)

#dentine carbon
EMEU_d13Cdentine_ridges_Region<-ggplot(C_N_dentine, aes(x=d13C, y=EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d13Cdentine_ridges_Region
EMEU_d13Cdentine_violin_boxjitter_region<-ggplot(data = C_N_dentine, aes(x =EuRegion, y = C_N_dentine$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d13Cdentine_violin_boxjitter_region


  #bone nitrogen
EMEU_d15Nbone_violin_boxjitter_country<-ggplot(data = C_N_Database_bone, aes(x = fct_reorder(C_N_Database_bone$Country, C_N_Database_bone$`Country Code`), y = C_N_Database_bone$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d15Nbone_violin_boxjitter_country
EMEU_d15Nbone_violin_boxjitter_region<-ggplot(data = C_N_Database_bone, aes(x = EuRegion, y = C_N_Database_bone$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d15Nbone_violin_boxjitter_region

EMEU_d15Nbone_ridges_country<-ggplot(C_N_Database_bone, aes(x=d15N, y=fct_reorder(C_N_Database_bone$Country, C_N_Database_bone$`Country Code`), fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d15Nbone_ridges_country

EMEU_d15Nbone_ridges_region<-ggplot(C_N_Database_bone, aes(x=d15N, y=EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d15Nbone_ridges_region

ggarrange(EMEU_d13Cbone_violin_boxjitter_country, EMEU_d15Nbone_violin_boxjitter_country, EMEU_d13Cbone_ridges_country, EMEU_d15Nbone_ridges_country, ncol=2, nrow=2)

#dentine nitrogen
EMEU_d15Ndentine_ridges_Region<-ggplot(C_N_dentine, aes(x=d15N, y=EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  xlab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d15Ndentine_ridges_Region
EMEU_d15Ndentine_violin_boxjitter_region<-ggplot(data = C_N_dentine, aes(x =EuRegion, y = C_N_dentine$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d15Ndentine_violin_boxjitter_region


#by environmental factors
C_N_Database_bone <- C_N_Database_bone %>% mutate(Environment = replace(Environment, Environment == 'Coastal and riverine', 'Coastal and Riverine'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(Environment = replace(Environment, Environment == 'Fenland', 'Wetlands incl. Fens'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(Environment = replace(Environment, Environment == 'Wetland', 'Wetlands incl. Fens'))

C_N_Database_bone$Environment = factor(C_N_Database_bone$Environment,
                                    levels=c("Island","Coastal", "Coastal and Riverine","Riverine","Lake", "Island, Lake","Coastal and Lake","Coastal and Fiordland","Lake, Fiordland", "Fiordland","Fiordland and Riverine", "Riverine, Alpine","Salt-marsh","Wetlands incl. Fens","Inland"),ordered=TRUE)
#carbon by environment
#bone
EMEU_d13Nbone_ridges_environment<-ggplot(C_N_Database_bone, aes(x=d13C, y=C_N_Database_bone$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d13Nbone_ridges_environment
EMEU_d13Cbone_violin_boxjitter_environment<-ggplot(data = C_N_Database_bone, aes(x =Environment, y = C_N_Database_bone$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d13Cbone_violin_boxjitter_environment

#dentine
C_N_dentine$Environment = factor(C_N_dentine$Environment,
                                       levels=c("Island","Coastal","Coastal and Fiordland","Fiordland","Lake, Fiordland","Lake","Riverine","Fenland","Inland"),ordered=TRUE)

EMEU_d13Ndentine_ridges_environment<-ggplot(C_N_dentine, aes(x=d13C, y=C_N_dentine$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d13Ndentine_ridges_environment
EMEU_d13Cdentine_violin_boxjitter_environment<-ggplot(data = C_N_dentine, aes(x =Environment, y = C_N_dentine$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d13Cdentine_violin_boxjitter_environment
#nitrogen by environment
#bone
EMEU_d15Nbone_ridges_environment<-ggplot(C_N_Database_bone, aes(x=d15N, y=C_N_Database_bone$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d15Nbone_ridges_environment
EMEU_d15Nbone_violin_boxjitter_environment<-ggplot(data = C_N_Database_bone, aes(x =Environment, y = C_N_Database_bone$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d15Nbone_violin_boxjitter_environment
#dentine
EMEU_d15Ndentine_ridges_environment<-ggplot(C_N_dentine, aes(x=d15N, y=C_N_dentine$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  xlab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d15Ndentine_ridges_environment
EMEU_d15Ndentine_violin_boxjitter_environment<-ggplot(data = C_N_dentine, aes(x =Environment, y = C_N_dentine$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d15Ndentine_violin_boxjitter_environment
#carbon by geology
#bone
EMEU_d13Cbone_violin_boxjitter_geology<-ggplot(data = C_N_Database_bone, aes(x =C_N_Database_bone$`Simplified Geology`, y = C_N_Database_bone$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d13Cbone_violin_boxjitter_geology



#dentine
EMEU_d13Ndentine_ridges_geology<-ggplot(C_N_dentine, aes(x=d13C, y=C_N_dentine$`Simplified Geology`, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_d13Ndentine_ridges_geology
EMEU_d13Cdentine_violin_boxjitter_geology<-ggplot(data = C_N_dentine, aes(x =C_N_dentine$`Simplified Geology`, y = C_N_dentine$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d13Cdentine_violin_boxjitter_geology
#nitrogen geology 
#bone
EMEU_d15Nbone_violin_boxjitter_geology<-ggplot(data = C_N_Database_bone, aes(x =C_N_Database_bone$`Simplified Geology`, y = C_N_Database_bone$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d15Nbone_violin_boxjitter_geology

grid.arrange(EMEU_d13Cbone_violin_boxjitter_geology, EMEU_d15Nbone_violin_boxjitter_geology, ncol=1)
#dentine
EMEU_d15Ndentine_violin_boxjitter_geology<-ggplot(data = C_N_dentine, aes(x =C_N_dentine$`Simplified Geology`, y = C_N_dentine$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,20),breaks=seq(0,20,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
EMEU_d15Ndentine_violin_boxjitter_geology

grid.arrange(EMEU_d13Cdentine_violin_boxjitter_geology, EMEU_d15Ndentine_violin_boxjitter_geology, ncol=1)

#England violin and ridge plots of human bone and dentine
#by region
England_CN_bone$Region = factor(England_CN_bone$Region,
                                   levels=c("Northeast","Yorkshire and North Lincolnshire","Central","Cambridge and South Cambs", "East", "Kent and East Sussex", "Upper Thames and Chilterns","Wessex", "Southwest"),ordered=TRUE)
#carbon
England_d13Cbone_violin_boxjitter_region<-ggplot(data = England_CN_bone, aes(x = Region, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_d13Cbone_violin_boxjitter_region

England_d13Cbone_ridges_region<-ggplot(England_CN_bone, aes(x=d13C, y=Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_d13Cbone_ridges_region
#nitrogen
England_d15Nbone_violin_boxjitter_region<-ggplot(data = England_CN_bone, aes(x = Region, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_d15Nbone_violin_boxjitter_region

England_d15Nbone_ridges_region<-ggplot(England_CN_bone, aes(x=d15N, y=Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_d15Nbone_ridges_region

ggarrange(England_d13Cbone_violin_boxjitter_region, England_d15Nbone_violin_boxjitter_region, England_d13Cbone_ridges_region, England_d15Nbone_ridges_region, ncol=2, nrow=2)

#dentine
England_CN_dentine$Region = factor(England_CN_dentine$Region,
                                levels=c("Northeast","Yorkshire and North Lincolnshire","Central","Cambridge and South Cambs", "East", "Kent and East Sussex", "Upper Thames and Chilterns","Wessex", "Southwest"),ordered=TRUE)

#carbon
England_d13Cdentine_violin_boxjitter_region<-ggplot(data = England_CN_dentine, aes(x = Region, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_d13Cdentine_violin_boxjitter_region

England_d13Cdentine_ridges_region<-ggplot(England_CN_dentine, aes(x=d13C, y=Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_d13Cdentine_ridges_region
#nitrogen
England_d15Ndentine_violin_boxjitter_region<-ggplot(data = England_CN_dentine, aes(x = Region, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_d15Ndentine_violin_boxjitter_region

England_d15Ndentine_ridges_region<-ggplot(England_CN_dentine, aes(x=d15N, y=Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  xlab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_d15Ndentine_ridges_region

ggarrange(England_d13Cdentine_violin_boxjitter_region, England_d15Ndentine_violin_boxjitter_region, England_d13Cdentine_ridges_region, England_d15Ndentine_ridges_region, ncol=2, nrow=2)


#England by environment
England_CN_bone <- England_CN_bone %>% mutate(Environment = replace(Environment, Environment == 'Coastal and riverine', 'Coastal and Riverine')) #inducing NAs for some reason
England_CN_bone$Environment = factor(England_CN_bone$Environment,
                                       levels=c("Island","Coastal", "Coastal and riverine","Riverine", "Wetland","Fenland","Inland"),ordered=TRUE)

#bone
#carbon
Eng_d13Cbone_ridges_environment<-ggplot(England_CN_bone, aes(x=d13C, y=England_CN_bone$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
Eng_d13Cbone_ridges_environment
Eng_d13Cbone_violin_boxjitter_environment<-ggplot(data = England_CN_bone, aes(x =Environment, y = England_CN_bone$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d13Cbone_violin_boxjitter_environment
#nitrogen
Eng_d15Nbone_ridges_environment<-ggplot(England_CN_bone, aes(x=d15N, y=England_CN_bone$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
Eng_d15Nbone_ridges_environment
Eng_d15Nbone_violin_boxjitter_environment<-ggplot(data = England_CN_bone, aes(x =Environment, y = England_CN_bone$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d15Nbone_violin_boxjitter_environment

#dentine
England_CN_dentine$Environment = factor(England_CN_dentine$Environment,
                                     levels=c("Island","Coastal","Riverine","Fenland","Inland"),ordered=TRUE)

#carbon
Eng_d13Cdentine_ridges_environment<-ggplot(England_CN_dentine, aes(x=d13C, y=England_CN_dentine$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
Eng_d13Cdentine_ridges_environment
Eng_d13Cdentine_violin_boxjitter_environment<-ggplot(data = England_CN_dentine, aes(x =Environment, y = England_CN_dentine$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d13Cdentine_violin_boxjitter_environment
#nitrogen
Eng_d15Ndentine_ridges_environment<-ggplot(England_CN_dentine, aes(x=d15N, y=England_CN_dentine$Environment, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  xlab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
Eng_d15Ndentine_ridges_environment
Eng_d15Ndentine_violin_boxjitter_environment<-ggplot(data = England_CN_dentine, aes(x =Environment, y = England_CN_dentine$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d15Ndentine_violin_boxjitter_environment

#England by geology
#bone
#carbon
Eng_d13Cbone_violin_boxjitter_geology<-ggplot(data = England_CN_bone, aes(x =England_CN_bone$`Simplified Geology`, y = England_CN_bone$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d13Cbone_violin_boxjitter_geology
#nitrogen
Eng_d15Nbone_violin_boxjitter_geology<-ggplot(data = England_CN_bone, aes(x =England_CN_bone$`Simplified Geology`, y = England_CN_bone$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d15Nbone_violin_boxjitter_geology

grid.arrange(Eng_d13Cbone_violin_boxjitter_geology,Eng_d15Nbone_violin_boxjitter_geology, ncol=1)
#dentine
#carbon
Eng_d13Cdentine_violin_boxjitter_geology<-ggplot(data = England_CN_dentine, aes(x =England_CN_dentine$`Simplified Geology`, y = England_CN_dentine$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d13Cdentine_violin_boxjitter_geology
#nitrogen
Eng_d15Ndentine_violin_boxjitter_geology<-ggplot(data = England_CN_dentine, aes(x =England_CN_dentine$`Simplified Geology`, y = England_CN_dentine$d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,19),breaks=seq(0,19,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
Eng_d15Ndentine_violin_boxjitter_geology

grid.arrange(Eng_d13Cdentine_violin_boxjitter_geology,Eng_d15Ndentine_violin_boxjitter_geology, ncol=1)


#hierarchical clustering and dendrograms
#dendrograms as an alternative to k means clustering which doesn't require you to select the number of cluster
library("ggplot2")
library("ggdendro")
library("ape")
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") # load code of A2R function

#prepping the data for hierarchical clustering
EMEUboneclean <- C_N_Database_bone
class(as.data.frame(EMEUboneclean))
head(EMEUboneclean)
EMEUboneclean <- data.frame(EMEUboneclean)
rownames(EMEUboneclean) <- EMEUboneclean[,1]
EMEUboneclean <- EMEUboneclean[,-1]
head(EMEUboneclean)
show(EMEUboneclean)
EMEUboneclean<-EMEUboneclean[c(32,33)]
head(EMEUboneclean)
EMEUboneclean <- scale(EMEUboneclean)
EMEUboneclean <- na.omit(EMEUboneclean)
head(EMEUboneclean)
#hierarchical clustering
library(factoextra)
library(cluster)
library(NbClust)
ddEMEU_CN_bone<-dist(EMEUboneclean, method = "euclidean") 
hcEMEU_CN_bone<-hclust(ddEMEU_CN_bone, method = "ward.D2")
ggdendrogram(hcEMEU_CN_bone)
ggdendrogram(hcEMEU_CN_bone, rotate = TRUE,)
plot(as.phylo(hcEMEU_CN_bone), type = "fan")
set.seed(123)
fviz_nbclust(EMEUboneclean, hcut, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(EMEUboneclean, FUN = hcut, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(EMEUboneclean, hcut, method = "wss")
NbClust(data = EMEUboneclean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2")
A2Rplot(hcEMEU_CN_bone, k = 2, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_bone, k = 3, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_bone, k = 6, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)

#import manually entered scatterplot groups from 8 groups above
library(readr)
library(readxl)
CNB_hclusters <- read_excel("Dropbox/PhD_Cantab/CNB_hclusters.xlsx")
#View(CNB_hclusters)
C_N_Database_bone$`CNB_hcluster`<-CNB_hclusters$CNB_EMEU_hclust_group
#when annotated, need to combine some groups in branches, all of 1.1 combine, all of 2.1 combine
C_N_Database_bone$`CNB_hcluster2`<-C_N_Database_bone$`CNB_hcluster`
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster2 = replace(CNB_hcluster2, CNB_hcluster2 == '1.1.1', '1.1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster2 = replace(CNB_hcluster2, CNB_hcluster2 == '1.1.2', '1.1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster2 = replace(CNB_hcluster2, CNB_hcluster2 == '2.1.1', '2.1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster2 = replace(CNB_hcluster2, CNB_hcluster2 == '2.1.2', '2.1'))

#with 3 clusters from gap_stat
C_N_Database_bone$`CNB_hcluster3`<-C_N_Database_bone$`CNB_hcluster`
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.1.1', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.1.2', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.2.1', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '1.2.2', '1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.1.1', '2.1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.1.2', '2.1'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.2.1', '2.2'))
C_N_Database_bone <- C_N_Database_bone %>% mutate(CNB_hcluster3 = replace(CNB_hcluster3, CNB_hcluster3 == '2.2.2', '2.2'))


#scatterplot for EMEU C&N dendrogram
ggplot(C_N_Database_bone,aes(d13C, d15N, color=`CNB_hcluster`))+ #8groups
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))

ggplot(C_N_Database_bone,aes(d13C, d15N, color=`CNB_hcluster2`))+ #6groups
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))

ggplot(C_N_Database_bone,aes(d13C, d15N, color=`CNB_hcluster3`))+ #3groups
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.title =element_text(size=22), legend.text = element_text(size=18))


#stacked bar plots 
#stacked barchart with country and cluster 
#counts for country categories
library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("scales")
library(scales)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("ztable")
library(ztable)
library(magrittr)
library(viridis)
library(forcats)

CNBcleanplus<- C_N_Database_bone
class(as.data.frame(CNBcleanplus))
head(CNBcleanplus)
CNBcleanplus <- data.frame(CNBcleanplus)
rownames(CNBcleanplus) <-CNBcleanplus[,1]
CNBcleanplus <- CNBcleanplus[,-1]
head(CNBcleanplus)
View(CNBcleanplus)
CNBcleanplus<-CNBcleanplus[c(2,3,37,40)] #country, country code, region and cluster group
head(CNBcleanplus)
CNBcleanplus <- na.omit(CNBcleanplus)
head(CNBcleanplus)

CNBdfr <- CNBcleanplus %>%             
  mutate(Country = as.factor(Country)     # categorical values to factor
         , `CNB_hcluster2` = as.ordered(`CNB_hcluster3`))# character to ordered factor (like a grade)


#fct_reorder(carbcleanplus$Country, carbcleanplus$`Country Code`)

#dfr <- na.omit(dfr)
CNBdfr_prop <- CNBdfr %>% 
  count(Country, `CNB_hcluster3`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNBdfr_prop)           

CNBdfr_prop2 <- CNBdfr %>% 
  count(`CNB_hcluster3`, Country) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNBdfr_prop2)  

ggplot(CNBdfr_prop, aes(CNBdfr_prop$`CNB_hcluster3`,CNBdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = Country, weight=`CNB_hcluster3`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Country")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(CNBdfr_prop, aes(Country,CNBdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = CNBdfr_prop$`CNB_hcluster3`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  theme_bw()


CNBdfr2 <- CNBcleanplus %>%             
  mutate(EuRegion = as.factor(EuRegion), `CNB_hcluster3` = as.ordered(`CNB_hcluster3`))

CNBdfr2_prop <- CNBdfr2 %>% 
  count(EuRegion, `CNB_hcluster3`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNBdfr2_prop)   

ggplot(CNBdfr2_prop, aes(CNBdfr2_prop$`CNB_hcluster3`,CNBdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = EuRegion, weight=`CNB_hcluster3`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))

ggplot(CNBdfr2_prop, aes(EuRegion,CNBdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = CNBdfr2_prop$`CNB_hcluster3`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))


#dentine hierarchical clustering
#prepping the data for hierarchical clustering
EMEUdentineclean <- C_N_dentine
class(as.data.frame(EMEUdentineclean))
head(EMEUdentineclean)
EMEUdentineclean <- data.frame(EMEUdentineclean)
rownames(EMEUdentineclean) <- EMEUdentineclean[,1]
EMEUdentineclean <- EMEUdentineclean[,-1]
head(EMEUdentineclean)
View(EMEUdentineclean)
EMEUdentineclean<-EMEUdentineclean[c(33,34)] #choosing just d13C and d15N
head(EMEUdentineclean)
EMEUdentineclean <- scale(EMEUdentineclean)
EMEUdentineclean <- na.omit(EMEUdentineclean)
head(EMEUdentineclean)
#hierarchical clustering
ddEMEU_CN_dentine<-dist(EMEUdentineclean, method = "euclidean") 
hcEMEU_CN_dentine<-hclust(ddEMEU_CN_dentine, method = "ward.D2")
ggdendrogram(hcEMEU_CN_dentine)
ggdendrogram(hcEMEU_CN_dentine, rotate = TRUE,)
plot(as.phylo(hcEMEU_CN_dentine), type = "fan")
set.seed(123)
fviz_nbclust(EMEUdentineclean, hcut, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(EMEUdentineclean, FUN = hcut, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(EMEUdentineclean, hcut, method = "wss")
NbClust(data = EMEUdentineclean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2")
A2Rplot(hcEMEU_CN_dentine, k = 7, boxes = FALSE, col.up = "gray50", col.down = c("black", "#E69F00","#0072B2","#009E73","#CC79A7","#56B4E9","#D55E00"))
A2Rplot(hcEMEU_CN_dentine, k = 8, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_dentine, k = 6, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_dentine, k = 5, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcEMEU_CN_dentine, k = 3, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)

#import manually entered scatterplot groups from 8 groups above
library(readr)
library(readxl)
CND_hclusters <- read_excel("Dropbox/PhD_Cantab/CND_hclusters.xlsx")
#View(CND_hclusters)
C_N_dentine$`CND_hcluster`<-CND_hclusters$CND_hcluster_group5
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '1.1000000000000001', '1.1'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '1.1', '1'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '1.2', '1'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '2.2.1', '2.2'))
C_N_dentine <- C_N_dentine %>% mutate(CND_hcluster = replace(CND_hcluster, CND_hcluster == '2.2.2', '2.2'))

#scatterplot for EMEU C&N dendrogram dentine
ggplot(C_N_dentine,aes(d13C, d15N, color=`CND_hcluster`))+ #5 groups
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.title =element_text(size=22), legend.text = element_text(size=18))


#stacked bar plots 
#stacked barchart with country and cluster 
#counts for country categories
library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("scales")
library(scales)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("ztable")
library(ztable)
library(magrittr)
library(viridis)
library(forcats)
CNDcleanplus<- C_N_dentine
class(as.data.frame(CNDcleanplus))
head(CNDcleanplus)
CNDcleanplus <- data.frame(CNDcleanplus)
rownames(CNDcleanplus) <-CNDcleanplus[,1]
CNDcleanplus <- CNDcleanplus[,-1]
head(CNDcleanplus)
View(CNDcleanplus)
CNDcleanplus<-CNDcleanplus[c(2,3,36,37)] #country, country code, region and cluster group
head(CNDcleanplus)
#CNDcleanplus <- na.omit(CNDcleanplus)
#head(CNDcleanplus)

CNDdfr <- CNDcleanplus %>%             
  mutate(Country = as.factor(Country)     # categorical values to factor
         , `CND_hcluster` = as.ordered(`CND_hcluster`))# character to ordered factor (like a grade)


#fct_reorder(carbcleanplus$Country, carbcleanplus$`Country Code`)

#dfr <- na.omit(dfr)
CNDdfr_prop <- CNDdfr %>% 
  count(Country, `CND_hcluster`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNDdfr_prop)           

CNDdfr_prop2 <- CNDdfr %>% 
  count(`CND_hcluster`, Country) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNDdfr_prop2)  

ggplot(CNDdfr_prop, aes(CNDdfr_prop$`CND_hcluster`,CNDdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = Country, weight=`CND_hcluster`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Country")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(CNDdfr_prop, aes(Country,CNDdfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = CNDdfr_prop$`CND_hcluster`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  theme_bw()


CNDdfr2 <- CNDcleanplus %>%             
  mutate(EuRegion = as.factor(EuRegion), `CND_hcluster` = as.ordered(`CND_hcluster`))

CNDdfr2_prop <- CNDdfr2 %>% 
  count(EuRegion, `CND_hcluster`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(CNDdfr2_prop)   

ggplot(CNDdfr2_prop, aes(CNDdfr2_prop$`CND_hcluster`,CNDdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = EuRegion, weight=`CND_hcluster`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))


ggplot(CNDdfr2_prop, aes(EuRegion,CNDdfr2_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = CNDdfr2_prop$`CND_hcluster`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))

###Age and Diet###
England_CN_bone <- England_CN_bone %>% mutate(`Age Category` = replace(`Age Category`, `Age Category` == 'F3/F4', 'F3/4'))
England_CN_bone$`Age Category` = factor(England_CN_bone$`Age Category`,
                                                 levels=c("Juvenile","0","0/1","1","1/2","2","1/2/F3","2/F3","2/M3","2/U3","F3","M3","U3","F3/4","M3/4","U3/4","F4","M4","U4","F4/5","M4/5","F3-5","F5","M5","U5","F4-6","M4-6","F5/6","M5/6","U5/6","F6","M6","U6","Adult"),ordered=TRUE)

#English bone
England_Age_N_violin<-ggplot(data = England_CN_bone, aes(x = England_CN_bone$`Age Category`, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_Age_N_violin #need to aggregate age categories not sure how I want to do that yet 

England_Age_N_ridges<-ggplot(England_CN_bone, aes(x=d15N, y=England_CN_bone$`Age Category`, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_Age_N_ridges

England_Age_C_violin<-ggplot(data = England_CN_bone, aes(x = England_CN_bone$`Age Category`, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_Age_C_violin #need to aggregate age categories not sure how I want to do that yet 

England_Age_C_ridges<-ggplot(England_CN_bone, aes(x=d13C, y=England_CN_bone$`Age Category`, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_Age_C_ridges

England_CN_bone$`Age Category Combined`<-England_CN_bone$`Age Category`
England_CN_bone$`Age Category Combined`<-as.character(England_CN_bone$`Age Category Combined`)
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '0', 'Child'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '1', 'Child'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2', 'Child'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '0/1', 'Child'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '1/2', 'Child'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '1/2/F3', 'Female 1-3'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2/F3', 'Female 1-3'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F3', 'Female 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F3/4', 'Female 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F4', 'Female 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F3-5', 'Female 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F4-6', 'Female 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F4/5', 'Female 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F5', 'Female 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F5/6', 'Female 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F6', 'Female 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2/M3', 'Male 1-3'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M3', 'Male 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M3/4', 'Male 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M4', 'Male 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M4-6', 'Male 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M4/5', 'Male 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M5', 'Male 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M5/6', 'Male 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M6', 'Male 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2/U3', 'Unknown sex 1-3'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U3', 'Unknown sex 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U3/4', 'Unknown sex 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U4', 'Unknown sex 3-5'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U5', 'Unknown sex 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U5/6', 'Unknown sex 5-6'))
England_CN_bone <- England_CN_bone %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U6', 'Unknown sex 5-6'))

England_CN_bone$`Age Category Combined` = factor(England_CN_bone$`Age Category Combined`,
                                                 levels=c("Child","Juvenile","Female 1-3","Male 1-3","Unknown sex 1-3", "Female 3-5","Male 3-5","Unknown sex 3-5","Female 5-6","Male 5-6","Unknown sex 5-6","Adult"),ordered=TRUE)

England_Age_N_violin2<-ggplot(data = England_CN_bone, aes(x = England_CN_bone$`Age Category Combined`, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_Age_N_violin2 

England_Age_N_ridges2<-ggplot(England_CN_bone, aes(x=d15N, y=England_CN_bone$`Age Category Combined`, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0,18),breaks=seq(0,18,2.0))+
  xlab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_Age_N_ridges2

England_Age_C_violin2<-ggplot(data = England_CN_bone, aes(x = England_CN_bone$`Age Category Combined`, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22),legend.position = "none")
England_Age_C_violin2 

England_Age_C_ridges2<-ggplot(England_CN_bone, aes(x=d13C, y=England_CN_bone$`Age Category Combined`, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-24.5,-14.5),breaks=seq(-24.5,-14.5,1.0))+
  xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
England_Age_C_ridges2

#English Dentine Age (do this??? would need to split by tooth... can I be arsed?)

#Bone vs dentine - bagplots and BEST tests 
library("ggsci")
#need the geom_bag() function from Ben Marwick's GitHub - https://gist.github.com/benmarwick/00772ccea2dd0b0f1745
devtools::source_gist("00772ccea2dd0b0f1745", filename = "000_geom_bag.r")
devtools::source_gist("00772ccea2dd0b0f1745", filename = "001_bag_functions.r")
England_Bone_Dentine_CN_simple<-read_excel("Dropbox/PhD_Cantab/England_Bone_Dentine_CN_simple.xlsx")

England_Bone_Dent_Bag<- ggplot(England_Bone_Dentine_CN_simple, aes(d13C, d15N, colour = `Dentine or Bone`, fill = `Dentine or Bone`)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
England_Bone_Dent_Bag

library(BEST) #make sure you have the latest JAGS and rjags installed, if you're having problems, quit R, reinstall JAGS, open R and reinstall both rjags and BEST
Bone_England<-subset(England_Bone_Dentine_CN_simple, `Dentine or Bone`=="Bone") #English human bone C&N
Dentine_England<-subset(England_Bone_Dentine_CN_simple, `Dentine or Bone`=="Dentine")#English human dentine C&N
EngBoneC<-c(Bone_England$d13C)
EngDentineC<-c(Dentine_England$d13C)
carbonEng1<-na.omit(EngBoneC)
carbonEng2<-na.omit(EngDentineC)
BESTout_England_collC <- BESTmcmc(carbonEng1, carbonEng2, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collC)
plotAll(BESTout_England_collC)

EngBoneN<-c(Bone_England$d15N)
EngDentineN<-c(Dentine_England$d15N)
nitrogenEng1<-na.omit(EngBoneN)
nitrogenEng2<-na.omit(EngDentineN)
BESTout_England_collN <- BESTmcmc(nitrogenEng1, nitrogenEng2, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collN)
plotAll(BESTout_England_collN)

#now doing comparison of both with the individuals under life stage 3 removed 
England_Bone_Dentine_CN_simple$`Age Category Combined`<-England_Bone_Dentine_CN_simple$`Age Category`
England_Bone_Dentine_CN_simple$`Age England_Bone_Dentine_CN_simple Combined`<-as.character(England_Bone_Dentine_CN_simple$`Age Category Combined`)
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '0', 'Child'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '1', 'Child'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2', 'Child'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '0/1', 'Child'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '1/2', 'Child'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '1/2/F3', 'Female 1-3'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2/F3', 'Female 1-3'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F3', 'Female 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F3/4', 'Female 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F3/F4', 'Female 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F4', 'Female 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F3-5', 'Female 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F4-6', 'Female 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F4/5', 'Female 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F5', 'Female 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F5/6', 'Female 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'F6', 'Female 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2/M3', 'Male 1-3'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M3', 'Male 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M3/4', 'Male 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M4', 'Male 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M4-6', 'Male 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M4/5', 'Male 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M5', 'Male 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M5/6', 'Male 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'M6', 'Male 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == '2/U3', 'Unknown sex 1-3'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U3', 'Unknown sex 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U3/4', 'Unknown sex 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U4', 'Unknown sex 3-5'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U5', 'Unknown sex 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U5/6', 'Unknown sex 5-6'))
England_Bone_Dentine_CN_simple <- England_Bone_Dentine_CN_simple %>% mutate(`Age Category Combined` = replace(`Age Category Combined`, `Age Category Combined` == 'U6', 'Unknown sex 5-6'))

England_bonedent_adultonly<-England_Bone_Dentine_CN_simple
England_bonedent_adultonly<-subset(England_bonedent_adultonly, `Age Category Combined`!="Child")
England_bonedent_adultonly<-subset(England_bonedent_adultonly, `Age Category Combined`!="Female 1-3")
England_bonedent_adultonly<-subset(England_bonedent_adultonly, `Age Category Combined`!="Male 1-3")
England_bonedent_adultonly<-subset(England_bonedent_adultonly, `Age Category Combined`!="Unknown sex 1-3")
England_bonedent_adultonly<-subset(England_bonedent_adultonly, `Age Category Combined`!="Juvenile")


England_Bone_Dent_Bag_adult<- ggplot(England_bonedent_adultonly, aes(d13C, d15N, colour = `Dentine or Bone`, fill = `Dentine or Bone`)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
England_Bone_Dent_Bag_adult

#same as above but with only matched individuals

England_matched_bone_dent<-read_excel("Dropbox/PhD_Cantab/England_Bone_Dentine_CN_simple.xlsx", 
                                         sheet = "matched_bone_dent")

England_CN_Bag_allAges_matched<- ggplot(England_matched_bone_dent, aes(d13C, d15N, colour = `Bone or Dentine`, fill = `Bone or Dentine`)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
England_CN_Bag_allAges_matched
#BEST test on this
Bone_England_matched_all<-subset(England_matched_bone_dent, `Bone or Dentine`=="Bone") #English human bone C&N matched
Dentine_England_matched_all<-subset(England_matched_bone_dent, `Bone or Dentine`=="Dentine")#English human dentine C&N matched 
EngBoneC_matchedall<-c(Bone_England_matched_all$d13C)
EngDentineC_matchedall<-c(Dentine_England_matched_all$d13C)
carbonEng1_matched<-na.omit(EngBoneC_matchedall)
carbonEng2_matched<-na.omit(EngDentineC_matchedall)
BESTout_England_collC_matched_all <- BESTmcmc(carbonEng1_matched, carbonEng2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collC_matched_all)
plotAll(BESTout_England_collC_matched_all)

EngBoneN_matched_all<-c(Bone_England_matched_all$d15N)
EngDentineN_matched_all<-c(Dentine_England_matched_all$d15N)
nitrogenEng1_matched<-na.omit(EngBoneN_matched_all)
nitrogenEng2_matched<-na.omit(EngDentineN_matched_all)
BESTout_England_collN_matched_all<- BESTmcmc(nitrogenEng1_matched, nitrogenEng2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collN_matched_all)
plotAll(BESTout_England_collN_matched_all)

##now without children <10/age cat 3
England_bonedent_matched_adultonly<-England_matched_bone_dent
England_bonedent_matched_adultonly<-subset(England_bonedent_matched_adultonly, `Sk_Age`!="0")
England_bonedent_matched_adultonly<-subset(England_bonedent_matched_adultonly, `Sk_Age`!="2/U3")
England_bonedent_matched_adultonly<-subset(England_bonedent_matched_adultonly, `Sk_Age`!="2")
England_bonedent_matched_adultonly<-subset(England_bonedent_matched_adultonly, `Sk_Age`!="1")
England_bonedent_matched_adultonly<-subset(England_bonedent_matched_adultonly, `Sk_Age`!="0/1")
England_bonedent_matched_adultonly<-subset(England_bonedent_matched_adultonly, `Sk_Age`!="1/2")
England_bonedent_matched_adultonly<-subset(England_bonedent_matched_adultonly, `Sk_Age`!="2/F3")


England_CN_Bag_adults_matched<- ggplot(England_bonedent_matched_adultonly, aes(d13C, d15N, colour = `Bone or Dentine`, fill = `Bone or Dentine`)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
England_CN_Bag_adults_matched

#BEST tests for the above w/o kids 
Bone_England_matched_adults<-subset(England_bonedent_matched_adultonly, `Bone or Dentine`=="Bone") #English human bone C&N matched
Dentine_England_matched_adults<-subset(England_bonedent_matched_adultonly, `Bone or Dentine`=="Dentine")#English human dentine C&N matched 
EngBoneC_matchedadults<-c(Bone_England_matched_adults$d13C)
EngDentineC_matchedadults<-c(Dentine_England_matched_adults$d13C)
carbonEng1_matched_adult<-na.omit(EngBoneC_matchedadults)
carbonEng2_matched_adult<-na.omit(EngDentineC_matchedadults)
BESTout_England_collC_matched_adults <- BESTmcmc(carbonEng1_matched_adult, carbonEng2_matched_adult, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collC_matched_adults)
plotAll(BESTout_England_collC_matched_adults)

EngBoneN_matched_adults<-c(Bone_England_matched_adults$d15N)
EngDentineN_matched_adults<-c(Dentine_England_matched_adults$d15N)
nitrogenEng1_matched_adults<-na.omit(EngBoneN_matched_adults)
nitrogenEng2_matched_adults<-na.omit(EngDentineN_matched_adults)
BESTout_England_collN_matched_adults<- BESTmcmc(nitrogenEng1_matched_adults, nitrogenEng2_matched_adults, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collN_matched_adults)
plotAll(BESTout_England_collN_matched_adults)



library(BEST) #make sure you have the latest JAGS and rjags installed, if you're having problems, quit R, reinstall JAGS, open R and reinstall both rjags and BEST
Bone_England_adult<-subset(England_bonedent_adultonly, `Dentine or Bone`=="Bone") #English human bone C&N
Dentine_England_adult<-subset(England_bonedent_adultonly, `Dentine or Bone`=="Dentine")#English human dentine C&N
EngBoneC_adult<-c(Bone_England_adult$d13C)
EngDentineC_adult<-c(Dentine_England_adult$d13C)
carbonEng1_adult<-na.omit(EngBoneC_adult)
carbonEng2_adult<-na.omit(EngDentineC_adult)
BESTout_England_collC_adult <- BESTmcmc(carbonEng1_adult, carbonEng2_adult, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collC_adult)
plotAll(BESTout_England_collC_adult)

EngBoneN_adult<-c(Bone_England_adult$d15N)
EngDentineN_adult<-c(Dentine_England_adult$d15N)
nitrogenEng1_adult<-na.omit(EngBoneN_adult)
nitrogenEng2_adult<-na.omit(EngDentineN_adult)
BESTout_England_collN_adult <- BESTmcmc(nitrogenEng1_adult, nitrogenEng2_adult, priors=NULL, parallel=FALSE)
#plot(BESTout_England_collN_adult)
plotAll(BESTout_England_collN_adult)




#Carbon, carbon and carbon

##violin plots of each carbon - ggarrange 
library(EnvStats)
England_d13C_bone_violin<-ggplot(data=England_CN_bone, aes(x="", y=England_CN_bone$d13C))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+
  theme(legend.position = "none")
England_d13C_dentine_violin<-ggplot(data=England_CN_dentine, aes(x="", y=England_CN_dentine$d13C))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+
  theme(legend.position = "none")
England_d13C_carbonate_violin<-ggplot(data=Oxy_England, aes(x="", y=Oxy_England$d13C))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-11),breaks=seq(-17,-11,2))+
  theme(legend.position = "none")

ggarrange(England_d13C_bone_violin, England_d13C_dentine_violin, England_d13C_carbonate_violin, nrow=1)
ggarrange(England_d13C_bone_violin, England_d13C_dentine_violin, England_d13C_carbonate_violin, ncol=1)

#rKin attempt for overlaps?? 

## big delta offset scatters  
matched_bone_dent_enamel_England<-read_excel("Dropbox/PhD_Cantab/England_Bone_Dentine_CN_simple.xlsx", sheet = "Matched_bone_dentine_enamel")

matched_bone_dent_enamel_England$PeriodBroad<-matched_bone_dent_enamel_England$`Date Category`
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-C', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A/B', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-D', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-E', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B/C', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D/E', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.350AD-790AD'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-G', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-G', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
matched_bone_dent_enamel_England <- matched_bone_dent_enamel_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'G', 'c.790AD-1066AD+'))



matched_bone_dent_enamel_England$`PeriodBroad` = factor(matched_bone_dent_enamel_England$`PeriodBroad`,
                                       levels=c("c.350AD-790AD","c.790AD-1066AD+"),ordered=TRUE)


England_bigdelta_CN_bag<- ggplot(matched_bone_dent_enamel_England, aes(`D13C_dent-bone`, `D15N_dent-bone`,)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(Delta^{15},N["dentine-bone"], " (\u2030)")))+xlab(expression(paste(Delta^{13},C["dentine-bone"]," (\u2030)")))+ #need to add "dentine-bone
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
England_bigdelta_CN_bag
#bag by period? 
ggplot(matched_bone_dent_enamel_England, aes(`D13C_dent-bone`, `D15N_dent-bone`,colour=`PeriodBroad`, fill=`PeriodBroad`)) +
  theme_bw()+
  geom_bag()+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  ylab(expression(paste(Delta^{15},N["dentine-bone"], " (\u2030)")))+xlab(expression(paste(Delta^{13},C["dentine-bone"]," (\u2030)")))+ #need to add "dentine-bone
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))

#big delta 13C tooth vs big delta 13 Ccollagen
ggplot(matched_bone_dent_enamel_England, aes(D13C_tooth_enamel_dent, `D13C_dent-bone`)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  xlab(expression(paste(Delta^{13},C["carbonate-dentine"], " (\u2030)")))+ylab(expression(paste(Delta^{13},C["dentine-bone"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
  ##bag by period
ggplot(matched_bone_dent_enamel_England, aes(D13C_tooth_enamel_dent, `D13C_dent-bone`, colour=`PeriodBroad`, fill=`PeriodBroad`)) +
  theme_bw()+
  geom_bag()+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  xlab(expression(paste(Delta^{13},C["carbonate-dentine"], " (\u2030)")))+ylab(expression(paste(Delta^{13},C["dentine-bone"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))

#big delta 13C enamel vs delta 13 Ccarb
ggplot(matched_bone_dent_enamel_England, aes(D13C_tooth_enamel_dent, enamel_d13C)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  xlab(expression(paste(Delta^{13},C["carbonate-dentine"], " (\u2030)")))+ylab(expression(paste(delta^{13},C["carb"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))

#same by period 
ggplot(matched_bone_dent_enamel_England, aes(D13C_tooth_enamel_dent, enamel_d13C, colour=`PeriodBroad`, fill=`PeriodBroad`)) +
  theme_bw()+
  geom_bag()+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  xlab(expression(paste(Delta^{13},C["carbonate-dentine"], " (\u2030)")))+ylab(expression(paste(delta^{13},C["carb"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))


#big delta tooth vs epsilon tooth linear regression 
library(ggpmisc)
ggplot(matched_bone_dent_enamel_England,aes(D13C_tooth_enamel_dent, `E13Cenamel-dentinecoll`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size=10) +
  ylab(expression(paste(epsilon^{13},C["enamel-dentine"], " (\u2030)")))+xlab(expression(paste(Delta^{13},C["enamel-dentine"], " (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
  

#big delta collagen vs epsilon collagen linear regression
ggplot(matched_bone_dent_enamel_England,aes(`D13C_dent-bone`, `E13Cdent-bone`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size=10) +
  ylab(expression(paste(epsilon^{13},C["dentine-bone"], " (\u2030)")))+xlab(expression(paste(Delta^{13},C["dentine-bone"], " (\u2030)")))+
  scale_y_continuous(limits=c(-2,2),breaks=seq(-2,2,1))+scale_x_continuous(limits=c(-2,2),breaks=seq(-2,2,1))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))


#big delta tooth vs d13C bone
ggplot(matched_bone_dent_enamel_England, aes(bone_d13C, D13C_tooth_enamel_dent)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(Delta^{13},C["enamel-dentine"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))

  ##bag by period
ggplot(matched_bone_dent_enamel_England, aes(bone_d13C, D13C_tooth_enamel_dent, colour=`PeriodBroad`, fill=`PeriodBroad`)) +
  theme_bw()+
  geom_bag()+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(Delta^{13},C["enamel-dentine"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))


#big delta tooth vs d15N bone
  ##bag by period
ggplot(matched_bone_dent_enamel_England, aes(bone_d15N, D13C_tooth_enamel_dent, colour=`PeriodBroad`, fill=`PeriodBroad`)) +
  theme_bw()+
  geom_bag()+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  xlab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+ylab(expression(paste(Delta^{13},C["enamel-dentine"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))

#big delta tooth vs d15N tooth
ggplot(matched_bone_dent_enamel_England, aes(dentine_d15N, D13C_tooth_enamel_dent)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  xlab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+ylab(expression(paste(Delta^{13},C["enamel-dentine"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))

  ##bag by period
ggplot(matched_bone_dent_enamel_England, aes(dentine_d15N, D13C_tooth_enamel_dent, colour=`PeriodBroad`, fill=`PeriodBroad`)) +
  theme_bw()+
  geom_bag()+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"), name="Broad Period")+
  xlab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+ylab(expression(paste(Delta^{13},C["enamel-dentine"]," (\u2030)")))+ 
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))


#3Dplot
library(scatterplot3d)
threeDplotcoloursCN<-c("#21908CFF","#FDE725FF")
threeDplotcoloursCN <- threeDplotcoloursCN[as.ordered(matched_bone_dent_enamel_England$PeriodBroad)]
scatterplot3d(matched_bone_dent_enamel_England$bone_d13C, matched_bone_dent_enamel_England$dentine_d13C, matched_bone_dent_enamel_England$enamel_d13C,
              xlab = (expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)"))),
              ylab = (expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)"))),
              zlab = (expression(paste(delta^{13},C["enamel carbonate (PDB)"], " (\u2030)"))), 
              angle=40, pch = 16, color=threeDplotcoloursCN)
legend("top", legend = c("c.350-790AD","c.790-1066+AD"), col=c("#21908CFF","#FDE725FF"), pch = 16,inset = -0.15, xpd = TRUE, horiz = TRUE)

scatterplot3d(matched_bone_dent_enamel_England$`D13C_dent-bone`, matched_bone_dent_enamel_England$D13C_tooth_enamel_dent, matched_bone_dent_enamel_England$`D15N_dent-bone`,
              xlab = (expression(paste(Delta^{13},C["dentine-bone"], " (\u2030)"))),
              ylab = (expression(paste(Delta^{13},C["enamel-dentine"], " (\u2030)"))),
              zlab = (expression(paste(Delta^{15},N["dentine-bone"], " (\u2030)"))), 
              angle=40, pch = 16, color=threeDplotcoloursCN)
legend("top", legend = c("c.350-790AD","c.790-1066+AD"), col=c("#21908CFF","#FDE725FF"), pch = 16,inset = -0.15, xpd = TRUE, horiz = TRUE)

scatterplot3d(matched_bone_dent_enamel_England$dentine_d13C, matched_bone_dent_enamel_England$enamel_d13C, matched_bone_dent_enamel_England$D13C_tooth_enamel_dent, 
              zlab = (expression(paste(Delta^{13},C["enamel-dentine"], " (\u2030)"))),
              xlab = (expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)"))),
              ylab = (expression(paste(delta^{13},C["enamel carbonate (PDB)"], " (\u2030)"))), 
              angle=40, pch = 16, color=threeDplotcoloursCN)
legend("top", legend = c("c.350-790AD","c.790-1066+AD"), col=c("#21908CFF","#FDE725FF"), pch = 16,inset = -0.15, xpd = TRUE, horiz = TRUE)

#accompanying 2d scatterplots
ggplot(matched_bone_dent_enamel_England,aes(bone_d13C,enamel_d13C))+
  theme_bw()+
  geom_point(aes(color=PeriodBroad),size=4,)+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"))+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+
  ylab(expression(paste(delta^{13},C["enamel carbonate (PDB)"], " (\u2030)")))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 18), legend.position = "none")

ggplot(matched_bone_dent_enamel_England,aes(bone_d13C,dentine_d13C))+
  theme_bw()+
  geom_point(aes(color=PeriodBroad),size=4,)+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"))+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+
  ylab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 18), legend.position = "none")

ggplot(matched_bone_dent_enamel_England,aes(dentine_d13C,enamel_d13C))+
  theme_bw()+
  geom_point(aes(color=PeriodBroad),size=4,)+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"))+
  xlab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+
  ylab(expression(paste(delta^{13},C["enamel carbonate (PDB)"], " (\u2030)")))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 18), legend.position = "none")


ggplot(matched_bone_dent_enamel_England,aes(`D13C_dent-bone`,`D15N_dent-bone`))+
  theme_bw()+
  geom_point(aes(color=PeriodBroad),size=4,)+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"))+
  xlab(expression(paste(Delta^{13},C["dentine-bone"], " (\u2030)")))+
  ylab(expression(paste(Delta^{15},N["dentine-bone"], " (\u2030)")))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 18), legend.position = "none")


ggplot(matched_bone_dent_enamel_England,aes(`D13C_dent-bone`,D13C_tooth_enamel_dent))+
  theme_bw()+
  geom_point(aes(color=PeriodBroad),size=4,)+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"))+
  xlab(expression(paste(Delta^{13},C["dentine-bone"], " (\u2030)")))+
  ylab(expression(paste(Delta^{13},C["enamel-dentine"], " (\u2030)")))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 18), legend.position = "none")


ggplot(matched_bone_dent_enamel_England,aes(D13C_tooth_enamel_dent,`D15N_dent-bone`))+
  theme_bw()+
  geom_point(aes(color=PeriodBroad),size=4,)+
  scale_colour_manual(values=c("#21908CFF","#FDE725FF"))+
  ylab(expression(paste(Delta^{15},N["dentine-bone"], " (\u2030)")))+
  xlab(expression(paste(Delta^{13},C["enamel-dentine"], " (\u2030)")))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 18), legend.position = "none")

###Height and Diet###
ggplot(England_CN_bone, aes(x=`Stature (cm)`)) + geom_histogram(binwidth=0.8)+
  geom_vline(data=C_N_Database_bone, aes(xintercept=166.21), color="red", linetype="dotted", size=2)+
  xlab(expression(paste("Stature (cm)")))+ylab("Count")+
  theme_bw() #bimodal and right skewed 

Eng_C_height_bone_scatter<- ggplot(England_CN_bone,aes(England_CN_bone$`Stature (cm)`, d13C))+
  theme_bw()+
  geom_point(size=3,shape=16)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlab(expression(paste("Stature (cm)")))+ylab(expression(paste(delta^{13},"C (\u2030)")))#+
  #scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))
Eng_C_height_bone_scatter

Eng_N_height_bone_scatter<- ggplot(England_CN_bone,aes(England_CN_bone$`Stature (cm)`,d15N))+
  theme_bw()+
  geom_point(size=3,shape=16)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlab(expression(paste("Stature (cm)")))+ylab(expression(paste(delta^{15},"N (\u2030)")))#+
#scale_x_continuous(limits=c(-24.5,-12.5),breaks=seq(-24.5,-12.5,1))+scale_y_continuous(limits=c(0,20),breaks=seq(0,20,1))
Eng_N_height_bone_scatter

ggMarginal(Eng_C_height_bone_scatter ) #marginal distribution for all bone
ggMarginal(Eng_C_height_bone_scatter , type = "boxplot")
ggMarginal(Eng_N_height_bone_scatter ) #marginal distribution for all bone
ggMarginal(Eng_N_height_bone_scatter , type = "boxplot")

#diet in England over time - going to look at bone only here for simplicity  #fill=Oxy_England$SimpleSex #fill=Oxy_England$SimpleSex
ggplot(data=England_CN_bone, aes(x=England_CN_bone$SimpleDate, y=England_CN_bone$d13C))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco()+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))#+
  #scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  #theme(legend.position = "top", legend.direction = "horizontal")


England_CN_bone$SimpleDate<-England_CN_bone$`Date Category`
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A-C', 'A-D'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A/B', 'A-D'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A-D', 'A-E'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-D', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-E', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-F', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B/C', 'B-G'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-F', 'C/D'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-E', 'C-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-G', 'C-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F-H', 'F-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-G', 'E-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'D-G', 'D-H'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'D/E', 'D-F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E', 'E/F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F', 'E/F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-H', 'E/F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F-I', 'E-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'G', 'G-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'G/H', 'G-I'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'I', 'G-I'))

library("ggsci")
# scale_fill_manual(values=cbPalette)
# scale_colour_manual(values=cbPalette)
England_CN_bone$`SimpleDate` = factor(England_CN_bone$`SimpleDate`,
                                  levels=c("A", "A-E","B","B-G","C","C/D","C-H","D","D-F","D-H","E/F","E-I","F/G","G-I"),ordered=TRUE)

ggplot(data=England_CN_bone, aes(x=England_CN_bone$`SimpleDate`, y=England_CN_bone$d13C, fill=`SimpleDate`))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+
  theme(legend.position = "none")

ggplot(data=England_CN_bone, aes(x=England_CN_bone$`SimpleDate`, y=England_CN_bone$d15N, fill=`SimpleDate`))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2))+
  theme(legend.position = "none")

ggplot(England_CN_bone,aes(x=d13C,y=d15N,color=`SimpleDate`))+
  theme_bw()+
  geom_point(shape=16, size=3)+
  scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))

ggplot(England_CN_bone,aes(x=d13C,y=d15N,color=`SimpleDate`))+
  theme_bw()+
  geom_point(shape=16, size=3)+
  scale_colour_viridis(discrete = TRUE)+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))

#combine date categories further similar to carbonate with pre-migration period, migration period to "viking" and viking to Norman
#now for date categories super simple - 200BC-450 AD, ~350-790AD, ~790AD-1066+
England_CN_bone$SimpleDate<-as.character(England_CN_bone$SimpleDate)
England_CN_bone$PeriodBroad<-England_CN_bone$`SimpleDate`
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A', '200BC-450AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-E', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-G', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-H', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.350AD-790AD'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-H', 'c.790AD-1066AD+'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E-I', 'c.790AD-1066AD+'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
England_CN_bone <- England_CN_bone %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'G-I', 'c.790AD-1066AD+'))

England_CN_bone$`PeriodBroad` = factor(England_CN_bone$`PeriodBroad`,
                                   levels=c("200BC-450AD", "c.350AD-790AD","c.790AD-1066AD+"),ordered=TRUE)


ggplot(data=England_CN_bone, aes(x=England_CN_bone$PeriodBroad, y=England_CN_bone$d13C, fill=PeriodBroad))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))

ggplot(data=England_CN_bone, aes(x=England_CN_bone$PeriodBroad, y=England_CN_bone$d15N, fill=PeriodBroad))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))

ggplot(England_CN_bone,aes(x=d13C,y=d15N,color=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=3)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))

England_Bone_Broad_Bag<- ggplot(England_CN_bone, aes(d13C, d15N, colour = PeriodBroad, fill = PeriodBroad)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  scale_fill_viridis(discrete = TRUE,name="Broad Period")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
England_Bone_Broad_Bag

#carbonate d13C through time
ggplot(data=Oxy_England, aes(x=PeriodBroad, y=d13C, fill=PeriodBroad))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_manual(values=c("#21908CFF","#FDE725FF"))+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-17,-11),breaks=seq(-17,-11,1))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))

#dentine d13C and d15N through time

ggplot(data=England_CN_dentine, aes(x=England_CN_dentine$`Date Category`, y=England_CN_dentine$d13C, fill=`Date Category`))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))

ggplot(data=England_CN_dentine, aes(x=England_CN_dentine$`Date Category`, y=England_CN_dentine$d15N, fill=`Date Category`))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(0,21),breaks=seq(0,21,2))+
  theme(legend.position = "none")


England_CN_dentine$PeriodBroad<-England_CN_dentine$`Date Category`
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-C', '200BC-450AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-D', '200BC-450AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A/B', '200BC-450AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-D', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-E', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-F', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-G', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B/C', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-F', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-G', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D/E', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E', 'c.450AD-790AD'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-G', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-H', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F-H', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
England_CN_dentine <- England_CN_dentine %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'G', 'c.790AD-1066AD+'))


England_CN_dentine$`PeriodBroad` = factor(England_CN_dentine$`PeriodBroad`,
                                       levels=c("200BC-450AD", "c.450AD-790AD","c.790AD-1066AD+"),ordered=TRUE)


ggplot(data=England_CN_dentine, aes(x=England_CN_dentine$`PeriodBroad`, y=England_CN_dentine$d13C, fill=`PeriodBroad`))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{13},C["dentine (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-25,-17),breaks=seq(-25,-17,2))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))

ggplot(data=England_CN_dentine, aes(x=England_CN_dentine$`PeriodBroad`, y=England_CN_dentine$d15N, fill=`PeriodBroad`))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_viridis(discrete = TRUE)+
  #scale_color_manual(values=qualcolourPalette)+
  ylab(expression(paste(delta^{15},N["dentine (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(0,21),breaks=seq(0,21,2))+
  theme(legend.position = "none", axis.text = element_text(size = 18), axis.title = element_text(size=20))



#BEST tests and BANOVAs for differences between time periods 
library(BEST)
England_Roman_Bone<-subset(England_CN_bone, PeriodBroad=="200BC-450AD")
England_PostRoman_Bone<-subset(England_CN_bone, PeriodBroad=="c.350AD-790AD")
England_Viking_Bone<-subset(England_CN_bone, PeriodBroad=="c.790AD-1066AD+")

EngBoneRomanC<-c(England_Roman_Bone$d13C)
EngBoneEMC<-c(England_PostRoman_Bone$d13C)
EngBoneVikingC<-c(England_Viking_Bone$d13C)
carbonEng1_period<-na.omit(EngBoneRomanC)
carbonEng2_period<-na.omit(EngBoneEMC)
carbonEng3_period<-na.omit(EngBoneVikingC)
BESTout_England_Roman_PR_C <- BESTmcmc(carbonEng1_period, carbonEng2_period, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_PR_C)
plotAll(BESTout_England_Roman_PR_C)

BESTout_England_PR_Vik_C <- BESTmcmc(carbonEng2_period, carbonEng3_period, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PR_Vik_C)
plotAll(BESTout_England_PR_Vik_C)

EngBoneRomanN<-c(England_Roman_Bone$d15N)
EngBoneEMN<-c(England_PostRoman_Bone$d15N)
EngBoneVikingN<-c(England_Viking_Bone$d15N)
nitrogenEng1_period<-na.omit(EngBoneRomanN)
nitrogenEng2_period<-na.omit(EngBoneEMN)
nitrogenEng3_period<-na.omit(EngBoneVikingN)
BESTout_England_Roman_PR_N <- BESTmcmc(nitrogenEng1_period, nitrogenEng2_period, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_PR_N)
plotAll(BESTout_England_Roman_PR_N)

BESTout_England_PR_Vik_N <- BESTmcmc(nitrogenEng2_period, nitrogenEng3_period, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PR_Vik_N)
plotAll(BESTout_England_PR_Vik_N)

#BANOVA
#first doing a normal ANOVAs and MANOVA 


#now attempting a BANOVA 
library(rjags)
library(BANOVA)
set.seed(400)
Eng_BANOVA_period_C_bone <- BANOVA.Poisson(d13C ~ owner + age + gender + selfbrand *
                            + conspic, l1_hyper = c(1,1,0.0001), data = England_CN_bone, id = England_CN_bone$`Unique ID`,
                          + burnin = 5000, sample = 1000, thin = 10)
summary(Eng_BANOVA_period_C_bone)
set.seed(400)
Eng_BANOVA_period_N_bone<- BANOVA.Poisson(attitude ~ owner + age + gender + selfbrand *conspic, l1_hyper = c(1,1,0.0001), data = England_CN_bone, id = England_CN_bone$`Unique ID`, burnin = 5000, sample = 1000, thin = 10)
summary(Eng_BANOVA_period_N_bone)

#burial practices and diet - number of grave goods, number of foreign objects, grave orientation, body position, internment style

England_CN_bone$`Number of Grave Goods` = factor(England_CN_bone$`Number of Grave Goods`,
                                       levels=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","21","22","25","26","43","51","Unknown"),ordered=TRUE)

ggplot(England_CN_bone,aes(x=d13C,y=d15N,colour=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=2)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ggtitle("Number of Grave Goods") +
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`Number of Grave Goods`) 

ggplot(England_CN_bone,aes(x=d13C,y=d15N,colour=`Number of Grave Goods`,fill=`Number of Grave Goods`))+
  geom_bag()+
  theme_bw()+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`Number of Grave Goods`) #not working for some reason will just keep as faceted scatters 


England_CN_bone$`Foreign Objects` = factor(England_CN_bone$`Foreign Objects`,
                                                 levels=c("0","1","2","3","4","5","6","9","11","21","Unknown"),ordered=TRUE)


ggplot(England_CN_bone,aes(x=d13C,y=d15N, colour=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=2)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ggtitle("Number of 'Foreign' Grave Goods") +
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`Foreign Objects`) 

England_CN_bone$`Grave Orientation` = factor(England_CN_bone$`Grave Orientation`,
                                           levels=c("N-S","NNE-SSW","NE-SW","ENE-WSW","E-W","ESE-WNW","SE-NW","SSE-NNW","S-N","SSW-NNE","SW-NE","WSW-ENE","W-E","WNW-ESE","NW-SE","NNW-SSE", "Damaged","Disarticulated","Disturbed"),ordered=TRUE)


ggplot(England_CN_bone,aes(x=d13C,y=d15N, colour=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=2)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ggtitle("Grave Orientation") +
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`Grave Orientation`) 

England_CN_bone <- England_CN_bone %>% mutate(`Body Position` = replace(`Body Position`, `Body Position` == 'Extended supine', 'Extended Supine'))
England_CN_bone <- England_CN_bone %>% mutate(`Body Position` = replace(`Body Position`, `Body Position` == 'unknown', 'Unknown'))


ggplot(England_CN_bone,aes(x=d13C,y=d15N,colour=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=2)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ggtitle("Body Position") +
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`Body Position`)

ggplot(England_CN_bone,aes(x=d13C,y=d15N, colour=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=2)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ggtitle("Internment Style") +
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`Internment Style`) #wayyyyy too much variation not including the plot, probably unhelpful archaeologically to reduce the "noise" here 

#grouping grave goods numbers into broader categories
England_CN_bone$ggoods_number_grouped<-England_CN_bone$`Number of Grave Goods`
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '1', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '2', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '3', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '4', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '5', '5-10'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '6', '5-10'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '7', '5-10'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '8', '5-10'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '9', '5-10'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '10', '5-10'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '11', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '12', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '13', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '14', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '15', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '16', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '17', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '18', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '19', '11-20'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '21', '20+'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '22', '20+'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '25', '20+'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '26', '20+'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '43', '20+'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == '51', '20+'))
England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == 'Unknown', 'Unknown/NA'))
#England_CN_bone <- England_CN_bone %>% mutate(`ggoods_number_grouped` = replace(`ggoods_number_grouped`, `ggoods_number_grouped` == NA, 'Unknown/NA'))
England_CN_bone$`ggoods_number_grouped` %>% replace_na("Unknown/NA") #not actually executed in the dataframe for some reason
England_CN_bone$`ggoods_number_grouped` [is.na(England_CN_bone$`ggoods_number_grouped` )] <- "Unknown/NA"

England_CN_bone$`ggoods_number_grouped` = factor(England_CN_bone$`ggoods_number_grouped`,
                                           levels=c("0","1-4", "5-10","11-20","20+","Unknown/NA"),ordered=TRUE)

ggplot(England_CN_bone,aes(x=d13C,y=d15N,colour=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=2)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ggtitle("Number of Grave Goods") +
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`ggoods_number_grouped`)+
  theme(axis.title = element_text(size=22), axis.text = element_text(size=20), plot.title = element_text(size=26), legend.title = element_text(size=22), legend.text = element_text(size=20),  strip.text = element_text(size = 20))

#same for number of foreign grave goods
England_CN_bone$foreign_ggoods_grouped<-England_CN_bone$`Foreign Objects`
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '1', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '2', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '3', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '4', '1-4'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '6', '5+'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '9', '5+'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '11', '10+'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == '21', '10+'))
England_CN_bone <- England_CN_bone %>% mutate(`foreign_ggoods_grouped` = replace(`foreign_ggoods_grouped`, `foreign_ggoods_grouped` == 'Unknown', 'Unknown/NA'))

England_CN_bone$`foreign_ggoods_grouped` = factor(England_CN_bone$`foreign_ggoods_grouped`,
                                                 levels=c("0","1-4", "5+","10+","Unknown/NA"),ordered=TRUE)
England_CN_bone$`foreign_ggoods_grouped` [is.na(England_CN_bone$`foreign_ggoods_grouped` )] <- "Unknown/NA"





ggplot(England_CN_bone,aes(x=d13C,y=d15N,colour=PeriodBroad))+
  theme_bw()+
  geom_point(shape=16, size=2)+
  scale_colour_viridis(discrete = TRUE, name="Broad Period")+
  ggtitle("Number of 'Foreign' Grave Goods") +
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+scale_y_continuous(limits=c(0,16),breaks=seq(0,16,2))+
  facet_wrap(~England_CN_bone$`foreign_ggoods_grouped`)+
  theme(axis.title = element_text(size=22), axis.text = element_text(size=20), plot.title = element_text(size=26), legend.title = element_text(size=22), legend.text = element_text(size=20),  strip.text = element_text(size = 20))



#sex differences in diet, and through time? 
England_CN_bone$SimpleSex<-England_CN_bone$Sex
England_CN_bone <- England_CN_bone %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F?', 'F'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M?', 'M'))
England_CN_bone <- England_CN_bone %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))

ggplot(data=England_CN_bone, aes(x=England_CN_bone$PeriodBroad, y=England_CN_bone$d13C, fill=England_CN_bone$SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco(name="Osteological Sex")+
  ylab(expression(paste(delta^{13},C["bone (PDB)"], " (\u2030)")))+xlab(expression(paste("Broad Period")))+
  scale_y_continuous(limits=c(-25,-15),breaks=seq(-25,-15,2))+
  theme(legend.position = "top", legend.direction = "horizontal")

ggplot(data=England_CN_bone, aes(x=England_CN_bone$PeriodBroad, y=England_CN_bone$d15N, fill=England_CN_bone$SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco(name="Osteological Sex")+
  ylab(expression(paste(delta^{15},N["bone (AIR)"], " (\u2030)")))+xlab(expression(paste("Broad Period")))+
  scale_y_continuous(limits=c(0,18),breaks=seq(0,18,2))+
  theme(legend.position = "top", legend.direction = "horizontal")

#BEST tests for sex total
library(BEST)
England_Female_bone<-subset(England_CN_bone, SimpleSex=="F")
England_Male_bone<-subset(England_CN_bone, SimpleSex=="M")

EngBoneF_C<-c(England_Female_bone$d13C)
EngBoneM_C<-c(England_Male_bone$d13C)

EngBoneF_C_clean<-na.omit(EngBoneF_C)
EngBoneM_C_clean<-na.omit(EngBoneM_C)

BESTout_England_Sex_C <- BESTmcmc(EngBoneF_C_clean, EngBoneM_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Sex_C)
plotAll(BESTout_England_Sex_C)

EngBoneF_N<-c(England_Female_bone$d15N)
EngBoneM_N<-c(England_Male_bone$d15N)

EngBoneF_N_clean<-na.omit(EngBoneF_N)
EngBoneM_N_clean<-na.omit(EngBoneM_N)

BESTout_England_Sex_N <- BESTmcmc(EngBoneF_N_clean, EngBoneM_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Sex_N)
plotAll(BESTout_England_Sex_N)

#for time periods by sex 

#England_Roman_Bone<-subset(England_CN_bone, PeriodBroad=="200BC-450AD")
#England_PostRoman_Bone<-subset(England_CN_bone, PeriodBroad=="c.350AD-790AD")
#England_Viking_Bone<-subset(England_CN_bone, PeriodBroad=="c.790AD-1066AD+")

Eng_Roman_Bone_F<-subset(England_Roman_Bone, SimpleSex=="F")
Eng_Roman_Bone_M<-subset(England_Roman_Bone, SimpleSex=="M")
Eng_PostRoman_Bone_F<-subset(England_PostRoman_Bone, SimpleSex=="F")
Eng_PostRoman_Bone_M<-subset(England_PostRoman_Bone, SimpleSex=="M")
Eng_Viking_Bone_F<-subset(England_Viking_Bone, SimpleSex=="F")
Eng_Viking_Bone_M<-subset(England_Viking_Bone, SimpleSex=="M")

EngBoneRoman_F_C<-c(Eng_Roman_Bone_F$d13C)
EngBoneRoman_M_C<-c(Eng_Roman_Bone_M$d13C)
EngBoneEM_F_C<-c(Eng_PostRoman_Bone_F$d13C)
EngBoneEM_M_C<-c(Eng_PostRoman_Bone_M$d13C)
EngBoneViking_F_C<-c(Eng_Viking_Bone_F$d13C)
EngBoneViking_M_C<-c(Eng_Viking_Bone_M$d13C)

EngBoneRoman_F_C_clean<-na.omit(EngBoneRoman_F_C)
EngBoneRoman_M_C_clean<-na.omit(EngBoneRoman_M_C)
EngBoneEM_F_C_clean<-na.omit(EngBoneEM_F_C)
EngBoneEM_M_C_clean<-na.omit(EngBoneEM_M_C)
EngBoneViking_F_C_clean<-na.omit(EngBoneViking_F_C)
EngBoneViking_M_C_clean<-na.omit(EngBoneViking_M_C)

#Roman F vs M carbon
BESTout_England_Roman_Sex_C <- BESTmcmc(EngBoneRoman_F_C_clean, EngBoneRoman_M_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_Sex_C)
plotAll(BESTout_England_Roman_Sex_C)

#Post-Roman F vs M carbon
BESTout_England_PostRoman_Sex_C <- BESTmcmc(EngBoneEM_F_C_clean, EngBoneEM_M_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PostRoman_Sex_C)
plotAll(BESTout_England_PostRoman_Sex_C)

#Viking F vs M carbon
BESTout_England_Viking_Sex_C <- BESTmcmc(EngBoneViking_F_C_clean, EngBoneViking_M_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PostRoman_Sex_C)
plotAll(BESTout_England_Viking_Sex_C)


#nitrogen
EngBoneRoman_F_N<-c(Eng_Roman_Bone_F$d15N)
EngBoneRoman_M_N<-c(Eng_Roman_Bone_M$d15N)
EngBoneEM_F_N<-c(Eng_PostRoman_Bone_F$d15N)
EngBoneEM_M_N<-c(Eng_PostRoman_Bone_M$d15N)
EngBoneViking_F_N<-c(Eng_Viking_Bone_F$d15N)
EngBoneViking_M_N<-c(Eng_Viking_Bone_M$d15N)

EngBoneRoman_F_N_clean<-na.omit(EngBoneRoman_F_N)
EngBoneRoman_M_N_clean<-na.omit(EngBoneRoman_M_N)
EngBoneEM_F_N_clean<-na.omit(EngBoneEM_F_N)
EngBoneEM_M_N_clean<-na.omit(EngBoneEM_M_N)
EngBoneViking_F_N_clean<-na.omit(EngBoneViking_F_N)
EngBoneViking_M_N_clean<-na.omit(EngBoneViking_M_N)

#Roman F vs M nitrogen
BESTout_England_Roman_Sex_N <- BESTmcmc(EngBoneRoman_F_N_clean, EngBoneRoman_M_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_Sex_N)
plotAll(BESTout_England_Roman_Sex_N)

#Post-Roman F vs M nitrogen
BESTout_England_PostRoman_Sex_N <- BESTmcmc(EngBoneEM_F_N_clean, EngBoneEM_M_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PostRoman_Sex_N)
plotAll(BESTout_England_PostRoman_Sex_N)

#Viking F vs M nitrogen
BESTout_England_Viking_Sex_N <- BESTmcmc(EngBoneViking_F_N_clean, EngBoneViking_M_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PostRoman_Sex_N)
plotAll(BESTout_England_Viking_Sex_N)

#same for d13C enamel
#by sex
England_Female_enamel<-subset(Oxy_England, SimpleSex=="F")
England_Male_enamel<-subset(Oxy_England, SimpleSex=="M")

EngEnamelF_C<-c(England_Female_enamel$d13C)
EngEnamelM_C<-c(England_Male_enamel$d13C)

EngEnamelF_C_clean<-na.omit(EngEnamelF_C)
EngEnamelM_C_clean<-na.omit(EngEnamelM_C)

BESTout_England_Sex_C <- BESTmcmc(EngEnamelF_C_clean, EngEnamelM_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Sex_C)
plotAll(BESTout_England_Sex_C)

#by period
England_PostRoman_Enamel<-subset(Oxy_England, PeriodBroad=="c.350AD-790AD")
England_Viking_Enamel<-subset(Oxy_England, PeriodBroad=="c.790AD-1066AD+")

EngEnamelEMC<-c(England_PostRoman_Enamel$d13C)
EngEnamelVikingC<-c(England_Viking_Enamel$d13C)
carbonate_Eng_EMC<-na.omit(EngEnamelEMC)
carbonate_Eng_Vik<-na.omit(EngEnamelVikingC)
BESTout_England_EM_Vik_enamel <- BESTmcmc(carbonate_Eng_EMC, carbonate_Eng_Vik, priors=NULL, parallel=FALSE)
#plot(BESTout_England_EM_Vik_enamel)
plotAll(BESTout_England_EM_Vik_enamel)

#by sex for Early Medieval and Viking
England_PostRoman_Enamel_F<-subset(England_PostRoman_Enamel, SimpleSex=="F")
England_PostRoman_Enamel_M<-subset(England_PostRoman_Enamel, SimpleSex=="M")
England_Viking_Enamel_F<-subset(England_Viking_Enamel, SimpleSex=="F")
England_Viking_Enamel_M<-subset(England_Viking_Enamel, SimpleSex=="M")

EngEnamelEMC_F<-c(England_PostRoman_Enamel_F$d13C)
EngEnamelEMC_M<-c(England_PostRoman_Enamel_M$d13C)
carbonate_Eng_EMC_F<-na.omit(EngEnamelEMC_F)
carbonate_Eng_EMC_M<-na.omit(EngEnamelEMC_M)

BESTout_England_EM_enamel_C_sex <- BESTmcmc(carbonate_Eng_EMC_F, carbonate_Eng_EMC_M, priors=NULL, parallel=FALSE)
#plot(BESTout_England_EM_enamel_C_sex)
plotAll(BESTout_England_EM_enamel_C_sex)

EngEnamelVikC_F<-c(England_Viking_Enamel_F$d13C)
EngEnamelVikC_M<-c(England_Viking_Enamel_M$d13C)
carbonate_Eng_VikC_F<-na.omit(EngEnamelVikC_F)
carbonate_Eng_VikC_M<-na.omit(EngEnamelVikC_M)

BESTout_England_Vik_enamel_C_sex <- BESTmcmc(carbonate_Eng_VikC_F, carbonate_Eng_VikC_M, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Vik_enamel_C_sex)
plotAll(BESTout_England_Vik_enamel_C_sex)

#same for Dentine
#by period
England_Roman_Dentine<-subset(England_CN_dentine, PeriodBroad=="200BC-450AD")
England_PostRoman_Dentine<-subset(England_CN_dentine, PeriodBroad=="c.450AD-790AD")
England_Viking_Dentine<-subset(England_CN_dentine, PeriodBroad=="c.790AD-1066AD+")

EngDentine_Roman_C<-c(England_Roman_Dentine$d13C)
EngDentine_EM_C<-c(England_PostRoman_Dentine$d13C)
EngDentine_Viking_C<-c(England_Viking_Dentine$d13C)

dentine_Eng_Roman_C<-na.omit(EngDentine_Roman_C)
dentine_Eng_EM_C<-na.omit(EngDentine_EM_C)
dentine_Eng_Vik_C<-na.omit(EngDentine_Viking_C)

BESTout_England_Roman_EM_dentine_C <- BESTmcmc(dentine_Eng_Roman_C, dentine_Eng_EM_C, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_EM_dentine_C)
plotAll(BESTout_England_Roman_EM_dentine_C)

BESTout_England_EM_Vik_dentine_C <- BESTmcmc(dentine_Eng_EM_C, dentine_Eng_Vik_C, priors=NULL, parallel=FALSE)
#plot(BESTout_England_EM_Vik_dentine_C)
plotAll(BESTout_England_EM_Vik_dentine_C)

##nitrogen
EngDentine_Roman_N<-c(England_Roman_Dentine$d15N)
EngDentine_EM_N<-c(England_PostRoman_Dentine$d15N)
EngDentine_Viking_N<-c(England_Viking_Dentine$d15N)
dentine_Eng_Roman_N<-na.omit(EngDentine_Roman_N)
dentine_Eng_EM_N<-na.omit(EngDentine_EM_N)
dentine_Eng_Vik_N<-na.omit(EngDentine_Viking_N)

BESTout_England_Roman_EM_dentine_N <- BESTmcmc(dentine_Eng_Roman_N, dentine_Eng_EM_N, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_EM_dentine_N)
plotAll(BESTout_England_Roman_EM_dentine_N)

BESTout_England_EM_Vik_dentine_N <- BESTmcmc(dentine_Eng_EM_N, dentine_Eng_Vik_N, priors=NULL, parallel=FALSE)
#plot(BESTout_England_EM_Vik_dentine_N)
plotAll(BESTout_England_EM_Vik_dentine_N)



#by sex and time period
England_CN_dentine$SimpleSex<-England_CN_dentine$Sex
England_CN_dentine <- England_CN_dentine %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F?', 'F'))
England_CN_dentine <- England_CN_dentine %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M?', 'M'))
England_CN_dentine <- England_CN_dentine %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))

England_Female_dentine<-subset(England_CN_dentine, SimpleSex=="F")
England_Male_dentine<-subset(England_CN_dentine, SimpleSex=="M")

EngDentineF_C<-c(England_Female_dentine$d13C)
EngDentineM_C<-c(England_Male_dentine$d13C)

EngDentineF_C_clean<-na.omit(EngDentineF_C)
EngDentineM_C_clean<-na.omit(EngDentineM_C)

BESTout_England_Sex_C_dent <- BESTmcmc(EngDentineF_C_clean, EngDentineM_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Sex_C_dent)
plotAll(BESTout_England_Sex_C_dent)

EngDentineF_N<-c(England_Female_dentine$d15N)
EngDentineM_N<-c(England_Male_dentine$d15N)

EngDentineF_N_clean<-na.omit(EngDentineF_N)
EngDentineM_N_clean<-na.omit(EngDentineM_N)

BESTout_England_Sex_N_dent <- BESTmcmc(EngDentineF_N_clean, EngDentineM_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Sex_N_dent)
plotAll(BESTout_England_Sex_N_dent)

#for time periods by sex 

#England_Roman_Bone<-subset(England_CN_bone, PeriodBroad=="200BC-450AD")
#England_PostRoman_Bone<-subset(England_CN_bone, PeriodBroad=="c.350AD-790AD")
#England_Viking_Bone<-subset(England_CN_bone, PeriodBroad=="c.790AD-1066AD+")

Eng_Roman_Dentine_F<-subset(England_Roman_Dentine, SimpleSex=="F")
Eng_Roman_Dentine_M<-subset(England_Roman_Dentine, SimpleSex=="M")
Eng_PostRoman_Dentine_F<-subset(England_PostRoman_Dentine, SimpleSex=="F")
Eng_PostRoman_Dentine_M<-subset(England_PostRoman_Dentine, SimpleSex=="M")
Eng_Viking_Dentine_F<-subset(England_Viking_Dentine, SimpleSex=="F")
Eng_Viking_Dentine_M<-subset(England_Viking_Dentine, SimpleSex=="M")

EngDentineRoman_F_C<-c(Eng_Roman_Dentine_F$d13C)
EngBDentineRoman_M_C<-c(Eng_Roman_Dentine_M$d13C)
EngDentineEM_F_C<-c(Eng_PostRoman_Dentine_F$d13C)
EngDentineEM_M_C<-c(Eng_PostRoman_Dentine_M$d13C)
EngDentineViking_F_C<-c(Eng_Viking_Dentine_F$d13C)
EngDentineViking_M_C<-c(Eng_Viking_Dentine_M$d13C)

EngDentineRoman_F_C_clean<-na.omit(EngDentineRoman_F_C)
EngDentineRoman_M_C_clean<-na.omit(EngBDentineRoman_M_C)
EngDentineEM_F_C_clean<-na.omit(EngDentineEM_F_C)
EngDentineEM_M_C_clean<-na.omit(EngDentineEM_M_C)
EngDentineViking_F_C_clean<-na.omit(EngDentineViking_F_C)
EngDentineViking_M_C_clean<-na.omit(EngDentineViking_M_C)

#Roman F vs M carbon
BESTout_England_Roman_Sex_C_dent <- BESTmcmc(EngDentineRoman_F_C_clean, EngDentineRoman_M_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_Sex_C_dent)
plotAll(BESTout_England_Roman_Sex_C_dent)

#Post-Roman F vs M carbon
BESTout_England_PostRoman_Sex_C_dent <- BESTmcmc(EngDentineEM_F_C_clean, EngDentineEM_M_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PostRoman_Sex_C_dent)
plotAll(BESTout_England_PostRoman_Sex_C_dent)

#Viking F vs M carbon
BESTout_England_Viking_Sex_C_dent <- BESTmcmc(EngDentineViking_F_C_clean, EngDentineViking_M_C_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PostRoman_Sex_C_dent)
plotAll(BESTout_England_Viking_Sex_C_dent)


#nitrogen
EngDentineRoman_F_N<-c(Eng_Roman_Dentine_F$d15N)
EngDentineRoman_M_N<-c(Eng_Roman_Dentine_M$d15N)
EngDentineEM_F_N<-c(Eng_PostRoman_Dentine_F$d15N)
EngDentineEM_M_N<-c(Eng_PostRoman_Dentine_M$d15N)
EngDentineViking_F_N<-c(Eng_Viking_Dentine_F$d15N)
EngDentineViking_M_N<-c(Eng_Viking_Dentine_M$d15N)

EngDentineRoman_F_N_clean<-na.omit(EngDentineRoman_F_N)
EngDentineRoman_M_N_clean<-na.omit(EngDentineRoman_M_N)
EngDentineEM_F_N_clean<-na.omit(EngDentineEM_F_N)
EngDentineEM_M_N_clean<-na.omit(EngDentineEM_M_N)
EngDentineViking_F_N_clean<-na.omit(EngDentineViking_F_N)
EngDentineViking_M_N_clean<-na.omit(EngDentineViking_M_N)

#Roman F vs M nitrogen
BESTout_England_Roman_Sex_N_dent <- BESTmcmc(EngDentineRoman_F_N_clean, EngDentineRoman_M_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Roman_Sex_N_dent)
plotAll(BESTout_England_Roman_Sex_N_dent)

#Post-Roman F vs M nitrogen
BESTout_England_PostRoman_Sex_N_dent <- BESTmcmc(EngDentineEM_F_N_clean, EngDentineEM_M_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_PostRoman_Sex_N_dent)
plotAll(BESTout_England_PostRoman_Sex_N_dent)

#Viking F vs M nitrogen
BESTout_England_Viking_Sex_N <- BESTmcmc(EngDentineViking_F_N_clean, EngDentineViking_M_N_clean, priors=NULL, parallel=FALSE)
#plot(BESTout_England_Viking_Sex_N)
plotAll(BESTout_England_Viking_Sex_N)




