library(readxl)
library(ggplot2)
library(ggpmisc)
Oxygen_Sr_Database <- read_excel("Dropbox/PhD_Cantab/Oxygen_Sr_Database.xlsx")
View(Oxygen_Sr_Database)
summary(Oxygen_Sr_Database)
#remove bone data
OSr_teeth<-subset(Oxygen_Sr_Database, `Bone/Tooth`!="Bone")
head(OSr_teeth)
summary(OSr_teeth)
#Ireland still seems to be skewing the data - to do with Ryan et al. 2018 - REMOVE this data 
OSr_teeth<-subset(OSr_teeth, Reference!='Ryan SE, Reynard LM, Crowley QG, Snoeck C, Tuross N (2018). "Early Medieval reliance on the land and the local: An Integrated multi-isotope study (87Sr/86Sr, δ18O, δ13C, δ15N) of diet and mirgration in Co. Meath, Ireland." Journal of Archaeological Science 98: 59-71.')
summary(OSr_teeth)

Oxy_England<- subset(OSr_teeth, Country=="England")
summary(Oxy_England)

#qqplots
require(qqplotr)
#qqplot all EM european d18O from teeth
EMEUqq <- ggplot(data = OSr_teeth, mapping = aes(sample = `O-PO4 SMOW Measured/Chenery 2012`)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
EMEUqq #close to normal but not quite
#qqplot all EM european d13C from teeth
EMEUqqd13C <- ggplot(data = OSr_teeth, mapping = aes(sample =d13C)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
EMEUqqd13C #very much not normal due to presumable c4 input
#qqplot all EMEU Sr from teeth
EMEUqqSr <- ggplot(data = OSr_teeth, mapping = aes(sample =`87Sr/86Sr`)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
EMEUqqSr #likewise very much not normal with a tail end 

#qqplot all EM England d18O from teeth
EMEngqqOxy <- ggplot(data = Oxy_England, mapping = aes(sample = `O-PO4 SMOW Measured/Chenery 2012`)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
EMEngqqOxy #close to normal but not quite
#qqplot all EM England d13C from teeth
EMEngqqd13Carb <- ggplot(data = Oxy_England, mapping = aes(sample =d13C)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
EMEngqqd13Carb #very close to normal, almost all within the envelope - all C3 input

#qqplot all EM England Sr from teeth
EMEngqqSr <- ggplot(data = Oxy_England, mapping = aes(sample =`87Sr/86Sr`)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
  theme_bw()
EMEngqqSr #exponential and not normal dist, unsurprising since its an exponential radioactive decay really 


# All Plotted in a box plot/violin with jitter ##

## Oxygen violin with boxplot and Jitter EMEU ##
ggplot(data = OSr_teeth, aes(x = "", y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,0.5))+
  theme_bw()

## Oxygen violin with boxplot and Jitter England ##
ggplot(data = Oxy_England, aes(x = "", y = Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,0.5))+
  theme_bw()

## Carbonate violin with boxplot and Jitter EMEU##
ggplot(data = OSr_teeth, aes(x = "", y = OSr_teeth$d13C)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme_bw()

## Carbonate violin with boxplot and Jitter England##
ggplot(data = Oxy_England, aes(x = "", y = Oxy_England$d13C)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme_bw()

## Strontium violin with boxplot and Jitter EMEU##
ggplot(data = OSr_teeth, aes(x = "", y = OSr_teeth$`87Sr/86Sr`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(bquote('^87Sr/^86Sr'))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+
  theme_bw()

## Strontium violin with boxplot and Jitter England##
ggplot(data = Oxy_England, aes(x = "", y = Oxy_England$`87Sr/86Sr`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(bquote('^87Sr/^86Sr'))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+
  theme_bw()

#violin plot with boxplot w/o jitter
ggplot(data = OSr_teeth, aes(x = "", y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE, show.legend = FALSE)+
  geom_boxplot(width=0.1, fill="white")+ 
  scale_fill_manual(values=c("seagreen"))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  theme_bw()+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,0.5))
#geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+

# Boxplot #
OSr_teeth$`Country Code` = as.character(OSr_teeth$`Country Code`)
OSr_teeth$`Country Code` = factor(OSr_teeth$`Country Code`,
                            levels=c("1","4","5","7","8","9","10","11", "12","13","14","15","16","17","19","22","23","25"),ordered=TRUE)
ggplot(data = OSr_teeth, aes(x = "", y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`, fill=OSr_teeth$`Country Code`)) + 
  geom_boxplot(position=position_dodge(1))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,0.5))+
  theme_bw()
  
#try this instead
OSr_teeth$`Country Code` = as.numeric(OSr_teeth$`Country Code`)
library(forcats)
library(viridis)
library(ggridges)
#oxygen
EMEU_d18O_boxjitter_country<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  #geom_hline(data=OSr_teeth, aes(yintercept= 16.6), color="black",linetype="dashed", size=1.5)+
  #geom_hline(data=OSr_teeth, aes(yintercept= 18.7), color="black",linetype="dashed", size=1.5)+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

EMEU_d18O_violin_boxjitter_country<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22),legend.position = "none")
EMEU_d18O_violin_boxjitter_country

EMEU_ridges_d18O_country<-ggplot(OSr_teeth, aes(x=`O-PO4 SMOW Measured/Chenery 2012`, y=fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+ylab(expression(paste("Country")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

EMEU_ridges_d18O_country

#enamel carbonate
EMEU_Carbonate_boxjitter_country<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$d13C)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

EMEU_Carbonate_violin_boxjitter_country<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22),legend.position = "none")
EMEU_Carbonate_violin_boxjitter_country

EMEU_ridges_carbonate_country<-ggplot(OSr_teeth, aes(x=d13C, y=fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  xlab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+ylab(expression(paste("Country")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

EMEU_ridges_carbonate_country

#enamel Strontium
EMEU_Sr_boxjitter_country<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`87Sr/86Sr`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste("87Sr/86Sr")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))


EMEU_Sr_violin_boxjitter_country<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste("87Sr/86Sr")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22),legend.position = "none")
EMEU_Sr_violin_boxjitter_country


EMEU_ridges_Sr_country<-ggplot(OSr_teeth, aes(x=`87Sr/86Sr`, y=fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  xlab(expression(paste("87Sr/86Sr")))+ylab(expression(paste("Country")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

EMEU_ridges_Sr_country



ggarrange(EMEU_d18O_boxjitter_country, EMEU_d18O_boxjitter_country, EMEU_Sr_boxjitter_country, ncol=1)

#box and jitter per site # too many don't even try

ggplot(data = OSr_teeth, aes(x = Carbonate_All$Site, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`, color=OSr_teeth$Site)) + 
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(14.5,19),breaks=seq(14.5,19,0.5))

#box and jitter per site fill colour #

ggplot(data = OSr_teeth, aes(x = OSr_teeth$Site, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), aes(fill=Carbonate_All$Site))+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(14.5,19),breaks=seq(14.5,19,0.5))+
  theme(legend.position="none")

#box and jitter per site fill colour, rotated #
ggplot(data = OSr_teeth, aes(x = OSr_teeth$Site, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), aes(fill=OSr_teeth$Site))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(14.5,19),breaks=seq(14.5,19,0.5))+
  theme(legend.position="none")

#box and jitter per site B&W #

ggplot(data = OSr_teeth, aes(x = OSr_teeth$Site, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(14.5,19),breaks=seq(14.5,19,0.5))+
  theme(legend.position="none")

#box and jitter per county B&W rotated#

ggplot(data = OSr_teeth, aes(x = OSr_teeth$County, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(14.75,19),breaks=seq(14.75,19,0.25))+
  theme(legend.position="none")

# Carbonate scatterplot # not sure why it doesn't want to play ball

ggplot(OSr_teeth,aes(d13C,`O-PO4 SMOW Measured/Chenery 2012`,color=OSr_teeth$Country))+
  theme_bw()+
  geom_point(size=3,shape=16)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1))+scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  scale_colour_manual("Country", values = c("mediumblue", "grey77","black","darkmagenta", "forestgreen", "deeppink", "chocolate", "cyan", "gold1", "red4","green","deeppink","bisque2","grey40", "navyblue", "darkorange", "pink4", "darkslategrey", "orangered1"))
  theme(legend.justification="top",axis.text=element_text(size=16),legend.title=element_text(size=16),legend.text=element_text(size=16),axis.title=element_text(size=16))

  #density plots
  library(ggalt)
  m <- ggplot(OSr_teeth,aes(d13C,`O-PO4 SMOW Measured/Chenery 2012`))+
    geom_point(size=3,shape=16)+
    ylab(expression(paste(delta^{18},"O (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
    scale_x_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1))+scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))
  
  m
  m + geom_bkde2d(bandwidth=c(0.7, 0.2)) #not sure how best to fiddle with these parameters
  m + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level..), geom = "polygon")
  m + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level.., alpha=0.5), geom = "polygon")
  
  Eng_carb_all_scatter<- ggplot(Oxy_England,aes(d13C,`O-PO4 SMOW Measured/Chenery 2012`))+
    geom_point(size=3,shape=16)+
    ylab(expression(paste(delta^{18},"O (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
    scale_x_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1))+scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))
  
  Eng_carb_all_scatter
  Eng_carb_all_scatter + geom_bkde2d(bandwidth=c(0.7, 0.2)) #not sure how best to fiddle with these parameters
  Eng_carb_all_scatter + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level..), geom = "polygon")
  Eng_carb_all_scatter + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level.., alpha=0.5), geom = "polygon")
  
  ##marginal plots for distribution and outliers
  library("ggExtra")
  ggMarginal(m) #marginal distribution for all carbonates
  ggMarginal(m, type = "boxplot")
  ggMarginal(Eng_carb_all_scatter) #marginal distribution for all carbonates
  ggMarginal(Eng_carb_all_scatter, type = "boxplot")
  


# d18O histogram all data#
ggplot(OSr_teeth, aes(x=`O-PO4 SMOW Measured/Chenery 2012`)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=OSr_teeth, aes(xintercept=17.2), color="red", linetype="dotted", size=2)+
  #geom_vline(data=OSr_teeth, aes(xintercept=15.8), color="red", size=1)+ #LLR17 
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#d13C histogram all data#
ggplot(OSr_teeth, aes(x=d13C)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=OSr_teeth, aes(xintercept=-13.70), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{13},C["carb"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#Sr histogram all data# something wrong here
ggplot(OSr_teeth, aes(x=`87Sr/86Sr`)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=OSr_teeth, aes(xintercept=0.7121), color="red", linetype="dotted", size=2)+
  #xlab(expression(paste(^{87},"Sr/",^{86},"Sr (\u2030)")))+ylab("Count")+
  theme_bw()
#d18O histogram for England

ggplot(Oxy_England, aes(x=`O-PO4 SMOW Measured/Chenery 2012`)) + geom_histogram(binwidth=0.1)+
  geom_vline(data=Oxy_England, aes(xintercept=17.36), color="red", linetype="dotted", size=2)+
 # geom_vline(data=Oxy_England, aes(xintercept=15.8), color="red", size=1)+ #LLR17 
  geom_vline(data=Oxy_England, aes(xintercept= 16.6), color="black",linetype="dashed", size=1.5)+
  geom_vline(data=Oxy_England, aes(xintercept= 18.7), color="black",linetype="dashed", size=1.5)+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+ylab("Count")+
  theme_bw()
#black dashed lines are Chenery et al.'s limits for the UK, blue dotted is the mean of the "English" dataset and Red here is LLR17 but can change to any individual or site mean
ggplot(Oxy_England, aes(x=d13C)) + geom_histogram(binwidth=0.08)+
  geom_vline(data=OSr_teeth, aes(xintercept=-14.10), color="red", linetype="dotted", size=2)+
  xlab(expression(paste(delta^{13},C["carb"], " (\u2030)")))+ylab("Count")+
  theme_bw()


# d18O histogram with density all##
ggplot(OSr_teeth, aes(x=`O-PO4 SMOW Measured/Chenery 2012`)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=0.1)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)), color="blue", linetype="dashed", size=1)
  
#histogram with just kent to check for bimodel dist#
kent_carb<-subset(OSr_teeth, County=="Kent")
show(kent_carb)
summary(kent_carb)
ggplot(data=kent_carb, aes(x=`O-PO4 SMOW Measured/Chenery 2012`)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=0.15)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(`O-PO4 SMOW Measured/Chenery 2012`)), color="blue", linetype="dashed", size=1)+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  ylab("Density")+
  ggtitle("Kent Tooth Enamel")

#boxplot all kent# 
ggplot(data = kent_carb, aes(x = "", y = kent_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon Kent Tooth Enamel")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))

#boxplot just kent by site# 
ggplot(data = kent_carb, aes(x = kent_carb$Site, y = kent_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), aes(fill=kent_carb$Site))+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none")

### ***KENT AND EAST SUSSEX*** 

#histogram with just KENT AND EAST SUSSEX to check for bimodel dist#
kentEsuss_carb<-subset(OSr_teeth, Region=="Kent and East Sussex")
show(kentEsuss_carb)
summary(kentEsuss_carb)
ggplot(data=kentEsuss_carb, aes(x=`O-PO4 SMOW Measured/Chenery 2012`)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=0.15)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(`O-PO4 SMOW Measured/Chenery 2012`)), color="blue", linetype="dashed", size=1)+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  ylab("Density")+
  ggtitle("Kent and East Sussex Tooth Enamel")

#boxplot all KENT AND EAST SUSSEX # 
ggplot(data = kentEsuss_carb, aes(x = "", y = kentEsuss_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon Kent Tooth Enamel")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))

#boxplot just KENT AND EAST SUSSEX  by site# 
ggplot(data = kentEsuss_carb, aes(x = kentEsuss_carb$Site, y = kentEsuss_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), aes(fill=kentEsuss_carb$Site))+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none")

#for KENT AND EAST SUSSEX  by sex
ggplot(data = kentEsuss_carb, aes(x = kentEsuss_carb$Sex, y = kentEsuss_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  geom_hline(data=kentEsuss_carb, aes(yintercept= 16.6), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=kentEsuss_carb, aes(yintercept= 18.7), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=kentEsuss_carb, aes(yintercept= 17.25), color="red",linetype="dashed", size=1.5)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme_bw() #bimodality shines through again with both men and women (visually) roughly equal on either side of that

Foxy_kentEsuss<-subset(kentEsuss_carb, Sex=="F")
Moxy_kentEsuss<-subset(kentEsuss_carb, Sex=="M")
library(BEST)
ksuss_sex1 <- c(Foxy_kentEsuss$`O-PO4 SMOW Measured/Chenery 2012`)
ksuss_sex2 <- c(Moxy_kentEsuss$`O-PO4 SMOW Measured/Chenery 2012`)
ksuss_sex1<-na.omit(ksuss_sex1)
ksuss_sex2<-na.omit(ksuss_sex2)
BESTout_kentEsuss_sex <- BESTmcmc(ksuss_sex1, ksuss_sex2, priors=NULL, parallel=FALSE) 
#plot(BESTout_kentEsuss_sex)
plotAll(BESTout_kentEsuss_sex) #no difference, very similar sample sizes 

# K means clustering # ***DONE IN SEPARATE R FILE***

## Do all the same graphs as above (and maybe some more) for each region - work out these on "Country" and then also by regions as defined by doing GIS 

##Cambridgeshire
#histogram for Cambridgeshire dist#
cambs_carb<-subset(OSr_teeth, County=="Cambridgeshire")
show(cambs_carb)
summary(cambs_carb)
ggplot(data=cambs_carb, aes(x=`O-PO4 SMOW Measured/Chenery 2012`)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=0.15)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(`O-PO4 SMOW Measured/Chenery 2012`)), color="blue", linetype="dashed", size=1)+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  ylab("Density")+
  ggtitle("Cambridgeshire Tooth Enamel")

#boxplot all Cambs# 
ggplot(data = cambs_carb, aes(x = "", y = cambs_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon Cambridgeshire Tooth Enamel")))+
  scale_y_continuous(limits=c(15,20.5),breaks=seq(15,20.5,1))

#boxplot just Cambs by site# 
ggplot(data = cambs_carb, aes(x = cambs_carb$Site, y = cambs_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), aes(fill=cambs_carb$Site))+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(15,20.5),breaks=seq(15,20.5,1))+
  theme(legend.position="none")

#quite a few outliers - check to see if these are the bed burials??????? 2 at Westfield Farm Ely really stand out for the whole region, Classified Ely in a different "region" so maybe retry with just "South cambs" region 
#compare the counties for Kent and Cambs with the regions defined geographically that will add a couple of sites/individuals to each and if those change the patterns much 

##Cambridge and South Cambs
#histogram for Cam + South Cambs dist#
sthcambs_carb<-subset(OSr_teeth, Region=="Cambridge and South Cambs")
show(sthcambs_carb)
summary(sthcambs_carb)
ggplot(data=sthcambs_carb, aes(x=`O-PO4 SMOW Measured/Chenery 2012`)) + geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth=0.15)+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=mean(`O-PO4 SMOW Measured/Chenery 2012`)), color="blue", linetype="dashed", size=1)+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  ylab("Density")+
  ggtitle("South Cambridgeshire Tooth Enamel")

#boxplot all South Cambs# 
ggplot(data = sthcambs_carb, aes(x = "", y = sthcambs_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon South Cambridgeshire Tooth Enamel")))+
  scale_y_continuous(limits=c(15,19),breaks=seq(15,19,1))

#boxplot just Cambs by site# 
ggplot(data = sthcambs_carb, aes(x = sthcambs_carb$Site, y = sthcambs_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(15,19),breaks=seq(15,19,1))+
  theme(legend.position="none")

#violin and boxplot of South Cambs by site#
ggplot(data = sthcambs_carb, aes(x = sthcambs_carb$Site, y = sthcambs_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  geom_hline(data=sthcambs_carb, aes(yintercept= 16.6), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=sthcambs_carb, aes(yintercept= 18.7), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=sthcambs_carb, aes(yintercept= 17.25), color="red",linetype="dashed", size=1.5)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(15,19),breaks=seq(15,19,1))+
  theme_bw()

#for south cambs by sex
ggplot(data = sthcambs_carb, aes(x = sthcambs_carb$Sex, y = sthcambs_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  geom_hline(data=sthcambs_carb, aes(yintercept= 16.6), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=sthcambs_carb, aes(yintercept= 18.7), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=sthcambs_carb, aes(yintercept= 17.25), color="red",linetype="dashed", size=1.5)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(15,19),breaks=seq(15,19,1))+
  theme_bw()

Foxy_sthcambs<-subset(sthcambs_carb, Sex=="F")
Moxy_sthcambs<-subset(sthcambs_carb, Sex=="M")
library(BEST)
cambs_sex1 <- c(Foxy_sthcambs$`O-PO4 SMOW Measured/Chenery 2012`)
cambs_sex2 <- c(Moxy_sthcambs$`O-PO4 SMOW Measured/Chenery 2012`)
cambs_sex1<-na.omit(cambs_sex1)
cambs_sex2<-na.omit(cambs_sex2)
BESTout_SthCambs_sex <- BESTmcmc(cambs_sex1, cambs_sex2, priors=NULL, parallel=FALSE) 
#plot(BESTout_SthCambs_sex)
plotAll(BESTout_SthCambs_sex) #no difference and equal sample sizes

#for south cambs by age category - violin plots
ggplot(data = sthcambs_carb, aes(x = sthcambs_carb$`Age Category`, y = sthcambs_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  geom_hline(data=sthcambs_carb, aes(yintercept= 16.6), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=sthcambs_carb, aes(yintercept= 18.7), color="black",linetype="dashed", size=1.5)+
  geom_hline(data=sthcambs_carb, aes(yintercept= 17.25), color="red",linetype="dashed", size=1.5)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(15,19),breaks=seq(15,19,1))+
  theme_bw() #roughly equal amounts of males and females of a range of adult ages are outside the UK zone 

#box and jitter of oxygen by time period for England#
summary(Oxy_England)

ggplot(data = Oxy_England, aes(x = Oxy_England$`Date Category`, y = Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`,)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme_bw()

Foxy_Eng<-subset(Oxy_England, Sex=="F") #Female Oxygen England
Moxy_Eng<-subset(Oxy_England, Sex=="M") #Male Oxygen England
Foxy_Eng2<-subset(Oxy_England, SimpleSex=="F") #Female Oxygen England with F & F?
Moxy_Eng2<-subset(Oxy_England, SimpleSex=="M") #Male Oxygen England with M and M?

Engsex1 <- c(Foxy_Eng2$`O-PO4 SMOW Measured/Chenery 2012`)
Engsex2 <- c(Moxy_Eng2$`O-PO4 SMOW Measured/Chenery 2012`)
Engsex1<-na.omit(Engsex1)
Engsex2<-na.omit(Engsex2)
BESTout_Eng_sex2 <- BESTmcmc(Engsex1, Engsex2, priors=NULL, parallel=FALSE) #not working as it says it includes NA or Inf? Check what Inf is
plot(BESTout_Eng_sex2)
plotAll(BESTout_Eng_sex2)

#make boxplots B&W and get rid of the legend 

F_Eng_Oxy_Box_Time <-ggplot(data = Foxy_Eng, aes(x = Foxy_Eng$`Date Category`, y = Foxy_Eng$`O-PO4 SMOW Measured/Chenery 2012`,)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Female Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme_bw()
F_Eng_Oxy_Box_Time2 <-ggplot(data = Foxy_Eng2, aes(x = Foxy_Eng$`Date Category`, y = Foxy_Eng2$`O-PO4 SMOW Measured/Chenery 2012`,)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Female Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme_bw()

M_Eng_Oxy_Box_Time <-ggplot(data = Moxy_Eng, aes(x = Moxy_Eng$`Date Category`, y = Moxy_Eng$`O-PO4 SMOW Measured/Chenery 2012`, )) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Male Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme_bw()

M_Eng_Oxy_Box_Time2 <-ggplot(data = Moxy_Eng2, aes(x = Moxy_Eng2$`Date Category`, y = Moxy_Eng2$`O-PO4 SMOW Measured/Chenery 2012`, )) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Male Anglo-Saxon Tooth Enamel")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme_bw()
M_Eng_Oxy_Box_Time2
grid.arrange(F_Eng_Oxy_Box_Time, F_Eng_Oxy_Box_Time2, M_Eng_Oxy_Box_Time, M_Eng_Oxy_Box_Time2, nrow=2, ncol=2) #issue with aes here? 

#try but with male and female boxplot/violin side by side in the same plot
FM_Eng_Oxy_Violin_Time<-ggplot(data=Oxy_England, aes(x=Oxy_England$`Date Category`, y=Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`, fill=Oxy_England$Sex))+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  #scale_fill_manual(values=c("seagreen1", "lightseagreen", "#999999"))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme_bw()
#going to add a new column with a simplified sex column to avoid having 5 violin plots side by side 
library(dplyr)
Oxy_England$SimpleSex<-Oxy_England$Sex
Oxy_England <- Oxy_England %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F?', 'F'))
Oxy_England <- Oxy_England %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M?', 'M'))
Oxy_England <- Oxy_England %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))
summary(Oxy_England)

ggplot(data=Oxy_England, aes(x=Oxy_England$`Date Category`, y=Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`, fill=Oxy_England$SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco()+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme(legend.position = "top", legend.direction = "horizontal")

#use mutate to create another column with simplified time periods as well ughhhhhhh but yay learning better data manipulation 
Oxy_England$SimpleDate<-Oxy_England$`Date Category`
summary(Oxy_England)
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A-C', 'A-D'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'A/B', 'A-D'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-D', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-E', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B-F', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'B/C', 'B-G'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'C-F', 'C/D'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F-H', 'F-I'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-G', 'E-H'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'D-G', 'D-H'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E', 'E/F'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'F', 'E/F'))
Oxy_England <- Oxy_England %>% mutate(SimpleDate = replace(SimpleDate, SimpleDate == 'E-H', 'E/F'))
library("ggsci")
# scale_fill_manual(values=cbPalette)
# scale_colour_manual(values=cbPalette)
Oxy_England$`SimpleDate` = factor(Oxy_England$`SimpleDate`,
                                   levels=c("A", "A-D","B","B-G", "C", "C/D", "D", "D/E", "D-F", "D-H", "E/F", "F/G","F-I"),ordered=TRUE)

ggplot(data=Oxy_England, aes(x=Oxy_England$`SimpleDate`, y=Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`, fill=Oxy_England$SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco()+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  labs(fill="Osteological Sex")+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme(legend.position = "top", legend.direction = "horizontal")
  #need to facet the variables for "SimpleDate"

#same as above but with big delta chenery
ggplot(data=Oxy_England, aes(x=Oxy_England$`SimpleDate`, y=Oxy_England$`Δ18Odw-MAP Chenery`, fill=Oxy_England$SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  labs(fill="Osteological Sex")+
  scale_y_continuous(limits=c(-9,9),breaks=seq(-9,9,1))+
  theme(legend.position = "top", legend.direction = "horizontal")

#now for date categories super simple - 200BC-450 AD, ~200-790AD, ~790AD-1066+
#A->200BC-450 AD
#A/B, A-C, A-D, B, B-D, B-E, B/C, C, C/D, D, D/E, E->~200AD-790 AD
Oxy_England$PeriodBroad<-Oxy_England$`Date Category`
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A', '200BC-450AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A/B', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-C', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'A-D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-E', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B/C', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C/D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D/E', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'C-F', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-F', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'B-G', 'c.350AD-790AD'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F-H', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E/F', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F-I', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'F/G', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E-G', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'E-H', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-G', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-H', 'c.790AD-1066AD+'))
Oxy_England <- Oxy_England %>% mutate(PeriodBroad = replace(PeriodBroad, PeriodBroad == 'D-F', 'c.790AD-1066AD+'))



Oxy_England$`PeriodBroad` = factor(Oxy_England$`PeriodBroad`,
                                  levels=c("200BC-450AD", "c.350AD-790AD","c.790AD-1066AD+"),ordered=TRUE)
ggplot(data=Oxy_England, aes(x=Oxy_England$`PeriodBroad`, y=Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`, fill=Oxy_England$SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco()+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  labs(fill="Osteological Sex")+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),legend.position = "top", legend.direction = "horizontal", legend.text = element_text(size=16), legend.title = element_text(size = 20))


#same as above but with big delta chenery
ggplot(data=Oxy_England, aes(x=Oxy_England$`PeriodBroad`, y=Oxy_England$`Δ18Odw-MAP Chenery`, fill=Oxy_England$SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = TRUE)+
  scale_fill_jco()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  labs(fill="Osteological Sex")+
  scale_y_continuous(limits=c(-9,9),breaks=seq(-9,9,1))+
  theme(legend.position = "top", legend.direction = "horizontal")

#all d18O over time boxplot
ggplot(data=Oxy_England, aes(x=Oxy_England$`PeriodBroad`, y=Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`, fill=`PeriodBroad`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme(legend.position = "none")

ggplot(data=Oxy_England, aes(x=Oxy_England$SimpleDate, y=Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`, fill=SimpleDate))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey()+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22),legend.position = "none")


#carbonate scatter by time period

#OSr scatter by time period


#BEST test to compare F&M mobility in England - REDO WITH SEXSIMPLE?
library(BEST)
sex1 <- c(Foxy_Eng$`O-PO4 SMOW Measured/Chenery 2012`)
sex2 <- c(Moxy_Eng$`O-PO4 SMOW Measured/Chenery 2012`)
sex1<-na.omit(sex1)
sex2<-na.omit(sex2)
BESTout_Eng_sex <- BESTmcmc(sex1, sex2, priors=NULL, parallel=FALSE) #not working as it says it includes NA or Inf? Check what Inf is
plot(BESTout_Eng_sex)
plotAll(BESTout_Eng_sex)


#BEST test to compare F&M mobility across EM Europe 
library(BEST) 
Foxy_EMEU<-subset(OSr_teeth, Sex=="F") #Female Oxygen EM Europe
Moxy_EMEU<-subset(OSr_teeth, Sex=="M") #Male Oxygen EM Europe
sex1eu<-c(Foxy_EMEU$`O-PO4 SMOW Measured/Chenery 2012`)
sex2eu<-c(Moxy_EMEU$`O-PO4 SMOW Measured/Chenery 2012`)
sex1eu<-na.omit(sex1eu)
sex2eu<-na.omit(sex2eu)
BESTout_EMEU_sex <- BESTmcmc(sex1eu, sex2eu, priors=NULL, parallel=FALSE)
#plot(BESTout_EMEU_sex)
plotAll(BESTout_EMEU_sex)

#box plots for Female vs. Male d18O across all of Europe 
F_EMEU_Oxy_Box_Time <-ggplot(data = Foxy_EMEU, aes(x = Foxy_EMEU$`Date Category`, y = Foxy_EMEU$`O-PO4 SMOW Measured/Chenery 2012`, )) + 
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Female Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  theme_bw()

M_EMEU_Oxy_Box_Time <-ggplot(data = Moxy_EMEU, aes(x = Moxy_EMEU$`Date Category`, y = Moxy_EMEU$`O-PO4 SMOW Measured/Chenery 2012`, )) + 
  geom_boxplot(position=position_dodge(1))+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Male Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  theme_bw()

grid.arrange(F_EMEU_Oxy_Box_Time,M_EMEU_Oxy_Box_Time, ncol= 1) #doesn't really show anything, very similar to regional variation 

#dendrograms as an alternative to k means clustering which doesn't require you to select the number of cluster
library("ggplot2")
library("ggdendro")
#install.packages("ape")
library("ape")
#start with computing hierarchical clustering
#ddOSr<-dist(scale(OSr_teeth), method = "euclidean") #Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric - remove all characters? Also have to remove NAs so may just ignore Sr as not enough have them 
#hcOSr<-hclust(ddOSr, method = "ward.D2")
#ggdendrogram(hcOSr)

#going to try it again with the same df I used for kmeans, so just clustering the UK via carbonate
ddEngcarb<-dist(Engcarbclean, method = "euclidean") #Engcarbclean should already be scaled from kmeans analysis but let's see if this works *fingers crossed*
hcEngcarb<-hclust(ddEngcarb, method = "ward.D2")
ggdendrogram(hcEngcarb)
ggdendrogram(hcEngcarb, rotate = TRUE,)

plot(as.phylo(hcEngcarb), type = "fan")

#yay it works! now to try with the whole of Early Medieval Europe
carbclean <- OSr_teeth
class(as.data.frame(carbclean))
head(carbclean)
carbclean <- data.frame(carbclean)
rownames(carbclean) <- carbclean[,1]
carbclean <- carbclean[,-1]
head(carbclean)
View(carbclean)
carbclean<-carbclean[c(23,27)]
head(carbclean)
#carbclean <- na.omit(carbclean)
#head(carbclean)
carbclean <- scale(carbclean)
carbclean <- na.omit(carbclean)
head(carbclean)

ddallcarb<-dist(carbclean, method = "euclidean")
hcallcarb<-hclust(ddallcarb, method = "ward.D2")
ggdendrogram(hcallcarb) 
ggdendrogram(hcallcarb, rotate = TRUE,)
plot(as.phylo(hcallcarb), type = "fan")
#plot(hcallcarb, type = "triangle")
plot(as.phylo(hcallcarb), type = "unrooted")
plot(as.phylo(hcallcarb), type = "cladogram", cex = 0.9, label.offset = 1)
# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(hcallcarb, k = 7, boxes = FALSE, col.up = "gray50", col.down = c("black", "#E69F00","#0072B2","#009E73","#CC79A7","#56B4E9","#D55E00"))
A2Rplot(hcallcarb, k = 8, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
fviz_nbclust(carbclean, kmeans, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(carbclean, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(carbclean, kmeans, method = "wss")
set.seed(123)
carb3 <- kmeans(carbclean, centers = 3, nstart = 25)
# Visualize
library("factoextra")
fviz_cluster(carb3, geom = "point",  data = carbclean) + ggtitle("k = 3")
fviz_cluster(carb3, data = carbclean, geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"])))+
  xlab(expression(paste(delta^{13},C["carbonate (PDB)"])))+
  theme(axis.title=element_text(size=22),legend.title=element_text(size=22), legend.text = element_text(size=18))
set.seed(123)
fviz_nbclust(carbclean, hcut, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(carbclean, FUN = hcut, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(carbclean, hcut, method = "wss")
NbClust(data = carbclean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2")

A2Rplot(hcallcarb, k = 3, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcallcarb, k = 5, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcallcarb, k = 2, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)

#ok now to try enamel clustering with O, C and Sr
enamelclean <- OSr_teeth
class(as.data.frame(enamelclean))
head(enamelclean)
enamelclean <- data.frame(enamelclean)
rownames(enamelclean) <- enamelclean[,1]
enamelclean <- enamelclean[,-1]
head(enamelclean)
show(enamelclean)
enamelclean<-enamelclean[c(23,27,33)]
head(enamelclean)
#carbclean <- na.omit(carbclean)
#head(carbclean)
enamelclean <- scale(enamelclean)
enamelclean <- na.omit(enamelclean)

ddallenamel<-dist(enamelclean, method = "euclidean")
hcallenamel<-hclust(ddallenamel, method = "ward.D2")
ggdendrogram(hcallenamel) #how well does this match up with lat and longitude? 
ggdendrogram(hcallenamel, rotate = TRUE,)
plot(as.phylo(hcallenamel), type = "fan") #this looks very similar to the carbonate one presumably because there aren't many with Sr as well 
A2Rplot(hcallenamel, k = 8, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcallenamel, k = 4, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
fviz_nbclust(enamelclean, kmeans, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(enamelclean, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(enamelclean, hcut, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(enamelclean, FUN = hcut, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(enamelclean, hcut, method = "wss")
NbClust(data = enamelclean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2")
NbClust(data = enamelclean, diss = NULL, distance = "euclidean", min.nc = 3, max.nc = 15, method = "ward.D2") #based on the dendrogram

A2Rplot(hcallenamel, k = 5, boxes = FALSE, col.up = "gray50", col.down = cbbPalette) #based on gap_stat method
A2Rplot(hcallenamel, k = 4, boxes = FALSE, col.up = "gray50", col.down = cbbPalette) #based on gap_stat method



#PCA for O, Sr and C combined data 
library(FactoMineR)
library(factoextra)
enamelclean2 <- OSr_teeth
class(as.data.frame(enamelclean2))
head(enamelclean2)
enamelclean2 <- data.frame(enamelclean2)
rownames(enamelclean2) <- enamelclean2[,1]
enamelclean2 <- enamelclean2[,-1]
View(enamelclean2)
enamelclean2<-enamelclean2[c(23,27,33,44)]
head(enamelclean2)
enamelclean2 <- na.omit(enamelclean2)
enamelclean2_2<-enamelclean2[,-4]
enamelclean2_2<-as.matrix(enamelclean2_2)
enamelclean2<-as.data.frame(enamelclean2)

res.pca=PCA(enamelclean2_2,scale.unit=TRUE,quali.sup=62,graph=FALSE) #not working for some reason, keeps on having error messages
OSrC.pca <- prcomp(enamelclean2[, -4],  scale = TRUE) #not taking the [, -4] argument so used the subset 
OSrC.pca <- prcomp(enamelclean2_2,  scale = TRUE)
fviz_pca_ind(OSrC.pca)
fviz_pca_ind(OSrC.pca, label="none", habillage=enamelclean2$EuRegion2)
fviz_pca_ind(OSrC.pca, label="none", habillage=enamelclean2$EuRegion2,
             addEllipses=TRUE)
fviz_pca_ind(OSrC.pca, label="none", habillage=enamelclean2$EuRegion2,
             addEllipses=TRUE, ellipse.level=0.95)
fviz_pca_var(OSrC.pca)

#just O and Sr
OSrClean <- OSr_teeth
class(as.data.frame(OSrClean))
head(OSrClean)
OSrClean <- data.frame(OSrClean)
rownames(OSrClean) <- OSrClean[,1]
OSrClean <- OSrClean[,-1]
head(OSrClean)
View(OSrClean)
OSrClean<-OSrClean[c(27,33)]
head(OSrClean)
#carbclean <- na.omit(carbclean)
#head(carbclean)
OSrClean <- scale(OSrClean)
OSrClean <- na.omit(OSrClean)

ddOSrenamel<-dist(OSrClean, method = "euclidean")
hcOSrenamel<-hclust(ddOSrenamel, method = "ward.D2")
ggdendrogram(hcOSrenamel) 
ggdendrogram(hcOSrenamel, rotate = TRUE,)
plot(as.phylo(hcOSrenamel), type = "fan") #this looks very similar to the carbonate one presumably because there aren't many with Sr as well 
A2Rplot(hcOSrenamel, k = 8, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
A2Rplot(hcOSrenamel, k = 7, boxes = FALSE, col.up = "gray50", col.down = cbbPalette)
fviz_nbclust(OSrClean, kmeans, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(OSrClean, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(OSrClean, hcut, method = "silhouette")
set.seed(123)
gap_stat <- clusGap(OSrClean, FUN = hcut, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
print(gap_stat, method = "globalmax")
fviz_gap_stat(gap_stat)
set.seed(123)
fviz_nbclust(OSrClean, hcut, method = "wss")
NbClust(data = OSrClean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward.D2")

A2Rplot(hcOSrenamel, k = 6, boxes = FALSE, col.up = "gray50", col.down = cbbPalette) #due to gap_stat global max 
A2Rplot(hcOSrenamel, k = 5, boxes = FALSE, col.up = "gray50", col.down = cbbPalette) #due to gap_stat global max 
A2Rplot(hcOSrenamel, k = 3, boxes = FALSE, col.up = "gray50", col.down = cbbPalette) #due to gap_stat global max 

#by geology all Europe
  #oxygen
Europe_Oxy_GeologySimple_Violin<-ggplot(data = OSr_teeth, aes(x = OSr_teeth$`Simplified Geology`, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
  #carbon
Europe_Carb_GeologySimple_Violin<-ggplot(data = OSr_teeth, aes(x = OSr_teeth$`Simplified Geology`, y = OSr_teeth$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["carb"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
#strontium
Europe_Sr_GeologySimple_Violin<-ggplot(data = OSr_teeth, aes(x = OSr_teeth$`Simplified Geology`, y = OSr_teeth$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(TeX('^{87}Sr/^{86}Sr'))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))

grid.arrange(Europe_Oxy_GeologySimple_Violin,Europe_Carb_GeologySimple_Violin,Europe_Sr_GeologySimple_Violin, ncol= 1)
#by geology England
#oxygen
Eng_Oxy_GeologySimple_Violin<-ggplot(data = Oxy_England, aes(x = Oxy_England$`Simplified Geology`, y = Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18))
#carbon
Eng_Carb_GeologySimple_Violin<-ggplot(data = Oxy_England, aes(x = Oxy_England$`Simplified Geology`, y = Oxy_England$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["carb"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18))

#strontium
Eng_Sr_GeologySimple_Violin<-ggplot(data = Oxy_England, aes(x = Oxy_England$`Simplified Geology`, y = Oxy_England$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(TeX('^{87}Sr/^{86}Sr'))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=18))
grid.arrange(Eng_Oxy_GeologySimple_Violin,Eng_Carb_GeologySimple_Violin, Eng_Sr_GeologySimple_Violin, ncol= 1)

#Europe without England
EMEU_woEng<-subset(OSr_teeth, Country!="England")
View(EMEU_woEng)
#oxygen
EuropewoEng_Oxy_GeologySimple_Violin<-ggplot(data = EMEU_woEng, aes(x = EMEU_woEng$`Simplified Geology`, y = EMEU_woEng$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
#carbon
EuropewoEng_Carb_GeologySimple_Violin<-ggplot(data = EMEU_woEng, aes(x = EMEU_woEng$`Simplified Geology`, y = EMEU_woEng$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["carb"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))
#strontium
EuropewoEng_Sr_GeologySimple_Violin<-ggplot(data = EMEU_woEng, aes(x = EMEU_woEng$`Simplified Geology`, y = EMEU_woEng$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(TeX('^{87}Sr/^{86}Sr'))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22))

grid.arrange(EuropewoEng_Oxy_GeologySimple_Violin,EuropewoEng_Carb_GeologySimple_Violin,EuropewoEng_Sr_GeologySimple_Violin, ncol= 1)

#scatterplot by geology
EMEU_OSr_scatter_geology <- ggplot(OSr_teeth) + 
  theme_bw() +
  geom_point(aes(x=`87Sr/86Sr`, y=`O-PO4 SMOW Measured/Chenery 2012`, color=OSr_teeth$`Simplified Geology`, size=3),show.legend = TRUE) + 
  scale_colour_manual(values=cbbPalette, name="Simplified Geology")+
  scale_size_continuous(name= "",)+
  xlab(TeX('^{87}Sr/^{86}Sr'))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  theme(axis.text=element_text(size=22), axis.title=element_text(size=22),legend.title = element_text(size=22),legend.text = element_text(size=20))
EMEU_OSr_scatter_geology



#BEST test for chalk and "other" geology in England
#oxygen
Chalk_carb_Eng<-subset(Oxy_England, `Simplified Geology`=="Chalk") 
OtherGeo_carb_Eng<-subset(Oxy_England, `Simplified Geology`=="Other") 
geo1engO<-c(Chalk_carb_Eng$`O-PO4 SMOW Measured/Chenery 2012`)
geo2engO<-c(OtherGeo_carb_Eng$`O-PO4 SMOW Measured/Chenery 2012`)
geo1engO<-na.omit(geo1engO)
geo2engO<-na.omit(geo2engO)
BESTout_eng_geo_oxy<- BESTmcmc(geo1engO, geo2engO, priors=NULL, parallel=FALSE)
#plot(BESTout_eng_geo_oxy)
plotAll(BESTout_eng_geo_oxy) #they come out as very different in a BEST test but group means are 0.5 per mille different 
#carbon
geo1engC<-c(Chalk_carb_Eng$d13C)
geo2engC<-c(OtherGeo_carb_Eng$d13C)
geo1engC<-na.omit(geo1engC)
geo2engC<-na.omit(geo2engC)
BESTout_eng_geo_carbon<- BESTmcmc(geo1engC, geo2engC, priors=NULL, parallel=FALSE)
#plot(BESTout_eng_geo_carbon)
plotAll(BESTout_eng_geo_carbon) #no difference in carbon between the two geological groups so what is it about chalk areas that means there is an internal bimodality? 

#strontium
geo1engSr<-c(Chalk_carb_Eng$`87Sr/86Sr`)
geo2engSr<-c(OtherGeo_carb_Eng$`87Sr/86Sr`)
geo1engSr<-na.omit(geo1engSr)
geo2engSr<-na.omit(geo2engSr)
BESTout_eng_geo_sr<- BESTmcmc(geo1engSr, geo2engSr, priors=NULL, parallel=FALSE)
#plot(BESTout_eng_geo_sr)
plotAll(BESTout_eng_geo_sr) #no difference in Sr between the two geological groups probably because there are a lot of people buried on each that don't belong there


#BANOVA for chalk, volcanic and other in Europe?


#Oxygen and Strontium biplots for Europe and England
#density plots
library(ggalt)
EMEU_OSr_Scatter <- ggplot(OSr_teeth,aes(`87Sr/86Sr`,`O-PO4 SMOW Measured/Chenery 2012`))+
  geom_point(size=3,shape=16)+
  ylab(expression(paste(delta^{18},"O (\u2030)")))+xlab(expression(paste("87Sr/86Sr")))+
  scale_x_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))

EMEU_OSr_Scatter
EMEU_OSr_Scatter + geom_bkde2d(bandwidth=c(0.7, 0.2)) #not sure how best to fiddle with these parameters
EMEU_OSr_Scatter + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level..), geom = "polygon")
EMEU_OSr_Scatter + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level.., alpha=0.5), geom = "polygon") #not working not sure why

Eng_OSr_scatter<- ggplot(Oxy_England,aes(`87Sr/86Sr`,`O-PO4 SMOW Measured/Chenery 2012`))+
  geom_point(size=3,shape=16)+
  ylab(expression(paste(delta^{18},"O (\u2030)")))+xlab(expression(paste("87Sr/86Sr")))+
  scale_x_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))

Eng_OSr_scatter
Eng_OSr_scatter + geom_bkde2d(bandwidth=c(0.7, 0.2)) #not sure how best to fiddle with these parameters
Eng_OSr_scatter + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level..), geom = "polygon")
Eng_OSr_scatter + stat_bkde2d(bandwidth=c(0.7, 0.2), aes(fill = ..level.., alpha=0.5), geom = "polygon")

##marginal plots for distribution and outliers
library("ggExtra")
ggMarginal(EMEU_OSr_Scatter) #marginal distribution for all carbonates
ggMarginal(EMEU_OSr_Scatter, type = "boxplot")
ggMarginal(Eng_OSr_scatter) #marginal distribution for all carbonates
ggMarginal(Eng_OSr_scatter, type = "boxplot")


#for England by age category - violin plots - might have to factor these variables to order the plot better
#oxygen
ggplot(data = Oxy_England, aes(x = Oxy_England$`Age Category`, y = Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Age Category")))+
  scale_y_continuous(limits=c(13.5,20.5),breaks=seq(13.5,20.5,1))+
  theme_bw()

#carbon (carbonate)
ggplot(data = Oxy_England, aes(x = Oxy_England$`Age Category`, y = Oxy_England$d13C)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)")))+xlab(expression(paste("Age Category")))+
  scale_y_continuous(limits=c(-17,-10),breaks=seq(-17,-10,1))+
  theme_bw()
#strontium
ggplot(data = Oxy_England, aes(x = Oxy_England$`Age Category`, y = Oxy_England$`87Sr/86Sr`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  ylab(expression(paste("87Sr/86Sr")))+xlab(expression(paste("Age Category")))+
  scale_y_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+
  theme_bw()


#scatters with Europe versus England carbonate and O/Sr then a boxplot of all d18O with England 
#mutate and add in new columns
#carbonate scatter
library(viridis)
#ggplot(OSr_teeth) + 
#theme_bw() +
 # geom_point(aes(x=d13C, y=OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`, colour=factor(`Country Code`), size=3)) + 
  #scale_color_viridis(discrete=TRUE)+
  #labs(x=expression(delta^13 * C  * " "("\u2030")), 
   #    y = expression(delta^18 * O  * " "("\u2030")), colour = "Country", size="")+
  #theme(legend.justification="top",axis.text=element_text(size=20),legend.title=element_text(size=20),legend.text=element_text(size=20),axis.title=element_text(size=20))


EMEU_eng_scatter_CO3 <- ggplot(OSr_teeth) + 
  theme_bw() +
  geom_point(aes(x=d13C, y=OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`, colour=factor(`Country Code`), size=3), show.legend = FALSE) + 
  scale_color_manual("Country", values = c("black","black","black","black","black","black","black","black","black","black","orange","black","black","black","black","black","black","black","black","black"))+
  xlab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))

  EMEU_eng_scatter_CO3 
# with just England coloured orange
  library(latex2exp)
EMEU_eng_scatter_OSr <- ggplot(OSr_teeth) + 
  theme_bw() +
  geom_point(aes(x=`87Sr/86Sr`, y=`O-PO4 SMOW Measured/Chenery 2012`, color=factor(`Country Code`), size=3),show.legend = FALSE, ) + 
  scale_color_manual("Country", values = c("black","black","black","black","black","black","black","black","black","black","orange","black","black","black","black","black","black","black","black","black"))+
  xlab(TeX('^{87}Sr/^{86}Sr'))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20))

EMEU_eng_scatter_OSr

ggMarginal(EMEU_eng_scatter_CO3 ) #marginal distribution for all carbonates
ggMarginal(EMEU_eng_scatter_CO3 , type = "boxplot")
ggMarginal(EMEU_eng_scatter_OSr) #marginal distribution for all carbonates
ggMarginal(EMEU_eng_scatter_OSr, type = "boxplot")

#by Environment type
#Europe
#oxygen #aes(fill="grey50")
OSr_teeth <- OSr_teeth %>% mutate(Environment = replace(Environment, Environment == 'Lakeland', 'Lake'))
OSr_teeth <- OSr_teeth %>% mutate(Environment = replace(Environment, Environment == 'Coastal and Riverine', 'Estuarine'))

ggplot(data = OSr_teeth, aes(x = OSr_teeth$Environment, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(12,21),breaks=seq(12,21,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")



#carbon (carbonate)
ggplot(data = OSr_teeth, aes(x = OSr_teeth$Environment, y = OSr_teeth$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(-17.5,-4.5),breaks=seq(-17.5,-4.5,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")

#England - also by region
Oxy_England <- Oxy_England %>% mutate(Environment = replace(Environment, Environment == 'Coastal and Riverine', 'Estuarine'))
#oxygen
Oxy_England$Region = factor(Oxy_England$Region,
                                   levels=c("Northeast","Yorkshire and North Lincolnshire", "Central", "East", "Cambridge and South Cambs", "Kent and East Sussex", "Upper Thames and Chilterns","Wessex", "Southwest"),ordered=TRUE)
ggplot(data = Oxy_England, aes(x = Oxy_England$Region, y = Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Region")))+
  scale_y_continuous(limits=c(12,21),breaks=seq(12,21,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1, size=14),legend.position = "none")

ggplot(data = Oxy_England, aes(x = Oxy_England$Environment, y = Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(12,21),breaks=seq(12,21,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1, size=14),legend.position = "none")

ggplot(Oxy_England, aes(x=Oxy_England$`O-PO4 SMOW Measured/Chenery 2012`, y=Oxy_England$Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(12,21),breaks=seq(12,21,1.0))+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

#carbon (carbonate)
ggplot(data = Oxy_England, aes(x = Oxy_England$Environment, y = Oxy_England$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(-17.5,-4.5),breaks=seq(-17.5,-4.5,1))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1, size=14),legend.position = "none")

ggplot(data = Oxy_England, aes(x = Oxy_England$Region, y = Oxy_England$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)")))+xlab(expression(paste("Region")))+
  scale_y_continuous(limits=c(-17.5,-10.5),breaks=seq(-17.5,-10.5,1))+
  theme(axis.text.y = element_text(size=18),axis.text.x = element_text(size=14,angle = 45, hjust = 1),axis.title=element_text(size=22), legend.position = "none")

ggplot(Oxy_England, aes(x=Oxy_England$d13C, y=Oxy_England$Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-17.5,-10.5),breaks=seq(-17.5,-10.5,1.0))+
  xlab(expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

ggplot(Oxy_England,aes(d13C,`O-PO4 SMOW Measured/Chenery 2012`, group=Oxy_England$Environment))+
  theme_bw()+
  geom_point(aes(shape=Oxy_England$Region, color=Oxy_England$Environment),size=4,)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-16.5,-11.5),breaks=seq(-16.5,-11.5,1))+scale_y_continuous(limits=c(14,21),breaks=seq(14,21,1))+
  scale_shape_manual(values=c(15,16,17,18,25,3,4,8,1), name="Region")+
  scale_colour_manual(values=cbbPalette, name="Environment")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

#strontium
ggplot(data = Oxy_England, aes(x = Oxy_England$Region, y = Oxy_England$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(TeX("^{87}Sr/^{86}Sr"))+xlab(expression(paste("Region")))+
  scale_y_continuous(limits=c(0.7060,0.7210),breaks=seq(0.7060,0.7210,0.005))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1, size=14),legend.position = "none")


#Alt, Lat and Long d18O scatters
#England
Eng_lm_d18O_alt<-ggplot(Oxy_England,aes(Oxy_England$`Alt (masl)`,`O-PO4 SMOW Measured/Chenery 2012`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Alt (masl)")))+
  scale_x_continuous(limits=c(0,200),breaks=seq(0,200,10))+scale_y_continuous(limits=c(14,21),breaks=seq(14,21,1))

Eng_lm_d18O_lat<-ggplot(Oxy_England,aes(Oxy_England$Latitude,`O-PO4 SMOW Measured/Chenery 2012`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Latitude")))+
  scale_x_continuous(limits=c(50,56),breaks=seq(50,56,0.5))+scale_y_continuous(limits=c(14,21),breaks=seq(14,21,1))

Eng_lm_d18O_long<-ggplot(Oxy_England,aes(Oxy_England$Longitude,`O-PO4 SMOW Measured/Chenery 2012`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Longitude")))+
  scale_x_continuous(limits=c(-2.5,1.5),breaks=seq(-2.5,1.5,0.5))+scale_y_continuous(limits=c(14,21),breaks=seq(14,21,1))


ggarrange(Eng_lm_d18O_alt,Eng_lm_d18O_lat, Eng_lm_d18O_long, ncol=1)

#EM Europe
EMEUAlt18O.formula<-OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`~OSr_teeth$`Alt (masl)` #doesn't seem to have done what I wanted
EMEUAlt18Ofit <- lm(`O-PO4 SMOW Measured/Chenery 2012` ~ `Alt (masl)`, data = OSr_teeth)
EMEU_lm_d18O_alt<-ggplot(OSr_teeth,aes(OSr_teeth$`Alt (masl)`,OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  #geom_line(data = fortify(EMEUAlt18Ofit), aes(x = `Alt (masl)`, y = .fitted, color="red"))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Alt (masl)")))+
  scale_x_continuous(limits=c(0,600),breaks=seq(0,600,50))+scale_y_continuous(limits=c(12,24),breaks=seq(12,24,1))

EMEU_lm_d18O_lat<-ggplot(OSr_teeth,aes(OSr_teeth$Latitude,`O-PO4 SMOW Measured/Chenery 2012`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Latitude")))+
  scale_x_continuous(limits=c(35,75),breaks=seq(35,75,5))+scale_y_continuous(limits=c(12,23),breaks=seq(12,23,1))

EMEU_lm_d18O_long<-ggplot(OSr_teeth,aes(OSr_teeth$Longitude,`O-PO4 SMOW Measured/Chenery 2012`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Longitude")))+
  scale_x_continuous(limits=c(-46,36),breaks=seq(-46,36,10))+scale_y_continuous(limits=c(12,21),breaks=seq(12,21,1))

#longitude w/o Greenland and Iceland(only Sr anyway)
EMEU_lm_d18O_longwoNthAtlantic<-ggplot(OSr_teeth,aes(OSr_teeth$Longitude,`O-PO4 SMOW Measured/Chenery 2012`))+
  theme_bw()+
  geom_point(size=4)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Longitude (w/o North Atlantic islands)")))+
  scale_x_continuous(limits=c(-16,36),breaks=seq(-16,36,10))+scale_y_continuous(limits=c(12,23),breaks=seq(12,23,1))


ggarrange(EMEU_lm_d18O_alt,EMEU_lm_d18O_lat,EMEU_lm_d18O_long,EMEU_lm_d18O_longwoNthAtlantic, ncol=1)

#brewing and stewing graphs big delta MAP by country and region
#need dw-MAP? for easier interpretation, to look at how fractionation from the theoretical drinking water the tooth is, so flip the current coulms
OSr_teeth$`Δ18Odw-MAP Levinson`<-OSr_teeth$`O-dw-SMOW Levinson`- OSr_teeth$`Modelled Mean Annual Precipitation (MAP) (‰)`
OSr_teeth$`Δ18Odw-MAP Chenery`<-OSr_teeth$`O-dw-SMOW Chenery 2012`- OSr_teeth$`Modelled Mean Annual Precipitation (MAP) (‰)`
head(OSr_teeth$`Δ18Odw-MAP Levinson`)
head(OSr_teeth$`Δ18Odw-MAP Chenery`)

#Levinson graphs
ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`Δ18OMAP-dw Levinson`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["MAP-dw(Levinson)"], " (\u2030)")))+xlab(expression(paste("Early Medieval Tooth Enamel")))+
  scale_y_continuous(limits=c(-13.5,16.5),breaks=seq(-13.5,16.5,2.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

DELTA_dwMAP_Levinson<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`Δ18Odw-MAP Levinson`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

DELTA_dwMAP_Levinson_ridges<-ggplot(OSr_teeth, aes(x=`Δ18Odw-MAP Levinson`, y=fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")




#Chenery graphs 
ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`Δ18OMAP-dw Chenery`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["MAP-dw(Chenery)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-13.5,16.5),breaks=seq(-13.5,16.5,2.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22)) #are iceland and finland just Sr? Check in database 

DELTA_dwMAP_Chenery<-ggplot(data = OSr_teeth, aes(x = fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), y = OSr_teeth$`Δ18Odw-MAP Chenery`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

DELTA_dwMAP_Chenery_ridges<-ggplot(OSr_teeth, aes(x=`Δ18Odw-MAP Chenery`, y=fct_reorder(OSr_teeth$Country, OSr_teeth$`Country Code`), fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  xlab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

ggarrange(DELTA_dwMAP_Chenery, DELTA_dwMAP_Levinson, DELTA_dwMAP_Chenery_ridges, DELTA_dwMAP_Levinson_ridges, ncol=2, nrow=2)

#by region in England
Oxy_England$`Δ18Odw-MAP Levinson`<-Oxy_England$`O-dw-SMOW Levinson`- Oxy_England$`Modelled Mean Annual Precipitation (MAP) (‰)`
Oxy_England$`Δ18Odw-MAP Chenery`<-Oxy_England$`O-dw-SMOW Chenery 2012`- Oxy_England$`Modelled Mean Annual Precipitation (MAP) (‰)`
head(Oxy_England$`Δ18Odw-MAP Levinson`)
head(Oxy_England$`Δ18Odw-MAP Chenery`)

DELTA_dwMAP_Levinson_Eng<-ggplot(data = Oxy_England, aes(x = Oxy_England$Region, y = Oxy_England$`Δ18Odw-MAP Levinson`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-9,9),breaks=seq(-9,9,1.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))


DELTA_dwMAP_Chenery_ridges_Eng<-ggplot(Oxy_England, aes(x=`Δ18Odw-MAP Chenery`, y=Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-9,9),breaks=seq(-9,9,1.0))+
  xlab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

DELTA_dwMAP_Chenery_Eng<-ggplot(data = Oxy_England, aes(x = Oxy_England$Region, y = Oxy_England$`Δ18Odw-MAP Chenery`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-9,9),breaks=seq(-9,9,1.0))+  
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

DELTA_dwMAP_Levinson_ridges_Eng<-ggplot(Oxy_England, aes(x=`Δ18Odw-MAP Levinson`, y=Region, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-9,9),breaks=seq(-9,9,1.0))+
  xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")


ggarrange(DELTA_dwMAP_Chenery_Eng, DELTA_dwMAP_Levinson_Eng, DELTA_dwMAP_Chenery_ridges_Eng, DELTA_dwMAP_Levinson_ridges_Eng, ncol=2, nrow=2)


#by period for England
DELTA_dwMAP_Chenery_Eng_Date<-ggplot(data = Oxy_England, aes(x = Oxy_England$SimpleDate, y = Oxy_England$`Δ18Odw-MAP Chenery`, fill=SimpleDate)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-9,9),breaks=seq(-9,9,1))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22),legend.position = "none")
DELTA_dwMAP_Chenery_Eng_Date

DELTA_dwMAP_Chenery_Eng_Period<-ggplot(data = Oxy_England, aes(x = Oxy_England$PeriodBroad, y = Oxy_England$`Δ18Odw-MAP Chenery`, fill=PeriodBroad)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-9,9),breaks=seq(-9,9,1))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22),legend.position = "none")
DELTA_dwMAP_Chenery_Eng_Period

#scatter plots for dw-MAP levinson vs. chenery
#European Level - R2=0.97 so almost perfect fit but there appears to be two different lines, need to try two different linear models
#Norway has a very obvious different trajectory but can do r-squared values by country as well and most are 0.99 or higher 
#country specific lm with the less perfect fits being due to outliers? what does this mean for using Chenery vs. Levinson? 
library(ggpmisc)

ggplot(OSr_teeth,aes(`Δ18Odw-MAP Levinson`,`Δ18Odw-MAP Chenery`))+
  geom_point(size=3,shape=16)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+scale_y_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+
  theme_bw()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=18))

ggplot(OSr_teeth,aes(`Δ18Odw-MAP Levinson`,`Δ18Odw-MAP Chenery`))+
  geom_point(size=3,shape=16)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+scale_y_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+
  theme_bw()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=18))+facet_wrap(~Country)

ggplot(OSr_teeth, aes(`Δ18Odw-MAP Levinson`,`Δ18Odw-MAP Chenery`, shape=EuRegion2, colour=EuRegion2, fill=EuRegion2))+
  geom_point(size=3)+
  geom_smooth(method='lm', formula=y~x)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+scale_y_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+
  theme_bw()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=18))

ggplot(OSr_teeth, aes(`Δ18Odw-MAP Levinson`,`Δ18Odw-MAP Chenery`, colour=EuRegion2, fill=EuRegion2))+
  geom_smooth(method='lm', formula=y~x)+
  labs(colour="Region", fill = "Region")+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+scale_y_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+
  theme_bw()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=18))


#England level - R2 =1 so they're systematically offset 
ggplot(Oxy_England,aes(`Δ18Odw-MAP Levinson`,`Δ18Odw-MAP Chenery`))+
  geom_point(size=3,shape=16)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+scale_y_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))


#PCA for d18O England
#all data must be numeric so going to subset for PCA to look at the interaction of Alt, Lat, Long
#prcomp(Oxy_England, scale=FALSE)

#exporting as csv file to input hclust groups in and re-import because the vector for the column would be waayyyyy too long 
library(readr)
#write_csv(OSr_teeth, path = "Dropbox/PhD_Cantab/OSr_teeth.csv")
#reimport but a lot of the site names got weird so not going to replace, but hopefully use the hclust group column to populate the new one in original OSr_teeth
library(readxl)
OSr_teeth_hclust_DONOTUSE <- read_excel("Dropbox/PhD_Cantab/OSr_teeth.xlsx")
#View(OSr_teeth_hclust_DONOTUSE)

OSr_teeth$`hclust group`<-OSr_teeth_hclust_DONOTUSE$`hclust group`
#View(OSr_teeth)
OSr_teeth$`hclust group` = as.character(OSr_teeth$`hclust group`)
OSr_teeth$`OCSr_hclust.group`<-OSr_teeth_hclust_DONOTUSE$`OCSr_hclust.group`
OSr_teeth$`OSr_hclust.group`<-OSr_teeth_hclust_DONOTUSE$`OSr_hclust.group`
OSr_teeth$`OCSr_hclust.group` = as.character(OSr_teeth$`OCSr_hclust.group`)
OSr_teeth$`OSr_hclust.group` = as.character(OSr_teeth$`OSr_hclust.group`)

#hclust scatter plot for d13C and d18O
ggplot(OSr_teeth,aes(d13C,`O-PO4 SMOW Measured/Chenery 2012`, color=`hclust group`))+
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1))+scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))

#hclust scatter plot for d18O and Sr
OSr_teeth <- OSr_teeth %>% mutate(OSr_hclust.group = replace(OSr_hclust.group, OSr_hclust.group == '1.1000000000000001', '1.1'))
OSr_teeth <- OSr_teeth %>% mutate(OSr_hclust.group = replace(OSr_hclust.group, OSr_hclust.group == '1.2.1', '1.2'))
OSr_teeth <- OSr_teeth %>% mutate(OSr_hclust.group = replace(OSr_hclust.group, OSr_hclust.group == '1.2.2', '1.2'))
OSr_teeth <- OSr_teeth %>% mutate(OSr_hclust.group = replace(OSr_hclust.group, OSr_hclust.group == '2.2.2.1', '2.2.2'))
OSr_teeth <- OSr_teeth %>% mutate(OSr_hclust.group = replace(OSr_hclust.group, OSr_hclust.group == '2.2.2.2', '2.2.2'))
ggplot(OSr_teeth,aes(OSr_teeth$`87Sr/86Sr`,`O-PO4 SMOW Measured/Chenery 2012`, color=`OSr_hclust.group`))+
  theme_bw()+
  geom_point(size=3,shape=16)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(TeX("^{87}Sr/^{86}Sr"))+
  scale_x_continuous(limits=c(0.706,0.743),breaks=seq(0.706,0.743,0.005))+scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))

#scatterplot for Carbon, Oxygen and Strontium
library(scatterplot3d)
OCSrenamel<- OSr_teeth
class(as.data.frame(OCSrenamel))
head(OCSrenamel)
OCSrenamel <- data.frame(OCSrenamel)
rownames(OCSrenamel) <-OCSrenamel[,1]
OCSrenamel <- OCSrenamel[,-1]
head(OCSrenamel)
View(OCSrenamel)
OCSrenamel<-OCSrenamel[c(1:4,23,27,33,40,42,44)] #country, Country code, Europe region, cluster group
head(OCSrenamel)
OCSrenamel <- na.omit(OCSrenamel)
head(OCSrenamel)
OCSrenamel %>%             
  mutate(`OCSr_hclust.group` = as.ordered(`OCSr_hclust.group`))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '1.1000000000000001', '1.1'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '1.1', '1'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '1.2', '1'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.2.1', '2.2'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.2.2', '2.2'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '3.2.1', '3.2'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '3.2.2', '3.2'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.2', '2'))
OCSrenamel <- OCSrenamel %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.1', '2'))

threeDplotcolours<-cbbPalette
threeDplotcolours <- threeDplotcolours[as.ordered(OCSrenamel$OCSr_hclust.group)]
plot.angle <- 40
#scatterplot3d(OCSrenamel$d13C, OCSrenamel$`O.PO4.SMOW.Measured.Chenery.2012`, OCSrenamel$`X87Sr.86Sr`, type="h", angle=plot.angle, color=threeDplotcolours, pch=16, 
             # col.axis="gray", col.grid="gray")
OCSrenamel$OCSr_hclust.group = factor(OCSrenamel$OCSr_hclust.group,
                            levels=c("1","2","3.1","3.2"),ordered=TRUE)

OCsr3d<-scatterplot3d(OCSrenamel$d13C, OCSrenamel$`O.PO4.SMOW.Measured.Chenery.2012`, OCSrenamel$`X87Sr.86Sr`,
              xlab = (expression(paste(delta^{13},C["carb (PDB)"], " (\u2030)"))),
              ylab = (expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)"))),
              zlab = TeX("^{87}Sr/^{86}Sr"), angle=plot.angle, pch = 16, color=threeDplotcolours)
legend("bottom", legend = c("1","2", "3.1","3.2"), col =  c("#000000", "#E69F00", "#56B4E9","#009E73", "#F0E442"), pch = 16,inset = -0.25, xpd = TRUE, horiz = TRUE)


#stacked barchart with country and cluster 
#counts for country categories
table(OSr_teeth$Country)
#counts for cluster categories
table(OSr_teeth$`hclust group`)

#cross classification of counts for Country and cluster group
EMEU_hclust_country_table<-table(OSr_teeth$`hclust group`, OSr_teeth$Country)
addmargins(EMEU_hclust_country_table)
EMEU_hclust_country_table2<-table(OSr_teeth$Country,OSr_teeth$`hclust group`)
prop.table(EMEU_hclust_country_table)
prop.table(EMEU_hclust_country_table2)

ggplot(OSr_teeth, aes(x=Country))+
  geom_bar()
ggplot(OSr_teeth, aes(x=`hclust group`))+
  geom_bar()

as.data.frame(EMEU_hclust_country_table) #showing as a df but ggplot not taking it
require(reshape2)
melt(EMEU_hclust_country_table)

ggplot(EMEU_hclust_country_table, aes(Var2, value)) +
  geom_bar(colour = "black", aes(fill = `Var1`, outline.colour = "black"), position = "stack", stat="identity") +
  theme_bw()

cluster_country_colour <- ggplot(EMEU_hclust_country_table, aes(EMEU_hclust_country_table$Var2, EMEU_hclust_country_table$Var1)) +
  geom_bar(colour = "black", aes(fill = Var1, outline.colour = "black"), position = "stack", stat="identity") +
  theme_bw()

ggplot(OSr_teeth, aes(Country)) +
  geom_bar(colour = "black", aes(fill = `hclust group`, weight=value, outline.colour = "black")) + theme_bw() +
  labs(x="Country", y = expression("Percentage (%)")) +
  theme(legend.title=element_blank())


#redoing with new European regions instead of Countries
OSr_teeth$EuRegion<-OSr_teeth$County
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Split-Dalmatia', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Zadar', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vis', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vukovar-Srijem', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Šibenik-Knin', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Roskilde', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Zealand', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Funen', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ålborg', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Saare', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Turku and Pori', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nord-Trøndelag', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nordland', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Troms', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rogaland', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sogn og Fjordane', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vest-Agder', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vestfold', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hordaland', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hedmark', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Trøndelag', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Telemark', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Akershus', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oppland', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Sør Trøndelag', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Uppland', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Uppsala', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kalmar', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Stockholm', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gotland', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Öland', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Gävleborg', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Adelsö', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Leningrad Oblast', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nord Trøndelag', 'Fennoscandia & Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Isle of Man', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Pembrokeshire', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Vale of Glamorgan', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Meath', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dublin', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ireland', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Outer Hebrides', 'Scotland and Scottish Isles'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Scotland', 'Scotland and Scottish Isles'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rousay', 'Scotland and Scottish Isles'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Bedfordshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Cambridgeshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Dorset', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Derbyshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'East Sussex', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kent', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hampshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Wiltshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Warwickshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Suffolk', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Oxfordshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lincolnshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Tyne & Wear', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Northumberland', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Yorkshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rutland', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Nottinghamshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Hertfordshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Kujalleq', 'North Atlantic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Mosfell', 'North Atlantic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Piemont', 'Po Valley'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Ibiza', 'Balearic & Tyrrhenian Seas'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Normandy', 'Normandy/Neustria'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Aragon', 'Inland & Western Iberia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Lower Saxony', 'Frisia & Saxony'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Friesland', 'Frisia & Saxony'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Rhineland', 'Austrasia & Burgundy'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Grand Est', 'Austrasia & Burgundy'))
#OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == NA, 'Fennoscandia & Baltic'))
#OSr_teeth$EuRegion %>% replace_na("Fennoscandia & Baltic")
#OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'NA', 'Fennoscandia & Baltic'))

#OSr_teeth$EuRegion[is.na(OSr_teeth$EuRegion)] <- as.factor(OSr_teeth$EuRegions[is.na(OSr_teeth$EuRegion)])
#OSr_teeth$EuRegion[is.na(OSr_teeth$EuRegion)] <- as.factor(OSr_teeth$Country[is.na(OSr_teeth$EuRegion)])
#OSr_teeth$EuRegion[is.na(OSr_teeth$EuRegion)] <- as.character(OSr_teeth$Country[is.na(OSr_teeth$EuRegion)])
OSr_teeth$EuRegion <- ifelse(is.na(OSr_teeth$EuRegion), OSr_teeth$Country, OSr_teeth$EuRegion) #on my macbook air had to do this instead, no idea why
OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'Norway', 'Fennoscandia & Baltic'))

OSr_teeth$EuRegion = factor(OSr_teeth$EuRegion,
                                  levels=c("North Atlantic", "Fennoscandia & Baltic", "Scotland and Scottish Isles", "Irish Sea", "England", "Frisia & Saxony", "Normandy/Neustria", "Austrasia & Burgundy","Po Valley", "Croatia", "Balearic & Tyrrhenian Seas","Inland & Western Iberia"),ordered=TRUE)
#no idea how I created "EuRegions" can't work that out but YAY forced NAs out 

#redo graphs with EuRegion instead of Country
##d18O box and ridgeplots
EMEU_d18O_boxjitter_EuRegion<-ggplot(data = OSr_teeth, aes(x = EuRegion, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_d18O_boxjitter_EuRegion

EMEU_d18O_violinjitter_EuRegion<-ggplot(data = OSr_teeth, aes(x = EuRegion, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_d18O_violinjitter_EuRegion

EMEU_ridges_d18O_EuRegion<-ggplot(OSr_teeth, aes(x=`O-PO4 SMOW Measured/Chenery 2012`, y=EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  stat_n_text() + 
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_ridges_d18O_EuRegion

#d13C box and ridgeplots
EMEU_Carbonate_boxjitter_EuRegion<-ggplot(data = OSr_teeth, aes(x = EuRegion, y = OSr_teeth$d13C)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_Carbonate_boxjitter_EuRegion

EMEU_d13C_violinjitter_EuRegion<-ggplot(data = OSr_teeth, aes(x = EuRegion, y = OSr_teeth$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_d13C_violinjitter_EuRegion

EMEU_ridges_carbonate_EuRegion<-ggplot(OSr_teeth, aes(x=d13C, y=EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  xlab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_ridges_carbonate_EuRegion

#Sr box and ridgeplots
EMEU_Sr_boxjitter_EuRegion<-ggplot(data = OSr_teeth, aes(x = EuRegion, y = OSr_teeth$`87Sr/86Sr`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste("87Sr/86Sr")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_Sr_boxjitter_EuRegion

EMEU_Sr_violinjitter_EuRegion<-ggplot(data = OSr_teeth, aes(x = EuRegion, y = OSr_teeth$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste("87Sr/86Sr")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_Sr_violinjitter_EuRegion

EMEU_ridges_Sr_EuRegion<-ggplot(OSr_teeth, aes(x=`87Sr/86Sr`, y=EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  xlab(expression(paste("87Sr/86Sr")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_ridges_Sr_EuRegion


#big delta box and ridgeplots comparing Chenery and Levinson 

DELTA_dwMAP_Levinson_Euregion<-ggplot(data = OSr_teeth, aes(x = OSr_teeth$EuRegion, y = OSr_teeth$`Δ18Odw-MAP Levinson`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

DELTA_dwMAP_Levinson_ridges_Euregion<-ggplot(OSr_teeth, aes(x=`Δ18Odw-MAP Levinson`, y=OSr_teeth$EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

DELTA_dwMAP_Chenery_Euregion<-ggplot(data = OSr_teeth, aes(x = OSr_teeth$EuRegion, y = OSr_teeth$`Δ18Odw-MAP Chenery`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))

DELTA_dwMAP_Chenery_ridges_Euregion<-ggplot(OSr_teeth, aes(x=`Δ18Odw-MAP Chenery`, y= OSr_teeth$EuRegion, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-12,14),breaks=seq(-12,14,2.0))+
  xlab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")

ggarrange(DELTA_dwMAP_Chenery_Euregion, DELTA_dwMAP_Levinson_Euregion, DELTA_dwMAP_Chenery_ridges_Euregion, DELTA_dwMAP_Levinson_ridges_Euregion, ncol=2, nrow=2)

#Big delta linear models by region to have next to country
ggplot(OSr_teeth,aes(`Δ18Odw-MAP Levinson`,`Δ18Odw-MAP Chenery`))+
  geom_point(size=3,shape=16)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+scale_y_continuous(limits=c(-14,17),breaks=seq(-14,17,2))+
  theme_bw()+
  theme(axis.text=element_text(size=9),axis.title=element_text(size=16))+facet_wrap(~EuRegion)

ggplot(OSr_teeth,aes(`Δ18Odw-MAP Levinson`,`Δ18Odw-MAP Chenery`))+
  geom_point(size=3,shape=16)+
  geom_smooth(method='lm', formula=y~x)+
  stat_poly_eq(formula = y~x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste(Delta^{18},O["dw-MAP (Levinson)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-14,17),breaks=seq(-14,17,4))+scale_y_continuous(limits=c(-14,17),breaks=seq(-14,17,4))+
  theme_bw()+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=22))+facet_wrap(~EuRegion2)

ggplot(data = Oxy_England, aes(x = Oxy_England$Region, y = Oxy_England$`Δ18Odw-MAP Chenery`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Region")))+
  scale_y_continuous(limits=c(-7,7),breaks=seq(-7,7,1))+
  theme(axis.title = element_text(size=22),axis.text.y =element_text(size=18),axis.text.x = element_text(angle = 45, hjust = 1, size = 16),legend.position = "none")
#stacked histograms see separate file


#Trying separation of sites in Fennoscandia & Baltic on advice from lab group 
OSr_teeth$EuRegion2<-OSr_teeth$County
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Split-Dalmatia', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Zadar', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Vis', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Vukovar-Srijem', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Šibenik-Knin', 'Croatia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Roskilde', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Zealand', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Funen', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Ålborg', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Saare', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Turku and Pori', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Nord-Trøndelag', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Nordland', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Troms', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Rogaland', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Sogn og Fjordane', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Vest-Agder', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Vestfold', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Hordaland', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Hedmark', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Trøndelag', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Telemark', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Akershus', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Oppland', 'Skagerrak-Kattegat-Jutland Basin'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Sør Trøndelag', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Uppland', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Uppsala', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Kalmar', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Stockholm', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Gotland', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Öland', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Gävleborg', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Adelsö', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Leningrad Oblast', 'Baltic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Nord Trøndelag', 'Atlantic & Arctic Norway'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Isle of Man', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Pembrokeshire', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Vale of Glamorgan', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Meath', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Dublin', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Ireland', 'Irish Sea'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Outer Hebrides', 'Scotland and Scottish Isles'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Scotland', 'Scotland and Scottish Isles'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Rousay', 'Scotland and Scottish Isles'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Bedfordshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Cambridgeshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Dorset', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Derbyshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'East Sussex', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Kent', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Hampshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Wiltshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Warwickshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Suffolk', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Oxfordshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Lincolnshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Tyne & Wear', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Northumberland', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Yorkshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Rutland', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Nottinghamshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Hertfordshire', 'England'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Kujalleq', 'North Atlantic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Mosfell', 'North Atlantic'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Piemont', 'Po Valley'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Ibiza', 'Balearic & Tyrrhenian Seas'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Normandy', 'Normandy/Neustria'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Aragon', 'Inland & Western Iberia'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Lower Saxony', 'Frisia & Saxony'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Friesland', 'Frisia & Saxony'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Rhineland', 'Austrasia & Burgundy'))
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Grand Est', 'Austrasia & Burgundy'))
#OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == NA, 'Fennoscandia & Baltic'))
#OSr_teeth$EuRegion %>% replace_na("Fennoscandia & Baltic")
#OSr_teeth <- OSr_teeth %>% mutate(EuRegion = replace(EuRegion, EuRegion == 'NA', 'Fennoscandia & Baltic'))

#OSr_teeth$EuRegion[is.na(OSr_teeth$EuRegion)] <- as.factor(OSr_teeth$EuRegions[is.na(OSr_teeth$EuRegion)])
#OSr_teeth$EuRegion[is.na(OSr_teeth$EuRegion)] <- as.factor(OSr_teeth$Country[is.na(OSr_teeth$EuRegion)])
#OSr_teeth$EuRegion2[is.na(OSr_teeth$EuRegion2)] <- as.character(OSr_teeth$Country[is.na(OSr_teeth$EuRegion)])
OSr_teeth$EuRegion2 <- ifelse(is.na(OSr_teeth$EuRegion2), OSr_teeth$Country, OSr_teeth$EuRegion2) #on my macbook air had to do this instead, no idea why
OSr_teeth <- OSr_teeth %>% mutate(EuRegion2 = replace(EuRegion2, EuRegion2 == 'Norway', 'Atlantic & Arctic Norway'))

OSr_teeth$EuRegion2 = factor(OSr_teeth$EuRegion2,
                            levels=c("North Atlantic", "Atlantic & Arctic Norway","Skagerrak-Kattegat-Jutland Basin","Baltic", "Scotland and Scottish Isles", "Irish Sea", "England", "Frisia & Saxony", "Normandy/Neustria", "Austrasia & Burgundy","Po Valley", "Croatia", "Balearic & Tyrrhenian Seas","Inland & Western Iberia"),ordered=TRUE)


#ridgeplots with new split Fennoscandia 
##d18O box and ridgeplots
EMEU_d18O_boxjitter_EuRegion2<-ggplot(data = OSr_teeth, aes(x = EuRegion2, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22)) su
EMEU_d18O_boxjitter_EuRegion2

EMEU_d18O_violinjitter_EuRegion2<-ggplot(data = OSr_teeth, aes(x = EuRegion2, y = OSr_teeth$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_d18O_violinjitter_EuRegion2

EMEU_ridges_d18O_EuRegion2<-ggplot(OSr_teeth, aes(x=`O-PO4 SMOW Measured/Chenery 2012`, y=EuRegion2, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(12.5,21),breaks=seq(12.5,21,1.0))+
  xlab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_ridges_d18O_EuRegion2

#d13C box and ridgeplots
EMEU_Carbonate_boxjitter_EuRegion2<-ggplot(data = OSr_teeth, aes(x = EuRegion2, y = OSr_teeth$d13C)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_Carbonate_boxjitter_EuRegion2

EMEU_d13C_violinjitter_EuRegion2<-ggplot(data = OSr_teeth, aes(x = EuRegion2, y = OSr_teeth$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_d13C_violinjitter_EuRegion2

EMEU_ridges_carbonate_EuRegion2<-ggplot(OSr_teeth, aes(x=d13C, y=EuRegion2, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(-17,-4),breaks=seq(-17,-4,1.0))+
  xlab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_ridges_carbonate_EuRegion2

#Sr box and ridgeplots
EMEU_Sr_boxjitter_EuRegion2<-ggplot(data = OSr_teeth, aes(x = EuRegion2, y = OSr_teeth$`87Sr/86Sr`)) + 
  geom_boxplot(position=position_dodge(1), show.legend = FALSE)+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(expression(paste("87Sr/86Sr")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  theme_bw()+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_Sr_boxjitter_EuRegion2

EMEU_Sr_violinjitter_EuRegion2<-ggplot(data = OSr_teeth, aes(x = EuRegion2, y = OSr_teeth$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  coord_flip()+
  geom_jitter(position = position_jitter(width = 0.03, height = 0.03), alpha=0.2)+
  ylab(TeX('^{87}Sr/^{86}Sr'))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))
EMEU_Sr_violinjitter_EuRegion2

EMEU_ridges_Sr_EuRegion2<-ggplot(OSr_teeth, aes(x=`87Sr/86Sr`, y=EuRegion2, fill=factor(..quantile..))) +
  theme_bw()+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis(discrete = TRUE, name = "Quartiles")+
  scale_x_continuous(limits=c(0.7060,0.7430),breaks=seq(0.7060,0.7430,0.005))+
  xlab(TeX('^{87}Sr/^{86}Sr'))+ylab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.position = "bottom", legend.direction = "horizontal")
EMEU_ridges_Sr_EuRegion2

#BEST test to compare F&M mobility in England just for 350-790AD (F v. M) and 790-1066AD (F v. M), and then those two periods as a whole d18o and D18O
library(BEST)
EngpreViking<-subset(Oxy_England,`PeriodBroad`=="c.350AD-790AD")
EngpostViking<-subset(Oxy_England,`PeriodBroad`=="c.790AD-1066AD+")

EngpreVikingallO<-c(EngpreViking$`O-PO4 SMOW Measured/Chenery 2012`)
EngpostVikingallO<-c(EngpostViking$`O-PO4 SMOW Measured/Chenery 2012`)
EngpreVikingallO<-na.omit(EngpreVikingallO)
EngpostVikingallO<-na.omit(EngpostVikingallO)
BESTout_Eng_Time_d18O <- BESTmcmc(EngpreVikingallO, EngpostVikingallO, priors=NULL, parallel=FALSE) 
plotAll(BESTout_Eng_Time_d18O)


EngpreVikingsF <- subset(EngpreViking, EngpreViking$SimpleSex=="F")
EngpreVikingsM <- subset(EngpreViking, EngpreViking$SimpleSex=="M")
EngpostVikingF <- subset(EngpostViking, EngpostViking$SimpleSex=="F")
EngpostVikingM <- subset(EngpostViking, EngpostViking$SimpleSex=="M")

EngpreVikingsF_d18O <- c(EngpreVikingsF$`O-PO4 SMOW Measured/Chenery 2012`)
EngpreVikingsM_d18O <- c(EngpreVikingsM$`O-PO4 SMOW Measured/Chenery 2012`)
EngpostVikingF_d18O <- c(EngpostVikingF$`O-PO4 SMOW Measured/Chenery 2012`)
EngpostVikingM_d18O <- c(EngpostVikingM$`O-PO4 SMOW Measured/Chenery 2012`)

EngpreVikingsF_d18O <- na.omit(EngpreVikingsF_d18O)
EngpreVikingsM_d18O <- na.omit(EngpreVikingsM_d18O)
EngpostVikingF_d18O <- na.omit(EngpostVikingF_d18O)
EngpostVikingM_d18O <- na.omit(EngpostVikingM_d18O)


#testing F vs M in pre-viking
BESTout_Eng_sex_previking <- BESTmcmc(EngpreVikingsF_d18O, EngpreVikingsM_d18O, priors=NULL, parallel=FALSE) 
plotAll(BESTout_Eng_sex_previking)
#testing F vs M in post-Viking
BESTout_Eng_sex_postviking <- BESTmcmc(EngpostVikingF_d18O, EngpostVikingM_d18O, priors=NULL, parallel=FALSE) 
plotAll(BESTout_Eng_sex_postviking)
#testing F vs F in pre vs post Viking
BESTout_Eng_F_time <- BESTmcmc(EngpreVikingsF_d18O, EngpostVikingF_d18O, priors=NULL, parallel=FALSE) 
plotAll(BESTout_Eng_F_time)
#testing M vs M in pre vs post Viking
BESTout_Eng_M_time <- BESTmcmc(EngpreVikingsM_d18O, EngpostVikingM_d18O, priors=NULL, parallel=FALSE) 
plotAll(BESTout_Eng_M_time)

#for big delta
EngpreVikingD18O<-c(EngpreViking$`Δ18Odw-MAP Chenery`)
EngpostVikingD18O<-c(EngpostViking$`Δ18Odw-MAP Chenery`)
EngpreVikingD18O<-na.omit(EngpreVikingD18O)
EngpostVikingD18O<-na.omit(EngpostVikingD18O)
BESTout_Eng_Time_bigdelta <- BESTmcmc(EngpreVikingD18O, EngpostVikingD18O, priors=NULL, parallel=FALSE) 
plotAll(BESTout_Eng_Time_bigdelta)


#Pederzani and Britton mean Delta 18 Oxyge brewing and stewing diffs
library(readxl)
Brewing_Stewing_Fractionation_Pederzani <- read_excel("Dropbox/PhD_Cantab/PhD Chapters/Brewing_Stewing_Fractionation_Pederzani.xlsx")
View(Brewing_Stewing_Fractionation_Pederzani)

Brewing_Stewing_Fractionation_Pederzani$Process = factor(Brewing_Stewing_Fractionation_Pederzani$Process,
                                  levels=c("Boiling","Brewing", "Milk", "Stewing (1h)", "Distillation", "Stewing (2h)", "Stewing (3h)", "Wine"),ordered=TRUE)


ggplot(data=Brewing_Stewing_Fractionation_Pederzani, aes(x=Process, y=Mean_D18O, fill=Process))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  theme_bw()+
  scale_fill_viridis_d()+
  geom_text(aes(label=Mean_D18O), position=position_dodge(width=0.5), vjust=-0.25, hjust=-0.15)+
  coord_flip()+
  ylab(expression(paste(Delta^{18},O["SMOW"], " (\u2030)")))+xlab(expression(paste("")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22))




