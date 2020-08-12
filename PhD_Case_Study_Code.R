#Case Study graphs for PhD - Kent and East Sussex and Finglesham
#Author: S.A. Leggett
#Date: 2020 

library(ggplot2)
library(magrittr)
library(tidyverse)
library(qqplotr)
library(ggExtra)
library(viridis)
library(ggsci)
library(ggpubr)
library(dplyr)
library(latex2exp)
library(gridExtra)


### ***KENT AND EAST SUSSEX*** ###
kentEsuss_carb<-subset(Oxy_England, Region=="Kent and East Sussex")
show(kentEsuss_carb)
summary(kentEsuss_carb)

kentEsuss_bone<-subset(C_N_Database_bone, Region=="Kent and East Sussex")
show(kentEsuss_bone)
summary(kentEsuss_bone)

kentEsuss_dent<-subset(C_N_dentine, Region=="Kent and East Sussex")
show(kentEsuss_dent)
summary(kentEsuss_dent)

kentEsuss_fauna<-subset(CN_fauna, Region=="Kent and East Sussex")
show(kentEsuss_fauna)
summary(kentEsuss_fauna)

kentEsuss_matched<-subset(matched_bone_dent_enamel_England, Region=="Kent and East Sussex")
show(kentEsuss_matched)
summary(kentEsuss_matched)

#KENT AND EAST SUSSEX - general
kent_CN_fauna_scatter<- ggplot(kentEsuss_fauna,aes(d13C,d15N, group=`Species Group`))+
  theme_bw()+
  geom_point(aes(shape=`Species Group`,color=`Species Group`,fill=`Species Group`),size=3,)+
  scale_shape_manual(values=c(15, 17, 23, 19, 25, 8),name="Species Group")+
  scale_color_brewer(palette = "Dark2", name="Species Group")+
  scale_fill_brewer(palette = "Dark2", name="Species Group")+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_x_continuous(limits=c(-26,-15),breaks=seq(-26,-15,1))+scale_y_continuous(limits=c(0,14),breaks=seq(0,14,1))+
  theme(legend.position ="bottom", legend.direction = "horizontal", legend.title = element_text(size=20), legend.text = element_text(siz=16), axis.title = element_text(size=20), axis.text = element_text(size=18))
kent_CN_fauna_scatter


kent_enamel_CO3_scatter<- ggplot(kentEsuss_carb,aes(d13C,`O-PO4 SMOW Measured/Chenery 2012`, color=`hclust group`))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
kent_enamel_CO3_scatter

ggMarginal(kent_enamel_CO3_scatter , type = "boxplot")
ggMarginal(kent_enamel_CO3_scatter)

kent_enamel_OSr_scatter<- ggplot(kentEsuss_carb,aes(`87Sr/86Sr`,`O-PO4 SMOW Measured/Chenery 2012`, color=kentEsuss_carb$OSr_hclust.group))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(TeX('^{87}Sr/^{86}Sr'))+
  scale_x_continuous(limits=c(0.707,0.713),breaks=seq(0.707,0.713,0.001))+scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
kent_enamel_OSr_scatter
ggMarginal(kent_enamel_OSr_scatter)

kent_bone_scatter<- ggplot(kentEsuss_bone,aes(d13C,d15N, color=kentEsuss_bone$CNB_hcluster3))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
kent_bone_scatter
ggMarginal(kent_bone_scatter)

kent_dentine_scatter<- ggplot(kentEsuss_dent,aes(d13C,d15N, color=CND_hcluster))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  xlab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
kent_dentine_scatter
ggMarginal(kent_dentine_scatter)

#grid arrange d18O, Sr and d13C (enamel), d15N, d13C for bone and dentine to look at all distributions


kentESuss_d13C_enamel_violin<-ggplot(data = kentEsuss_carb, aes(x = "", y = kentEsuss_carb$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

kentESuss_d13C_bone_violin<-ggplot(data = kentEsuss_bone, aes(x = "", y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

kentESuss_d13C_dentine_violin<-ggplot(data = kentEsuss_dent, aes(x = "", y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

kentESuss_d15N_bone_violin<-ggplot(data = kentEsuss_bone, aes(x = "", y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

kentESuss_d15N_dentine_violin<-ggplot(data = kentEsuss_dent, aes(x = "", y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

kentESuss_d18O_violin<-ggplot(data = kentEsuss_carb, aes(x = "", y = kentEsuss_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))


kentESuss_DELTA18O_violin<-ggplot(data = kentEsuss_carb, aes(x = "", y = kentEsuss_carb$`Δ18Odw-MAP Chenery`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-4,5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))


kentESuss_Sr_violin<-ggplot(data = kentEsuss_carb, aes(x = "", y = kentEsuss_carb$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(TeX('^{87}Sr/^{86}Sr'))+xlab(expression(paste("")))+
  scale_y_continuous(limits=c(0.7080,0.712),breaks=seq(0.7080,0.712,0.001))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(kentESuss_d13C_enamel_violin, kentESuss_d13C_bone_violin, kentESuss_d13C_dentine_violin, kentESuss_d18O_violin, kentESuss_d15N_bone_violin, kentESuss_d15N_dentine_violin, kentESuss_DELTA18O_violin, kentESuss_Sr_violin,ncol= 3, nrow=3)



##Oxygen
#histogram with just KENT AND EAST SUSSEX to check for bimodel dist for oxygen#
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


#ALL ISOTOPES BY ENVIRONMENT #
#oxygen# 
ggplot(data = kentEsuss_carb, aes(x = Environment, y = `O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Big Delta 
ggplot(data = kentEsuss_carb, aes(x = Environment, y = `Δ18Odw-MAP Chenery`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-4,5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))


#Enamel Carbon
ggplot(data = kentEsuss_carb, aes(x = Environment, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Strontium
ggplot(data = kentEsuss_carb, aes(x = Environment, y = `87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(TeX('^{87}Sr/^{86}Sr'))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(0.7063,0.7422),breaks=seq(0.7063,0.7422,0.01))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))


#Bone Carbon
ggplot(data = kentEsuss_bone, aes(x = Environment, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Bone Nitrogen
ggplot(data = kentEsuss_bone, aes(x = Environment, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Dentine Carbon
ggplot(data = kentEsuss_dent, aes(x = Environment, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Dentine Nitrogen
ggplot(data = kentEsuss_dent, aes(x = Environment, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("Environment")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))


#ALL ISOTOPES BY SITE#

#oxygen# 
ggplot(data = kentEsuss_carb, aes(x = kentEsuss_carb$Site, y = kentEsuss_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Enamel Carbon
ggplot(data = kentEsuss_carb, aes(x = kentEsuss_carb$Site, y = kentEsuss_carb$d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Strontium
ggplot(data = kentEsuss_carb, aes(x = kentEsuss_carb$Site, y = kentEsuss_carb$`87Sr/86Sr`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(TeX('^{87}Sr/^{86}Sr'))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(0.707,0.712),breaks=seq(0.707,0.712,0.001))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Big Delta by site
ggplot(data = kentEsuss_carb, aes(x = Site, y = `Δ18Odw-MAP Chenery`)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-4,5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Bone Carbon
ggplot(data = kentEsuss_bone, aes(x = Site, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Bone Nitrogen
ggplot(data = kentEsuss_bone, aes(x = Site, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Dentine Carbon
ggplot(data = kentEsuss_dent, aes(x = Site, y = d13C)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

#Dentine Nitrogen
ggplot(data = kentEsuss_dent, aes(x = Site, y = d15N)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("Site")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))


#ALL ISOTOPES BY DATE CATEGORY#
ggplot(data=kentEsuss_carb, aes(x=`Date Category`, y=`O-PO4 SMOW Measured/Chenery 2012`, fill=`Date Category`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=kentEsuss_carb, aes(x=PeriodBroad, y=`O-PO4 SMOW Measured/Chenery 2012`, fill=PeriodBroad))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  scale_fill_viridis(discrete = TRUE)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=kentEsuss_carb, aes(x=`Date Category`, y=`Δ18Odw-MAP Chenery`, fill=`Date Category`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-5,5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

#same but with some combined date categories
kentEsuss_carb$mergeddate<-kentEsuss_carb$`Date Category`
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'A', 'A-D'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'B', 'B-D'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'B/C', 'B-D'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'C', 'C/D'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'D', 'C/D'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'D/E', 'D-F'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'E', 'D-F'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'E/F', 'D-F'))
kentEsuss_carb$`mergeddate` = factor(kentEsuss_carb$`mergeddate`,
                                      levels=c("A-D", "B-D", "C/D", "D-F"),ordered=TRUE)


ggplot(data=kentEsuss_carb, aes(x=`mergeddate`, y=`Δ18Odw-MAP Chenery`, fill=`mergeddate`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-5,5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))


ggplot(data=kentEsuss_carb, aes(x=`Date Category`, y=`d13C`, fill=`Date Category`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=kentEsuss_carb, aes(x=`mergeddate`, y=`d13C`, fill=`mergeddate`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=kentEsuss_bone, aes(x=`Date Category`, y=d13C, fill=`Date Category`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

kentEsuss_bone$mergeddate<-kentEsuss_bone$`Date Category`
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'A/B', 'A-C'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'B', 'B-D'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'B/C', 'B-D'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'C', 'C/D'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'D', 'C/D'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'D/E', 'D-F'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'E', 'D-F'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(mergeddate = replace(mergeddate, mergeddate == 'E/F', 'D-F'))


kentEsuss_bone$`mergeddate` = factor(kentEsuss_bone$`mergeddate`,
                                     levels=c("A-C","B-D","C/D","D-F"),ordered=TRUE)

ggplot(data=kentEsuss_bone, aes(x=`mergeddate`, y=d13C, fill=`mergeddate`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))



ggplot(data=kentEsuss_bone, aes(x=`Date Category`, y=d15N, fill=`Date Category`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=kentEsuss_bone, aes(x=`mergeddate`, y=d15N, fill=`mergeddate`))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(kentEsuss_bone,aes(d13C,d15N, color=kentEsuss_bone$mergeddate))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_jco(name="Date Category")+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")



#LIFE COURSE AND MATCHED TISSUES#
library("ggsci")
#need the geom_bag() function from Ben Marwick's GitHub - https://gist.github.com/benmarwick/00772ccea2dd0b0f1745
devtools::source_gist("00772ccea2dd0b0f1745", filename = "000_geom_bag.r")
devtools::source_gist("00772ccea2dd0b0f1745", filename = "001_bag_functions.r")
kent_matched_bone_dent<-subset(England_matched_bone_dent, Region=="Kent and East Sussex")
#bone vs dentine d13C and d15N bagplot
Kent_Bone_Dent_Bag<- ggplot(kent_matched_bone_dent, aes(d13C, d15N, colour = `Bone or Dentine`, fill = `Bone or Dentine`)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Kent_Bone_Dent_Bag
#BEST test on this
library(BEST)
Bone_kent_matched_all<-subset(kent_matched_bone_dent, `Bone or Dentine`=="Bone") #English human bone C&N matched
Dentine_kent_matched_all<-subset(kent_matched_bone_dent, `Bone or Dentine`=="Dentine")#English human dentine C&N matched 
KentBoneC_matchedall<-c(Bone_kent_matched_all$d13C)
KentDentineC_matchedall<-c(Dentine_kent_matched_all$d13C)
carbonKent1_matched<-na.omit(KentBoneC_matchedall)
carbonKent2_matched<-na.omit(KentDentineC_matchedall)
BESTout_Kent_collC_matched_all <- BESTmcmc(carbonKent1_matched, carbonKent2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_Kent_collC_matched_all)
plotAll(BESTout_Kent_collC_matched_all)

KentBoneN_matched_all<-c(Bone_kent_matched_all$d15N)
KentDentineN_matched_all<-c(Dentine_kent_matched_all$d15N)
nitrogenKent1_matched<-na.omit(KentBoneN_matched_all)
nitrogenKent2_matched<-na.omit(KentDentineN_matched_all)
BESTout_Kent_collN_matched_all<- BESTmcmc(nitrogenKent1_matched, nitrogenKent2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_Kent_collN_matched_all)
plotAll(BESTout_Kent_collN_matched_all)
#no differences for d13C or d15N between bone and dentine as per whole English dataset

#3D plots of big deltas and carbon by Site? 
library(scatterplot3d)
threeDplotcolourskent<-c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442")
threeDplotcolourskent<- threeDplotcolourskent[as.ordered(kentEsuss_matched$Site)]
scatterplot3d(kentEsuss_matched$bone_d13C, kentEsuss_matched$dentine_d13C, kentEsuss_matched$enamel_d13C,
              xlab = (expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)"))),
              ylab = (expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)"))),
              zlab = (expression(paste(delta^{13},C["enamel carbonate (PDB)"], " (\u2030)"))), 
              angle=40, pch = 16, color=threeDplotcolourskent)
legend("topleft", legend = c("Buckland Dover","Finglesham","Holborough","Mill Hill, Deal", "Neat's Court Sheppey"), col=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), pch = 16,inset = -0.15, xpd = TRUE, horiz = FALSE)

scatterplot3d(kentEsuss_matched$`D13C_dent-bone`, kentEsuss_matched$D13C_tooth_enamel_dent, kentEsuss_matched$`D15N_dent-bone`,
              xlab = (expression(paste(Delta^{13},C["dentine-bone"], " (\u2030)"))),
              ylab = (expression(paste(Delta^{13},C["enamel-dentine"], " (\u2030)"))),
              zlab = (expression(paste(Delta^{15},N["dentine-bone"], " (\u2030)"))), 
              angle=40, pch = 16, color=threeDplotcolourskent)
legend("topleft", legend = c("Buckland Dover","Finglesham","Holborough","Mill Hill, Deal", "Neat's Court Sheppey"), col=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442"), pch = 16,inset = -0.15, xpd = TRUE, horiz = FALSE)






#for KENT AND EAST SUSSEX  by sex
ggplot(data = kentEsuss_carb, aes(x = kentEsuss_carb$SimpleSex, y = kentEsuss_carb$`O-PO4 SMOW Measured/Chenery 2012`)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), aes())+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.02))+
  #geom_hline(data=kentEsuss_carb, aes(yintercept= 16.6), color="black",linetype="dashed", size=1.5)+
  #geom_hline(data=kentEsuss_carb, aes(yintercept= 18.7), color="black",linetype="dashed", size=1.5)+
  #geom_hline(data=kentEsuss_carb, aes(yintercept= 17.25), color="red",linetype="dashed", size=1.5)+
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


#Stacked Barplots of different cluster numbers
library(scales)
library(ggthemes)
library(ztable)
library(magrittr)
library(viridis)
library(forcats)
##Whole Region


#Carbonate cluster

#
##By Site



##### **FINGLESHAM** #####
#import Fingelsham only data for cellwise to reduce chances of problems, limited to cells iwht numeric data that is individualised for burials and does not pertain to the site, and isn't free text
library(readxl)
FING_Finglesham_Kent <- read_excel("Dropbox/PhD_Cantab/Run_Files/Results/Cleaned data/Cleaned results by site, all data combined/FING_Finglesham_Kent.xlsx", 
                                   sheet = "cellWise_trial")
View(FING_Finglesham_Kent)
#clusters in scatterplot - dietary all the same, a few deviants in carbonate
FING_Finglesham_Kent$O_C_cluster<-as.character(FING_Finglesham_Kent$O_C_cluster)
FING_Finglesham_Kent$CNB_cluster<-as.character(FING_Finglesham_Kent$CNB_cluster)
FING_Finglesham_Kent$CND_cluster<-as.character(FING_Finglesham_Kent$CND_cluster)


finglesham_enamel_CO3_scatter<- ggplot(FING_Finglesham_Kent,aes(enamel_d13C, enamel_d18O_chenery, color=O_C_cluster))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste(delta^{13},C["carbonate (PDB)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_enamel_CO3_scatter

ggMarginal(finglesham_enamel_CO3_scatter)

finglesham_bone_scatter<- ggplot(FING_Finglesham_Kent,aes(bone_d13C,bone_d15N, color=CNB_cluster))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_bone_scatter
ggMarginal(finglesham_bone_scatter)

finglesham_dentine_scatter<- ggplot(FING_Finglesham_Kent,aes(dentine_d13C,dentine_d15N, color=CND_cluster))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_manual(values=cbbPalette, name="Cluster Number")+
  xlab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_dentine_scatter
ggMarginal(finglesham_dentine_scatter)

#change over time Finglesham
ggplot(data=FING_Finglesham_Kent, aes(x=date_cat, y=FING_Finglesham_Kent$enamel_d18O_chenery, fill=date_cat))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(14,19.5),breaks=seq(14,19.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=FING_Finglesham_Kent, aes(x=date_cat, y=FING_Finglesham_Kent$DELTA18O_dwMAP, fill=date_cat))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-5,5),breaks=seq(-5,5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=FING_Finglesham_Kent, aes(x=date_cat, y=FING_Finglesham_Kent$bone_d13C, fill=date_cat))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18))

ggplot(data=FING_Finglesham_Kent, aes(x=date_cat, y=bone_d15N, fill=date_cat))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="white")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  scale_fill_grey(start = 0.8, end = 0.2)+
  ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18)) #these aren't very 

####didn't do dentine as violin plots weren't super informative here 
#scatter or bag plots by period 
Finglesham_bone_period_Bag<- ggplot(FING_Finglesham_Kent, aes(bone_d13C, bone_d15N, colour = date_cat, fill = date_cat)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_viridis(discrete = TRUE)+
  scale_colour_viridis(discrete = TRUE)+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Finglesham_bone_period_Bag #not working properly since some of the periods are small sample sizes

finglesham_bone_scatter_period<- ggplot(FING_Finglesham_Kent,aes(bone_d13C,bone_d15N, color=date_cat))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_viridis(discrete=TRUE,name="Date Category")+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-21,-19),breaks=seq(-21,-19,1))+scale_y_continuous(limits=c(7,12),breaks=seq(7,12,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_bone_scatter_period

finglesham_dentine_scatter_period<- ggplot(FING_Finglesham_Kent,aes(dentine_d13C,dentine_d15N, color=date_cat))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_viridis(discrete=TRUE,name="Date Category")+
  xlab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-21,-19),breaks=seq(-21,-19,1))+scale_y_continuous(limits=c(7,13),breaks=seq(7,13,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_dentine_scatter_period

finglesham_carbonate_scatter_period<- ggplot(FING_Finglesham_Kent,aes(enamel_d13C,enamel_d18O_chenery, color=date_cat))+
  theme_bw()+
  geom_point(size=3,shape=16, show.legend = TRUE)+
  scale_colour_viridis(discrete=TRUE,name="Date Category")+
  xlab(expression(paste(delta^{13},C["enamel carbonate (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{18},O["phosphate (SMOW)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-16,-12),breaks=seq(-16,-12,1))+scale_y_continuous(limits=c(15,20),breaks=seq(15,20,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
finglesham_carbonate_scatter_period

#bagplot dentine vs. bone Finglesham
Finglesham_match_bone_dent<-subset(kent_matched_bone_dent, Site=="Finglesham")

Finglesham_Bone_Dent_Bag<- ggplot(Finglesham_match_bone_dent, aes(d13C, d15N, colour = `Bone or Dentine`, fill = `Bone or Dentine`)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Finglesham_Bone_Dent_Bag

#BEST test of this
Bone_FING_matched_all<-subset(Finglesham_match_bone_dent, `Bone or Dentine`=="Bone") #English human bone C&N matched
Dentine_FING_matched_all<-subset(Finglesham_match_bone_dent, `Bone or Dentine`=="Dentine")#English human dentine C&N matched 
FINGBoneC_matchedall<-c(Bone_FING_matched_all$d13C)
FINGDentineC_matchedall<-c(Dentine_FING_matched_all$d13C)
carbonFING1_matched<-na.omit(FINGBoneC_matchedall)
carbonFING2_matched<-na.omit(FINGDentineC_matchedall)
BESTout_FING_collC_matched_all <- BESTmcmc(carbonFING1_matched, carbonFING2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_FING_collC_matched_all)
plotAll(BESTout_FING_collC_matched_all)

FINGBoneN_matched_all<-c(Bone_FING_matched_all$d15N)
FINGDentineN_matched_all<-c(Dentine_FING_matched_all$d15N)
nitrogenFING1_matched<-na.omit(FINGBoneN_matched_all)
nitrogenFING2_matched<-na.omit(FINGDentineN_matched_all)
BESTout_FING_collN_matched_all<- BESTmcmc(nitrogenFING1_matched, nitrogenFING2_matched, priors=NULL, parallel=FALSE)
#plot(BESTout_FING_collN_matched_all)
plotAll(BESTout_FING_collN_matched_all)


#3D plots of tissue offsets for Finglesham
library(scales)
show_col(viridis_pal()(7))
threeDplotcoloursFING<-c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF","#8FD744FF","#FDE725FF")
threeDplotcoloursFING<- threeDplotcoloursFING[as.ordered(FING_Finglesham_Kent$date_cat)]
scatterplot3d(FING_Finglesham_Kent$bone_d13C, FING_Finglesham_Kent$dentine_d13C, FING_Finglesham_Kent$enamel_d13C,
              xlab = (expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)"))),
              ylab = (expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)"))),
              zlab = (expression(paste(delta^{13},C["enamel carbonate (PDB)"], " (\u2030)"))), 
              angle=45, pch = 16, color=threeDplotcoloursFING)
legend("topleft", legend = c("B", "B-D", "B/C","C","C/D","D","E"), col=c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF","#8FD744FF","#FDE725FF"), pch = 16,inset = -0.15, xpd = TRUE, horiz = FALSE)

scatterplot3d(FING_Finglesham_Kent$DELTA13C_dent_bone, FING_Finglesham_Kent$DELTA13C_enamel_dent, FING_Finglesham_Kent$DELTA15N_dent_bone,
              xlab = (expression(paste(Delta^{13},C["dentine-bone"], " (\u2030)"))),
              ylab = (expression(paste(Delta^{13},C["enamel-dentine"], " (\u2030)"))),
              zlab = (expression(paste(Delta^{15},N["dentine-bone"], " (\u2030)"))), 
              angle=45, pch = 16, color=threeDplotcoloursFING)
legend("topleft", legend = c("B", "B-D", "B/C","C","C/D","D","E"), col=c("#440154FF", "#443A83FF", "#31688EFF", "#21908CFF", "#35B779FF","#8FD744FF","#FDE725FF"), pch = 16,inset = -0.15, xpd = TRUE, horiz = FALSE)



#cellWise
library(cellWise)
library(gridExtra) 

FING_DDC<-FING_Finglesham_Kent
FING_DDC <- data.frame(FING_DDC)
rownames(FING_DDC) <- FING_DDC[,1] #make ID numbers row numbers 
View(FING_DDC)

# Default options for DDC:
DDCpars = list(fracNA = 0.5, numDiscrete = 3, precScale = 1e-12,
               cleanNAfirst = "automatic", tolProb = 0.99, 
               corrlim = 0.5, combinRule = "wmean",
               returnBigXimp = FALSE, silent = FALSE,
               nLocScale = 25000, fastDDC = FALSE,
               standType = "1stepM", corrType = "gkwls",
               transFun = "wrap", nbngbrs = 100)

# A small list giving the same results:
DDCpars = list(fastDDC = FALSE)

DDC_finglesham = DDC(FING_DDC,DDCpars)

remX = DDC_finglesham$remX; dim(remX)

cellMap(D=remX, R=DDC_finglesham$stdResid, rowlabels = rownames(remX), 
        columnlabels = colnames(remX))
#now to make it prettier
ggpDDC = cellMap(D=remX, 
                 R=DDC_finglesham$stdResid,
                 indcells=DDC_finglesham$indcells,
                 indrows=DDC_finglesham$indrows,
                 rowlabels=rownames(remX),
                 columnlabels=colnames(remX),                 
                 mTitle="DetectDeviatingCells",
                 rowtitle = "Grave Number",
                 columntitle = "Variable",
                 sizetitles = 2.0,
                 autolabel=T)
plot(ggpDDC) 
# Red cells have higher value than predicted, blue cells lower,
# white cells are missing values, and all other cells are yellow.
#have to chang column and row names to make it look ok
rowlabels=c("6","8","15","18","21A","21B", "26A","30","47B","48","57", "61","62B","63","64", "72", "73","82","84","105",  
            "113", "116", "121", "123","124","125A","129A", "129B", "135", "138","144", "145A", "150", "158", "165","175",
            "180", "193", "199","208")
columnlabels=c("height","number of grave goods", "grave orientation", "bone d13C", "bone d15N","dentine d13C","dentine d15N","enamel d13C","enamel d18O (Chenery)",
               "D13C dent-bone","D15N dent-bone","D13C enamel-dent","D18O dw-MAP")

ggpDDC2 = cellMap(D=remX, 
                 R=DDC_finglesham$stdResid,
                 indcells=DDC_finglesham$indcells,
                 indrows=DDC_finglesham$indrows,
                 rowlabels=rowlabels,
                 columnlabels=columnlabels,                 
                 mTitle="DetectDeviatingCells",
                 rowtitle = "Grave Number",
                 columntitle = "Variable",
                 sizetitles = 2.0,
                 autolabel=T)
plot(ggpDDC2) 



#For all of Kent
library(readxl)
Kent_DDC_trial <- read_excel("Dropbox/PhD_Cantab/Kent_DDC_trial.xlsx")
View(Kent_DDC_trial)
KENT_DDC<-Kent_DDC_trial
KENT_DDC <- data.frame(KENT_DDC)
rownames(KENT_DDC) <- KENT_DDC[,1] #make ID numbers row numbers 
View(KENT_DDC)

# Default options for DDC:
DDCpars = list(fracNA = 0.5, numDiscrete = 3, precScale = 1e-12,
               cleanNAfirst = "automatic", tolProb = 0.99, 
               corrlim = 0.5, combinRule = "wmean",
               returnBigXimp = FALSE, silent = FALSE,
               nLocScale = 25000, fastDDC = FALSE,
               standType = "1stepM", corrType = "gkwls",
               transFun = "wrap", nbngbrs = 100)

# A small list giving the same results:
DDCpars = list(fastDDC = FALSE)

DDC_kent = DDC(KENT_DDC,DDCpars)

remX = DDC_kent$remX; dim(remX)

cellMap(D=remX, R=DDC_kent$stdResid, rowlabels = rownames(remX), 
        columnlabels = colnames(remX))
#now to make it prettier
kent_ggpDDC = cellMap(D=remX, 
                 R=DDC_kent$stdResid,
                 indcells=DDC_kent$indcells,
                 indrows=DDC_kent$indrows,
                 rowlabels=rownames(remX),
                 columnlabels=colnames(remX),                 
                 mTitle="DetectDeviatingCells",
                 rowtitle = "ID",
                 columntitle = "Variable",
                 sizetitles = 2.0,
                 autolabel=T)
plot(kent_ggpDDC) 
# Red cells have higher value than predicted, blue cells lower,
# white cells are missing values, and all other cells are yellow.
#have to chang column and row names to make it look ok
columnlabels_kent=c("no. grave goods","grave orientation", "bone d13C", "bone d15N","dentine d13C","dentine d15N","enamel d13C","enamel d18O (Chenery)",
               "D13C dent-bone","D15N dent-bone","D13C enamel-dent","D18O dw-MAP", "Dist. from Church (km)")

kent_ggpDDC2 = cellMap(D=remX, 
                  R=DDC_kent$stdResid,
                  indcells=DDC_kent$indcells,
                  indrows=DDC_kent$indrows,
                  rowlabels=rownames(remX),
                  columnlabels=columnlabels_kent,                 
                  mTitle="DetectDeviatingCells",
                  rowtitle = "ID",
                  columntitle = "Variable",
                  sizetitles = 2.0,
                  autolabel=T)
plot(kent_ggpDDC2) 

#Kent just diet and cultural variables
kent_diet_DDC<-KENT_DDC
kent_diet_DDC<-kent_diet_DDC[c(1:18,20:22,24)]
View(kent_diet_DDC)

kent_diet_DDC = DDC(kent_diet_DDC,DDCpars)

remX = kent_diet_DDC$remX; dim(remX)

cellMap(D=remX, R=kent_diet_DDC$stdResid, rowlabels = rownames(remX), 
        columnlabels = colnames(remX))

columnlabels_kent_diet=c("stature (cm)", "no. grave goods","grave orientation", "bone d13C", "bone d15N","dentine d13C","dentine d15N","enamel d13C",
                    "D13C dent-bone","D15N dent-bone","D13C enamel-dent", "Dist. from Church (km)")

kent_ggpDDC2_diet = cellMap(D=remX, 
                       R=kent_diet_DDC$stdResid,
                       indcells=kent_diet_DDC$indcells,
                       indrows=kent_diet_DDC$indrows,
                       rowlabels=rownames(remX),
                       columnlabels=columnlabels_kent_diet,                 
                       mTitle="DetectDeviatingCells",
                       rowtitle = "ID",
                       columntitle = "Variable",
                       sizetitles = 2.0,
                       autolabel=T)
plot(kent_ggpDDC2_diet) 

#Kent just mobility and cultural variables

kent_mobility_DDC<-KENT_DDC
View(kent_mobility_DDC)
kent_mobility_DDC<-kent_mobility_DDC[c(1:13,19,23,24)]
View(kent_mobility_DDC)

kent_mobility_DDC = DDC(kent_mobility_DDC,DDCpars)

remX = kent_mobility_DDC$remX; dim(remX)

cellMap(D=remX, R=kent_mobility_DDC$stdResid, rowlabels = rownames(remX), 
        columnlabels = colnames(remX))

columnlabels_kent_mobility=c("stature (cm)", "no. grave goods","grave orientation", "enamel d18O (Chenery)", "D18O dw-MAP","Dist. from Church (km)")

kent_ggpDDC2_mobility = cellMap(D=remX, 
                            R=kent_mobility_DDC$stdResid,
                            indcells=kent_mobility_DDC$indcells,
                            indrows=kent_mobility_DDC$indrows,
                            rowlabels=rownames(remX),
                            columnlabels=columnlabels_kent_mobility,                 
                            mTitle="DetectDeviatingCells",
                            rowtitle = "ID",
                            columntitle = "Variable",
                            sizetitles = 2.0,
                            autolabel=T)
plot(kent_ggpDDC2_mobility) 


#Regional differences in sex 
#big delta18O
library(dplyr)
kentEsuss_carb$SimpleSex<-kentEsuss_carb$Sex
kentEsuss_carb <- kentEsuss_carb %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F?', 'F'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M?', 'M'))
kentEsuss_carb <- kentEsuss_carb %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))

ggplot(data=kentEsuss_carb, aes(x=`Date Category`, y=`Δ18Odw-MAP Chenery`, fill=SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE)+
  #geom_boxplot(width= 0.1, colour="white")+
  #geom_jitter(position = position_jitter(width = 0.001, height = 0.001))+
  scale_fill_jco()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Date Category")))+
  labs(fill="Osteological Sex")+
  scale_y_continuous(limits=c(-5,4),breaks=seq(-5,4,1))+
  theme(legend.position = "top", legend.direction = "horizontal",axis.title = element_text(size = 20), axis.text = element_text(size=18))

#diet
kentEsuss_bone$SimpleSex<-kentEsuss_bone$Sex
kentEsuss_bone <- kentEsuss_bone %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F?', 'F'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M?', 'M'))
kentEsuss_bone <- kentEsuss_bone %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))


kent_bone_scatter_sex<- ggplot(kentEsuss_bone,aes(d13C,d15N, color=SimpleSex))+
  theme_bw()+
  geom_point(size=4,shape=16, show.legend = TRUE)+
  scale_colour_jco(name="Osteological Sex")+
  xlab(expression(paste(delta^{13},C["bone collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["bone collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
kent_bone_scatter_sex

kent_diet_sex_bag<- ggplot(kentEsuss_bone, aes(d13C, d15N, colour = SimpleSex, fill = SimpleSex)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
kent_diet_sex_bag #doesn't work says logical subscript was too long?? 

kentEsuss_dent$SimpleSex<-kentEsuss_dent$Sex
kentEsuss_dent <- kentEsuss_dent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F?', 'F'))
kentEsuss_dent <- kentEsuss_dent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M?', 'M'))
kentEsuss_dent <- kentEsuss_dent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))


kent_dentine_scatter_sex<- ggplot(kentEsuss_dent,aes(d13C,d15N, color=SimpleSex))+
  theme_bw()+
  geom_point(size=4,shape=16, show.legend = TRUE)+
  scale_colour_jco(name="Osteological Sex")+
  xlab(expression(paste(delta^{13},C["dentine collagen (PDB)"], " (\u2030)")))+ylab(expression(paste(delta^{15},N["dentine collagen (AIR)"], " (\u2030)")))+
  scale_x_continuous(limits=c(-23,-18),breaks=seq(-23,-18,1))+scale_y_continuous(limits=c(3,14.5),breaks=seq(3,14.5,1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=18), legend.position = "bottom",legend.direction = "horizontal")
kent_dentine_scatter_sex

kent_diet_sex_bag_dent<- ggplot(kentEsuss_dent, aes(d13C, d15N, colour = SimpleSex, fill = SimpleSex)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco()+
  scale_colour_jco()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
kent_diet_sex_bag_dent 


ggplot(data=kentEsuss_carb, aes(x=`SimpleSex`, y=`Δ18Odw-MAP Chenery`, fill=SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = FALSE)+
  geom_boxplot(width= 0.1, colour="white")+
  geom_jitter(position = position_jitter(width = 0.001, height = 0.001))+
  scale_fill_jco()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Osteological Sex")))+
  scale_y_continuous(limits=c(-5,4),breaks=seq(-5,4,1))+
  theme(legend.position = "none", axis.title = element_text(size = 20), axis.text = element_text(size=18))


#Finglesham
FING_Finglesham_Kent$SimpleSex<-FING_Finglesham_Kent$age_cat
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F4', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F4/5', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F5', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F5/6', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'F6', 'F'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M4', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M4/5', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M5', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'M6', 'M'))
FING_Finglesham_Kent <- FING_Finglesham_Kent %>% mutate(SimpleSex = replace(SimpleSex, SimpleSex == 'NA', 'U'))


ggplot(data=FING_Finglesham_Kent, aes(x=`SimpleSex`, y=DELTA18O_dwMAP, fill=SimpleSex))+
  theme_bw()+
  geom_violin(trim=FALSE, show.legend = FALSE)+
  geom_boxplot(width= 0.1, colour="white")+
  geom_jitter(position = position_jitter(width = 0.001, height = 0.001))+
  scale_fill_jco()+
  ylab(expression(paste(Delta^{18},O["dw-MAP (Chenery)"], " (\u2030)")))+xlab(expression(paste("Osteological Sex")))+
  scale_y_continuous(limits=c(-4,3),breaks=seq(-4,3,1))+
  theme(legend.position = "none",axis.title = element_text(size = 20), axis.text = element_text(size=18))

library(BEST)
FING_Female<-subset(FING_Finglesham_Kent, SimpleSex=="F")
FING_Male<-subset(FING_Finglesham_Kent, SimpleSex=="M")
FING_sex1 <- c(FING_Female$DELTA18O_dwMAP)
FING_sex2 <- c(FING_Male$DELTA18O_dwMAP)
FING_sex1<-na.omit(FING_sex1)
FING_sex2<-na.omit(FING_sex2)
BESTout_FING_sex_mobility <- BESTmcmc(FING_sex1, FING_sex2, priors=NULL, parallel=FALSE)
#plot(BESTout_FING_sex_mobility)
plotAll(BESTout_FING_sex_mobility)

FING_Bone_sex_Bag<- ggplot(FING_Finglesham_Kent, aes(bone_d13C, bone_d15N, colour = SimpleSex, fill = SimpleSex)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco(name="Osteological Sex")+
  scale_colour_jco(name="Osteological Sex")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_Bone_sex_Bag

FING_dent_sex_Bag<- ggplot(FING_Finglesham_Kent, aes(dentine_d13C, dentine_d15N, colour = SimpleSex, fill = SimpleSex)) +
  theme_bw()+
  geom_bag()+
  ylab(expression(paste(delta^{15},"N (\u2030)")))+xlab(expression(paste(delta^{13},"C (\u2030)")))+
  scale_fill_jco(name="Osteological Sex")+
  scale_colour_jco(name="Osteological Sex")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_dent_sex_Bag

FING_carboffset_dent_bag<-ggplot(FING_Finglesham_Kent, aes(DELTA13C_enamel_dent, dentine_d15N)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(delta^{15},N["dentine coll"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_carboffset_dent_bag

FING_carboffset_dent_bagC<-ggplot(FING_Finglesham_Kent, aes(DELTA13C_enamel_dent, enamel_d13C)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(delta^{13},C["carb"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_carboffset_dent_bagC

FING_change_overlife_bag<-ggplot(FING_Finglesham_Kent, aes(DELTA13C_dent_bone, DELTA15N_dent_bone)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(Delta^{15},N["dentine-bone"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["dentine-bone"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_change_overlife_bag

FING_carbonD_carbdent_bag<-ggplot(FING_Finglesham_Kent, aes(dentine_d13C, DELTA13C_enamel_dent)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+xlab(expression(paste(delta^{13},C["dentine coll (PDB)"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
FING_carbonD_carbdent_bag

#bag plots round 2
Kent_carboffset_dent_bag<-ggplot(Kent_DDC_trial, aes(DELTA13C_enamel_dent, dentine_d15N)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(delta^{15},N["dentine coll"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Kent_carboffset_dent_bag
  
Kent_carboffset_dent_bagC<-ggplot(Kent_DDC_trial, aes(DELTA13C_enamel_dent, enamel_d13C)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(delta^{13},C["carb"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Kent_carboffset_dent_bagC

Kent_change_overlife_bag<-ggplot(Kent_DDC_trial, aes(DELTA13C_dent_bone, DELTA15N_dent_bone)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(Delta^{15},N["dentine-bone"]," (\u2030)")))+xlab(expression(paste(Delta^{13},C["dentine-bone"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Kent_change_overlife_bag

Kent_carbonD_carbone_bag<-ggplot(Kent_DDC_trial, aes(bone_d13C, DELTA13C_enamel_dent)) +
  theme_bw()+
  geom_bag(color='grey10', fill='grey10')+
  ylab(expression(paste(Delta^{13},C["carbonate-dentine"]," (\u2030)")))+xlab(expression(paste(delta^{13},C["bone coll (PDB)"]," (\u2030)")))+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22),legend.title =element_text(size=24), legend.text =element_text(size=22))
Kent_carbonD_carbone_bag

ggplot(data = Kent_DDC_trial, aes(x = Site, y = DELTA13C_enamel_dent)) + 
  theme_bw()+
  geom_violin(trim=FALSE)+
  geom_boxplot(width= 0.1, position=position_dodge(1), colour="grey50")+
  geom_jitter(position = position_jitter(width = 0.01, height = 0.01))+
  ylab(expression(paste(Delta^{13},C["carbonate-dentine"], " (\u2030)")))+xlab(expression(paste("Site")))+
  #scale_y_continuous(limits=c(-16,-11),breaks=seq(-16,-11,1))+
  theme(legend.position="none", axis.title = element_text(size = 20), axis.text = element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))

  
library(BEST)
Kent_Female<-subset(kentEsuss_carb, SimpleSex=="F")
Kent_Male<-subset(kentEsuss_carb, SimpleSex=="M")
KENT_sex1 <- c(Kent_Female$`Δ18Odw-MAP Chenery`)
KENT_sex2 <- c(Kent_Male$`Δ18Odw-MAP Chenery`)
KENT_sex1<-na.omit(KENT_sex1)
KENT_sex2<-na.omit(KENT_sex2)
BESTout_KENT_sex_mobility <- BESTmcmc(KENT_sex1, KENT_sex2, priors=NULL, parallel=FALSE)
#plot(BESTout_KENT_sex_mobility)
plotAll(BESTout_KENT_sex_mobility)



