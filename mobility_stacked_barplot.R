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
carbcleanplus<- OSr_teeth
class(as.data.frame(carbcleanplus))
head(carbcleanplus)
carbcleanplus <- data.frame(carbcleanplus)
rownames(carbcleanplus) <-carbcleanplus[,1]
carbcleanplus <- carbcleanplus[,-1]
head(carbcleanplus)
View(carbcleanplus)
carbcleanplus<-carbcleanplus[c(2,3,39,40,44)]
head(carbcleanplus)
carbcleanplus <- na.omit(carbcleanplus)
head(carbcleanplus)




dfr <- carbcleanplus %>%             
  mutate(Country = as.factor(Country)     # categorical values to factor
         , `hclust group` = as.ordered(`hclust group`))# character to ordered factor (like a grade)


#fct_reorder(carbcleanplus$Country, carbcleanplus$`Country Code`)

#dfr <- na.omit(dfr)
dfr_prop <- dfr %>% 
  count(Country, `hclust group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfr_prop)           

dfr_prop2 <- dfr %>% 
  count(`hclust group`, Country) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfr_prop2)  

ggplot() + geom_bar(aes(y = dfr_prop$prop, x = dfr_prop$Country, fill = dfr_prop$`hclust group`), data = dfr_prop,
                    stat="identity")+theme_bw()

ggplot(dfr_prop, aes(dfr_prop$`hclust group`,dfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = Country, weight=`hclust group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Country")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

#ggplot(dfr_prop, aes(Country,dfr_prop$prop,)) +
  #geom_bar(colour = "black", aes(fill = dfr_prop$`hclust group`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  #scale_fill_viridis(discrete = TRUE, name = "Cluster")+
  #xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  #theme_bw()

ggplot(dfr_prop, aes(Country,dfr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = dfr_prop$`hclust group`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  theme_bw()


dfr_prop$Country = factor(dfr_prop$Country,
                                  levels=c("Croatia", "Spain", "Italy", "Germany", "England", "Ireland", "Isle of Man", "Denmark", "Scotland", "Sweden", "Estonia", "Russia", "Norway"),ordered=TRUE)


ggplot(dfr_prop2, aes(dfr_prop2$Country, dfr_prop2$prop)) +
  geom_bar(colour = "black", aes(fill = `hclust group`, outline.colour = "black"), position = "fill", stat="identity") +
  theme_bw()

ggplot(data = dfr_prop, aes(factor(`hclust group`))) + 
  geom_bar(aes(group = Country, weights=prop, fill=Country))+
  position_fill()


dfr_perc <- dfr %>% 
  count(Country, `hclust group`) %>% 
  mutate(perc = prop.table(n)*100) %>%      # mutate count(n) into perc
  select(-n) %>%                            # remove the count...
  spread(`hclust group`, perc)                        # to spread perc by subgroup
as.data.frame(dfr_perc)

ggplot(dfr_prop2, aes(dfr_prop2$Country, dfr_prop2$prop)) +
  geom_bar(colour = "black", aes(fill = `hclust group`, outline.colour = "black"), position = "stack", stat="identity") +
  theme_bw()



#now to do by EuRegion
dfr2 <- carbcleanplus %>%             
  mutate(EuRegion = as.factor(EuRegion)     # categorical values to factor
         , `hclust group` = as.ordered(`hclust.group`))# character to ordered factor (like a grade)

#dfr <- na.omit(dfr)
dfr2_prop <- dfr2 %>% 
  count(EuRegion, `hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfr2_prop)           

ggplot() + geom_bar(aes(y = dfr2_prop$prop, x = dfr2_prop$EuRegion, fill = dfr2_prop$`hclust.group`), data = dfr2_prop,
                    stat="identity")+theme_bw()

ggplot(dfr2_prop, aes(dfr2_prop$`hclust.group`,dfr2_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = EuRegion, weight=`hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(dfr2_prop, aes(EuRegion,dfr2_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = dfr2_prop$`hclust.group`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(dfr2_prop, aes(EuRegion,dfr2_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = dfr2_prop$`hclust.group`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme_bw()


dfr_prop$Country = factor(dfr_prop$Country,
                          levels=c("Croatia", "Spain", "Italy", "Germany", "England", "Ireland", "Isle of Man", "Denmark", "Scotland", "Sweden", "Estonia", "Russia", "Norway"),ordered=TRUE)


ggplot(dfr_prop2, aes(dfr_prop2$Country, dfr_prop2$prop)) +
  geom_bar(colour = "black", aes(fill = `hclust group`, outline.colour = "black"), position = "fill", stat="identity") +
  theme_bw()

ggplot(data = dfr_prop, aes(factor(`hclust group`))) + 
  geom_bar(aes(group = Country, weights=prop, fill=Country))+
  position_fill()


dfr_perc <- dfr %>% 
  count(Country, `hclust group`) %>% 
  mutate(perc = prop.table(n)*100) %>%      # mutate count(n) into perc
  select(-n) %>%                            # remove the count...
  spread(`hclust group`, perc)                        # to spread perc by subgroup
as.data.frame(dfr_perc)

#ggplot(dfr_prop2, aes(dfr_prop2$Country, dfr_prop2$prop)) +
  #geom_bar(colour = "black", aes(fill = `hclust group`, outline.colour = "black"), position = "stack", stat="identity") +
  #theme_bw()

#now for EuRegion2
dfr3 <- carbcleanplus %>%             
  mutate(EuRegion2 = as.factor(EuRegion2)     # categorical values to factor
         , `hclust group` = as.ordered(`hclust.group`))# character to ordered factor (like a grade)

#dfr <- na.omit(dfr)
dfr3_prop <- dfr3 %>% 
  count(EuRegion2, `hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfr3_prop)           

ggplot(dfr3_prop, aes(dfr3_prop$`hclust.group`,dfr3_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = EuRegion2, weight=`hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))


ggplot(dfr3_prop, aes(EuRegion2,dfr3_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = dfr3_prop$`hclust.group`, weight=EuRegion2, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))


#stacked bar plot for O and Sr
OSrenamelcleanplus<- OSr_teeth
class(as.data.frame(OSrenamelcleanplus))
head(OSrenamelcleanplus)
OSrenamelcleanplus <- data.frame(OSrenamelcleanplus)
rownames(OSrenamelcleanplus) <-OSrenamelcleanplus[,1]
OSrenamelcleanplus <- OSrenamelcleanplus[,-1]
head(OSrenamelcleanplus)
View(OSrenamelcleanplus)
OSrenamelcleanplus<-OSrenamelcleanplus[c(2,3,40,43,44)] #country, Country code, Europe region, cluster group, EuRegion2
head(OSrenamelcleanplus)
OSrenamelcleanplus <- na.omit(OSrenamelcleanplus)
head(OSrenamelcleanplus)

dfrOSr <- OSrenamelcleanplus %>%             
  mutate(EuRegion = as.factor(EuRegion), `OSr_hclust.group` = as.ordered(`OSr_hclust.group`))

dfrOSr_prop <- dfrOSr %>% 
  count(EuRegion, `OSr_hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfrOSr_prop)   

dfrOSr2 <- OSrenamelcleanplus %>%             
  mutate(Country = as.factor(Country), `OSr_hclust.group` = as.ordered(`OSr_hclust.group`))

dfrOSr2_prop <- dfrOSr2  %>% 
  count(Country, `OSr_hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfrOSr2_prop)  


ggplot(dfrOSr_prop, aes(dfrOSr_prop$`OSr_hclust.group`,dfrOSr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = EuRegion, weight=`OSr_hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(dfrOSr_prop, aes(EuRegion,dfrOSr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = dfrOSr_prop$`OSr_hclust.group`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(dfrOSr2_prop, aes(dfrOSr2_prop$`OSr_hclust.group`,dfrOSr2_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = Country, weight=`OSr_hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Country")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()
dfrOSr2_prop$Country = factor(dfrOSr2_prop$Country,
                          levels=c("Croatia", "Spain", "Italy", "France", "Germany", "Netherlands", "England", "Wales","Ireland", "Isle of Man", "Denmark", "Scotland", "Orkney", "Sweden", "Estonia", "Russia", "Norway"),ordered=TRUE)


ggplot(dfrOSr2_prop, aes(Country,dfrOSr2_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = dfrOSr2_prop$`OSr_hclust.group`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  theme_bw()


#now for EuRegion2
dfrOSr3 <- OSrenamelcleanplus %>%             
  mutate(EuRegion2 = as.factor(EuRegion2)     # categorical values to factor
         , `OSr_hclust.group` = as.ordered(`OSr_hclust.group`))# character to ordered factor (like a grade)

#dfr <- na.omit(dfr)
dfrOSr3_prop <- dfrOSr3 %>% 
  count(EuRegion2, `OSr_hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfrOSr3_prop)           

ggplot(dfrOSr3_prop, aes(dfrOSr3_prop$`OSr_hclust.group`,dfrOSr3_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = EuRegion2, weight=`OSr_hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))
ggplot(dfrOSr3_prop, aes(EuRegion2,dfrOSr3_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = dfrOSr3_prop$`OSr_hclust.group`, weight=EuRegion2, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))




#For carbon, oxygen and Strontium
OCSrenamelcleanplus<- OSr_teeth
class(as.data.frame(OCSrenamelcleanplus))
head(OCSrenamelcleanplus)
OCSrenamelcleanplus <- data.frame(OCSrenamelcleanplus)
rownames(OCSrenamelcleanplus) <-OCSrenamelcleanplus[,1]
OCSrenamelcleanplus <- OCSrenamelcleanplus[,-1]
head(OCSrenamelcleanplus)
View(OCSrenamelcleanplus)
OCSrenamelcleanplus<-OCSrenamelcleanplus[c(2,3,40,42,44)] #country, Country code, Europe region, cluster group
head(OCSrenamelcleanplus)
OCSrenamelcleanplus <- na.omit(OCSrenamelcleanplus)
head(OCSrenamelcleanplus)

OCSrenamelcleanplus$OCSr_hclust.group<-as.character(OCSrenamelcleanplus$OCSr_hclust.group)
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '1.1000000000000001', '1.1'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '1.1', '1'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '1.2', '1'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.2.1', '2.2'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.2.2', '2.2'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '3.2.1', '3.2'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '3.2.2', '3.2'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.2', '2'))
OCSrenamelcleanplus <- OCSrenamelcleanplus %>% mutate(OCSr_hclust.group = replace(OCSr_hclust.group, OCSr_hclust.group == '2.1', '2'))


dfrOCSr <- OCSrenamelcleanplus %>%             
  mutate(EuRegion = as.factor(EuRegion), `OCSr_hclust.group` = as.ordered(`OCSr_hclust.group`))

dfrOCSr_prop <- dfrOCSr %>% 
  count(EuRegion, `OCSr_hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfrOCSr_prop)   

dfrOCSr2 <- OCSrenamelcleanplus %>%             
  mutate(Country = as.factor(Country), `OCSr_hclust.group` = as.ordered(`OCSr_hclust.group`))

dfrOCSr2_prop <- dfrOCSr2  %>% 
  count(Country, `OCSr_hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfrOCSr2_prop)  


ggplot(dfrOCSr_prop, aes(dfrOCSr_prop$`OCSr_hclust.group`,dfrOCSr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = EuRegion, weight=`OCSr_hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()

ggplot(dfrOCSr_prop, aes(EuRegion,dfrOCSr_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = dfrOCSr_prop$`OCSr_hclust.group`, weight=EuRegion, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme_bw()

dfrOCSr2_prop$Country = factor(dfrOCSr2_prop$Country,
                              levels=c("Croatia", "Spain", "Italy", "France", "Germany", "Netherlands", "England", "Wales","Ireland", "Isle of Man", "Denmark", "Scotland", "Orkney", "Sweden", "Estonia", "Russia", "Norway"),ordered=TRUE)

ggplot(dfrOCSr2_prop, aes(dfrOCSr2_prop$`OCSr_hclust.group`,dfrOCSr2_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = Country, weight=`OCSr_hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Country")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme_bw()
ggplot(dfrOCSr2_prop, aes(Country,dfrOCSr2_prop$prop,)) +
  geom_bar(colour = "black", aes(fill = dfrOCSr2_prop$`OCSr_hclust.group`, weight=Country, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Country")))+ylab(expression(paste("Proportion")))+
  theme_bw()

#now for EuRegion2
dfrOCSr3 <- OCSrenamelcleanplus %>%             
  mutate(EuRegion2 = as.factor(EuRegion2)     # categorical values to factor
         , `OCSr_hclust.group` = as.ordered(`OCSr_hclust.group`))# character to ordered factor (like a grade)

#dfr <- na.omit(dfr)
dfrOCSr3_prop <- dfrOCSr3 %>% 
  count(EuRegion2, `OCSr_hclust.group`) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))    # prop = n/sum(n) works too
as.data.frame(dfrOCSr3_prop)           

ggplot(dfrOCSr3_prop, aes(dfrOCSr3_prop$`OCSr_hclust.group`,dfrOCSr3_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = EuRegion2, weight=`OCSr_hclust.group`, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_viridis(discrete = TRUE, name = "Region")+
  xlab(expression(paste("Cluster")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=22),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))

ggplot(dfrOCSr3_prop, aes(EuRegion2,dfrOCSr3_prop$prop,)) +
  theme_bw()+
  geom_bar(colour = "black", aes(fill = dfrOCSr3_prop$`OCSr_hclust.group`, weight=EuRegion2, outline.colour = "black"), position = "fill", stat="identity") +
  scale_fill_manual(values=cbbPalette, name="Cluster")+
  xlab(expression(paste("Region")))+ylab(expression(paste("Proportion")))+
  theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=22), legend.text =element_text(size=18),legend.title = element_text(size = 22))



