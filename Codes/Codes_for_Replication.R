
# Codes for replication: A meta-analytical review of intervention experiments to reduce food waste

# Author: Ziqian Xia

#Pacakages to be loaded

library(MAc)
library(tidyverse)
library(effectsize)
library(metafor)
library(meta)
library(metaviz)
library(texreg)
library(stargazer)
library(broom)
library(viridis)
library(RColorBrewer)
library(forcats)
# calculating r coefficient for regression results using sample_size

#---------------------Data Extraction: Effect Size Calculation------------------



#variance of Zs, Z's vi is not affected by other value but N                                                                                                                                          
#vi   = 1/(N - 3) 
#Extract from Reg
#regrc <- codesheet%>%
#  filter(type=='reg',test=="t")%>%
#  mutate(stat=stat*directing)%>%
#  mutate(r_c = ((stat)^2/((stat)^2 + N))^(1/2))%>%
#  mutate(z=r_to_z(regrc$r_c))%>%
#  mutate(vi   = 1/(N - 3) )%>%
#  select(-r_c)

#r_to_z(regrc$r_c)

# Reg finished

#Extract from ANOVA
#ftype <- codesheet%>%
#  filter(type=='aov',test=="f")%>%
#  select(stat,nt,nc)%>%
#  mutate(d=fd$d,var.d=fd$var_d)




#fd<-as.data.frame(f_to_d(f=ftype$stat,n.1=ftype$nt,n.2=ftype$nc))

#fdr<-as.data.frame(r_from_d1(ftype$d, ftype$nt, ftype$nc, ftype$var.d))

#r_to_z(fdr$r)

#ftype2 <- codesheet%>%
#  filter(type=='aov',test=="f")%>%
#  mutate(z=r_to_z(fdr$r),vi   = 1/(N - 3) )


#Extract from SMD
#smdtype <- codesheet%>%
#  filter(is.na(codesheet$type))%>%
#  select(d,vd, nt, nc,directing)%>%
#  mutate(d2=directing*d)

#smdr<-as.data.frame(r_from_d1(smdtype$d2, smdtype$nt, smdtype$nc, smdtype$vd))
#r_to_z(smdr$r)

#smdtypez <- codesheet%>%
#  filter(is.na(codesheet$type))%>%
#  mutate(z=r_to_z(smdr$r))%>%
#  mutate(vi   = 1/(N - 3))


#full<-bind_rows(ftype2,regrc,smdtypez)


#cleaned<-full_z_value%>%
#  mutate(vi   = 1/(N - 3))


# z-score data are saved in cleaned file, 
#and it is further examined by review team

#Typo Correction
cleaned$Type_1[cleaned$Type_1=='Environmental Alternation']<-'Environmental alteration'

#-------------Formal Analysis-------------------
#Data is pre-loaded in the environment.



m1<-rma(yi=z, vi = vi, method="DL", data = cleaned,test = "knha",slab = cleaned$citation)
rma(yi=z, vi = vi, method="REML", data = cleaned,  test = "knha")
rma(yi=z, vi = vi, method='HS', data = cleaned)
rma(yi=z, vi = vi, method='SJ', data = cleaned)
rma(yi=z, vi = vi, method='EB', data = cleaned)


# A Quick visualization of results

fullplot<-viz_forest(m1,                              # rma object from the previous step # how the summary effect is called
                     group = cleaned$Type_1, # the short reference is used for study labels
                     method = "DL",  
                     study_labels = cleaned$citation,# choose estimation method, REML recommended
                     xlab = "Z Coefficient", # name x-axis               # choose visualization: rainforest, thickforest...
                     annotate_CI = TRUE,
                     type='cumulative',
                     variant = "rain",
                     # argument, if 95%CIs should be displayed
                     table_headers = "Z-Score [95% CI]")

### carry out publication bias analysis
regtest(m1)

### draw funnel plot with missing studies filled in
taf <- trimfill(m1)

# No study is filled.

funnel(taf, legend=TRUE)
funnel(m1, legend=TRUE)
ggsave('funnel.png',dpi=600,width = 6,height = 5)


#---------Generate Authors+Year---------------------
cleaned<-cleaned%>%
  mutate(citation=paste(Authors,Year))


#-------------------CUM----------------
cleaned<-cleaned%>%
  mutate(citation=paste(Authors,Year))

m.gen <- metagen(TE = z,
                 seTE = (vi^0.5),
                 studlab = citation,
                 data = cleaned,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Food Waste Intervention")

forest(metacum(m.gen, pooled = "random",sortvar = Year))
dev.off()

#--------Subgroup----------------------


rma(z, vi, subset=(Type_1=="Education"), data=cleaned,method = 'DL')
rma(z, vi, subset=(Type_1=="Tips"), data=cleaned,method = 'DL')
rma(z, vi, subset=(Type_1=="Consequence"), data=cleaned,method = 'DL')
rma(z, vi, subset=(Type_1=="Environmental alteration"), data=cleaned,method = 'DL')
rma(z, vi, subset=(Type_1=="Feedback"), data=cleaned,method = 'DL')
rma(z, vi, subset=(Type_1=="Incentive"), data=cleaned,method = 'DL')

#----------------Subgroup Analysis----------------------
#Note 'Prompt','Feedback','Consequence' are merged as information intervention

rma(z, vi, mods = ~Duration_intervention_days+as.factor(exper)+as.factor(measure)
              +as.factor(region)+
      as.factor(scenario)+
      Year, data=cleaned,method = 'REML')


summary(rma(z, vi,subset=(Type_1=="Prompt"|Type_1=='Feedback'|Type_1=="Consequence"|Type_1=="Tips"), mods = ~Duration_intervention_days+as.factor(exper)+as.factor(measure)
              +as.factor(region)+as.factor(scenario)+Year, data=cleaned,method = 'REML'))

rma(z, vi,subset=(Type_1=="Education"), mods = ~Duration_intervention_days+as.factor(exper)+as.factor(measure)
           +as.factor(region)+as.factor(scenario)+Year, data=cleaned,method = 'REML')

#-------------Description Analysis----------
cleaned%>%
  group_by(Type_1)%>%
  count()%>%
  mutate((n/58)*100)

uniquemeta%>%
  group_by(region)%>%
  count()%>%
  mutate(n/33)

cleaned%>%
  group_by(region)%>%
  count()%>%
  mutate((n/58)*100)

cleaned%>%
  group_by(measure)%>%
  count()%>%
  mutate((n/58)*100)


cleaned%>%
  group_by(exper)%>%
  count()%>%
  mutate((n/58)*100)

# Fig 4

factor(subgroup$type,levels = c('Subgroup Effect','Main Effect'))

#Typo correction
subgroup$type[subgroup$type=='Environmental Alternation']<-'Environmental alteration'

subgroup%>%
  mutate(type = fct_reorder(type, yi)) %>%
  ggplot( aes(y=type, x=yi, xmin=lowerci, xmax=upperci, color = effect))+
  #Add data points and color them black
  #Add 'special' points for the summary estimates, by making them diamond shaped
  geom_point(shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-0.1,0.75), name='Effect Size (Z-Score)')+
  scale_color_brewer(palette ='Dark2')+
  #Give y-axis a meaningful label
  ylab('Intervention Types')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, linetype='dashed')+
  facet_grid(effect~., scales= 'free', space='free')+
  theme_light()+
  theme(strip.text.y = element_text(angle = 0),legend.position='none',
        axis.title.y = element_blank(),text = element_text(size=13))

ggsave('Subgroup-size.png',dpi = 600, width = 8, height = 6)


#-----------------Additional Plots---------------
library(RColorBrewer)
cleaned%>%
  select(N,Type_1)%>%
  mutate(size=case_when(N<150~"N<150",
                        150<=N & N<300~"150≤N<300",
                        300<=N & N<500~"300≤N<500",
                        N>=500~"N≥500"))%>%
  mutate(typefac=factor(cleaned$Type_1,levels = c("Prompt","Education","Tips",
                                                  "Consequence","Feedback",
                                                  "Incentive","Environmental alteration")))%>%
  ggplot(aes(fill=size, y=N, x=typefac)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_brewer(palette = "Set3")+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank())+
  labs(fill='Sample Size',y='Percentage')

ggsave('descriptive-reordered.png',dpi = 600, width = 5, height = 6)