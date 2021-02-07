if (!require("pacman")) 
  install.packages("pacman")
pacman::p_load('ggplot2', 'plyr', 'lme4', 'AICcmodavg', 'MASS')

#Data
##SLA
crop2.Leaf <- read.csv("./Data_csv/2/SLA_2.csv")
crop3.Leaf <- read.csv("./Data_csv/3/SLA_3.csv")
SLA <- rbind(crop2.Leaf, crop3.Leaf)
SLA <- SLA[(1:672),c(1:5,8)]
SLA$sample <- as.factor(SLA$sample)
SLA$spp <- factor(SLA$spp, levels= c("BA", "CO", "DW", "LE", "PB", "SO", "SF"), labels=c("Barley", "Corn", "Durum Wheat", "Lentil", "Pinto Bean", "Sorghum", "Sunflower"))

##Stomatal Conductance
crop2.Por <- read.csv("./Data_csv/2/Por_2.csv")
crop3.Por <- read.csv("./Data_csv/3/Por_3.csv")
Por.dat <- rbind(crop2.Por, crop3.Por)
Por <- Por.dat[(1:1112),c(1:5,7,14)]
Por_ <- Por[!(Por$date=='8/24/16'),]

#Quantum Photosynthetic Yield 
crop2.OS1 <- read.csv("./Data_csv/2/OS1_2_.csv")
crop3.OS1 <- read.csv("./Data_csv/3/OS1_3_.csv")
OS1.dat <- rbind(crop2.OS1, crop3.OS1)
OS1.dat <- subset(OS1.dat, yield<=1)
OS1 <- OS1.dat[(1:1679),c(1:4,6,7,24)]


#Chlorophyll Content
crop2.CCM <- read.csv("./Data_csv/2/CCM_2_.csv")
crop3.CCM <- read.csv("./Data_csv/3/CCM_3_.csv")
CCM.dat <- rbind(crop2.CCM, crop3.CCM)
CCM <- CCM.dat[(1:1677),c(1:6,11)]


#Calculations means and differences
##SLA
###Mean
sla.mean <- ddply(SLA, .(t_c, spp, block), summarize, 
                  mean=round(mean(SLA),2), 
                  se=round(sqrt(var(SLA)/length(SLA)),2))

###Percent change
sla.prop <- data.frame(subset(sla.mean, t_c=="T"), 
                       prop=((subset(sla.mean, t_c=="T")$mean/subset(sla.mean, t_c=="C")$mean)-1)*100)

##Stomatal Conductance
###Means 

por.mean <- ddply(Por, .(t_c, spp, block, pre_post),      
                  summarize,  mean=round(mean(conductance),2), 
                  se=round(sqrt(var(conductance)/length(conductance)),2))

###Percent change

por.post.change <- data.frame(subset(por.mean, t_c=="T" & pre_post=="post"), 
                            prop=((subset(por.mean, t_c=="T" & pre_post=="post")$mean/subset(por.mean, t_c=="C" & pre_post=="na")$mean)-1)*100)
por.pre.change <- data.frame(subset(por.mean, t_c=="T" & pre_post=="pre"), 
                           prop=((subset(por.mean, t_c=="T" & pre_post=="pre")$mean/subset(por.mean, t_c=="C" & pre_post=="na")$mean)-1)*100)
por.pp.change <- rbind(por.post.change,por.pre.change)

##Quantum Yield
###Means
os1.mean <- ddply(OS1, .(t_c, spp, block),      
                  summarize,  mean=round(mean(yield),2), 
                  se=round(sqrt(var(yield)/length(yield)),2))
os1.mean.pp <- ddply(OS1, .(t_c, spp, block, pre_post),      
                  summarize,  mean=round(mean(yield),2), 
                  se=round(sqrt(var(yield)/length(yield)),2))
###Proportion Change
   
os1.prop <- data.frame(subset(os1.mean, t_c=="T")[2:4],
                           prop=((subset(os1.mean, t_c=="T")$mean/subset(os1.mean, t_c=="C")$mean) - 1)*100)
   
os1.post.change <- data.frame(subset(os1.mean.pp, t_c=="T" & pre_post=="post"), 
                                prop=((subset(os1.mean.pp, t_c=="T" & pre_post=="post")$mean/subset(os1.mean.pp, t_c=="C" & pre_post=="na")$mean)-1)*100)
os1.pre.change <- data.frame(subset(os1.mean.pp, t_c=="T" & pre_post=="pre"), 
                               prop=((subset(os1.mean.pp, t_c=="T" & pre_post=="pre")$mean/subset(os1.mean.pp, t_c=="C" & pre_post=="na")$mean)-1)*100)
os1.pp.change <- rbind(os1.post.change,os1.pre.change)


##Chlorophyll Content
###Means

ccm.mean <- ddply(CCM, .(t_c, spp, block), summarize, 
                       mean=round(mean(concentration),2), 
                       se=round(sqrt(var(concentration)/length(concentration)),2))
ccm.mean.pp <- ddply(CCM, .(t_c, spp,date, pre_post),      
                  summarize,  mean=round(mean(concentration),2), 
                  se=round(sqrt(var(concentration)/length(concentration)),2))

###Proportion Change
   #----formeanwithoutpre/post
ccm.prop <- data.frame(subset(ccm.mean, t_c=="T")[2:4],
                           prop=((subset(ccm.mean, t_c=="T")$mean/subset(ccm.mean, t_c=="C")$mean) - 1)*100)
   #-----formeanwithpre/post
ccm.post.change <- data.frame(subset(ccm.mean.pp, t_c=="T" & pre_post=="post"), 
                                prop=((subset(ccm.mean.pp, t_c=="T" & pre_post=="post")$mean/subset(ccm.mean.pp, t_c=="C" & pre_post=="na")$mean)-1)*100)
ccm.pre.change <- data.frame(subset(ccm.mean.pp, t_c=="T" & pre_post=="pre"), 
                               prop=((subset(ccm.mean.pp, t_c=="T" & pre_post=="pre")$mean/subset(ccm.mean.pp, t_c=="C" & pre_post=="na")$mean)-1)*100)
ccm.pp.change <- rbind(ccm.post.change,ccm.pre.change)



#Graphs

##SLA
###Mean
ggplot(sla.mean, aes(x=t_c, y=mean, color=block)) + facet_wrap(~spp) +
  geom_point(size=2) +  geom_errorbar(aes(ymax=mean+se, ymin=mean-se), width=0.1) + 
  geom_line(aes(group=block)) + labs(title="Specific Leaf Area and Dust", y="SLA (cm^-2g^-1)")+scale_color_manual(values=c("#E69F00","#56B4E9")) + scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust"))
###Percent change
ggplot(sla.prop, aes(x=spp, y=prop, shape=block, color=spp)) + geom_point() +
  geom_path(aes(group=spp),position=position_dodge(width=0.5))  + scale_color_discrete(guide=FALSE) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + geom_hline(yintercept = 0, color="black")+
  labs(title="Dust Effect on Specific Leaf Area", x="Species", y="Change in SLA (%)")

##Stomatal Conductance 
###Mean
ggplot(por.mean, aes(x=t_c, y=mean, color=block, shape=pre_post,  label=pre_post))+facet_wrap(~spp, scales="free") + theme_bw(14) + geom_point() + 
  geom_label(data=subset(por.mean, pre_post %in% c("pre","post")),aes(fontface=2), size=3,label.size=0.5, position=position_dodge(width=0.75)) + 
  geom_line(data=subset(por.mean, pre_post %in% c("pre","na")), aes(group=block)) +
  geom_line(data=subset(por.mean, pre_post %in% c("post","na")), aes(group=block))+
  labs(title="Stomatal Activity and Dust (all)",  x="Treatment", y="Conductance (mmol m^-2 s^-1)")+scale_color_manual(values=c("#E69F00","#56B4E9"))+
  scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust"))+scale_shape_discrete(guide=FALSE)


###Percent change
ggplot(por.pp.change, aes(x=spp, y=prop, color=block, shape=pre_post))+
  geom_path(aes(group=interaction(spp,block)), arrow=arrow(angle=30, ends="first",type="open",length=unit(0.1, "inches")), position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + 
  scale_color_brewer(palette = "Set2") + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_hline(yintercept = 0, color="black") + labs(title="Dust Effect on Stomatal Activity (all)", x="Species", y="Change in Conductance (%)")

##Quantum Yield
###Mean
  ---withoutpre/post
ggplot(os1.mean, aes(x=t_c, y=mean, color=block)) + facet_wrap(~spp) + 
  theme_bw() + geom_point() + geom_line(aes(group=block)) + geom_errorbar(aes(ymax=mean+se, ymin=mean-se), width=0.1) + scale_color_manual(values=c("#E69F00","#56B4E9")) +
  labs(title="Photosynthetic Activity and Dust (all)",x="Treatment", y="Quantum Yield PS II") + scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust"))
   ---withpre/post
ggplot(os1.mean.pp, aes(x=t_c, y=mean, color=block, shape=pre_post, label=pre_post)) +
  facet_wrap(~spp, scales="free") + theme_bw(14) + geom_point() + geom_errorbar(aes(x=t_c, y=mean, ymin=mean-se, ymax=mean+se), width=0.1) +
  geom_label(data=subset(os1.mean.pp, pre_post %in% c("pre","post")),aes(fontface=2), size=3, label.size=0.5, position=position_dodge(width=0.75)) + 
  geom_line(data=subset(os1.mean.pp, pre_post %in% c("pre","na")), aes(group=block)) +
  geom_line(data=subset(os1.mean.pp, pre_post %in% c("post","na")), aes(group=block))+
  labs(x="Treatment", y="Quantum Yield PS II (all)")  + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust")) + scale_shape_discrete(guide=FALSE)

###% Change 
  ----withoutpre/post
ggplot(os1.prop, aes(x=spp, y=prop,shape=block, color=spp)) +
  geom_point(size=2) + geom_line(aes(group=spp)) + theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  geom_hline(yintercept = 0, color="black") + labs(title="Dust Effect on Photosynthetic Activity (all)", x="Species", y="Change in Quantum Yield PS II (%)") + scale_color_discrete(guide=FALSE)
   ---withpre/post
ggplot(os1.pp.change, aes(x=spp, y=prop, color=block, shape=pre_post))+
  geom_path(aes(group=interaction(spp,block)), arrow=arrow(angle=30, ends="first",type="open",length=unit(0.1, "inches")), position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + 
  scale_color_brewer(palette = "Set2") + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_hline(yintercept = 0, color="black") + labs(title="Dust Effect on Quantum Yield PS II (all)", x="Species", y="Change in Conductance (%)")
##Chlorophyll Content 
###Mean
  ---withoutpre/post
ggplot(ccm.mean, aes(x=t_c, y=mean, color=block)) +
  facet_wrap(~spp, scales="free") + theme_bw(14) + geom_point() + geom_errorbar(aes(ymax=mean+se, ymin=mean-se), width=0.1) + 
  geom_line(aes(group=block))+scale_color_manual(values=c("#E69F00","#56B4E9")) + labs(title="Chlorophyll Concentration and Dust (all)", y="Concentration (mg/m^2)") + scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust"))
   ---withpre/post
ggplot(ccm.mean.pp, aes(x=t_c, y=mean, color=block, shape=pre_post, label=pre_post)) +
  facet_wrap(~spp, scales="free") + theme_bw(14) + geom_point() + 
  geom_label(data=subset(ccm.mean.pp, pre_post %in% c("pre","post")),aes(fontface=2), size=3, label.size=0.5, position=position_dodge(width=0.75)) + 
  geom_line(data=subset(ccm.mean.pp, pre_post %in% c("pre","na")), aes(group=block)) +
  geom_line(data=subset(ccm.mean.pp, pre_post %in% c("post","na")), aes(group=block))+
  labs(x="Treatment", y="Chlorophyll Concentration (all)")  + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust")) + scale_shape_discrete(guide=FALSE)

###Proportion Change
  ---withoutpre/post
ggplot(ccm.prop, aes(x=spp, y=prop,shape=block, color=spp)) +
  geom_point(size=2) +  geom_line(aes(group=spp)) + theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  geom_hline(yintercept = 0, color="black") + labs(title="Dust Effect on Chlorophyll Concentration (all)", x="Species", y="Change in Concentration (%)") + scale_color_discrete(guide=FALSE)
   ----withoutpre/post
ggplot(ccm.prop, aes(x=spp, y=prop, color=block, shape=pre_post)) +
  geom_path(aes(group=interaction(spp,block)), arrow=arrow(angle=30, ends="first", type="open", length=unit(0.1, "inches")), position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + scale_color_brewer(palette = "Set2") + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_hline(yintercept = 0, color="black") + labs(title="Chlorophyll Concentration (all)", x="Species", y="% change")
    ----withpre/post 
ggplot(ccm.pp.change, aes(x=spp, y=prop, color=block, shape=pre_post))+
  geom_path(aes(group=interaction(spp,block)), arrow=arrow(angle=30, ends="first",type="open",length=unit(0.1, "inches")), position=position_dodge(width=0.5)) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) + 
  scale_color_brewer(palette = "Set2") + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_hline(yintercept = 0, color="black") + labs(title="Dust Effect on Chlorophyll Concentration (all)", x="Species", y="Change in Conductance (%)")




#Analysis
##Specific Leaf Area
###Models 
sla.null <- glmer(SLA~1+ (1|block), data=SLA, family=Gamma(link="identity")) 
sla.spp <- glmer(SLA~spp + (1|block), data=SLA, family=Gamma(link="identity"))
sla.treat <- glmer(SLA~t_c + (1|block), data=SLA,family=Gamma(link="identity"))
sla.add <- glmer(SLA~spp+t_c + (1|block), data=SLA,family=Gamma(link="identity"))
sla.int <- glmer(SLA~ spp+ t_c + spp:t_c + (1|block), data=SLA,family=Gamma(link="identity"))
###AIC
sla.mod.names <- c("sla.null", "sla.spp", "sla.treat", "sla.add", "sla.int")
sla.mods <- list( )

for(i in 1:length(sla.mod.names)) {
  sla.mods[[i]] <- get(sla.mod.names[i]) }
print(aictab(cand.set = sla.mods, 
             modnames = sla.mod.names)) 
###CIs - species and treatment
sla.spp <- data.frame(response="SLA", 
                      variable="spp",
                      lmer.spp.CI(x=SLA, response=SLA$SLA))
sla.treat <- data.frame(response="SLA", 
                        variable="t_c",
                        lmer.tc.CI.sla(x=SLA, response=SLA$SLA))

##Stomatal Conductance 
###Models
conduct.null <- lmer(conductance ~ 1 + (1|block), data=Por, REML=FALSE)
conduct.spp <- lmer(conductance ~ spp + (1|block), data=Por, REML=FALSE)
conduct.treat <- lmer(conductance ~ t_c + (1|block), data=Por, REML=FALSE)
conduct.add <- lmer(conductance ~ spp + t_c + (1|block), data=Por, REML=FALSE)
conduct.int <- lmer(conductance ~ t_c + spp + t_c:spp + (1|block), data=Por, REML=FALSE)

###AIC
por.cand.mod.names <- c("conduct.null", "conduct.spp", "conduct.treat", "conduct.add", "conduct.int")
por.cand.mods <- list( )

for(i in 1:length(por.cand.mod.names)) {
  por.cand.mods[[i]] <- get(por.cand.mod.names[i]) }
print(aictab(cand.set = por.cand.mods, 
             modnames = por.cand.mod.names))

###CIs - species & treat 
por.spp <- data.frame(response="conductance", 
                      variable="spp",
                      lmer.spp.CI(x=Por, response=Por$conductance))

por.treat <- data.frame(response="conductance",
                        variable="t_c",
                        lmer.tc.CI(x=Por, response=Por$conductance))

##Quantum Yield
###Models
yield.null <- lmer(yield ~ 1 + (1|block), data=OS1, REML=FALSE)
yield.spp <- lmer(yield ~ spp + (1|block), data=OS1, REML=FALSE)
yield.treat <- lmer(yield ~ t_c + (1|block), data=OS1, REML=FALSE)
yield.add <- lmer(yield ~ spp + t_c + (1|block), data=OS1, REML=FALSE)
yield.int <- lmer(yield ~ t_c + spp + t_c:spp + (1|block), data=OS1, REML=FALSE)
###AIC
yield.mod.names <- c("yield.null", "yield.spp", "yield.treat", "yield.add", "yield.int")
yield.mods <- list( )

for(i in 1:length(yield.mod.names)) {
  yield.mods[[i]] <- get(yield.mod.names[i]) }
print(aictab(cand.set = yield.mods, 
             modnames = yield.mod.names))
###CIs - species & treat 
os1.spp <- data.frame(response="yield", 
                      variable="spp",
                      lmer.spp.CI(x=OS1, response=OS1$yield))
os1.treat <- data.frame(response="yield", 
                        variable="t_c",
                        lmer.tc.CI(x=OS1, response=OS1$yield))

##Chlorophyll Concentration
###Models
conc.null <- lmer(concentration ~ 1 + (1|block/date), data=CCM, REML=FALSE)
conc.spp <- lmer(concentration ~ spp + (1|block/date), data=CCM, REML=FALSE)
conc.treat <- lmer(concentration ~ t_c + (1|block/date), data=CCM, REML=FALSE)
conc.add <- lmer(concentration ~ spp + t_c + (1|block/date), data=CCM, REML=FALSE)
conc.int <- lmer(concentration ~ t_c * spp + (1|block/date), data=CCM, REML=FALSE)

###AIC 
conc.mod.names <- c("conc.null", "conc.spp", "conc.treat", "conc.add", "conc.int")
conc.mods <- list( )

for(i in 1:length(conc.mod.names)) {
  conc.mods[[i]] <- get(conc.mod.names[i]) }
print(aictab(cand.set = conc.mods, 
             modnames = conc.mod.names))

###CI - species & treat 
ccm.spp <- data.frame(response="concentration", 
                      variable="spp",
                      lmer.spp.CI(x=CCM, response=CCM$concentration))
ccm.treat <- data.frame(response="concentration", 
                        variable="t_c",
                        lmer.tc.CI(x=CCM, response=CCM$concentration))

#Combined CIs
spp.CI <- rbind(por.spp, ccm.spp, os1.spp, sla.spp)                      #relabel terms
spp.CI$term <- factor(spp.CI$term, levels=c("Intercept", "sppCO", "sppLE", "sppPB", "sppSO", "sppSf", "sppDW"), 
                      labels=c("Intercept", "Corn", "Lentil","Pinto Bean", "Sorghum", "Sunflower", "Wheat"))

treat.CI <- rbind(por.treat, ccm.treat, os1.treat, sla.treat) 
#relabel terms
treat.CI$term <- factor(treat.CI$term, levels=c("Intercept", "t_cT"), 
                        labels=c("Intercept", "Dusted"))

lmer(response)
#Effects Graphs 

##Graph CI's for dust - all responses 
ggplot(subset(treat.CI, term != "Intercept")) +
  coord_flip() + theme_bw(16) +
  geom_hline(yintercept = 0) + 
  geom_errorbar(aes(x=term,
                    ymin=lower, ymax=upper), 
                width=0.1, size=1, color="#377eb8") +
  geom_point(aes(x=term, y=mean), size=4, pch=21, 
             bg="#377eb8", color="white", stroke=2) + facet_wrap(~response) + labs(title="Overall Dust Effect (all)", x="species", y="response")

##Graph CI's for species - all responses
ggplot(subset(spp.CI, term != "Intercept")) + 
 coord_flip() + theme_bw(16) +
  geom_hline(yintercept = 0)  +
  geom_errorbar(aes(x=term,
                    ymin=lower, ymax=upper), 
                width=0.1, size=1, color="#377eb8") +
  geom_point(aes(x=term, y=mean), size=4, pch=21, 
             bg="#377eb8", color="white", stroke=2) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
+ facet_wrap(~response, scale="free") +
  labs(title="Overall Difference in Species (all)", x="Species", y="response") 
+
 coord_flip()


#Functions

#Species CI's
lmer.spp.CI <- function(x, response) { 
  require(lme4)
  x=CCM
  response=CCM$concentration
  
  x1 <- x$t_c
  y <- response
  x2 <- x$spp
  re1 <- x$block
  re2 <- x$date
  model <-lmer(y~x1 + x2 +(1|re1/re2)) 
  n.sims<- 1000
  results<-array(NA,c(n.sims,8))
  colnames(results)<-c("Intercept","t_cT", "sppDW", "sppPB", "sppSf", "sppCO", "sppLE", "sppSO")
  for (i in 1:n.sims){
    y.sim <- unlist(simulate(model))
    model.sim<-lmer(y.sim~x1 +x2 +(1|re1/re2)) 
    results[i,]<-fixef(model.sim) 
  }
  CIs<-as.data.frame(t(rbind(round(apply(results,2,quantile,prob=c(0.025,0.975)), 3), 
                             (round(apply(results,2,mean),3)) ) ) ) 
  CIs$term <- rownames(CIs)
  rownames(CIs) <- c()  
  colnames(CIs)[1:3] <- c("lower","upper","mean")
  return(CIs)
}
#Treatment CI's 
lmer.tc.CI <- function(x, response) { 
  require(lme4)
  
  x1 <- x$t_c
  y <- response
  re3 <- x$spp
  re1 <- x$block
  re2 <- x$date
  model <-lmer(y~x1 + (1|re3) + (1|re1/re2)) 
  n.sims<- 1000
  results<-array(NA,c(n.sims,2))
  colnames(results)<-c("Intercept","t_cT")
  for (i in 1:n.sims){
    y.sim <- unlist(simulate(model))
    model.sim<-lmer(y.sim~x1 + (1|re3) + (1|re1/re2)) 
    results[i,]<-fixef(model.sim) 
  }
  CIs<-as.data.frame(t(rbind(round(apply(results,2,quantile,prob=c(0.025,0.975)), 3), 
                             (round(apply(results,2,mean),3)) ) ) ) 
  CIs$term <- rownames(CIs)
  rownames(CIs) <- c()  
  colnames(CIs)[1:3] <- c("lower","upper","mean")
  return(CIs)
}

lmer.tc.CI.sla <- function(x, response) { 
  require(lme4)
  
  x1 <- x$t_c
  y <- response
  #x2 <- x$spp
  re1 <- x$block
  model <-lmer(y~x1 +(1|re1)) 
  n.sims<- 1000
  results<-array(NA,c(n.sims,2))
  colnames(results)<-c("Intercept","t_cT")
  for (i in 1:n.sims){
    y.sim <- unlist(simulate(model))
    model.sim<-lmer(y.sim~x1 +(1|re1)) 
    results[i,]<-fixef(model.sim) 
  }
  CIs<-as.data.frame(t(rbind(round(apply(results,2,quantile,prob=c(0.025,0.975)), 3), 
                             (round(apply(results,2,mean),3)) ) ) ) 
  CIs$term <- rownames(CIs)
  rownames(CIs) <- c()  
  colnames(CIs)[1:3] <- c("lower","upper","mean")
  return(CIs)
}
stairs <- cbind(ccm.mean.pp, stairs=rep(ccm.mean.pp[1:105], 0:2, each=3))
ggplot(ccm.mean.pp, aes(x=date, y=mean, shape=pre_post, label=pre_post, color=t_c)) +
  facet_wrap(~spp, scales="free") + theme_bw(14) + geom_point() + 
  geom_label(data=subset(ccm.mean.pp, pre_post %in% c("pre","post")),aes(fontface=2), size=2, label.size=0.3, position=position_dodge(width=0.75)) + 
  geom_line(data=subset(ccm.mean.pp, pre_post %in% c("pre","post")), aes(group=date)) +
  geom_line(data=subset(ccm.mean.pp, pre_post %in% c("na")), aes(group=t_c))+
  labs(x="Treatment", y="Chlorophyll Concentration (all)")  + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust")) + scale_shape_discrete(guide=FALSE) +
  geom_step(data=ccm.mean.pp$pre_post, direction="hv")

ggplot(ccm.mean.pp, aes(x=date, y=mean, shape=pre_post, label=pre_post, color=t_c)) +
  facet_wrap(~spp, scales="free") + theme_bw(14) + geom_point() + 
 # geom_line(data=subset(ccm.mean.pp, pre_post %in% c("pre","post")), aes(group=date)) +
  #geom_line(data=subset(ccm.mean.pp, pre_post %in% c("post", "pre")), aes(group=pre_post)) +
  geom_line(data=subset(ccm.mean.pp, pre_post %in% c("na")), aes(group=t_c))+
  labs(x="Treatment", y="Chlorophyll Concentration (all)")  + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_x_discrete("Treatment", labels=c("C"="No Dust", "T"="Dust")) + scale_shape_discrete(guide=FALSE) +
  geom_step(data=subset(ccm.mean.pp, pre_post %in% c("post","pre")), aes(group=t_c))
