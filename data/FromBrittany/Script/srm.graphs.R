con.os1 <- subset(os1.mean.date, t_c=="C")
con.os1$spp <- factor(con.os1$spp, levels= c("BA", "CO", "DW", "LE", "PB", "SO", "SF"), 
                      labels=c("Barley", "Corn", "Durum Wheat", "Lentil", "Pinto Bean", "Sorghum", "Sunflower"))

trt.os1 <- subset(os1.mean.date, t_c=="T")
trt.os1$order <- ifelse(trt.os1$pre_post=="pre",1,2)
trt.os1 <- trt.os1[with(trt.os1,order(date,order)),]
trt.os1$spp <- factor(trt.os1$spp, levels= c("BA", "CO", "DW", "LE", "PB", "SO", "SF"), 
                      labels=c("Barley", "Corn", "Durum Wheat", "Lentil", "Pinto Bean", "Sorghum", "Sunflower"))
os1.gg <-  ggplot() + facet_wrap(~spp, scale="free", ncol=4) + theme_bw(10) +
  geom_path(data=trt.os1, aes(x=as.numeric(date),
                              y=mean), color="#56B4E9", alpha=0.5) +
  geom_errorbar(data=con.os1, aes(x=as.numeric(date), ymin=mean-se, ymax=mean+se, color=t_c), color="#E69F00", width=0.15) + 
  geom_line(data=con.os1, aes(x=as.numeric(date),mean, 
                              group=1), color="#E69F00", alpha=0.5) +
  geom_point(data=con.os1, aes(x=as.numeric(date),mean), 
             color="#E69F00", shape=19, size=2) +
  geom_errorbar(data=trt.os1, aes(x=as.numeric(date), ymin=mean-se, ymax=mean+se, group=pre_post),
                color="#56B4E9", width=0.15, position=position_dodge(width=0.25)) + 
  geom_point(data=trt.os1, aes(x=as.numeric(date),mean,shape=pre_post),
             color="#56B4E9", size=2, position=position_dodge(width=0.25)) +
  scale_shape_manual(values=c(17,2,19), name="",
                     breaks=c("pre","post"),
                     labels=c("Before","After")) +
  theme(legend.position=c(1,0), legend.justification=c(1,0), 
        panel.grid.major.y=element_line(color="grey68"), panel.grid.major.x=element_line(color=NA)) 
os1.gg + scale_x_continuous("Dust Event", breaks=c(1,2,3,4,5,6,7,8,9,10), 
                            labels=c("8/24", "8/26","8/29", "8/31","9/2", "9/21","9/23","9/26","9/28","9/30")) +
  labs(y=expression(Yield ~""~(~Delta~"F/Fm"))) 





con.ccm <- subset(ccm.mean.date, t_c=="C")
con.ccm$spp <- factor(con.ccm$spp, levels= c("BA", "CO", "DW", "LE", "PB", "SO", "SF"), 
                      labels=c("Barley", "Corn", "Durum Wheat", "Lentil", "Pinto Bean", "Sorghum", "Sunflower"))

trt.ccm <- subset(ccm.mean.date, t_c=="T")
trt.ccm$order <- ifelse(trt.ccm$pre_post=="pre",1,2)
trt.ccm <- trt.ccm[with(trt.ccm,order(date,order)),]
trt.ccm$spp <- factor(trt.ccm$spp, levels= c("BA", "CO", "DW", "LE", "PB", "SO", "SF"), 
                      labels=c("Barley", "Corn", "Durum Wheat", "Lentil", "Pinto Bean", "Sorghum", "Sunflower"))
ccm.gg <-  ggplot() + facet_wrap(~spp, scale="free", ncol=4) + theme_bw(10) +
  geom_path(data=trt.ccm, aes(x=as.numeric(date),
                              y=mean), color="#56B4E9", alpha=0.5) +
  geom_errorbar(data=con.ccm, aes(x=as.numeric(date), ymin=mean-se, ymax=mean+se, color=t_c), color="#E69F00", width=0.15) + 
  geom_line(data=con.ccm, aes(x=as.numeric(date),mean, 
                              group=1), color="#E69F00", alpha=0.5) +
  geom_point(data=con.ccm, aes(x=as.numeric(date),mean), 
             color="#E69F00", shape=19, size=2) +
  geom_errorbar(data=trt.ccm, aes(x=as.numeric(date), ymin=mean-se, ymax=mean+se, group=pre_post),
                color="#56B4E9", width=0.15, position=position_dodge(width=0.25)) + 
  geom_point(data=trt.ccm, aes(x=as.numeric(date),mean,shape=pre_post),
             color="#56B4E9", size=2, position=position_dodge(width=0.25)) +
  scale_shape_manual(values=c(17,2,19), name="",
                     breaks=c("pre","post"),
                     labels=c("Before","After")) +
  theme(legend.position=c(1,0), legend.justification=c(1,0), 
        panel.grid.major.y=element_line(color="grey68"), panel.grid.major.x=element_line(color=NA)) 
ccm.gg + scale_x_continuous("Dust Event", breaks=c(1,2,3,4,5,6,7,8,9,10), 
                            labels=c("8/24", "8/26","8/29", "8/31","9/2", "9/21","9/23","9/26","9/28","9/30")) +
  labs(y=expression(Concentration ~""~("mg/"~m^{2}))) (mg/m^2)
