setwd("C:/Users/devan.mcgranahan/Google Drive/Research/Projects/Greenhouse dust")

load(file="./data/clip.sum.Rdata")
load(file="./data/clip.net.Rdata")


colnames(clip.sum)[[2]] <- "Dust"
clip.sum$Dust<- revalue(clip.sum$Dust, 
                        c("C"="Unexposed", 
                          "T"="High exposure"))

colnames(clip.net)[[4]] <- "Dust"
clip.net$Dust<- revalue(clip.net$Dust, 
                        c("C"="Unexposed", 
                          "T"="High exposure"))

ggplot() + theme_bw(14) + facet_wrap(~species, scales="free_y") +
  geom_hline(yintercept = 1, color="black") + 
  geom_line(data=clip.net, aes(x=clipping, y=recovery, 
                               colour=Dust, group=pot), 
            alpha=0.3, show.legend = FALSE) +
  geom_line(data=clip.sum, aes(x=clipping, y=mean, 
                               colour=Dust, group=Dust, show.legend = FALSE), 
            alpha=1, size=1.2, position=position_dodge(width=0.3)) +
  geom_errorbar(data=clip.sum, aes(x=clipping, colour=Dust,
                                   ymin=mean-se, ymax=mean+se), 
                position=position_dodge(width=0.3), 
                width=0.2, size=1, show.legend = FALSE) +
  geom_point(data=clip.sum, aes(x=clipping, y=mean, 
                                bg=Dust), 
             pch=21, size=4, color="white",
             position=position_dodge(width=0.3), stroke=1.5, show.legend = FALSE) +
  scale_x_discrete(labels=c("Initial","First", "Second")) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(x="Clipping event", y="Proportion biomass recovery") +
  theme(legend.position=c(0.85, 0.15))
  
##
## Raw data loading, prep
##

pacman::p_load(googlesheets, plyr, ggplot2) 
gs_auth(new_user = TRUE)
(my_sheets <- gs_ls()) # View available sheets
dat_gs <- gs_title("perennial grass dust data")
clip.d <- as.data.frame(dat_gs %>% gs_read_csv(ws="clippings combined"))

str(clip.d)

clip.recov <- data.frame(clip.d[1:4], 
                         net0=(clip.d$gross0-7.5)/(clip.d$gross0-7.5),
                         net1=(clip.d$gross1-7.5)/(clip.d$gross0-7.5), 
                         net2=(clip.d$gross2-7.5)/(clip.d$gross0-7.5))

clip.net <- data.frame(clip.recov[1:4], 
                       stack(round(clip.recov[5:7],2)))
# save(clip.net, file="./data/clip.net.Rdata")

colnames(clip.net)[5:6] <- c("recovery","clipping")

clip.sum <- ddply(clip.net, .(species, trt, clipping), 
                  summarize, mean=mean(recovery), 
                  se=sd(recovery)/sqrt(length(recovery)))
# save(clip.sum, file="./data/clip.sum.Rdata")

