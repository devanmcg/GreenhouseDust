setwd("C:/Users/devan.mcgranahan/Google Drive/Research/Projects/Greenhouse dust")

load(file="./data/clip.net.Rdata")

clip.rec <- subset(clip.net, recovery!=1) 
clip.rec$sp_pot <- unite(data=clip.rec, sp_pot, 
                         c(species, pot))$sp_pot

pacman::p_load(lme4, car, AICcmodavg, tidyr)

rem0 <- glmer(recovery+0.001 ~ 1 + (1|clipping/block), 
              clip.rec, family=Gamma(link="log"))

rem1 <- glmer(recovery+0.001 ~ trt + (1|clipping/block),
              clip.rec, family=Gamma(link="log"))
coef(rem1)
Anova(rem1, type="2")

rem2 <- glmer(recovery+0.001 ~ trt + species + (1|clipping/block), 
              clip.rec, family=Gamma(link="log"))
coef(rem2)
Anova(rem2, type="2")

anova(rem0, rem1, rem2)

Cand.models <- list(rem0, rem1, rem2 )
Modnames <- paste("mod", 1:length(Cand.models), sep = " ")
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)
print(modavg(parm = "trtT", cand.set = Cand.models,
             modnames = Modnames), digits = 4)

with(clip.rec, unite_(col=sp.pot, c(species, pot)) )  

## 
## Distributions
##

ggplot(subset(clip.net, recovery!=1), aes(x=recovery)) + theme_bw(16) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=0.1,
                 colour="black", fill="lightgreen") +
  geom_density(alpha=.2, fill="#FF6666")