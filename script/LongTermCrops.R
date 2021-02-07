#
#  Long-term responses
#
# Mixed-effect model fitting
# Specific leaf area
sla.null <- lme4::lmer(scale(log(SLA)) ~ 1 + (1|block), 
                       data=SLA2, REML=FALSE)
sla.spp <- lme4::lmer(scale(log(SLA)) ~ spp + (1|block), 
                      data=SLA2, REML=FALSE)
sla.treat <- lme4::lmer(scale(log(SLA)) ~ t_c + (1|block), 
                        data=SLA2, REML=FALSE)
sla.add <- lme4::lmer(scale(log(SLA)) ~ spp + t_c + (1|block), 
                      data=SLA2, REML=FALSE)
sla.int <- lme4::lmer(scale(log(SLA)) ~ spp * t_c + (1|block), 
                      data=SLA2, REML=FALSE)

# AICc-based model selection 
# Specific leaf area 
  sla.mod.names <- c("sla.null", "sla.spp", 
                     "sla.treat", "sla.add",
                     "sla.int")
  sla.mods <- lst( )
  
  for(i in 1:length(sla.mod.names)) {
    sla.mods[[i]] <- get(sla.mod.names[i]) }
  sla_aic_tab <- AICcmodavg::aictab(cand.set = sla.mods, 
                                    modnames = sla.mod.names) 
  
  sla_aic <- 
    sla_aic_tab %>% as_tibble %>%
    mutate(Modnames = as.character(Modnames)) %>%
    mutate(Modnames = recode(Modnames, 
                             sla.null = 'null (Intercept only)' , 
                             sla.spp = "species", 
                             sla.treat = "dust" , 
                             sla.add = "species + dust", 
                             sla.int = "species x dust")) %>%
    rename(Model = Modnames, 
           `$\\Delta$AICc` = Delta_AICc) %>%
    bind_cols(tibble(Response = c("Specific leaf area", "","","","")), 
              .) 

sla.add0 <- lme4::lmer(scale(log(SLA)) ~ 0 + spp + t_c + (1|block), 
                      data=SLA2, REML=FALSE)
sla.int0 <- lme4::lmer(scale(log(SLA)) ~ 0 + spp * t_c + (1|block), 
                      data=SLA2, REML=FALSE)
# Stomatal conductance
conduct.null <- lme4::lmer(scale(lcond) ~ 1 + (1|block:date:pot),
                           data=Por, REML=FALSE)
conduct.spp <- lme4::lmer(scale(lcond) ~ spp + (1|block:date:pot), 
                          data=Por, REML=FALSE)
conduct.treat <- lme4::lmer(scale(lcond) ~ t_c + (1|block:date:pot), 
                            data=Por, REML=FALSE)
conduct.add <- lme4::lmer(scale(lcond) ~ 0 + spp + t_c + (1|block:date:pot), 
                          data=Por, REML=FALSE)
conduct.int <- lme4::lmer(scale(lcond) ~ 0 + spp * t_c  + (1|block:date:pot), 
                          data=Por, REML=FALSE)
# Chlorophyll content
conc.null <- lme4::lmer(scale(lconc) ~ 1 + (1|block:date:pot), 
                        data=CCM2, REML=FALSE)
conc.spp <- lme4::lmer(scale(lconc) ~ 0 + spp + (1|block:date:pot), 
                       data=CCM2, REML=FALSE)
conc.treat <- lme4::lmer(scale(lconc) ~ t_c + (1|block:date:pot), 
                         data=CCM2, REML=FALSE)
conc.add <- lme4::lmer(scale(lconc) ~ 0 + spp + t_c + (1|block:date:pot), 
                       data=CCM2, REML=FALSE)
conc.int <- lme4::lmer(scale(lconc) ~ 0 + spp*t_c + (1|block:date),
                       data=CCM2, REML=FALSE)
# Quantum yield
yield.null <- lme4::lmer(scale(yield)~ 1 + (1|block:date:pot), 
                         data=OS1, REML=FALSE)
yield.spp <- lme4::lmer(scale(yield)~ spp + (1|block:date:pot), 
                        data=OS1, REML=FALSE)
yield.treat <- lme4::lmer(scale(yield)~ t_c + (1|block:date:pot), 
                          data=OS1, REML=FALSE)
yield.add <- lme4::lmer(scale(yield)~ 0 + spp + t_c + (1|block:date:pot), 
                        data=OS1, REML=FALSE)
yield.int <- lme4::lmer(scale(yield) ~ 0 + spp*t_c + (1|block:date:pot), 
                        data=OS1, REML=FALSE) 


# This suggests that the comparisons made are between the following two models.
# scale(lconc) ~ 0 + spp + t\_c + spp:t\_c + (1|block:date:pot) and
# scale(lconc) ~ spp + (1|block:date:pot)
# However, the correct comparison should have been as follows:
#   scale(lconc) ~ 0 + spp + t\_c + (1|block:date:pot) and
# scale(lconc) ~ 0 + spp + (1|block:date:pot)
# And, the result of this comparison reveals the effect of dust (NOT species).