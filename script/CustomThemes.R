cbPal5 <- c("#009E73","#E69F00",  "#0072B2", "#CC79A7", "#999999")
cbPal8 <- c("#66c2a5","#fc8d62",  "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3" )
ColPts5 <- c(22,24,21,25,23)

# A general theme for book graphics. Size = 18

theme_cb1 <- function (base_size = 18, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="bold"),
          legend.key = element_rect(colour = NA),
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "#A6CEE3", colour = "grey50", size = 0.2))
}

# A general theme for book graphics; . Size = 18

theme_CF <- function (base_size = 18, base_family = "") 
{
  # CF = "clean facet." No shading or lines
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="plain", 
                                    margin = 
                                      margin(0,0,1.5,0, "mm")),
          legend.key = element_rect(colour = NA),
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = NA, size = NA))
}

theme_CFcatX <- function (base_size = 18, base_family = "") 
{
    # CF = "clean facet." No shading or lines
    # catX = no minor X gridlines for categorical variables
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="plain", 
                                    margin = 
                                      margin(0,0,1.5,0, "mm")),
          legend.key = element_rect(colour = NA),
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor.y = element_line(colour = "grey98", size = 0.5), 
          panel.grid.minor.x = element_blank(), 
          strip.background = element_rect(fill = NA, size = NA))
}

theme_CFbp <- function (base_size = 18, base_family = "") 
{
  # CF = "clean facet." No shading or lines
  # bp = for boxplots
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="plain", 
                                    margin = 
                                      margin(0,0,1.5,0, "mm")),
          legend.key = element_rect(colour = NA),
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major.y = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor.y = element_line(colour = "grey98", size = 0.5), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_blank(), 
          strip.background = element_rect(fill = NA, size = NA))
}

# No gridlines for general graphics

theme_empty <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.line = element_line(colour = "black"),
          axis.text = element_text(size = rel(0.9)), 
          axis.title = element_text(face="bold"),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(face="bold"),
          panel.border = element_rect(fill = NA, colour = NA), 
          panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "white"), 
          strip.background = element_rect(fill = "white", 
                                          colour = "grey50", 
                                          size = 0.2), 
          panel.background = element_rect(fill = "white", 
                                          colour = "white"), 
          plot.background = element_rect(fill = "white", 
                                         colour = "white"), 
          legend.background= element_rect(fill = "white", 
                                          colour = "white"), 
          legend.key = element_rect(fill = "white", 
                                    colour = "white"), 
          plot.caption = element_text(size = 9, hjust = 1, vjust=-5,
                                      color = "grey40", face = "italic"))
}

theme_emptyMap <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.line = element_line(color = NA),
          axis.text = element_text(color = NA), 
          axis.title = element_text(color = NA),
          axis.ticks = element_line(color = NA), 
          strip.text = element_text(face="bold"),
          panel.border = element_rect(fill = NA, colour = NA), 
          panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "white"), 
          strip.background = element_rect(fill = "white", 
                                          colour = NA, 
                                          size = 0.0), 
          panel.background = element_rect(fill = "white", 
                                          colour = "white"), 
          plot.background = element_rect(fill = "white", 
                                         colour = "white"), 
          legend.background= element_rect(fill = "white", 
                                          colour = "white"), 
          legend.key = element_rect(fill = "white", 
                                    colour = "white"), 
          plot.caption = element_text(size = 9, hjust = 1, vjust=-5,
                                      color = "grey40", face = "italic"))
}

theme_emptyMapbm <- function (base_size = 12, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.line = element_line(color = NA),
          axis.text = element_text(color = NA), 
          axis.title = element_text(color = NA),
          axis.ticks = element_line(color = NA), 
          strip.text = element_text(face="bold"),
          panel.border = element_rect(fill = NA, colour = NA), 
          panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "white"), 
          strip.background = element_rect(fill = "white", 
                                          colour = NA, 
                                          size = 0.0), 
          panel.background = element_rect(fill = "transparent", 
                                          colour = NA), 
          plot.background = element_rect(fill = "transparent", 
                                         colour = NA), 
          legend.background= element_rect(fill = "transparent", 
                                          colour = NA), 
          legend.key = element_rect(fill = "transparent", 
                                    colour =NA),
            plot.caption = element_text(size = 9, hjust = 1, vjust=-5,
                                      color = "grey40", face = "italic"))
}