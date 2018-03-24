## Load necessary packages
library(dplyr)
library(ggplot2)
library(scales)

## Import data
virginiaTeamsInvestment <- read.csv("virginiaTeamsInvestment.csv", na.strings = "", colClasses = c(rep("character", 4), "numeric", "character", rep("numeric", 7), "factor"))

## Custom ggplot theme
theme_TCR <- function() {
        theme_bw(base_size = 12, base_family = "Avenir") %+replace% 
                theme(
                        panel.border = element_blank(),
                        axis.line.y = element_line(color = "black", size = 0.5),
                        axis.ticks.x = element_blank(),
                        axis.text = element_text(color = "black", size = 11),
                        panel.grid.major.x = element_blank(),
                        panel.grid.major.y = element_line(linetype = "dotted", 
                                                          color = "grey"),
                        panel.grid.minor = element_blank(),
                        plot.title = element_text(face = "bold", size = 16),
                        axis.title.x = element_text(margin = margin(20,0,10,0)),
                        axis.title.y = element_text(margin = margin(0,15,0,0), angle = 90),
                        legend.position = "bottom",
                        legend.title = element_blank(),
                        legend.direction = "horizontal",
                        legend.box = "horizontal",
                        legend.key.size = unit(10, "pt"),
                        plot.margin = margin(r = 30, t = 10),
                        panel.background = element_rect(fill = "transparent", color = NA),
                        plot.background = element_rect(fill = "transparent", color = NA),
                        legend.background = element_rect(fill = "transparent", color = NA)
                ) 
}

## Cost
ggplot(virginiaTeamsInvestment, aes(x = LeagueTier, y = WeightedCost)) + 
        geom_boxplot(fill = "coral1", alpha = 0.9) + 
        theme_TCR() +
        scale_y_continuous(expand = c(0,0), labels = dollar_format()) +
        ylab("Single-Season Cost") +
        xlab("Level of Play")
# ggsave("boxplot_Cost.png", width = 10, height = 4, units = "in", bg = "transparent")


## Travel distance
ggplot(virginiaTeamsInvestment, aes(x = LeagueTier, y = WeightedDistance)) + 
        geom_boxplot(fill = "coral1", alpha = 0.9) + 
        theme_TCR() +
        scale_y_continuous(expand = c(0,0), labels = dollar_format("")) +
        ylab("Single-Season Travel Distance (Miles)") +
        xlab("Level of Play")
# ggsave("boxplot_TravelDistance.png", width = 5, height = 4, units = "in", bg = "transparent")


## Travel time
ggplot(virginiaTeamsInvestment, aes(x = LeagueTier, y = WeightedTime/60)) + 
        geom_boxplot(fill = "coral1", alpha = 0.9) + 
        theme_TCR() +
        scale_y_continuous(expand = c(0,0)) +
        ylab("Single-Season Travel Time (Hours)") +
        xlab("Level of Play")
# ggsave("boxplot_TravelTime.png", width = 5, height = 4, units = "in", bg = "transparent")