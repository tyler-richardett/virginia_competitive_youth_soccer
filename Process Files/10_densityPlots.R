## Load necessary packages
library(dplyr)
library(ggplot2)
library(scales)
library(ROSE)
library(rpart)
library(rpart.plot)

## Import data
virginiaTractsFields <- read.csv("virginiaTractsFields.csv", na.strings = "", stringsAsFactors = FALSE)
virginiaTracts <- read.csv("virginiaTracts.csv", na.strings = "", stringsAsFactors = FALSE)

## Reshape data for ggplot2
virginiaTractsFields <- virginiaTractsFields %>% mutate(Type = "Virginia Tracts with Fields") %>% select(Type, GEOID:HHI.Factor)
virginiaTracts <- virginiaTracts %>% mutate(Type = "All Virginia Tracts") %>% select(Type, GEOID:HHI.Factor)

virginiaComparison <- rbind(virginiaTractsFields[,c(1,4,10,11)], virginiaTracts[,c(1,4,10,11)])
virginiaComparison$Type <- as.factor(virginiaComparison$Type)
virginiaComparison <- virginiaComparison %>% mutate(Minority.Percent = Minority.Percent/100, Poverty.Rate = Poverty.Rate/100, Household.Income = Household.Income/1000)

## Custom ggplot theme
theme_TCR <- function() {
        theme_bw(base_size = 12, base_family = "Avenir") %+replace% 
                theme(
                        panel.border = element_blank(),
                        axis.line.x = element_line(color = "black", size = 0.5),
                        axis.ticks.y = element_blank(),
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

## Household income
ggplot(virginiaComparison, aes(x = Household.Income, fill = Type)) + 
        geom_density(alpha = 0.5) + 
        theme_TCR() + 
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(values = c("grey50", "coral1"),
                          labels = c("All Virginia Tracts    ",
                                     "Virginia Tracts with Fields")) +
        scale_x_continuous(labels = dollar_format("$", "K")) +
        ylab("Density") +
        xlab("Household Income (2016 Dollars)")
# ggsave("densityPlot_HHI.png", width = 5, height = 4, units = "in", bg = "transparent")        


## Share of nonwhite
ggplot(virginiaComparison, aes(x = Minority.Percent, fill = Type)) + 
        geom_density(alpha = 0.5) + 
        theme_TCR() + 
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(values = c("grey50", "coral1"),
                          labels = c("All Virginia Tracts    ",
                                     "Virginia Tracts with Fields")) +
        scale_x_continuous(labels = percent_format()) +
        ylab("Density") +
        xlab("Share of Nonwhite Residents")
# ggsave("densityPlot_Race.png", width = 10, height = 4, units = "in", bg = "transparent")


## Poverty rate
ggplot(virginiaComparison, aes(x = Poverty.Rate, fill = Type)) + 
        geom_density(alpha = 0.5) + 
        theme_TCR() + 
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(values = c("grey50", "coral1"),
                          labels = c("All Virginia Tracts    ",
                                     "Virginia Tracts with Fields")) +
        scale_x_continuous(labels = percent_format()) +
        ylab("Density") +
        xlab("Poverty Rate (Families with Children Under 18)")
# ggsave("densityPlot_Poverty.png", width = 5, height = 4, units = "in", bg = "transparent")


## Check for predictability
virginiaTree <- virginiaTracts %>% mutate(Field = ifelse(GEOID %in% virginiaTractsFields$GEOID, TRUE, FALSE)) %>% select(Minority.Percent, Poverty.Rate, Household.Income, Field)
virginiaTree$Field <- as.factor(virginiaTree$Field)
virginiaTree <- virginiaTree[complete.cases(virginiaTree),]

virginiaTreeRose <- ROSE(Field ~ ., virginiaTree, seed = 1)$data
virginiaTreeRoseFit <- rpart(Field ~ ., virginiaTreeRose)
rpart.plot(virginiaTreeRoseFit, nn = TRUE, branch.lty = 3, extra = 104)
roc.curve(virginiaTreeRose$Field, predict(fit, newdata = virginiaTreeRose)[,2])

# ggplot(virginiaTreeRose, aes(x = Household.Income, y = Poverty.Rate, color = Field)) + geom_point()