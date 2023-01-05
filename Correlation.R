########################################################################################-
# Codes for creating examples by creating artificial samples for the correlation piece #
# Roemer Janse - 22-01-2021                                                            #
# ------------------------------------------------------------------------------------ #

rm(list = ls())

pacman::p_load("dplyr", "tidyverse", "ggplot2", "ggthemes", "plotrix", "reshape2", "MASS", "xlsx")

setwd("~/OneDrive/Documents/Research/[] Methodology/1. Correlation/Figures")
setwd("~/Research/[] Methodology/1. Correlation/Figures")

##### Figure 1: different correlations for different associations #####
# Creating custom theme for figures
theme_min <- function(){
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"))
}

# Creating function for figures
Figure1 <- function(df, ymin, ymax, text, xpos, ypos){
    ggplot(df, aes(x = x, y = y)) + 
    geom_point() + ylim(ymin, ymax) + xlab(NULL) + ylab(NULL) +
    theme_min() +
    geom_label(label = text, x = xpos, y = ypos, label.padding = unit(0.55, "lines"), label.size = 0.35)
}

# Figure 1A: linear model with ρ = -1
x <- c(-10:10)
y <- x * -0.1

df1 <- data.frame(x, y)

cor(df1)

Fig1A <- Figure1(df1, -10, 10, "A", -9.5, 10) 

Fig1A

ggsave("Fig1A.tiff", width = 5, height = 5, dpi = 600)

# Figure 1B: linear model with ρ = 1
x <- c(-10:10)
y <- x * 2

df2 <- data.frame(x, y)

cor(df2)

Fig1B <- Figure1(df2, -10, 10, "B", -9.5, 10)

Fig1B

ggsave("Fig1B.tiff", width = 5, height = 5, dpi = 600)

## Figure 1D: sinusoidal association with ρ = 0
x <- c(-10:20)
y <- sin(0.5 * x)

df3 <- data.frame(x, y)

cor(df3)

Fig1D <- Figure1(df3, -8, 8, "D", -9.5, 8) 

Fig1D

ggsave("Fig1D.tiff", width = 5, height = 5, dpi = 600)

# Figure 1E: quadratic model with ρ = 0
x <- c(-10:10)
y <- x^2 

df4 <- data.frame(x, y)

cor(df4)

Fig1E <- Figure1(df4, -5, 110, "E", -9.5, 110)

Fig1E

ggsave("Fig1E.tiff", width = 5, height = 5, dpi = 600)

# Figure 1F: exponential model with ρ = 0.5
x <- seq(1, 20, by = 0.25)
y <- 2.779^(1.1 * x)

df5 <- data.frame(x, y) %>% mutate(y = y * 10e-10)

cor(df5)

Fig1F <- Figure1(df5, 0, 7.5, "F", 1.5, 7.5)

Fig1F

ggsave("Fig1F.tiff", width = 5, height = 5, dpi = 600)

# Figure 1C: scattered linear model with σ = 0.5
x <- c(-10:10)
set.seed(1)
y <- jitter(x, 175) * 0.5
x <- x * 2

df6 <- data.frame(x, y)

cor(df6)

Fig1C <- Figure1(df6, -40, 40, "C", -38.5, 40) + xlim(-40, 40) 

Fig1C

ggsave("Fig1C.tiff", width = 5, height = 5, dpi = 600)

dev.off()

rm(x, y, df1, df2, df3, df4, df5, df6, Fig1A, Fig1B, Fig1C, Fig1D, Fig1E, Fig1F, Figure1)

##### Figure 2: length and width of correlation #####
# General figure
x <- c(1:50)
set.seed(3)
y <- x + jitter(1:50, 150)

df1 <- data.frame(x, y)
cor(df1)

df1.1 <- df1 %>% slice(1:25)
cor(df1.1)

df1.2 <- df1 %>% slice(26:50)
cor(df1.2)

Fig2A <- ggplot() + 
         geom_point(aes(x = df1$x, y = df1$y)) + xlab(NULL) + ylab(NULL) + xlim(-10, 65) + ylim(-30, 140) +
         geom_point(aes(x = df1.1$x, y = df1.1$y)) +
         theme_min() +
         stat_ellipse(aes(x = df1$x, y = df1$y), colour = rgb(243, 110, 53, max = 255)) +
         stat_ellipse(aes(x = df1.1$x, y = df1.1$y), colour = rgb(243, 110, 53, max = 255)) +
         geom_label(aes(label = "A", x = -10, y = 140), label.padding = unit(0.55, "lines"), label.size = 0.35)

Fig2A

ggsave("Fig2A.tiff", width = 5, height = 5, dpi = 600)

# Split figures
Fig2B <- ggplot(df1.1, aes(x = x, y = y)) +
         geom_point() + xlab(NULL) + ylab(NULL) + xlim(-10, 65) + ylim(-30, 140) +
         theme_min() +
         stat_ellipse(aes(x = x, y = y), colour = rgb(243, 110, 53, max = 255)) +
         geom_label(data = NULL, label = "B", x = -10, y = 140, label.padding = unit(0.55, "lines"), label.size = 0.35)

Fig2B

ggsave("Fig2B.tiff", width = 5, height = 5, dpi = 600)

Fig2C <- ggplot(df1.2, aes(x = x, y = y)) +
         geom_point() + xlab(NULL) + ylab(NULL) + xlim(-10, 65) + ylim(-30, 140) +
         theme_min() +
         stat_ellipse(aes(x = x, y = y), colour = "red")

Fig2C

ggsave("Fig2C.tiff", width = 5, height = 5, dpi = 600)

dev.off()

##### Figure 3. Difference between agreement and relation #####
rm(x, y, df1, df1.1, df1.2, Fig2A, Fig2B, Fig2C)

x <- c(0:20)
y <- x

df1 <- data.frame(x, y) %>% mutate(df = 1)

x <- c(0:20)
y <- 2 * x

df2 <- data.frame(x, y) %>% mutate(df = 2)

x <- c(0:20)
y <- 15 - 0.25 * x

df3 <- data.frame(x, y) %>% mutate(df = 3)

x <- c(0:20)
y <- 0.2 * x + 7.5

df4 <- data.frame(x, y) %>% mutate(df = 4)

df <- rbind(df1, df2, df3, df4) %>% mutate(df = as.factor(df))

Fig3 <- ggplot(data = df, aes(x = x, y = y, group = df)) + xlim(0, 20) + ylim(0, 20) + xlab(NULL) + ylab(NULL) +
        geom_line(aes(linetype = df, colour = df)) + 
        scale_linetype_manual(values = c("dashed", "solid", "solid", "solid")) +
        scale_color_manual(values = c("black", rgb(243, 110, 53, max = 255), rgb(0, 101, 172, max = 255), rgb(59, 162, 75, max = 255)), 
                           labels = c("y = x; r = 1", "y = 2x; r = 1", "y = 15 - 0.25x; r = -1", "y = 7.5 + 0.2x; r = 1"),
                           name = "Equation; Correlation") +
        theme(legend.position = c(0.95, 0.05),
              legend.justification = c("right", "bottom"),
              legend.key = element_rect(fill = "white")) +
        theme_min() +
        guides(colour = guide_legend(), linetype = "none")
        
Fig3

ggsave("Fig3.tiff", width = 5, height = 5, dpi = 600)

rm(df, df1, df2, df3, df4, Fig3, x, y)

##### Artificial Data for Limits of Agreement #####
set.seed(1)
mdrd <- mvrnorm(n = 20, mu = 120, Sigma = runif(1, 5, 15))

set.seed(10)
ckd_epi <- mvrnorm(n = 20, mu = 120, Sigma = runif(1, 5, 15))

df <- data.frame(mdrd, ckd_epi) %>% mutate(mdrd = round(mdrd, digits = 1),
                                           ckd_epi = round(ckd_epi, digits = 1),
                                           diff = ckd_epi - mdrd, mean = (mdrd + ckd_epi) / 2)

write.xlsx(df, "~/OneDrive/Documents/Research/[] Epidemiology/1. Correlation/correlation.xlsx")

d <- round(mean(df$diff), digits = 2)
sd <- round(sd(df$diff), digits = 2)

ul <- round(d + 1.96 * sd, digits = 2)
ll <- round(d - 1.96 * sd, digits = 2)

UL <- rep(ul, times = 20)
LL <- rep(ll, times = 20)
D <- rep(d, times = 20)


### Figure 4: assumptions for limits of agreement
# Bland-Altman plot
Fig4A <- ggplot(df, aes(x = mean, y = diff)) + 
         xlab("Mean eGFR (1.73ml/min/m2)") + ylab("Difference CKD-EPI - MDRD (ml/min/m2)") + ylim(-10, 10) +
         geom_point() +
         geom_line(aes(y = UL, linetype = "dashed")) +
         geom_line(aes(y = LL, linetype = "dashed")) +
         geom_line(aes(y = D, linetype = "dashed")) +
         annotate(geom = "text", x = 122, y = 9, label = "Upper Limit") +
         annotate(geom = "text", x = 122.25, y = -0.5, label = "Mean") +
         annotate(geom = "text", x = 122, y = -7, label = "Lower Limit") +
         scale_linetype_manual(values = c("dashed", "dashed", "dashed")) +
         theme_min() +
         guides(linetype = "none") +
         geom_label(label = "A", x = 117.6, y = 9.5, label.padding = unit(0.55, "lines"), label.size = 0.35)

Fig4A

ggsave("Fig4A.tiff", width = 5, height = 5, dpi = 600)

# Histogram of differences
Fig4B <- ggplot(df, aes(x = diff)) + xlab("Difference CKD-EPI - MDRD") + ylab(NULL) +
         geom_histogram(aes(y = ..density..), binwidth = 2, col = "black", fill = "gray") +
         geom_density(col = "black") +
         theme_min() +
         theme(axis.ticks.y = element_blank(),
               axis.text.y = element_blank()) +
         scale_y_continuous(expand = c(0, 0)) +
         geom_label(label = "B", x = -9, y = 0.143, label.padding = unit(0.55, "lines"), label.size = 0.35)
        
Fig4B 

ggsave("Fig4B.tiff", width = 5, height = 5, dpi = 600)

dev.off()

rm(list = ls())






