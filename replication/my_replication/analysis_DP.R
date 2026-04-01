# Title: Inclusion to Exclude: How Femonationalism Impacts Policy Preferences 
# Replication Project 
# Author: Damaris Parasca 

# remove objects
rm(list=ls())
gc()

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load necessary libraries 
pacman::p_load(tidyverse, ggplot2, patchwork, marginaleffects, ggpubr, readr, stargazer)

set.seed(123)
df <- read_csv("data.csv")

# split df by immigration attitude 
df_pro <- subset(df, df$imm_pro_con==0)
df_anti <- subset(df, df$imm_pro_con==2)

# custom themes for plots 
my_theme = theme(
  axis.title.x = element_text(size = 8),
  axis.text.x = element_text(size = 8),
  axis.title.y = element_text(size = 8)
)

my_theme_2 = theme(
  axis.title.x = element_text(size = 10),
  axis.text.x = element_text(size = 10),
  axis.title.y = element_text(size = 10)
)

# Figure 3 

## title: Conditional average treatment effect on support for gender policies
## outcome: gender policy
## treatments: t1 and t2

m_gender <- lm(genderpolicies ~ treatment * immigration, data = df)
summary(m_gender)
stargazer(m_gender)

## Panel 1 - t1
preds_t1 <- predictions(m_gender, newdata = datagrid(
  "immigration" = 0:100,
  "treatment" = c("control", "t1")
))

pred_t1 <- ggplot(preds_t1, aes(immigration, estimate, colour = treatment)) +
  geom_line(show.legend = FALSE) +
  # add ticks at the bottom to show where data exists
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + 
  theme_minimal() +
  scale_colour_manual(name = "treatment", values = c("#2111d1", "#d11141")) +
  xlab("") +
  ylab("Support for gender policies") +
  # add explanations for the lines
  annotate(geom = "text", x = 13, y = 7, size = 2, color = "#d11141", 
           fontface = 2, label = "Treatment group T1") +
  annotate(geom = "curve", x = 10, y = 6.6, xend = 13, yend = 5.1,curvature = .4, 
           arrow = arrow(length = unit(2, "mm")), colour = "#d11141") +
  annotate(geom = "text", x = 50, y = 3.5, size = 2, color = "#2111d1",
           fontface = 2,label = "Control group") +
  annotate(geom = "curve", x = 38, y = 3.5, xend = 25, yend = 4.4, curvature = -.4,
           arrow = arrow(length = unit(2, "mm")), colour = "#2111d1") +
  my_theme

pred_t1

## Panel 3 - t1 ATE
tst_t1 <- comparisons(m_gender,
  variable = list(treatment = c("control", "t1")),
  newdata = datagrid(immigration = 1:100),
  conf_level = 0.9
)

ate_t1 <- ggplot(tst_t1, aes(immigration, estimate)) + geom_line(colour = "#d11141") +
  # confidence interval around values
  geom_ribbon(aes(ymax = (conf.high), ymin = (conf.low)), alpha = 0.2, fill = "#d11141") +
  # adding the bottom ticks 
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + 
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 0),
               color = "#2111d1", linetype = "dashed") +
  xlab("Pre-treatment attitudes towards immigrants") +
  ylab("CATE") +
  my_theme +
  theme_minimal() +
  ylim(-1, 2)

ate_t1

## Panel 2 - t2
preds_t2 <- predictions(m_gender, newdata = datagrid(
  "immigration" = 0:100,
  "treatment" = c("control", "t2")
))

pred_t2 <- ggplot(preds_t2, aes(immigration, estimate, colour = treatment)) +
  geom_line(show.legend = FALSE) +
  # bottom ticks - immigration attitudes in the dataset
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + # Rug plot added 
  theme_minimal() +
  scale_colour_manual(name = "treatment", values = c("#2111d1", "#d11141")) +
  xlab("") +
  ylab("") +
  # add explanations for the lines 
  annotate(geom = "text", x = 13, y = 7, size = 2, color = "#d11141",
           fontface = 2, label = "Treatment group T2") +
  annotate(geom = "curve", x = 10, y = 6.6, xend = 13, yend = 5.3, curvature = .4,
           arrow = arrow(length = unit(2, "mm")), colour = "#d11141") +
  annotate(geom = "text", x = 50, y = 3.5, size = 2, color = "#2111d1",
           fontface = 2, label = "Control group") +
  annotate(geom = "curve", x = 38, y = 3.5, xend = 25, yend = 4.4, curvature = -.4,
           arrow = arrow(length = unit(2, "mm")), colour = "#2111d1") +
  my_theme

pred_t2

## Panel 4 - T2 ATE
tst_t2 <- comparisons(m_gender,
  variable = list(treatment = c("control", "t2")),
  newdata = datagrid(immigration = 1:100),
  conf_level = 0.9
)

ate_t2 <- ggplot(tst_t2, aes(immigration, estimate)) + geom_line(colour = "#d11141") +
  geom_ribbon(aes(ymax = (conf.high), ymin = (conf.low)), alpha = 0.2, fill = "#d11141") +
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + 
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 0),
               color = "#2111d1",linetype = "dashed") +
  xlab("Pre-treatment attitudes towards immigrants") +
  ylab("") +
  theme_minimal() +
  my_theme +
  ylim(-1, 2)

ate_t2

pdf("joined_gender.pdf")
# join plots into one figure 
plot <- pred_t1 / ate_t1 - pred_t2 / ate_t2 +
  plot_annotation(
#   title = "Conditional average treatment effect on support for gender policies",
    subtitle = "
                T1 = republican politican x value threat condition
                T2 = republican politican x safety threat condition
                 ",
    caption = "
                Note: The x-axis represents pre-treatment attitudes towards immigrants with lower values representing negative attitudes
                towards immigrants and higher numbers representing positive attitudes towards immigrants
              ",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8),
      plot.subtitle = element_text(size = 8)
    )
  )
print(plot)
dev.off()

plot

# Figure 4 

## title: Mean levels of support for gender policies for pro- and anti-immigration respondents
## outcome: gender policies
## treatments: all treatments t1-t4

col1 <- c("#0818A8", "#8e99fa")
# dark blue = anti-immigration respondents; light blue = pro-immigration respondents

pdf("support_gp.pdf")
plot <- plot_predictions(m_gender, by = c("treatment", "immigration_dichot")) +
  ylab("Support for gender policies") +
  xlab("Treatment condition") +
  theme_minimal() +
  theme(legend.position = "none", axis.title.x = element_text(margin = margin(t = 20))) +
  my_theme_2 +
  scale_x_discrete(labels = c("control" = "Control", 
                              "t1" = "T1:\nRepublican\n× Value threat", 
                              "t2" = "T2:\nRepublican\n× Safety threat", 
                              "t3" = "T3:\nDemocratic\n× Value threat", 
                              "t4" = "T4:\nDemocratic\n× Safety threat")) +
  scale_color_manual(values = col1) +
  geom_bracket(xmin = c("control", "control"), xmax = c("t1", "t2"),
    y.position = c(4.8, 4.6), label = c(".65**", ".83 ***"), tip.length = -0.02,
    color = "slategrey", label.size = 3, vjust = 1, hjust = .5, color = "slategrey") +
  annotate(geom = "text", x = 1.5, y = 8.3, size = 3.5, color = "#8e99fa",
           fontface = 2, label = "Pro-immigration respondents") +
  annotate(geom = "text", x = 1.5, y = 6.5, size = 3.5, color = "#0818A8",
           fontface = 2, label = "Anti-immigration respondents") +
  plot_annotation(#title = 'Mean level of support for gender policies for pro- and anti-immigration respondents',
                  theme = theme(plot.title = element_text(size = 10, face = "bold")))
print(plot)
dev.off()

plot

# Figure 5

## title: Conditional average treatment effects on support for stricter integration demands
## outcome: integration policy
## treatments: t3 and t4

m_integ <- lm(integration ~ treatment * immigration, data = df)
summary(m_integ)
stargazer(m_integ)

## Panel 1 - T3
preds_t3 <- predictions(m_integ, newdata = datagrid(
  "immigration" = 0:100,
  "treatment" = c("control", "t3")
))

pred_t3 <- ggplot(preds_t3, aes(immigration, estimate, colour = treatment)) +
  geom_line(show.legend = FALSE) +
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + 
  theme_minimal() +
  scale_colour_manual(name = "treatment", values = c("#2111d1", "#d11141")) +
  xlab("") +
  ylab("Support for stricter\nintegration policies") +
  annotate(geom = "text", x = 75, y = 7, size = 2, color = "#d11141",
           fontface = 2, label = "Treatment group T3") +
  annotate(geom = "curve", x = 85, y = 6.6, xend = 82, yend = 5.1, curvature = -.4,
           arrow = arrow(length = unit(2, "mm")), colour = "#d11141") +
  annotate(geom = "text", x = 50, y = 3.5, size = 2, color = "#2111d1",
           fontface = 2, label = "Control group") +
  annotate(geom = "curve", x = 62, y = 3.5, xend = 77, yend = 4, curvature = .4,
           arrow = arrow(length = unit(2, "mm")), colour = "#2111d1") +
  my_theme

pred_t3

## Panel 3 - T3 ATE
tst_t3 <- comparisons(m_integ,
  variable = list(treatment = c("control", "t3")),
  newdata = datagrid(immigration = 1:100),
  conf_level = 0.9)

ate_t3 <- ggplot(tst_t3, aes(immigration, estimate)) + geom_line(colour = "#d11141") +
  geom_ribbon(aes(ymax = (conf.high), ymin = (conf.low)), alpha = 0.2, fill = "#d11141") +
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + 
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 0), color = "#2111d1",
               linetype = "dashed") + 
  xlab("Pre-treatment attitudes towards immigrants") +
  ylab("CATE") +
  theme_minimal() +
  my_theme +
  ylim(-1, 2)

ate_t3

## Panel 2 - T4
preds_t4 <- predictions(m_integ, newdata = datagrid("immigration" = 0:100,
  "treatment" = c("control", "t4")))

pred_t4 <- ggplot(preds_t4, aes(immigration, estimate, colour = treatment)) +
  geom_line(show.legend = FALSE) +
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + 
  theme_minimal() +
  scale_colour_manual(name = "treatment", values = c("#2111d1", "#d11141")) +
  xlab("") +
  ylab("") +
  annotate(geom = "text", x = 75, y = 7, size = 2, color = "#d11141",
           fontface = 2, label = "Treatment group T4") +
  annotate(geom = "curve", x = 85, y = 6.6, xend = 82, yend = 5.1, curvature = -.4,
           arrow = arrow(length = unit(2, "mm")), colour = "#d11141") +
  annotate(geom = "text", x = 50, y = 3.5, size = 2, color = "#2111d1",
           fontface = 2, label = "Control group") +
  annotate(geom = "curve", x = 62, y = 3.5, xend = 77, yend = 4, curvature = .4,
           arrow = arrow(length = unit(2, "mm")), colour = "#2111d1") +
  my_theme

pred_t4

## Panel 4 - T4 ATE
tst_t4 <- comparisons(m_integ,
  variable = list(treatment = c("control", "t4")),
  newdata = datagrid(immigration = 1:100),
  conf_level = 0.9)

ate_t4 <- ggplot(tst_t4, aes(immigration, estimate)) + geom_line(colour = "#d11141") +
  geom_ribbon(aes(ymax = (conf.high), ymin = (conf.low)), alpha = 0.2, fill = "#d11141") +
  geom_rug(data = df, aes(x = immigration), inherit.aes = FALSE, 
           alpha = 0.4, size = 0.3, sides = "b", color = "grey50") + 
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 0), color = "#2111d1", linetype = "dashed") +
  xlab("Pre-treatment attitudes towards immigrants") +
  ylab("") +
  theme_minimal() +
  my_theme +
  ylim(-1, 2)

ate_t4

## export
pdf("joined_integ.pdf")
plot <- pred_t3 / ate_t3 - pred_t4 / ate_t4 +
  plot_annotation(
#    title = "Conditional average treatment effect on support for stricter integration policies",
    subtitle = "
                  T3 = democratic politican x value threat condition
                  T4 = democratic politican x safety threat condition
                  ",
    caption = "
                  Note: The x-axis represents pre-treatment attitudes towards immigrants with lower values representing negative attitudes
                  towards immigrants and higher numbers representing positive attitudes towards immigrants.",
    theme = theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8),
      plot.subtitle = element_text(size = 8)
    )
  )
print(plot)
dev.off()
plot

# Figure 6

## title: Mean levels of support for stricter integration demands
## outcome: stricter integration demands
## treatments: all treatments t1-t4

# dark blue = anti-immigration respondents; light blue = pro-immigration respondents
col2 <- c("#0818A8", "#8e99fa")

plot2 <- plot_predictions(m_integ, by = c("treatment", "immigration_dichot")) +
  ylab("Support for stricter integration policies ") +
  xlab("Treatment condition") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t = 20))) +
  my_theme_2 +
  scale_x_discrete(labels = c("control" = "Control", 
                              "t1" = "T1:\nRepublican\n× Value threat", 
                              "t2" = "T2:\nRepublican\n× Safety threat", 
                              "t3" = "T3:\nDemocratic\n× Value threat", 
                              "t4" = "T4:\nDemocratic\n× Safety threat")) +
  scale_color_manual(values = col2) +
  geom_bracket(
    xmin = c("control", "control", "control", "control"),
    xmax = c("t1", "t2", "t3", "t4"),
    y.position = c(4.3, 4.1, 3.9, 3.7),
    label = c(".56 ***", ".62 ***", ".67 ***", ".83 ***"),
    tip.length = -0.02,
    color = "slategrey",
    label.size = 3,
    # Adjust label size
    vjust = 1,
    # Adjust vertical position of labels
    hjust = .5           # Adjust horizontal position of labels
  ) +
  annotate(geom = "text", x = 1.5, y = 8.3, size = 3.5, color = "#0818A8",
           fontface = 2, label = "Anti-immigration respondents") +
  annotate(geom = "text", x = 1.5, y = 6.3, size = 3.5, color = "#8e99fa",
           fontface = 2, label = "Pro-immigration respondents") +
  theme(legend.position = "none", linetype = "dashed")

plot2

pdf("int_attitudes.pdf")
plot <- plot2 +
  plot_annotation(#title = "Mean level of support for stricter integration policies",
                  theme = theme(plot.title = element_text(size = 10, face =
                                                            "bold")))
print(plot)
dev.off()

plot

# Figure 7

## title: Mean support levels for gender policies and stricter integration policies by gender
## outcome:  gender policies & stricter integration demands
## treatments: all treatments t1-t4

col3 <- c("Men" = "#FF0000",
          "Women, non-binary persons and other gender identities"  = "#770737")
tmp <- df %>%
  mutate(
    immigration_dichot = recode(immigration_dichot, "0" = "Anti-immigration", "1" = "Pro-immigration"),
    Gender = recode(gender_collapsed, "Male" = "Men", "Other" = "Women, non-binary persons and other gender identities")
  )

## Upper panel: outcome 1 gender policies
m_gc <- lm (genderpolicies ~ treatment * immigration * gender_collapsed,
         data = tmp)
summary(m_gc)

plot1 <- plot_predictions(m_gc, by = c("treatment", "Gender", "immigration_dichot")) +
  ylab("Support for gender policies ") +
  xlab(" ") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35, hjust = 1, size = 7)) +
  my_theme +
  scale_color_manual(values = col3) +
  scale_x_discrete(labels = c("control" = "Control", 
                              "t1" = "T1:\nRepublican x\n Value threat", 
                              "t2" = "T2:\nRepublican x\n Safety threat", 
                              "t3" = "T3:\nDemocratic x\n Value threat", 
                              "t4" = "T4:\nDemocratic x\n Safety threat")) +
  ylim(4.5, 8.7)

plot1

## Lower panel: outcome 2 stricter integration demands
plot2 <- plot_predictions(m_gc, by = c("treatment", "Gender", "immigration_dichot")) +
  ylab("Support for stricter integration policies ") +
  xlab(" ") +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(angle = 35, hjust = 1, size = 7)) +
  my_theme +
  scale_color_manual(values = col3) +
  scale_x_discrete(labels = c("control" = "Control", 
                              "t1" = "T1:\nRepublican x\n Value threat", 
                              "t2" = "T2:\nRepublican x\n Safety threat", 
                              "t3" = "T3:\nDemocratic x\n Value threat", 
                              "t4" = "T4:\nDemocratic x\n Safety threat")) +
  ylim(4, 8.7)

plot2

pdf("gender_attitudes.pdf")
plot <- plot1 / plot2 +
  plot_annotation(#title = "Mean support levels by gender",
                  theme = theme(plot.title = element_text(size = 10, face =
                                                        "bold")))
print(plot)
dev.off()

plot

## outcome 1: gender policies
predt3 <- predictions(m_gender, newdata = datagrid(
  "immigration" = 0:100,
  "treatment" = c("control", "t3")
))

predt3 <- predt3 %>%
  filter(immigration %in% c(10, 25)) %>%
  group_by(immigration) %>%
  mutate(ate = estimate - estimate[treatment == "control"]) %>%
  filter(treatment == "t3")
print(predt3)
# Info: set c(10,25,...) to the immigration attitude score you would like to look at

predt4 <- predictions(m, newdata = datagrid(
  "immigration" = 0:100,
  "treatment" = c("control", "t4")
))

predt4 <- predt4 %>%
  filter(immigration %in% c(10, 25)) %>%
  group_by(immigration) %>%
  mutate(ate = estimate - estimate[treatment == "control"]) %>%
  filter(treatment == "t4")
print(predt4)

df_control_anti <- subset(df_anti, df_anti$treatment == "control")
avg_predictions(m_gender, newdata = df_control_anti)

## outcome 2: stricter integration policies
pred <- predictions(m_integ, newdata = datagrid(
  "immigration" = 0:100,
  "treatment" = c("control", "t1")
))

print(pred)

pred2 <- avg_predictions(m_gender, newdata = datagrid(
  "immigration" = 0:49,
  "treatment" = c("control", "t1")
))

print(pred2)

df_control_pro <- subset(df_pro, df_pro$treatment == "control")
avg_predictions(m_integ, newdata = df_control_pro)

##############
# TWIST
##############

# select the variables of interest
vars_of_interest <- c("genderpolicies", "treatment", "party_collapsed", "immigration", "integration")

# filter the dataframe so there are no missing values on these variables
df_clean <- df[complete.cases(df[, vars_of_interest]), ]

# fit the original model on new data
m_og1 <- lm(genderpolicies ~ treatment * immigration, data = df_clean)

# fit the twist
m_twist1 <- lm(genderpolicies ~ treatment * party_collapsed + immigration, data = df_clean)
summary(m_twist1)
stargazer(m_twist1)

# perform an F test
anova_result1 <- anova(m_og1, m_twist1, test='F')
print(anova_result1)

# fit the original and twist models for integration policy
m_og2 <- lm(integration ~ treatment * immigration, data = df_clean)

m_twist2 <- lm(integration ~ treatment * party_collapsed + immigration, data = df_clean)
summary(m_twist2)
stargazer(m_twist2)

# perform an F test
anova_result2 <- anova(m_og2, m_twist2, test='F')
print(anova_result2)

# Recreating figure 4
# Outcome: gender policies
# Comparison: Republican vs Democrat

# define the colour vector
col_twist <- c(
  "Democratic"  = "#0818A8",   
  "Independent" = "slategrey", 
  "Republican"  = "red"       
)

pdf("support_gp_twist.pdf")
plot_twist1 <- plot_predictions(m_twist1, by = c("treatment", "party_collapsed")) +
  ylab("Support for gender policies") +
  xlab("Treatment condition") +
  theme_minimal() +
  my_theme_2 + 
  theme(legend.position = "none", 
        axis.title.x = element_text(margin = margin(t = 20))) +
  scale_color_manual(values = col_twist) +
  scale_x_discrete(labels = c("control" = "Control", 
                              "t1" = "T1:\nRepublican\n× Value threat", 
                              "t2" = "T2:\nRepublican\n× Safety threat", 
                              "t3" = "T3:\nDemocratic\n× Value threat", 
                              "t4" = "T4:\nDemocratic\n× Safety threat")) +
  annotate(geom = "text", x = 1.5, y = 9, size = 3.5, color = "#0818A8",
           fontface = 2, label = "Democrat") +
  annotate(geom = "text", x = 1.5, y = 6.5, size = 3.5, color = "red",
           fontface = 2, label = "Republican") +
  annotate(geom = "text", x = 1.5, y = 7.5, size = 3, color = "slategrey",
           fontface = 2, label = "Independent") +
  geom_bracket(xmin = c("control", "control"), xmax = c("t1", "t2"),
               y.position = c(4.8, 4.6), label = c(".65**", ".83 ***"), 
               tip.length = -0.02, color = "slategrey", label.size = 3)
print(plot_twist1)
dev.off()

plot_twist1

# Recreating figure 4
# Outcome: integration policies
# Comparison: Republican vs Democrat

pdf("support_ip_twist.pdf")
plot_twist2 <- plot_predictions(m_twist2, by = c("treatment", "party_collapsed")) +
  ylab("Support for stricter integration policies") +
  xlab("Treatment condition") +
  theme_minimal() +
  my_theme_2 + 
  theme(legend.position = "none", 
        axis.title.x = element_text(margin = margin(t = 20))) +
  scale_color_manual(values = col_twist) +
  scale_x_discrete(labels = c("control" = "Control", 
                              "t1" = "T1:\nRepublican\n× Value threat", 
                              "t2" = "T2:\nRepublican\n× Safety threat", 
                              "t3" = "T3:\nDemocratic\n× Value threat", 
                              "t4" = "T4:\nDemocratic\n× Safety threat")) +
  annotate(geom = "text", x = 1.5, y = 4, size = 3.5, color = "#0818A8",
           fontface = 2, label = "Democrat") +
  annotate(geom = "text", x = 1.5, y = 8, size = 3.5, color = "red",
           fontface = 2, label = "Republican") +
  annotate(geom = "text", x = 1.5, y = 6.5, size = 3, color = "slategrey",
           fontface = 2, label = "Independent") +
  geom_bracket(xmin = c("control", "control"), xmax = c("t1", "t2"),
               y.position = c(3.4, 3.2), label = c(".65**", ".83 ***"), 
               tip.length = -0.02, color = "slategrey", label.size = 3)
print(plot_twist2)
dev.off()

plot_twist2
