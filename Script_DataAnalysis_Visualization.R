
#----Read Packages--------------------------------------------------------------

  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(psych)
  library(ggpubr)
  library(tidyr)
  library(ragg)
  library(reshape2)
  library(gridExtra)


#----Call in .csv Files---------------------------------------------------------


data <- read_csv("CalculatedSurveys_IPRandHome.csv")

# response rate data for average FR/BR comparision by subject
data_rr <- read_csv("ResponseRatesBySubject.csv")

# response rate data aggregated across participants by cues
dataFR <- read.csv("ForwardResponses_ByParticipant.csv")
dataBR <- read.csv("BackwardResponses_ByParticipant.csv")

#----Alter Response Rate Data Frames for Later Analysis-------------------------

# remove group count from dataFR/BR
dataFR <- dataFR %>%
  select(-GroupCount)

dataBR <- dataBR %>% 
  select(-GroupCount)

# Multiply data frames by 100 to get %
dataFR[, 2:180] <- (dataFR[, 2:180] * 100)
dataBR[, 2:180] <- (dataBR[, 2:180] * 100)

#----Calculate Descriptive Stats------------------------------------------------

## IPR ##
d.summary.extended <- data %>%
  select(SUS_IPR_CalcScore, QUEST_IPR_CalcScore, IMI_IPR_Choice_CalcScore, IMI_IPR_Interest_CalcScore, IMI_IPR_Competence_CalcScore, IMI_IPR_Effort_CalcScore, IMI_IPR_Value_CalcScore, IMI_IPR_Pressure_CalcScore) %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname") %>%
  print()

d.summary <- d.summary.extended %>%
  select(var = rowname, min, q25=Q0.25, median, q75=Q0.75, max, mean, sd) %>%
  print()

## Home ##
d.summary.extended <- data %>%
  select(SUS_Home_CalcScore, QUEST_Home_CalcScore, IMI_Home_Choice_CalcScore, IMI_Home_Interest_CalcScore, IMI_Home_Competence_CalcScore, IMI_Home_Effort_CalcScore, IMI_Home_Value_CalcScore, IMI_Home_Pressure_CalcScore) %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname") %>%
  print()

d.summary <- d.summary.extended %>%
  select(var = rowname, min, q25=Q0.25, median, q75=Q0.75, max, mean, sd) %>%
  print()

#----Complete Survey Statistical Analyses (One-Tailed T-Test/mean CI)------------

# Call in the function that calculates CI and SEM
source("calculate_CI_SEM.R")

CL <- 0.9 # Change your desired confidence interval value here!

## IPR ##

#SUS
ttest_SUS_IPR <- t.test(data$SUS_IPR_CalcScore, mu = 68, alternative = 'greater')
CISEM_SUS_IPR <- calculate_CI_SEM(data$SUS_IPR_CalcScore, confidence_level = CL) 

  ttest_SUS_IPR
  CISEM_SUS_IPR


# QUEST
ttest_QUEST_IPR <- t.test(data$QUEST_IPR_CalcScore, mu = 4, alternative = 'greater')
CISEM_QUEST_IPR <- calculate_CI_SEM(data$QUEST_IPR_CalcScore, confidence_level = CL) 

  ttest_QUEST_IPR
  CISEM_QUEST_IPR


# IMI
ttest_IMI_Choice_IPR <- t.test(data$IMI_IPR_Choice_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Interest_IPR <- t.test(data$IMI_IPR_Interest_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Competence_IPR <- t.test(data$IMI_IPR_Competence_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Effort_IPR <- t.test(data$IMI_IPR_Effort_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Value_IPR <- t.test(data$IMI_IPR_Value_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Pressure_IPR <- t.test(data$IMI_IPR_Pressure_CalcScore, mu = 4, alternative = 'less')

CISEM_IMI_Choice_IPR <- calculate_CI_SEM(data$IMI_IPR_Choice_CalcScore, confidence_level = CL) 
CISEM_IMI_Interest_IPR <- calculate_CI_SEM(data$IMI_IPR_Interest_CalcScore, confidence_level = CL) 
CISEM_IMI_Competence_IPR <- calculate_CI_SEM(data$IMI_IPR_Competence_CalcScore, confidence_level = CL) 
CISEM_IMI_Effort_IPR <- calculate_CI_SEM(data$IMI_IPR_Effort_CalcScore, confidence_level = CL)
CISEM_IMI_Value_IPR <- calculate_CI_SEM(data$IMI_IPR_Value_CalcScore, confidence_level = CL) 
CISEM_IMI_Pressure_IPR <- calculate_CI_SEM(data$IMI_IPR_Pressure_CalcScore, confidence_level = CL) 

  ttest_IMI_Choice_IPR
  CISEM_IMI_Choice_IPR
  ttest_IMI_Interest_IPR
  CISEM_IMI_Interest_IPR
  ttest_IMI_Competence_IPR
  CISEM_IMI_Competence_IPR
  ttest_IMI_Effort_IPR
  CISEM_IMI_Effort_IPR
  ttest_IMI_Value_IPR
  CISEM_IMI_Value_IPR
  ttest_IMI_Pressure_IPR
  CISEM_IMI_Pressure_IPR


## HOME ##

# SUS 
ttest_SUS_Home <- t.test(data$SUS_Home_CalcScore, mu = 68, alternative = 'greater')
CISEM_SUS_Home <- calculate_CI_SEM(data$SUS_Home_CalcScore, confidence_level = CL) 

  ttest_SUS_Home
  CISEM_SUS_Home


# QUEST
ttest_QUEST_Home <- t.test(data$QUEST_Home_CalcScore, mu = 4, alternative = 'greater')
CISEM_QUEST_Home <- calculate_CI_SEM(data$QUEST_Home_CalcScore, confidence_level = CL) 

  ttest_QUEST_Home
  CISEM_QUEST_Home

# IMI
ttest_IMI_Choice_Home <- t.test(data$IMI_Home_Choice_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Interest_Home <- t.test(data$IMI_Home_Interest_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Competence_Home <- t.test(data$IMI_Home_Competence_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Effort_Home <- t.test(data$IMI_Home_Effort_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Value_Home <- t.test(data$IMI_Home_Value_CalcScore, mu = 4, alternative = 'greater')
ttest_IMI_Pressure_Home <- t.test(data$IMI_Home_Pressure_CalcScore, mu = 4, alternative = 'less')

CISEM_IMI_Choice_Home <- calculate_CI_SEM(data$IMI_Home_Choice_CalcScore, confidence_level = CL) 
CISEM_IMI_Interest_Home <- calculate_CI_SEM(data$IMI_Home_Interest_CalcScore, confidence_level = CL) 
CISEM_IMI_Competence_Home <- calculate_CI_SEM(data$IMI_Home_Competence_CalcScore, confidence_level = CL) 
CISEM_IMI_Effort_Home <- calculate_CI_SEM(data$IMI_Home_Effort_CalcScore, confidence_level = CL)
CISEM_IMI_Value_Home <- calculate_CI_SEM(data$IMI_Home_Value_CalcScore, confidence_level = CL) 
CISEM_IMI_Pressure_Home <- calculate_CI_SEM(data$IMI_Home_Pressure_CalcScore, confidence_level = CL) 

ttest_IMI_Choice_Home
CISEM_IMI_Choice_Home
ttest_IMI_Interest_Home
CISEM_IMI_Interest_Home
ttest_IMI_Competence_Home
CISEM_IMI_Competence_Home
ttest_IMI_Effort_Home
CISEM_IMI_Effort_Home
ttest_IMI_Value_Home
CISEM_IMI_Value_Home
ttest_IMI_Pressure_Home
CISEM_IMI_Pressure_Home



#----Calculate/Plot QUEST Category Ranking--------------------------------------

# Create data set representing (N) responses for each quest category

# IPR
quest.item.i.1 <- sum(data$survey_ipr_quest_items___1)
quest.item.i.2 <- sum(data$survey_ipr_quest_items___2)
quest.item.i.3 <- sum(data$survey_ipr_quest_items___3)
quest.item.i.4 <- sum(data$survey_ipr_quest_items___4)
quest.item.i.5 <- sum(data$survey_ipr_quest_items___5)
quest.item.i.6 <- sum(data$survey_ipr_quest_items___6)
quest.item.i.7 <- sum(data$survey_ipr_quest_items___7)
quest.item.i.8 <- sum(data$survey_ipr_quest_items___8)
quest.item.i.9 <- sum(data$survey_ipr_quest_items___9)
quest.item.i.10 <- sum(data$survey_ipr_quest_items___10)
quest.item.i.11 <- sum(data$survey_ipr_quest_items___11)
quest.item.i.12 <- sum(data$survey_ipr_quest_items___12)

# Home
quest.item.h.1 <- sum(data$survey_final_quest_items___1, na.rm=TRUE)
quest.item.h.2 <- sum(data$survey_final_quest_items___2, na.rm=TRUE)
quest.item.h.3 <- sum(data$survey_final_quest_items___3, na.rm=TRUE)
quest.item.h.4 <- sum(data$survey_final_quest_items___4, na.rm=TRUE)
quest.item.h.5 <- sum(data$survey_final_quest_items___5, na.rm=TRUE)
quest.item.h.6 <- sum(data$survey_final_quest_items___6, na.rm=TRUE)
quest.item.h.7 <- sum(data$survey_final_quest_items___7, na.rm=TRUE)
quest.item.h.8 <- sum(data$survey_final_quest_items___8, na.rm=TRUE)
quest.item.h.9 <- sum(data$survey_final_quest_items___9, na.rm=TRUE)
quest.item.h.10 <- sum(data$survey_final_quest_items___10, na.rm=TRUE)
quest.item.h.11 <- sum(data$survey_final_quest_items___11, na.rm=TRUE)
quest.item.h.12 <- sum(data$survey_final_quest_items___12, na.rm=TRUE)

# create a new data frame (data.precat) that combines the above calculated values
data.precat <- data.frame(
  group = c("Dimensions", "Weight", "Adjustments", "Safety", "Durability", "Ease of Use", "Comfort", "Effectiveness", "Equipment Delivery", "Troubleshooting", "Instructions", "Follow-up Communication"),
  IPR = c(quest.item.i.1, quest.item.i.2, quest.item.i.3, quest.item.i.4, quest.item.i.5, quest.item.i.6, quest.item.i.7, quest.item.i.8, quest.item.i.9, quest.item.i.10, quest.item.i.11, quest.item.i.12),
  Home = c(quest.item.h.1, quest.item.h.2, quest.item.h.3, quest.item.h.4, quest.item.h.5, quest.item.h.6, quest.item.h.7, quest.item.h.8, quest.item.h.9, quest.item.h.10, quest.item.h.11, quest.item.h.12)
)

# melt the dataframe into a more useable format for plotting
data.questcat <- melt(data.precat,id.vars='group', measure.vars=c('IPR','Home'))


## Graph the results in a bar plot that includes both home and IPR data

q12 <- ggplot(data.questcat, aes(x = value, y = reorder(group, value), fill=variable)) +
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values=c('#7fdfe1', '#f9918a'),
                    labels = c("IRF", "Home"))+
  theme_classic() +
  labs(title="QUEST\nFrequency") +
  scale_x_continuous(position = "top", expand = c(0,0)) +
  scale_y_discrete(labels = c(
    'Follow-up Communication' = "Follow-Up", 'Equipment Delivery' = "Delivery", 
    'Troubleshooting' = "Troubleshoot")) + 
  theme(legend.title=element_blank(),
        legend.text = element_text(size=18, family = "serif"),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.05),
        axis.ticks.y = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=16, family="serif"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size = 16, family="serif"),
        plot.title=element_text(size = 22, hjust=0.5, family="serif"),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm')) 

q12 # make sure this looks right!

# save as a SVG
q12_svg_filename <- paste("q12_", Sys.Date(), ".svg")

svg(q12_svg_filename,         # File name
    width = 12, height = 7, # Width and height in inches
    bg = "white"
)
q12
dev.off()

# save as a PDF
q12_pdf_filename <- paste("q12_", Sys.Date(), ".pdf")
pdf(q12_pdf_filename,         # File name
    width = 12, height = 7, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk"    # Color model (cmyk is required for most publications)
)
q12
dev.off()



#----Plot Survey Results--------------------------------------------------------

# Generate SUS dot plot with changes from IRF to Home

data.s <- melt(data,id.vars='study_id', measure.vars=c('SUS_IPR_CalcScore','SUS_Home_CalcScore'))
sus_plot <- ggplot(data.s, aes(x=variable, y=value)) +
  geom_dotplot(aes(), binaxis = "y", stackdir = "center", binpositions = "all", dotsize = 1.5, 
               color = "gray85", fill = "gray85", alpha = 0.85) +
  geom_line(aes(group = study_id), color = "gray85") +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(size=20, family="serif"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=18, family="serif"),
        plot.title=element_text(size=22, family="serif", hjust = 0.5),
        legend.position= "none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm')) +
  scale_x_discrete(labels = c(
    'SUS_IPR_CalcScore' = "IRF",
    'SUS_Home_CalcScore' = "Home")) +
  scale_y_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100), limits= c(8,100)) +
  labs(title="Usability\n(SUS)") +
  annotate("pointrange", x = 1, y = ttest_SUS_IPR$estimate[1], ymin = CISEM_SUS_IPR[3], ymax = CISEM_SUS_IPR[4], color = "#F8766D", size = 3, shape = 95, linewidth = 2) + 
  annotate("pointrange", x = 2, y = ttest_SUS_Home$estimate[1], ymin = CISEM_SUS_Home[3], ymax = CISEM_SUS_Home[4], color = "#00BFC4", size = 3, shape = 95, linewidth = 2) + 
  geom_hline(yintercept = 68, linetype = "longdash")

# save the SUS plot
s_svg_filename <- paste("sus_", Sys.Date(), ".svg")

svg(s_svg_filename,         # File name
    width = 7, height = 11, # Width and height in inches
    bg = "white")

sus_plot

dev.off()

s_pdf_filename <- paste("sus_", Sys.Date(), ".pdf")

pdf(s_pdf_filename,         # File name
    width = 7, height = 11, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

sus_plot

dev.off()    



# Generate QUEST dot plot with changes from IPR to Home

data.q <- melt(data,id.vars='study_id', measure.vars=c('QUEST_IPR_CalcScore','QUEST_Home_CalcScore'))
quest_plot <- ggplot(data.q, aes(x=variable, y=value)) +
  geom_dotplot(aes(), binaxis = "y", stackdir = "center", binpositions = "all", dotsize = 1.5, 
               color = "gray85", fill = "gray85", alpha = 0.85) +
  geom_line(aes(group = study_id), color = "gray85") +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(size=20, family="serif"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=18, family="serif"),
        plot.title=element_text(size=22, family="serif", hjust = 0.5),
        legend.position= "none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm')) +
  scale_y_continuous(breaks = c(1,2,3,4,5), limits= c(1,5)) +
  scale_x_discrete(labels = c(
    'QUEST_IPR_CalcScore' = "IRF",
    'QUEST_Home_CalcScore' = "Home")) +
  labs(title="Satisfaction\n(QUEST)") +
  annotate("pointrange", x = 1, y = ttest_QUEST_IPR$estimate[1], ymin = CISEM_QUEST_IPR[3], ymax = CISEM_QUEST_IPR[4], color = "#F8766D", size = 3, shape = 95, linewidth = 2) + 
  annotate("pointrange", x = 2, y = ttest_QUEST_Home$estimate[1], ymin = CISEM_QUEST_Home[3], ymax = CISEM_QUEST_Home[4], color = "#00BFC4", size = 3, shape = 95, linewidth = 2) + 
  geom_hline(yintercept = 3, linetype = "longdash")

# save the QUEST plot
q_svg_filename <- paste("quest_",Sys.Date(),".svg")

svg(q_svg_filename,         # File name
    width = 7, height = 11, # Width and height in inches
    bg = "white")

quest_plot

dev.off()

q_pdf_filename <- paste("quest_",Sys.Date(),".pdf")

pdf(q_pdf_filename,         # File name
    width = 7, height = 11, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

quest_plot

dev.off()    



# Generate IMI plot with all 6 cagetories

data.i1 <- melt(data,id.vars='study_id', measure.vars=c('IMI_IPR_Choice_CalcScore', 'IMI_IPR_Interest_CalcScore', 
                                                        'IMI_IPR_Competence_CalcScore', 'IMI_IPR_Effort_CalcScore',  'IMI_IPR_Value_CalcScore', 'IMI_IPR_Pressure_CalcScore'))

six_colors = c("#CD9600", "#7CAE00", "#00BE67", "#00A9FF", "#C77CFF", "#FF61CC")

i1 <- ggplot(data.i1, aes(x=variable, y=value)) +
  #geom_jitter(aes(x=variable, y=value, color=variable), size=7, width = 0.15, height=0, alpha=0.7) +
  geom_dotplot(aes(), binaxis = "y", stackdir = "center", binpositions = "all", dotsize = 0.75, 
               color = "gray85", fill = "gray85", alpha = 0.75) +
  #geom_line(aes(group = study_id), color = "gray85") +
  theme_classic() +
  theme(axis.title.y=element_text(size=20, family="serif", vjust = 3),
        axis.text.y=element_text(size=18, family="serif"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=20, family="serif", angle = 35, vjust = 1, hjust = 1),
        plot.title=element_text(size=22, family="serif", hjust = 0.5),
        legend.position= "none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm')) +
  scale_y_continuous(breaks = c(1,2,3,4,5,6,7), limits= c(1,7)) +
  scale_x_discrete(labels = c(
    'IMI_IPR_Choice_CalcScore' = "Choice",
    'IMI_IPR_Interest_CalcScore' = "Interest",
    'IMI_IPR_Competence_CalcScore' = "Competence",
    'IMI_IPR_Effort_CalcScore' = "Effort",
    'IMI_IPR_Value_CalcScore' = "Value",
    'IMI_IPR_Pressure_CalcScore' = "Pressure")) +
  labs(title="Intrinsic Motivation\n(IMI)", y="Survey Score") +
  annotate("pointrange", x = 1, y = ttest_IMI_Choice_IPR$estimate[1], ymin = CISEM_IMI_Choice_IPR[3], ymax = CISEM_IMI_Choice_IPR[4], color = six_colors[1], size = 3, shape = 95, linewidth = 2) + 
  annotate("pointrange", x = 2, y = ttest_IMI_Interest_IPR$estimate[1], ymin = CISEM_IMI_Interest_IPR[3], ymax = CISEM_IMI_Interest_IPR[4], color = six_colors[2], size = 3, shape = 95, linewidth = 2) + 
  annotate("pointrange", x = 3, y = ttest_IMI_Competence_IPR$estimate[1], ymin = CISEM_IMI_Competence_IPR[3], ymax = CISEM_IMI_Competence_IPR[4], color = six_colors[3], size = 3, shape = 95, linewidth = 2) + 
  annotate("pointrange", x = 4, y = ttest_IMI_Effort_IPR$estimate[1], ymin = CISEM_IMI_Effort_IPR[3], ymax = CISEM_IMI_Effort_IPR[4], color = six_colors[4], size = 3, shape = 95, linewidth = 2) + 
  annotate("pointrange", x = 5, y = ttest_IMI_Value_IPR$estimate[1], ymin = CISEM_IMI_Value_IPR[3], ymax = CISEM_IMI_Value_IPR[4], color = six_colors[5], size = 3, shape = 95, linewidth = 2) + 
  annotate("pointrange", x = 6, y = ttest_IMI_Pressure_IPR$estimate[1], ymin = CISEM_IMI_Pressure_IPR[4], ymax = CISEM_IMI_Pressure_IPR[3], color = six_colors[6], size = 3, shape = 95, linewidth = 2) + 
  geom_hline(yintercept = 4, linetype = "longdash")


# save the IMI plot 
imi_ipr_svg_filename <- paste("imi_ipr_",Sys.Date(),".svg")

svg(imi_ipr_svg_filename,         # File name
    width = 15.5, height = 12, # Width and height in inches
    bg = "white")
i1

dev.off()

imi_ipr_pdf_filename <- paste("imi_ipr_",Sys.Date(),".pdf")

pdf(imi_ipr_pdf_filename,         # File name
    width = 15.5, height = 12, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

i1

dev.off() 



## Combine the plots into a single figure

combo_surveys <- ggarrange(
  ggarrange(i1, sus_plot, quest_plot, align="h", widths = c(5.5, 3, 2.9),
            ncol = 3))
combo_surveys

combo_svg_filename <- paste("combo_surveys_",Sys.Date(),".svg")

svg(combo_svg_filename,         # File name
    width = 18, height = 7, # Width and height in inches
    bg = "white")
combo_surveys

dev.off()

combo_pdf_filename <- paste("combo_surveys_",Sys.Date(),".pdf")

pdf(combo_pdf_filename,         # File name
    width = 18, height = 9, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

combo_surveys

dev.off()  



#----Plot Response Rate Results-------------------------------------------------

# Plot response rates by participant (averaged across cues/days)

data.response <- melt(data_rr,id.vars='Workspace_labels', measure.vars=c('mean_PreCue_RR', 'mean_PostCue_RR'))

# Complete statistical analysis

ttest_rr_PrePost <- t.test(data_rr$mean_PostCue_RR, data_rr$mean_PreCue_RR, paired = TRUE)
ttest_rr_PrePost


# Dotplot style with SEM bars:

# calculate SEM for plotting
summary_response_data <- data.response %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), 
            se_value = (sd(value, na.rm = TRUE) / sqrt(n()))
  )

# create short-named variables calling previous calculations
PreCueMean = summary_response_data$mean_value[1]
PostCueMean = summary_response_data$mean_value[2]
PreCueSEM1 = (PreCueMean - summary_response_data$se_value[1])
PreCueSEM2 = (PreCueMean + summary_response_data$se_value[1])
PostCueSEM1 = (PostCueMean - summary_response_data$se_value[2])
PostCueSEM2 = (PostCueMean + summary_response_data$se_value[2])

# plot
rr2 <- ggplot(data.response, aes(x = variable, y = value, group = Workspace_labels, color = variable)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_line(color = 'gray') + 
  geom_point(aes(x = 0.9, y = PreCueMean), color = "black", size = 4, shape = 19) + # PreCue Mean
  geom_point(aes(x = 2.1, y = PostCueMean), color = "black", size = 4, shape = 19) + # PostCue Mean
  # SEM lines/endpoints
  geom_point(aes(x = 0.9, y = PreCueSEM1), color = "black", size = 6, shape = 95) + # PreCue Top
  geom_point(aes(x = 0.9, y = PreCueSEM2), color = "black", size = 6, shape = 95) + # PreCue Bottom
  geom_point(aes(x = 2.1, y = PostCueSEM1), color = "black", size = 6, shape = 95) + # PostCue Top
  geom_point(aes(x = 2.1, y = PostCueSEM2), color = "black", size = 6, shape = 95) + # PostCue Bottom
  geom_segment(aes(x = 0.9, xend = 0.9, y = PreCueSEM1, yend = PreCueSEM2), color = "black", size = 1) +
  geom_segment(aes(x = 2.1, xend = 2.1, y = PostCueSEM1, yend = PostCueSEM2), color = "black", size = 1) +
  # Style
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits= c(0,100)) +
  scale_x_discrete(labels = c('Pre-Cue', 'Post-Cue')) + 
  theme_classic() + 
  theme(axis.title.y=element_text(size=20, family="serif", vjust = 3),
        axis.text.y=element_text(size=18, family="serif"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=20, family="serif", angle = 0),
        plot.title=element_text(size=22, family="serif", hjust = 0.5),
        legend.position= "none",
        plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm')) +
  labs(y = "% of Time Movement Detected") 


# Plot forward/backward response rates by cue (averaged across participants)

# Calculate the means and SEM
FR_cue_averages <- colMeans(dataFR[, 2:180])
n_FR <- nrow(dataFR)
FR_cue_sem <- apply(dataFR[, 2:180], 2, function(x) sd(x) / sqrt(n_FR))

BR_cue_averages <- colMeans(dataBR[, 2:180])
n_BR <- nrow(dataBR)
BR_cue_sem <- apply(dataBR[, 2:180], 2, function(x) sd(x) / sqrt(n_BR))

# Create new data frames for easier plotting
time_points <- c(1:179)

data_FR_stats <- data.frame(Cue = time_points, Average = FR_cue_averages, SEM = FR_cue_sem, Category = "FR")
data_BR_stats <- data.frame(Cue = time_points, Average = BR_cue_averages, SEM = BR_cue_sem, Category = "BR")

# Combine the data frames
combined_df <- rbind(data_FR_stats, data_BR_stats)

# plot
FR_color <- "#00BFC4"
BR_color <- "#F8766D"

rrcues<- ggplot(combined_df, aes(x = Cue, y = Average, color = Category, fill = Category)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = Average - SEM, ymax = Average + SEM), alpha = 0.2, color = NA) +
  labs(x= "10 Cues") +
  scale_color_manual(values = c("BR" = BR_color, "FR" = FR_color), labels = c("Pre-Cue", "Post-Cue")) +
  scale_fill_manual(values = c("BR" = BR_color, "FR" = FR_color), labels = c("Pre-Cue", "Post-Cue")) +
  theme_classic() +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits= c(0,100)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180), limits = c(0,180)) + 
  theme_classic() + 
  theme(axis.title.x=element_text(size=20, family="serif"),
        axis.text.y=element_text(size=18, family="serif"),
        axis.text.x=element_text(size=20, family="serif", angle = 0),
        plot.title=element_text(size=22, family="serif", hjust = 0.5),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.5), 'cm')) +
  geom_vline(xintercept = 60, linetype = "solid", color = "black", size = 0.8) +
  geom_vline(xintercept = 120, linetype = "solid", color = "black", size = 0.8) 



# Combine response rates plots together

combo_responses <- ggarrange(
  ggarrange(rr2, rrcues, align="h", widths = c(2, 4),
            ncol = 2))
combo_responses

comboresponses_svg_filename <- paste("combo_responses_", Sys.Date(), ".svg")

svg(comboresponses_svg_filename,         # File name
    width = 18, height = 7, # Width and height in inches
    bg = "white")
combo_responses

dev.off()

comboresponses_pdf_filename <- paste("combo_responses_", Sys.Date(), ".pdf")

pdf(comboresponses_pdf_filename,         # File name
    width = 18, height = 9, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

combo_responses

dev.off()  

# NOTE: the plot was then altered in Inkscape to reduce ink:
    # one shared y-axis labels in the middle of the two plots
    # removal of x-axis labels for # of cues, use of single line marking 10 cues instead
    # removal of legend with Pre-Cue and Post-Cue labels added in better sized font for 180 cue figure
    # Session number label added to top of 180 cue figure


#----Motor and Cognition Statistical Analysis (Appendix A)---------------------------

# Calculate Fugl Meyer Motor Score
data$fm_motor <- data$baseline_fm_total - data$baseline_fm_h_total

# separate motor scores into higher-functioning and lower-functioning groups
FM_high <- subset(data, fm_motor >42) # 42 and below representing moderate-severe, above 42 being mild impairments
FM_low <- subset(data, fm_motor <=42)
FM_high_n <- length(FM_high$fm_motor) # number of participants in high functioning group
FM_low_n <- length(FM_low$fm_motor) # number of participants in low functioning group

# separate cognitive scores into normal and impaired groups
Moca_high <-subset(data, elig_moca_total >= 26)
Moca_low <- subset(data, elig_moca_total < 26)
Moca_high_n <- length(Moca_high$elig_moca_total) # number of participants in high functioning group
Moca_low_n <- length(Moca_low$elig_moca_total) # number of participants in low functioning group


# Motor Function: Conduct Independent T-Tests based on Fugl Meyer Scores

#  SUS
t.test(FM_high$SUS_IPR_CalcScore, FM_low$SUS_IPR_CalcScore, alternative = "two.sided", var.equal = FALSE)

# QUEST
t.test(FM_high$QUEST_IPR_CalcScore, FM_low$QUEST_IPR_CalcScore, alternative = "two.sided", var.equal = FALSE)

# IMI
t.test(FM_high$IMI_IPR_Pressure_CalcScore, FM_low$IMI_IPR_Pressure_CalcScore, alternative = "two.sided", var.equal = FALSE)
t.test(FM_high$IMI_IPR_Effort_CalcScore, FM_low$IMI_IPR_Effort_CalcScore, alternative = "two.sided", var.equal = FALSE)
t.test(FM_high$IMI_IPR_Value_CalcScore, FM_low$IMI_IPR_Value_CalcScore, alternative = "two.sided", var.equal = FALSE)
t.test(FM_high$IMI_IPR_Competence_CalcScore, FM_low$IMI_IPR_Competence_CalcScore, alternative = "two.sided", var.equal = FALSE) ## difference but both competent
t.test(FM_high$IMI_IPR_Choice_CalcScore, FM_low$IMI_IPR_Choice_CalcScore, alternative = "two.sided", var.equal = FALSE)


# Cognition: Conduct Independent T-Tests based on MoCA Scores

# SUS 
t.test(Moca_high$SUS_IPR_CalcScore, Moca_low$SUS_IPR_CalcScore, alternative = "two.sided", var.equal = FALSE)

# QUEST
t.test(Moca_high$QUEST_IPR_CalcScore, Moca_low$QUEST_IPR_CalcScore, alternative = "two.sided", var.equal = FALSE)

# IMI
t.test(Moca_high$IMI_IPR_Pressure_CalcScore, Moca_low$IMI_IPR_Pressure_CalcScore, alternative = "two.sided", var.equal = FALSE)
t.test(Moca_high$IMI_IPR_Effort_CalcScore, Moca_low$IMI_IPR_Effort_CalcScore, alternative = "two.sided", var.equal = FALSE)
t.test(Moca_high$IMI_IPR_Value_CalcScore, Moca_low$IMI_IPR_Value_CalcScore, alternative = "two.sided", var.equal = FALSE)
t.test(Moca_high$IMI_IPR_Competence_CalcScore, Moca_low$IMI_IPR_Competence_CalcScore, alternative = "two.sided", var.equal = FALSE) ## difference but both competent
t.test(Moca_high$IMI_IPR_Choice_CalcScore, Moca_low$IMI_IPR_Choice_CalcScore, alternative = "two.sided", var.equal = FALSE)

