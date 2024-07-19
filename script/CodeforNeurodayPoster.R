#Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(imputeTS)
library(gt)
library(scales)
library(stringr)

## read in the data
study_data <- read.csv("study_data2.csv") 
diseasemodel_data <- read_csv("diseasemodel_data2.csv") 
outcome_data <- read_csv("outcome_data2.csv") 

ltp_studies <- outcome_data %>%
  filter(QuestionId == "b96a42a9-0651-4389-bdfd-0e174fd924d5") %>% #get LTP studies ONLY for poster
  select(StudyId)

study_data <- study_data %>%
  filter(StudyId %in% ltp_studies$StudyId)

diseasemodel_data <- diseasemodel_data %>%
  filter(StudyId %in% ltp_studies$StudyId)

outcome_data <- outcome_data %>% 
  filter(StudyId %in% ltp_studies$StudyId)
  
  
# risk of bias
study_data <- study_data %>% 
  filter(Year>= 2018 & Year<= 2023, SystematicSearchName== "2020-2023 Update")

# data wrangling to get into usable form
study_quality_data <- study_data %>%
  select(QuestionId, Answer) %>% 
  mutate(study_quality_items = case_when(QuestionId == "972e9525-756c-4e74-a704-c0d5647a83ac"~"Blinding", 
                                          QuestionId == "4d244624-344e-4189-8f86-024be2613471"~"Conflict of Interest",
                                          QuestionId == "acd222cb-0239-4541-9661-fa1f7fd04676"~"Exclusion Criteria",
                                          QuestionId == "2bcd3117-dd39-4aa6-86fc-f6bf14ec457d"~"Sample Size Calculation", 
                                          QuestionId == "e5b2f8e1-96cd-4266-ae8c-6bb363dc7b9d"~"Welfare Committee Approval")) %>% 
  filter(!is.na(study_quality_items)) %>% 
  select(-QuestionId) %>% 
  mutate(IndicatorColumn = 1)


# data for all studies
overall_data <- study_quality_data %>% 
  select(-Answer) %>% 
  group_by(study_quality_items) %>% 
  summarise(overall_number= sum(IndicatorColumn))

# studeis where data was included
true_data <- study_quality_data %>% 
  filter(Answer == "True") %>% 
  select(-Answer) %>% 
  group_by(study_quality_items) %>% 
  summarise(true_number= sum(IndicatorColumn))  

# joining data and plotting
barplot_data <- overall_data %>% 
  left_join(true_data) %>% 
  mutate(percentage_of_studies = true_number/overall_number*100,
         across(where(is.numeric), ~round(., 2))) %>% 
  ggplot(aes(x=study_quality_items, y= percentage_of_studies))+
  geom_bar(stat="identity", fill="darkgoldenrod3")+
  geom_text(aes(label=percentage_of_studies), vjust=1.3, color="white", size=3.5)+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Studies (%) Reporting Different Study Quality Items")+
  labs(x = "Study Quality Items", y = "Included (%)")
  
barplot_data


## doughnut chart
model_data <- diseasemodel_data %>% 
  filter(Year>= 2018 & Year<= 2023, SystematicSearchName== "2020-2023 Update",
         QuestionId =="8255f1cf-1db1-4756-b77c-003df2860ed9") %>% 
  mutate(Count = 1, 
        Answer = str_replace_all(Answer,'APPswe', 'APPSwe'))%>% 
  select(Answer, Count) %>% 
  group_by(Answer) %>% 
  summarise(Count = sum(Count)) %>% 
  filter(Answer != "Other (please leave a comment)")

#plot data 
hsize <- 4
  
# Define the colors for each answer
color_palette <- c("#CF9E02","#e0ad02","#fcc203","#0D4781", "#1868b8", "#2683e0", "#3d97f2", "#72b5f7")

# Reorder the levels of the Answer factor so that the desired answers are first
model_data$Answer <- factor(model_data$Answer, levels = c("APP NL-G-F Knock-in", "PDGF-APPSw,Ind (line J9)", "Tg2576", "APPPS1", "5xFAD (C57BL6)", "5xFAD (B6SJL)", "APPSwe/PSEN1dE9",
                                                          "TgF344-AD (rat)"))

# Create the donut plot using the reordered levels and color palette
donut_plot <- model_data %>%
  mutate(x = hsize) %>%
  ggplot(aes(x = hsize, y = Count, fill = Answer)) +
  geom_col(color = "black") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 13,  color="white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = color_palette) +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.text=element_text(size=22)) +
  guides(fill = guide_legend(title = "AD Model"))


donut_plot

## Ephys Data 
outcome_data <- outcome_data %>% 
  filter(Year>= 2018 & Year<= 2023, SystematicSearchName== "2020-2023 Update")


# brain pathway data 
brain_pathway <- outcome_data %>% 
  select(StudyId, QuestionId, Answer) %>% 
  filter(QuestionId == "3d4cee7b-a194-4bb1-9440-b1d31054b729") %>% 
  mutate(ReportedStatus = case_when(Answer == "Schaffer collaterals (CA1)"~"Reported", 
                                    Answer == "Dorsal ganglion (DG)"~"Reported", 
                                    Answer == "Other (please leave a comment)"~"NotReported")) %>% 
  distinct() %>% 
  mutate(Count = 1,
         Outcome = "Brain Pathway") %>% 
  select(-StudyId, -Answer, - QuestionId) 


total_stimulations <- outcome_data %>% 
  select(StudyId, QuestionId, Answer) %>% 
  filter(QuestionId == "234b33c0-79ab-44a0-9b78-ef321ac3d2b5")
  


## Anesthetics and time left to recover after slicing data 
ana_slice_data <- study_data %>% 
  select(QuestionId, Answer) %>% 
  mutate(Outcome = case_when(QuestionId == "100f3582-43df-4b67-8573-303bd751eedf"~"Anesthesia",
         QuestionId =="19d7c978-90d8-4825-bdc3-661bacf7de81"~"Slice Recovery Time"),
         ReportedStatus = case_when(Answer == "NR"~"NotReported",
                                    Answer == "Not reported"~"NotReported",
                                    TRUE~"Reported"),
         Count = 1) %>% 
  filter(!is.na(Outcome)) %>% 
  select(-QuestionId, -Answer)

# gets other outcomes
reported_data <- outcome_data %>% 
  select(QuestionId,StudyId, Answer) %>% 
  mutate(Outcome = case_when(QuestionId == "234b33c0-79ab-44a0-9b78-ef321ac3d2b5"~"Total Stimulations***", 
                             QuestionId == "b96a42a9-0651-4389-bdfd-0e174fd924d5"~"Stimulation Type LTP**"),
         ReportedStatus = case_when(Answer == "NR"~"NotReported",
                                    TRUE~"Reported"), 
         Count = 1) %>%
  filter(!is.na(Outcome),
         !str_detect(Answer,"Other")) %>%
  distinct() %>% 
  select(-Answer, - QuestionId, -StudyId)


## To get number of experiments
no_of_studies_data <- outcome_data %>% 
  filter(QuestionId %in% c("234b33c0-79ab-44a0-9b78-ef321ac3d2b5","b96a42a9-0651-4389-bdfd-0e174fd924d5")) %>% 
  mutate(Outcome = case_when(QuestionId == "234b33c0-79ab-44a0-9b78-ef321ac3d2b5"~"Total Stimulations***", 
                             QuestionId == "b96a42a9-0651-4389-bdfd-0e174fd924d5"~"Stimulation Type LTP**"), 
         Count = 1) %>% 
  select(Outcome, Count) %>% 
  group_by(Outcome) %>% 
  summarise(NoExperiments = sum(Count)) %>% 
  cbind(Reported = 0, NotReported = 0)



## Combine Data
data_for_table <- reported_data %>% 
  full_join(ana_slice_data) %>% 
  full_join(brain_pathway) %>% 
  group_by(ReportedStatus, Outcome) %>% 
  summarise(Count = sum(Count)) %>% 
  filter(Outcome != "Total Stimulations***" | ReportedStatus != "NotReported")%>% 
  pivot_wider(names_from = ReportedStatus, values_from = Count) %>%
  mutate(NoExperiments =case_when(Outcome == "Brain Pathway"~66, 
                                  Outcome == "Anesthesia"~Reported+NotReported, 
                                  Outcome == "Slice Recovery Time"~Reported+NotReported))%>%
  na_replace(0) %>% 
  full_join(no_of_studies_data) %>% 
  group_by(Outcome) %>% 
  summarise(Reported = sum(Reported), 
            NotReported = sum(NotReported), 
            NoExperiments = sum(NoExperiments)) %>% 
  mutate(percentageofstudies = Reported/NoExperiments*100, 
         across(where(is.numeric), ~round(., 2)))


#Plot Table
ephys_table <- data_for_table %>%
  select(Outcome, percentageofstudies) %>%
  gt() %>%
  tab_header(title = md("**Reporting quality of electrophysiology measures**")) %>%
  cols_label(percentageofstudies = "% Experiments Reported*") %>%
  tab_source_note(source_note = "* Across 66 studies assessing LTP") %>%
  tab_source_note(source_note = "** 67 = experiments") %>%
  tab_source_note(source_note = "*** 73 = experiments") %>%
  tab_style(locations = cells_column_labels(columns = everything()),
            style = list(cell_borders(sides = "bottom", weight = px(3)),
                         cell_text(weight = "bold"),
                         cell_fill(color = "darkgoldenrod3", alpha = 0.9)))
ephys_table


