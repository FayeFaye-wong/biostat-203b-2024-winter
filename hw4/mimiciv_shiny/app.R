library(bigrquery)
library(dbplyr)
library(DBI)
library(tidyverse)
library(forcats)
library(shiny)
library(ggplot2)
library(gridExtra)
library(knitr)
library(DT)

# path to the service account token 
satoken <- "./hw4/biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)
con_bq


mimic_icu_cohort <- readRDS("~/mimiciv_shiny/mimic_icu_cohort.rds")
mimic_icu_cohort$subject_id <- as.numeric(mimic_icu_cohort$subject_id)

# Prepare data for Patient Plot
labevents_tble <- tbl(con_bq, "labevents")
patients_tble <- tbl(con_bq, "patients")
admissions_tble <- tbl(con_bq, "admissions")
icustays_tble <- tbl(con_bq, "icustays")
transfers_tble <- tbl(con_bq, "transfers")
procedures_icd_tble <- tbl(con_bq, "procedures_icd")
diagnoses_icd_tble <- tbl(con_bq, "diagnoses_icd")
d_icd_procedures_tble <- tbl(con_bq, "d_icd_procedures")
d_icd_diagnoses_tble <- tbl(con_bq, "d_icd_diagnoses")

procedures_icd_tble <- procedures_icd_tble %>% 
  left_join(d_icd_procedures_tble, by = "icd_code") %>%
  select(subject_id, hadm_id, icd_code, chartdate, long_title)
diagnoses_icd_tble <- diagnoses_icd_tble %>% 
  left_join(d_icd_diagnoses_tble, by = "icd_code") %>%
  select(subject_id, hadm_id, icd_code, long_title)

procedures_icd_tble$chartdate <- as.POSIXct(procedures_icd_tble$chartdate)
procedures_icd_tble$long_title <- as.factor(procedures_icd_tble$long_title)
icustays_tble$intime <- as.POSIXct(icustays_tble$intime)
icustays_tble$outtime <- as.POSIXct(icustays_tble$outtime)
transfers_tble$intime <- as.POSIXct(transfers_tble$intime)
transfers_tble$outtime <- as.POSIXct(transfers_tble$outtime)
labevents_tble$charttime <- as.POSIXct(labevents_tble$charttime)
admissions_tble$admittime <- as.POSIXct(admissions_tble$admittime)
admissions_tble$dischtime <- as.POSIXct(admissions_tble$dischtime)

chartevents_tble <- tbl(con_bq, "chartevents")
d_items_tble <- tbl(con_bq, "d_items")

d_items_need <- d_items_tble %>%
  filter(linksto == "chartevents" & 
           abbreviation %in% c("HR", "NBPd", "NBPs", "RR", "Temperature F"))

chartevent_m <- chartevents_tble %>%
  left_join(d_items_need, by = "itemid") %>%  
  select(subject_id, stay_id,charttime, value, valuenum, abbreviation, itemid)
chartevent_m$charttime <- as.POSIXct(chartevent_m$charttime)
chartevent_m$abbreviation <- as.factor(chartevent_m$abbreviation)
chartevent_m$stay_id <- as.factor(chartevent_m$stay_id)

  


sql_query <- "
SELECT *
FROM transfers
WHERE careunit IS NOT NULL AND intime IS NOT NULL AND outtime IS NOT NULL
"
result <- dbGetQuery(con_bq, sql_query)

# Function Prepare
# Let the y-axis be the type of event (ADT, lab, procedure)
convert_to_text <- function(x) {
  y_labels <- c("  ","Procedure", "  "," Lab", "  ", "ADT")
  y_labels[ceiling(x * length(y_labels))]
}

# Make the legend more readable
truncate_legend <- function(x, max_chars) {
  ifelse(nchar(x) > max_chars, substr(x, 1, max_chars), x)
}
                                          


ui <- fluidPage(
  navbarPage("ICU Cohort Explorer",
             tabPanel("Summary",
                      selectInput("variable", "Select Variable to Explore", 
                                  choices = c("demographics", 
                                              "lab measurements", "vitals")),
                      plotOutput("plot1"),
                      dataTableOutput("table1")
             ),
             tabPanel("Patient Info",
                      selectInput("patient_id", "Select Patient ID", 
                                  choices = sort(unique(mimic_icu_cohort$subject_id))),
                      plotOutput("patient_info1"),
                      plotOutput("patient_info2")
             )
  )
)

server <- function(input, output) {
  # Render summary plots
  output$plot1 <- renderPlot({
    if (input$variable == "demographics") {
      gender_counts <- mimic_icu_cohort %>%
        count(gender)
      p1 <- ggplot(gender_counts, aes(x = "", y = n, fill = gender)) +
              geom_bar(stat = "identity", width = 1) + 
              coord_polar("y", start = 0) +
              labs(title = "Gender Distribution", fill = "Gender") +
              scale_fill_manual(values = c("lightpink1", "lightblue2")) +
              geom_text(aes(label = paste0(n, " (", round((n/sum(n))*100), 
                                           "%)")), 
                        position = position_stack(vjust = 0.5)) + theme_void()
      p2 <- ggplot(mimic_icu_cohort, aes(x = age)) +
              geom_histogram(binwidth = 5, 
                             fill = "lightblue2", color = "black") +
              labs(title = "Age Distribution", x = "Age", y = "Frequency") +
              theme_minimal()
      race_counts <- mimic_icu_cohort %>%
        count(race)
      p3 <- ggplot(race_counts, aes(x = "", y = n, fill = race)) +
        geom_bar(stat = "identity", width = 1) + 
        labs(title = "Race Distribution", fill = "Race") +
        geom_text(aes(label = paste0(n, " (", round((n/sum(n))*100), "%)")),
                  position = position_stack(vjust = 0.5)) + theme_void()
      insurance_counts <- mimic_icu_cohort %>%
        count(insurance)
      p4 <- ggplot(insurance_counts, aes(x = "", y = n, fill = insurance)) +
        coord_polar("y", start = 0) +
        geom_bar(stat = "identity", width = 1) + 
        labs(title = "Insurance Distribution", fill = "Insurance") +
        geom_text(aes(label = paste0(n, " (", round((n/sum(n))*100), "%)")),
                  position = position_stack(vjust = 0.5)) + theme_void()
      marital_status_counts <- mimic_icu_cohort %>%
        count(marital_status)
      p5 <- ggplot(marital_status_counts, aes(x = "", y = n, 
                                              fill = marital_status)) +
        geom_bar(stat = "identity", width = 1) + 
        labs(title = "Marital Status Distribution", fill = "Marital Status") +
        geom_text(aes(label = paste0(n, " (", round((n/sum(n))*100), "%)")),
                  position = position_stack(vjust = 0.5)) + theme_void()
      language_counts <- mimic_icu_cohort %>%
        count(language)
      p6 <- ggplot(language_counts, aes(x = "", y = n, fill = language)) +
        geom_bar(stat = "identity", width = 1) + 
        coord_polar("y", start = 0) +
        labs(title = "Language Distribution", fill = "Language") +
        geom_text(aes(label = paste0(n, " (", round((n/sum(n))*100), "%)")),
                  position = position_stack(vjust = 0.5)) + theme_void() 
      grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2) 
    }
    if (input$variable == "lab measurements") {
      p1 <- ggplot(mimic_icu_cohort, aes(x = Sodium)) +
        geom_histogram(binwidth = 2, fill = "lightblue2", color = "black") +
        labs(title = "Sodium Distribution", 
             x = "Sodium (mmol/L)", y = "Frequency") +
        theme_minimal()
      p2 <- ggplot(mimic_icu_cohort, aes(x = Potassium)) +
        geom_histogram(binwidth = 0.2, fill = "lightblue2", color = "black") +
        labs(title = "Potassium Distribution", 
             x = "Potassium (mmol/L)", y = "Frequency") +
        theme_minimal()
      p3 <- ggplot(mimic_icu_cohort, aes(x = Chloride)) +
        geom_histogram(binwidth = 2, fill = "lightblue2", color = "black") +
        labs(title = "Chloride Distribution", 
             x = "Chloride (mmol/L)", y = "Frequency") +
        theme_minimal()
      p4 <- ggplot(mimic_icu_cohort, aes(x = Glucose)) +
        geom_histogram(binwidth = 10, fill = "lightblue2", color = "black") +
        labs(title = "Glucose Distribution", 
             x = "Glucose (mg/dL)", y = "Frequency") +
        theme_minimal()
      p5 <- ggplot(mimic_icu_cohort, aes(x = Hematocrit)) +
        geom_histogram(binwidth = 3, fill = "lightblue2", color = "black") +
        labs(title = "Hematocrit Distribution", 
             x = "Hematocrit (%)", y = "Frequency") +
        theme_minimal()
      p6 <- ggplot(mimic_icu_cohort, aes(x = `White Blood Cells`)) +
        geom_histogram(binwidth = 3, fill = "lightblue2", color = "black") +
        labs(title = "White Blood Cells Distribution", 
             x = "White Blood Cells (cells/uL)", y = "Frequency") +
        theme_minimal() + xlim(0, 150)
      p7 <- ggplot(mimic_icu_cohort, aes(x = Creatinine)) +
        geom_histogram(binwidth = 0.2, fill = "lightblue2", color = "black") +
        labs(title = "Creatinine Distribution", 
             x = "Creatinine (mg/dL)", y = "Frequency") +
        theme_minimal()
      p8 <- ggplot(mimic_icu_cohort, aes(x = Bicarbonate)) +
        geom_histogram(binwidth = 2, fill = "lightblue2", color = "black") +
        labs(title = "Bicarbonate Distribution", 
             x = "Bicarbonate (mmol/L)", y = "Frequency") +
        theme_minimal()
      grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
    }
    if (input$variable == "vitals") {
      p1 <- ggplot(mimic_icu_cohort, aes(x = `Heart Rate`)) +
        geom_histogram(binwidth = 5, fill = "lightblue2", color = "black") +
        labs(title = "Heart Rate Distribution", 
             x = "Heart Rate (bpm)", y = "Frequency") +
        theme_minimal()
      p2 <- ggplot(mimic_icu_cohort, aes(
        x = `Non Invasive Blood Pressure systolic`)) +
        geom_histogram(binwidth = 5, fill = "lightblue2", color = "black") +
        labs(title = "Systolic Blood Pressure Distribution", 
             x = "Systolic Blood Pressure (mmHg)", y = "Frequency") +
        theme_minimal() + xlim(0, 300)
      p3 <- ggplot(mimic_icu_cohort, aes(
        x = `Non Invasive Blood Pressure diastolic`)) +
        geom_histogram(binwidth = 5, fill = "lightblue2", color = "black") +
        labs(title = "Diastolic Blood Pressure Distribution", 
             x = "Diastolic Blood Pressure (mmHg)", y = "Frequency") +
        theme_minimal() + xlim(0, 200)
      p4 <- ggplot(mimic_icu_cohort, aes(x = `Temperature Fahrenheit`)) +
        geom_histogram(binwidth = 0.5, fill = "lightblue2", color = "black") +
        labs(title = "Temperature Distribution", 
             x = "Temperature (Fahrenheit)", y = "Frequency") +
        theme_minimal() + xlim(70, 125)
      p5 <- ggplot(mimic_icu_cohort, aes(x = `Respiratory Rate`)) +
        geom_histogram(binwidth = 2, fill = "lightblue2", color = "black") +
        labs(title = "Respiratory Rate Distribution", 
             x = "Respiratory Rate (breaths/min)", y = "Frequency") +
        theme_minimal()
      
      grid.arrange(p1, p2, p3, p4, p5, nrow = 2)
        
    }
    
  })
  
  
  # Render summary tables
  output$table1 <- renderDataTable({
    if(input$variable == "demographics") {
      summary <- summary(mimic_icu_cohort$age)
      age_summary_df <- data.frame(
        Statistic = names(summary),
        Value = round(as.numeric(summary), 2)
      )
      return(datatable(age_summary_df, options = list(searching = FALSE, 
                                                      lengthChange = FALSE,
                                                      info = FALSE)))
    }
    if(input$variable == "lab measurements") {
      lab_summary <- mimic_icu_cohort %>%
        select(Sodium, Potassium, Chloride, Creatinine, Bicarbonate, 
               Glucose, Hematocrit, `White Blood Cells`) %>%
        summarize_all(list(mean = ~round(mean(., na.rm = TRUE), 2),
                           sd = ~round(sd(., na.rm = TRUE), 2),
                           min = ~round(min(., na.rm = TRUE), 2),
                           median = ~round(median(., na.rm = TRUE), 2),
                           max = ~round(max(., na.rm = TRUE), 2)))
      lab_summary_long <- lab_summary %>%
        pivot_longer(cols = everything(), 
                     names_to = c(".value", "variable"), 
                     names_sep = "_")
      
      return(datatable(lab_summary_long, options = list(searching = FALSE, 
                                                        lengthChange = FALSE,
                                                        info = FALSE)))
    }
    if(input$variable == "vitals") {
      vitals_summary <- mimic_icu_cohort %>%
        select(`Heart Rate`, `Non Invasive Blood Pressure systolic`, 
               `Non Invasive Blood Pressure diastolic`, 
               `Temperature Fahrenheit`, `Respiratory Rate`) %>%
        summarize_all(list(mean = ~round(mean(., na.rm = TRUE), 2),
                           sd = ~round(sd(., na.rm = TRUE), 2),
                           min = ~round(min(., na.rm = TRUE), 2),
                           median = ~round(median(., na.rm = TRUE), 2),
                           max = ~round(max(., na.rm = TRUE), 2)))
      vitals_summary_long <- vitals_summary %>%
        pivot_longer(cols = everything(), 
                     names_to = c(".value", "variable"), 
                     names_sep = "_")
      
      return(datatable(vitals_summary_long, options = list(searching = FALSE, 
                                                           lengthChange = FALSE,
                                                           info = FALSE)))
    }
  })  
    output$patient_info1 <- renderPlot({
      id <- input$patient_id
      id <- as.numeric(id)
      # Filter data for selected patient
      patient_transfers <- transfers_tble %>%
        filter(subject_id == id) %>%
        mutate(intime = as.POSIXct(intime), outtime = as.POSIXct(outtime)) %>%
        as_tibble()
      patient_diagnoses <- diagnoses_icd_tble %>%
        filter(subject_id == id) %>%
        as_tibble()
      patient_procedures <- procedures_icd_tble %>%
        filter(subject_id == id) %>%
        as_tibble()
      patient_labevents <- labevents_tble %>%
        filter(subject_id == id) %>%
        as_tibble()
      patient_admissions <- admissions_tble %>%
        filter(subject_id == id) %>%
        as_tibble()
      patients_info <- patients_tble %>%
        filter(subject_id == id) %>%
        as_tibble()
      icu_ccu_id <- patient_transfers %>%
        filter(grepl("ICU|CCU", careunit, ignore.case = TRUE)) %>%
        select(transfer_id)
      
      # Find the top three diagnoses
      title_freq <- table(patient_diagnoses$long_title)
      
      top_three_titles <- names(sort(title_freq, decreasing = TRUE)[1:3])
      
      subtitle <- sprintf("\n%s\n%s\n%s", top_three_titles[1], 
                          top_three_titles[2], top_three_titles[3])
      
      p1 <- ggplot() +
        geom_segment(data = patient_transfers[patient_transfers$transfer_id 
                                              %in% icu_ccu_id$transfer_id,], 
                     aes(x = intime, xend = outtime, 
                         color = careunit, y = 1, yend = 1), 
                     size = 3) +
        geom_segment(data = patient_transfers[!patient_transfers$transfer_id 
                                              %in% icu_ccu_id$transfer_id,],
                     aes(x = intime, xend = outtime, 
                         color = careunit, y = 1, yend = 1),
                     size = 1.5) + 
        geom_point(data = patient_labevents, aes(x = as.POSIXct(charttime), 
                                                 y = 0.6), shape = "x") +
        geom_point(data = patient_procedures, 
                   aes(x = as.POSIXct(chartdate), 
                       y = 0.2, shape = long_title)) +
        scale_x_datetime(labels = scales::date_format("%m-%d")) +
        scale_y_continuous(labels = convert_to_text) +
        labs(title = paste("Patient", patient_admissions$subject_id,
                           patients_info$gender,
                           patients_info$anchor_age,
                           patient_admissions$race),
             subtitle = subtitle, x=("Calendar Time"), y=(" "))+
        guides(linewidth = "none", 
               shape = guide_legend(ncol = 1, title = "Procedure"), 
               color = guide_legend(ncol  = 1, title = "Care Unit"))+
        theme_minimal()+
        theme(legend.position = "bottom")
      p1
    })
    output$patient_info2 <- renderPlot({
      id <- input$patient_id
      id <- as.numeric(id)
      chartevents_patient <- chartevent_m %>%
        filter(subject_id == id) %>%
        as_tibble()
      chartevents_patient <- na.omit(chartevents_patient)
      
      p2 <- 
        ggplot() +
        geom_line(data = chartevents_patient, 
                  aes(x = charttime, y = valuenum, 
                      group = interaction(stay_id, abbreviation), 
                      color = interaction(stay_id, abbreviation))) +
        geom_point(data = chartevents_patient, 
                   aes(x = charttime, y = valuenum, 
                       group = interaction(stay_id, abbreviation), 
                       color = interaction(stay_id, abbreviation))) +
        labs(title = paste("Stay ID:", chartevents_patient$stay_id),
             x = "Time",
             y = "Value") +
        facet_grid(cols = vars(stay_id), 
                   rows = vars(abbreviation), scales = "free") +
        guides(color = FALSE) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1),
              strip.background = element_rect(fill = "lightgray")) +
        scale_x_datetime(labels = scales::date_format("%m-%d %H:%M")) +
        scale_color_manual(values = c("orange", "orange", "yellow4", "yellow4", 
                                      "springgreen3",  "springgreen3", 
                                      "deepskyblue","deepskyblue", 
                                      "magenta2", "magenta2"))
      p2
    })
}
    
  

# Run the application
shinyApp(ui = ui, server = server)