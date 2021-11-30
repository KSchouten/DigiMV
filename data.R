library(tidyverse)
library(plotly)

#' Read the data as a tibble
#'
#' @return A tibble with selected columns
read_data <- function(){
  
  # Read data from Excel version of the file, which is a lot faster than reading the ODS file with read_ods()
  data <- readxl::read_excel("data/DigiMV2020_20211020.xlsx", sheet = 2, col_types = "text")
  
  
  # Small orgs: sum of org. 
  finance_columns_small <- c("org_income_current" = "jeuRopbr990_Micro_jeuRopbr990_Micro", # "jeu10800_Micro_jeu10800_Micro",
                             "org_income_last" = "jeu10800_Micro_jeu10801_Micro",
                             "personnel_costs_current" = "jeu13251_Micro_jeu13251_Micro",
                             "personnel_costs_last" =  "jeu13251_Micro_jeu13252_Micro")
  
  # Large orgs: sum of org. income and sum of personnel costs
  finance_columns_large <- c("org_income_current" = "jeu10420_jeu10420", # "jeu10800_jeu10800", 
                             "org_income_last" = "jeu10800_jeu10801", 
                             "personnel_costs_current" = "jeu10900_jeu10900", 
                             "personnel_costs_last" = "jeu10900_jeu10901")
  
  # Personnel info
  personnel_columns <- c("total_persons" = "qPersTotTot_AantalPers",
                         "total_fte" = "qPersTotTot_AantalFte", 
                         "total_persons_care" = "qPersTotZorgTot_AantalPers",
                         "total_fte_care" = "qPersTotZorgTot_AantalFte", 
                         "extra_trainees" = "qPersTotStag_AantalPers", 
                         "extra_volunteers" = "qPersTotVrij_AantalPers")
  # Patient info
  patient_columns <- c("unique_patients_start_current" = "qap10111uniek_qap10111uniek", 
                       "unique_patients_added" = "qap10101uniek_qap10101uniek", 
                      "unique_patients_left" = "qap10501uniek_qap10501uniek")

  # Generic columns
  generic_columns <- c("code" = "Code", "name" = "Name", "type" = "entMain_RechtsVorm_ent_RechtsVorm",
                       personnel_columns, patient_columns)
  
  # Combine small and large organizations
  selected_data <- bind_rows(
    data %>% select(all_of(generic_columns), 
                    all_of(finance_columns_small), 
                    filter = Q_TypeMicroJa_Ans_TypeMicroJa) %>% filter(filter == "ja") %>%
      mutate(micro = TRUE),
    data %>% select(all_of(generic_columns),
                    all_of(finance_columns_large), 
                    filter = Q_TypeMicroNee_Ans_TypeMicroNee) %>% filter(filter == "ja") %>%
      mutate(micro = FALSE)
  ) %>% select(-filter) %>%
    # Organizations of type 'Stichting' are filtered out for now
    filter(type!= "Stichting") %>%
    mutate(across(total_persons:personnel_costs_last, as.numeric))
  
  # TODO: 98 rows are excluded from selected_data: these might be non-profits, stichtingen?
  data %>% anti_join(selected_data, by = c("Code" = "code"))
  
  selected_data
}

# Income vs personnel costs - plot
#' A static income vs personnel costs plot
#'
#' Colors points based on whether it's a micro or non-micro organization
#' 
#' @param selected_data The dataset from read_data()
#'
#' @return A ggplot
plot_income_vs_personnel_costs <- function(selected_data){
  selected_data %>% filter(personnel_costs_current > 0 & org_income_current > 0) %>%
    ggplot() +
    geom_point(aes(x = log(personnel_costs_current), y = log(org_income_current), color = micro)) +
    theme_minimal()
}


#' Compute income vs. personnel costs outliers
#' 
#' Prepares data for compute_outliers
#'
#' @param selected_data The dataset from read_data()
#'
#' @return A modified version of selected_data
prepare_income_vs_personnel_costs <- function(selected_data){
  personnel_costs_outliers_data <- selected_data %>% 
    filter(org_income_current > 0 & personnel_costs_current > 0)  %>%
    mutate(log_income = log(org_income_current),
           log_costs = log(personnel_costs_current)) %>% 
    mutate(x = log_costs) 
}

#' Income vs. FTE outlier plot
#' 
#' Prepares data for compute_outliers
#'
#' @param selected_data The dataset from read_data()
#'
#' @return A modified version of selected_data
prepare_income_vs_fte <- function(selected_data){
  fte_outliers_data <- selected_data %>% 
    mutate(log_income = log(org_income_current),
           log_fte = log(total_fte)) %>%
    filter(org_income_current > 0 & total_fte > 0) %>%
    mutate(x = log_fte)
  
}


#' Income vs. # patients outlier plot
#' 
#' Prepares data for compute_outliers
#'
#' @param selected_data The dataset from read_data()
#'
#' @return A modified version of selected_data
prepare_income_vs_patients <- function(selected_data){
  
  patients_outliers_data <- selected_data %>% 
    mutate(patients_max = unique_patients_start_current + unique_patients_added,
           log_income = log(org_income_current),
           log_patients = log(patients_max)) %>%
    filter(org_income_current > 0 & patients_max > 0) %>%
    mutate(x = log_patients) 
  
}

#' Compute the outliers on the data
#'
#' @param data The dataset with at least the log_income column
#'
#' @return The data with additional model_income and outlier columns
compute_outliers <- function(data){
  # This is the basic geom_smooth model
  smoothmod <- mgcv::gam(formula = log_income ~ s(x, bs = "cs"), data = data)
  
  data %>% mutate(model_income = predict(smoothmod, .),
                  res_income = log_income - model_income) %>%
    mutate(mean_income = mean(res_income),
           max_sd_income = mean_income + 2*sd(res_income),
           outlier = res_income > max_sd_income)
}


#' Create a plotly interactive outlier plot
#'
#' @param data The dataset, incl. model_income for the smooth line, and log_income for the y axis
#' @param ... All specific aesthetic info for the main geom_point (x variable plus addditional values for the hover box)
#' 
#'
#' @return A plotly plot object
create_outlier_plot <- function(data, ...){
  plot <- data %>% ggplot() +
    geom_point(aes(y = log_income, color = outlier, name = name, code = code,
                   org_income_current = org_income_current, ...),
               position = "jitter") +
    geom_point(aes(x, model_income), color = "blue") +
    theme_minimal()
  
  ggplotly(plot) 
}






