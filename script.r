##### Codebase for Reading in Sensor Data #####

#### Load Packages ####
library(tidyverse)
library(janitor)
library(lubridate)
library(openxlsx)
library(readxl)
library(weathermetrics)
# For debugging rm(list=ls())

#### Create object with CSV for each new file you want to run ###
#### person_location <- read_csv("input_files/raw_sensor_data/NAME_OF_FILE.csv")

stephanie <- read_csv("input_files/raw_sensor_data/20190730_BC29_Stephanie.csv")

#### Build the function to ingest, clean, analyze, build graphs, write out excel files and graphs. 
process_sensors <- function(person_location) {
    # create temporary object with clean column names
    temp <<- clean_names(person_location) 
    # parse date objects and remove unneeded columns
    temp <<- temp %>%  
      # select(-x8,-x9,-x10,-x11) %>%
      mutate(date = mdy(date)) %>%
      mutate(month = month(date)) %>%
      mutate(day = day(date)) %>%
      mutate(hour = hour(time)) %>%
      mutate(minute = minute(time)) %>%
      #mutate(temperature = temperature_f) %>%
      select(date,time, month, day, hour, minute, temperature, humidity) %>%
      mutate(heat_index = heat.index(t=temperature, rh=humidity, temperature.metric = "fahrenheit")) 
    # calculate average temperature, humidity, heat index by month for the sensor, and store it as temporary object 
    temp_humidity_heat_index_month <<- temp %>%
      group_by(month) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      )
    # calculate average temperature by day for the sensor, and store it as temporary object 
    temp_humidity_heat_index_day <<- temp %>%
      mutate(day = make_datetime(year = 2019, month = month, day = day)) %>%
      group_by(day) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      ) %>%
      select(day, mean_temp, mean_humidity, mean_heat_index)
    # calculate average temperature by hour for the sensor, and store it as temporary object 
    temp_humidity_heat_index_hour <<- temp %>%
      mutate(hour = make_datetime(year = 2019, month = month, day = day, hour = hour)) %>%
      group_by(hour) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      ) %>%
      select(hour, mean_temp, mean_humidity, mean_heat_index)
    # calculate average temperature by hour for the sensor, and store it as temporary object
    temp_humidity_heat_index_minute <<- temp %>%
      mutate(minute = make_datetime(year = 2019, month = month, day = day, hour = hour, min = minute)) %>%
      group_by(minute) %>%
      summarise(mean_temp = round(mean(temperature),1),
                mean_humidity = round(mean(humidity),1),
                mean_heat_index = round(mean(heat_index),1)
      )
    # Create an empty Excel workbook 
    wb <- createWorkbook("workbook")
    # Create four empty Excel worksheets in Workbook
    addWorksheet(wb, "temp_humidity_heat_index_month")
    addWorksheet(wb, "temp_humidity_heat_index_day")
    addWorksheet(wb, "temp_humidity_heat_index_hour")
    addWorksheet(wb, "temp_humidity_heat_index_minute")
    # Write aggregated dataframes to corresponding worksheets
    writeData(wb, sheet = 1, temp_humidity_heat_index_month)
    writeData(wb, sheet = 2, temp_humidity_heat_index_day)
    writeData(wb, sheet = 3, temp_humidity_heat_index_hour)
    writeData(wb, sheet = 4, temp_humidity_heat_index_minute)
    # Create folder in output files 
    folder <- paste0("output_files/", deparse(substitute(person_location)))
    dir.create(path = folder)
    excel_filename <- paste0("output_files/", deparse(substitute(person_location)),"/",  deparse(substitute(person_location)), "_", date(now()), "temp_humidity_heat_index_means.xlsx")
    # Save worksheet to correct folder
    saveWorkbook(wb, excel_filename, overwrite = TRUE)
    #### Build graphs ####
    # Build monthly graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by month")
    temp_month_graph <- ggplot(data = temp_humidity_heat_index_month, aes(x = month, y = mean_temp)) +
      geom_bar(stat="identity") +
      ggtitle(chart_title) +
      xlab("Month Number") +
      ylab("Mean Temperature")
    plot(temp_month_graph)
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_month.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggsave(filename, plot = temp_month_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
    
      # Build daily graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by day")
    temp_day_graph <- ggplot() +
      geom_line(data = temp_humidity_heat_index_day, aes(x = day, y = mean_temp, colour="real temp")) +
      geom_line(data = temp_humidity_heat_index_day, aes(x = day, y = mean_heat_index, colour="heat index")) +
      scale_color_manual(
        values = c('real temp' = 'red',
          'heat index' = 'purple')
        ) +
      ggtitle(chart_title) +  
      xlab("Days") +
      ylab("Means") + 
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day")
    plot(temp_day_graph)    
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_day.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggsave(filename, plot = temp_day_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
    
    # Build hourly graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by hour")
    temp_hour_graph <- ggplot(data = temp_humidity_heat_index_hour, aes(x = hour, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") + 
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle=50,hjust=1))
    plot(temp_hour_graph)
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_hour.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggsave(filename, plot = temp_hour_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
    # Build minute graph temperature
    chart_title <- paste0(deparse(substitute(person_location)), " mean temperature by minute")
    temp_minute_graph <- ggplot(data =  temp_humidity_heat_index_minute, aes(x = minute, y = mean_temp)) +
      geom_line() +
      ggtitle(chart_title) +
      xlab("Hours") +
      ylab("Mean Temperature") + 
      scale_x_datetime(date_labels = "%b %d %H", date_breaks = "1 day") + 
      theme(axis.text.x = element_text(angle=50,hjust=1))
    plot(temp_minute_graph)
    filename <- paste0(deparse(substitute(person_location)),"_", date(now()), "_temp_means_minute.pdf")
    filepath <- paste0("output_files/", deparse(substitute(person_location)),"/")
    ggsave(filename, plot = temp_minute_graph, device = "pdf", path = filepath, scale = 1, width = 11, height = 8, units = "in", dpi = 300)
    
    
}

#### Excecute the function process_sensors, with the object name we just built
process_sensors(stephanie)


