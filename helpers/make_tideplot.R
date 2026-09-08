#' -------------------------
#' Make some tide plots for a widget!
#' -------------------------

library(MarineTides)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)

make_tideplot <- function(start_date, offset = 5){
    
    end_date <- ymd(start_date) + offset 
    
tide <- tide_level(
    "Boston",
    start_date = start_date,
    end_date = end_date
) |>
    # put into ft
    mutate(tide_level_ft = 
               tide_level |> as.numeric() |> (\(.x){.x*3.2808})())

daily_minmax <- tide |>
    mutate(h = as.numeric(tide_level),
           h_diff = h - lag(h,1),
           h_diff_2 = h - lag(h, 3)) |>
    filter(!is.na(h_diff)) |>
    filter((sign(h_diff) != sign(h_diff_2))) |>
    mutate(highlow = ifelse(h_diff>h_diff_2, "High", "Low"))

tideplot <- ggplot(tide, 
       aes(x = tide_time, 
           y = tide_level_ft)) +
    # geom_ribbon(aes(ymax = tide_level_ft), ymin = 0,
    #             fill = "lightblue") +
     geom_line(linewidth = 0.5) +
    geom_point(data = daily_minmax, aes(color = highlow), size = 1.5)+
    scale_color_manual(values = c("orange", "purple"),
                       guide = "none") +
    scale_x_datetime(date_breaks  = "6 hours",
                     date_labels = "%a %H:%M") +
    labs(y = "Tide Height (ft)", x = "") +
    geom_hline(yintercept = 0, color = "red", lty = 2) +
    theme_bw() + theme(axis.text.x = element_text(angle=-90))


return(ggplotly(tideplot))
}
