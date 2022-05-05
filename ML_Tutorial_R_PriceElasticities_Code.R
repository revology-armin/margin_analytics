
library(lazyeval)
library(knitr)
library(vip)
library(gt)
library(data.table)
library(corrplot)
library(ggthemes)
library(ggrepel)
library(ggpubr)
library(readxl)
library(skimr)
library(prophet)
library(forecast)
library(anytime)
library(doParallel)
library(scales)
library(gridExtra)
library(plotly)
library(timeDate)
library(tidyverse)
library(tidymodels)
library(DataExplorer)


# 1. Baseline functions ---------------------------------------------------

header_changes = function(mydata){
  setnames(mydata, names(mydata), tolower(names(mydata)))
  setnames(mydata, names(mydata), gsub(" ","_", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("_(sum)","", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("_(agg)","", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("_(avg)","_avg", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("(c)","", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("$","dollar_volume", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("%","pct", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("w/o","without", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("-","_", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("&","", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("/","_", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("\\(","", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub("\\)","", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), gsub(".","", names(mydata), fixed = TRUE))
  setnames(mydata, names(mydata), iconv(names(mydata), "UTF-8","ASCII", sub=""))
}


`%ni%` = Negate('%in%')  


#function to round any number to the desired decimal interval (i.e. 4.28 to 4.50)
round_to_decimal <- function(x,decimal){
  y=x/decimal
  z=round(y)
  z*decimal
}

#quantile calculations
quantile_25 = function(x){quantile(x, 0.25)}
quantile_75 = function(x){quantile(x, 0.75)}
quantile_90 = function(x){quantile(x, 0.90)}


### Function to calculate rmse ################
rmse_calc = function(df){
  
  sqrt(mean((df$.fitted - df$units)^2))
  
}


### Function to calculate rmse with log-log regression ################
rmse_log = function(df){
  
  sqrt(mean((exp(df$.fitted) - exp(df$`log(units + 1)`))^2))
  
}


round_2 = function(x) (round(x, 2)) 


### Function for messy string cleaning ################
clean_text <- function(x) {
  x %>%
    str_remove_all("^RT:? ") %>%
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    str_replace_all("&amp;", "and") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("@[[:alnum:]]+") %>%
    str_remove_all("#[[:alnum:]]+") %>%
    str_replace_all("\\\n", " ") %>%
    str_to_lower() %>%
    str_trim("both") %>%
    str_replace_all("brt", " ") 
}


### Function to adjust elasticity estimates to 0 if the model outputs a positive number  ################
elast_adjust = function(x){
  if_else(x > 0 ,0,x)
}


### Function to obtain variable importance plots ################
varimp_plots = function(model){
  vip(model, scale = TRUE, num_features =25) 
}


### Function to obtain variable importance scores ################
varimp_scores = function(model){
  vi(model, scale = TRUE, ice = T)
}


# Data prepping for analysis and modeling ----

data = readRDS('C:/Users/armin/OneDrive/Documents/GitHub/margin_analytics/cpg_retail_data_example_A.rds')
data$date <- as.Date(data$date)
data$promo_price <- NULL



# Create key pricing and margin metrics====
data = data %>% mutate(year  = lubridate::year(date),
                       avg_selling_price = sales_dollars / units, 
                       gp_per_unit = avg_selling_price - cost_per_unit, 
                       gp_pct = gp_per_unit/avg_selling_price,
                       gp_dollars = sales_dollars * gp_pct,
                       pct_units_sold_on_promo = promo_units / units,
                       pct_units_feature = (feature_units) / units,
                       pct_units_display = (display_units) / units) %>% 
  mutate_at(vars(contains('price')), round_2) %>% na.omit() %>% 
  group_by(retailer, product, year) %>% 
  mutate(price_ratio_vs_med = avg_selling_price/median(avg_selling_price, na.rm = T),
         price_ratio_vs_mean = avg_selling_price/mean(avg_selling_price, na.rm = T)) %>% ungroup()




# Specify retailer to analyze---- 
unique(data$retailer)

#keep only those products that have at least 104 weeks of sales history
#focus on the ones with the biggest price variation
retailer_filtering = data %>% group_by(retailer, product) %>% summarise(price_var = sd(avg_selling_price)/mean(avg_selling_price),
                                                                        units  = sum(units),
                                                                        num_weeks = n(),
                                                                        pct_units_feature = sum(feature_units, na.rm = T) / sum(units, na.rm = T),
                                                                        pct_units_display = sum(display_units, na.rm = T)/ sum(units, na.rm = T)) %>% ungroup() %>%
  filter(num_weeks >= 104) %>% group_by(retailer) %>%
  summarise(price_var = mean(price_var),
            num_weeks = mean(num_weeks),
            num_products = n_distinct(product),
            pct_units_feature = mean(pct_units_feature),
            pct_units_display = mean(pct_units_display)) %>% ungroup()


target_retailer = 'Pubyern'

data = data %>% filter(retailer ==  target_retailer) 

focus_product_list =  unique(data$product)



# Create holiday variables  ====

superbowl = tibble(holiday = 'superbowl', date = as.Date(c('2014-02-02', '2015-02-01','2016-02-07')))

#Custom data frame of US holidays
holidays = listHolidays("US")
currentYear <- getRmetricsOptions("currentYear")
holiday_years = tibble(year = currentYear:(currentYear-10))

holiday_df = tibble(holiday = holidays)  %>% crossing(holiday_years) %>%
  rowwise() %>%
  mutate(date = lazy_eval(paste0("as.Date(timeDate::", holiday,"(",year,"))"))) %>% rowwise() %>% 
  select(-year) %>% bind_rows(superbowl)


holiday_df_before = holiday_df %>% mutate(date = date-1)
holiday_df_after = holiday_df %>% mutate(date = date + 1)
holiday_df = holiday_df %>% bind_rows(holiday_df_after) %>% bind_rows(holiday_df_before) %>% arrange(holiday, date)


#Create dummy variables
holiday_matrix = holiday_df %>% mutate(count = 1) %>% spread(holiday, count) 



data = data %>% left_join(holiday_matrix) %>% replace(is.na(.), 0)



# Create dummy variable for promotion ====

data %>% group_by(product) %>% skim(pct_units_display, pct_units_feature)

#Determine what constitutes a promotion or a feature and display
feature_or_display_unit_threshold = 1/3  #at least 33% of the stores/units have to be on feature or on display support


data = data %>% mutate(feature_or_display_promo  = if_else(pct_units_feature >= feature_or_display_unit_threshold |
                                                             pct_units_display >= feature_or_display_unit_threshold,1,0))




# Quick exploratory analysis of our data ====
skim(data %>% select(-starts_with('US')))


#alternate data explorer for better rmarkdown

data %>% 
  select(feature_or_display_promo, pct_units_display, pct_units_feature, pct_units_sold_on_promo, gp_dollars,
         avg_selling_price, units) %>% 
  DataExplorer::create_report(output_dir = '/home/kakasa09/Documents/Github/revology/Articles/',
                              report_title = "Data Profiling Report", 
                              config = configure_report(global_ggtheme = quote(theme_light(base_size = 20L)), add_plot_prcomp = FALSE,
                                                        plot_correlation_args = list("theme_config" = list(legend.position = "none"))))

# Create variables for seasonality  ====

#Understand seasonality at the category level (ice pops)
#use forecast package to extract fourier terms https://rdrr.io/cran/forecast/man/fourier.html
#msts data: https://robjhyndman.com/hyndsight/seasonal-periods/
#loop through various orders for fourier terms and the RMSE for the best one

data_prophet_cat = data %>% group_by(date) %>% summarise(units = sum(units, na.rm = T)) %>% ungroup() %>% 
  select(date, units) %>% rename(ds = date, y = units)

min_date = as.Date(min(data_prophet_cat$ds))
num_periods = length(unique(data_prophet_cat$ds))

train = 0.7 #training data portion
train_periods = ceiling(train * num_periods)

validation_periods = num_periods -train_periods # number of weeks used in validation data


data_p_train = data_prophet_cat %>% filter(ds <= (min_date + ((train_periods-1)*7)))
data_p_validation = data_prophet_cat %>% anti_join(data_p_train, by = 'ds')


library(doParallel)
registerDoParallel(cores=detectCores()-2)

fourier_term_ranking = foreach(i = 3:20, .combine = rbind, 
                               .packages = c('prophet','forecast','tidyverse')) %dopar% {
                                 ts_m <- prophet(data_p_train,  yearly.seasonality = T)
                                 ts_m <- prophet(weekly.seasonality = F)
                                 ts_m <- add_seasonality(ts_m, name = 'yearly', fourier.order = i, period = 365.25)
                                 ts_m = fit.prophet(ts_m, data_p_train)
                                 
                                 ts_m_future <- make_future_dataframe(ts_m, periods = validation_periods, freq = 'week')
                                 
                                 forecast <- predict(ts_m, ts_m_future)
                                 forecast$ds <- as.Date(forecast$ds)
                                 
                                 data_p_validation_small = data_p_validation %>% inner_join(select(forecast, ds, yhat))
                                 
                                 rmse = sqrt(mean((data_p_validation_small$y - data_p_validation_small$yhat)^2))
                                 fourier_terms_s = tibble(K = i, rmse= rmse)
                                 
                                 fourier_terms_s
                               }


fourier_term_optimal = fourier_term_ranking$K[which.min(fourier_term_ranking$rmse)]

data_ts = data_prophet_cat %>% arrange(ds)

data_ts = ts(data_ts$y, freq = 365.25/7,
             start = lubridate::decimal_date(lubridate::ymd(min(data_ts$ds))))

data_msts = msts(data_ts, seasonal.periods =  365.25/7)

fourier_series = fourier(data_msts, K=fourier_term_optimal) %>%
  data.frame()  #fourier series to be used in our models


#create fourier series data set for later use

fourier_series_df = data_prophet_cat %>% rename(date = ds) %>% select(-y) %>%
  arrange(date) %>%
  bind_cols(fourier_series) %>%
  nest(fourier_values = matches('.52'))



# 4. Visualize unit sales and profitability trends --------

# View key cost, pricing and margin stats for products of interest ====


for(i in seq_along(focus_product_list)){
  price_summary_by_prod = data %>% filter(product == focus_product_list[i]) %>% group_by(product) %>% skim(matches('price|cost|gp_per_unit|gp_pct'))
  print(price_summary_by_prod)
}


#alternative summary (for rmarkdown)

price_summary_by_prod = data %>% select(product, cost_per_unit, avg_selling_price, gp_per_unit, gp_pct) %>% 
  pivot_longer(-product, names_to = 'metrics', values_to = 'values') %>%
  group_by(product, metrics) %>% 
  summarise_all(list(min = min,
                     quantile_25 = quantile_25,
                     median = median,
                     quantile_75 = quantile_75, 
                     max = max)) %>%
  pivot_longer(-c(product,metrics), names_to = 'stats', values_to = 'values') %>%
  pivot_wider(names_from = 'metrics', values_from = 'values')


#View list of most important variables by product
price_summary_by_prod %>%
  gt(rowname_col = 'stats', groupname_col = 'product') %>% cols_align(align = 'center') %>%
  tab_spanner(label = "Key Pricing and Profitability Metrics", columns = c('avg_selling_price','cost_per_unit', 'gp_per_unit', 'gp_pct')) %>%
  fmt_currency(
    columns = vars(avg_selling_price, cost_per_unit, gp_per_unit),
    currency = "USD"
  ) %>% 
  fmt_percent(
    columns = vars(gp_pct),
    decimals = 0
  ) %>% 
  cols_label(
    avg_selling_price = 'Avg. Selling Price',
    cost_per_unit = 'Cost/Unit',
    gp_pct  = "Gross Profit Pct",
    gp_per_unit = "GP$/Unit"
  ) %>%
  tab_header(
    title = md("*Summary Stats**")
  )



# Explore ASP, Units and seasonality trends =====


for(i in seq_along(focus_product_list)){
  
  data_filtered = data %>% filter(product == focus_product_list[i]) %>% 
    group_by(product, date) %>% summarise(units = sum(units, na.rm = T)) %>% 
    ungroup() %>% arrange(date)
  
  data_prophet = data_filtered %>%
    select(date, units) %>% rename(ds = date, y = units)
  
  
  min_date = as.Date(min(data_prophet$ds))
  num_periods = length(unique(data_prophet$ds))
  
  train = 0.7 #training data portion
  train_periods = ceiling(train * num_periods)
  
  validation_periods = num_periods -train_periods # number of weeks used in validation data
  
  
  data_p_train = data_prophet %>% filter(ds <= (min_date + ((train_periods-1)*7)))
  data_p_validation = data_prophet %>% anti_join(data_p_train, by = 'ds')
  
  fourier_term_optimal = fourier_series_df %>% 
    sample_n(1) %>% unnest(cols = c('fourier_values'))
  
  fourier_term_optimal = (dim(fourier_term_optimal)[2]-2)/2
  
  ts_m <- prophet(data_p_train,  yearly.seasonality = T)
  ts_m <- prophet(weekly.seasonality = F)
  ts_m <- add_seasonality(ts_m, name = 'yearly', fourier.order = fourier_term_optimal, period = 365.25)
  ts_m = fit.prophet(ts_m, data_p_train)
  
  ts_m_future <- make_future_dataframe(ts_m, periods = validation_periods, freq = 'week')
  
  forecast <- predict(ts_m, ts_m_future)
  
  
  ### Plot the units and forecast ################
  pe_good_plot_forecast =  plot(ts_m, forecast)
  
  pe_good_plot_forecast$data$ds <- as.Date(pe_good_plot_forecast$data$ds)
  
  pe_good_plot_forecast = pe_good_plot_forecast +
    scale_x_date(name = 'Date') + 
    scale_y_continuous(name= 'Units', labels = scales::comma_format()) +
    ggtitle('Simple time series forecast') +
    theme(axis.title.y = element_text(color = 'grey23', size=14),
          axis.title.x = element_text(color = 'grey23', size=14),
          axis.text = element_text(size = 14),
          plot.title = element_text(vjust=1, 
                                    colour="red",
                                    size = 14,
                                    face="bold"),
          legend.title = element_blank(), legend.position = "none")
  
  
  
  ### Plot the seasonality ################
  pe_good_plot_seasonality = prophet_plot_components(ts_m, forecast)[[2]]   #seasonality plot
  
  
  pe_good_plot_seasonality = pe_good_plot_seasonality +
    scale_y_continuous(name= 'Units', labels = scales::comma_format()) +
    ggtitle("Annual seasonality") +
    theme(axis.title.y = element_text(color = 'grey23', size=14),
          axis.title.x = element_text(color = 'grey23', size=14),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 14),
          plot.title = element_text(vjust=1, 
                                    colour="black",
                                    size = 14,
                                    face="bold"),
          legend.title = element_blank(), legend.position = "none")
  
  
  
  ### Plot price vs. units relationship ################
  data_filtered =   data %>% filter(product == focus_product_list[i])
  
  
  pe_good_plot_cor  <- ggplot(data_filtered, aes(avg_selling_price, units)) +
    geom_point(aes(color = "black"), shape = 21, fill = "white", size = 3, stroke = 1.5) + 
    scale_x_continuous(name = 'Price', labels = scales::dollar_format(decimal = ".")) + 
    scale_y_continuous(name= 'Units', labels = scales::comma_format()) +
    geom_smooth(method = 'loess', se = T, color = "darkgrey") + 
    theme_fivethirtyeight() +    
    ggtitle("Price vs. Units response") +
    scale_color_economist() +   
    theme(axis.title.y = element_text(color = 'grey23', size=14),
          axis.title.x = element_text(color = 'grey23', size=14),
          axis.text = element_text(size = 14),
          plot.title = element_text(vjust=1, 
                                    colour="black",
                                    size = 14,
                                    face="bold"),
          legend.title = element_blank(), legend.position = "none")
  
  
  
  ### Plot gross profit by month ################
  
  data_filtered =   data %>% filter(product == focus_product_list[i]) %>% mutate(year_month = lubridate::floor_date(date, 'month')) %>%
    group_by(year_month) %>%
    summarise(gp_dollars = sum(gp_dollars, na.rm= T),
              units = sum(units, na.rm = T)) %>% ungroup()
  
  
  pe_good_plot_gp  <- ggplot(data_filtered, aes(as.Date(year_month), gp_dollars)) +
    geom_col(aes(color = "black"),  fill = "red") + 
    scale_x_date(name = 'Date') + 
    scale_y_continuous(name = 'Gross Profit $', labels = scales::dollar_format(),
                       limits = c(min(data_filtered$gp_dollars-1), max(data_filtered$gp_dollars+1)),
                       oob = rescale_none) +
    theme_economist() +    
    ggtitle('Monthly Gross Profit $') +
    scale_color_economist() +        
    theme(axis.title.y = element_text(color = 'grey23', size=14),
          axis.title.x = element_text(color = 'grey23', size=14),
          axis.text = element_text(size = 14),
          plot.title = element_text(vjust=1, 
                                    colour="red",
                                    size = 14,
                                    face="bold"),
          legend.title = element_blank(), legend.position = "none")
  
  
  
  pe_good_plot_units <- ggplot(data_filtered, aes(as.Date(year_month), units)) +
    geom_col(aes(color = "black"),  fill = "blue") + 
    scale_x_date(name = 'Date') + 
    scale_y_continuous(name = 'Units', labels = scales::comma_format(),
                       limits = c(min(data_filtered$units-1), max(data_filtered$units+1)),
                       oob = rescale_none) +
    theme_economist() +    
    ggtitle('Monthly Unit Sales') +
    scale_color_economist() +        
    theme(axis.title.y = element_text(color = 'grey23', size=14),
          axis.title.x = element_text(color = 'grey23', size=14),
          axis.text = element_text(size = 14),
          plot.title = element_text(vjust=1, 
                                    colour="blue",
                                    size = 14,
                                    face="bold"),
          legend.title = element_blank(), legend.position = "none")
  
  
  figure1 <- ggarrange(pe_good_plot_units, pe_good_plot_seasonality,
                       pe_good_plot_gp, pe_good_plot_cor,
                       ncol = 2, nrow = 2)
  
  figure1 <-  annotate_figure(figure1,
                              #top = text_grob(paste(focus_product_list[i]), color = "red", face = "bold", size = 14),
                              bottom = text_grob("revology.ai", color = "darkgrey",
                                                 hjust = 1, x = 1, face = "bold", size = 11),
                              top = text_grob("", color = "darkgrey",
                                              hjust = 1, x = 1, face = "bold", size = 18),
                              fig.lab = paste0(focus_product_list[i]), fig.lab.face = 'bold', fig.lab.size = 18)
  
  print(figure1)
  
}





# 5. Variable Importance using basic Random Forest --------

# Create initial data set for variable importance =====

by_product_data_reg = data %>% select(product, date, units, avg_selling_price, feature_or_display_promo, superbowl:USWashingtonsBirthday) %>%
  inner_join(fourier_series_df) %>% unnest(cols = 'fourier_values') %>%
  select(-date) 

by_product_data_reg$product <- factor(by_product_data_reg$product)


# Variable importance =====

### Run basic Random Forest model ################
rf_basic <- rand_forest(mode = "regression", mtry = floor(sqrt(.preds())), trees = 1000) %>%
  set_engine("ranger", importance = 'permutation') %>%
  fit(
    units  ~ . ,
    data = by_product_data_reg) 




### Variable importance plots ################


varimp_score =  varimp_scores(rf_basic)

varimp_ggplot <- varimp_plots(rf_basic) + 
  scale_y_continuous(name= 'Variable Importance (max = 100)', labels = scales::comma_format()) +
  ggtitle("Variable Importance Plot") +
  theme(axis.title.y = element_text(color = 'grey23', size=12),
        axis.title.x = element_text(color = 'grey23', size=16),
        axis.text = element_text(size = 14),
        plot.title = element_text(vjust=1, 
                                  colour="red",
                                  size = 14,
                                  face="bold"),
        legend.title = element_blank(), legend.position = "none")


varimp_ggplot



### Group fourier terms into one seasonality metric ################

#Exclude any variables with < 5 relative importance
#Remove "product" since we will be doing group-wise regression (by product)
rf_basic_varimp = varimp_score %>% 
  header_changes() %>% mutate(variable = if_else(grepl('.52', variable, ignore.case = T),'seasonality_fourier', variable)) %>%
  group_by(variable) %>% summarise(variable_importance = round(max(importance),0)) %>% ungroup() %>% 
  filter(variable != 'product') %>% 
  mutate(variable_importance  = round((variable_importance/max(variable_importance))*100,0)) %>% 
  arrange(desc(variable_importance)) %>% filter(variable_importance >= 5) %>%
  spread(variable, variable_importance,fill = 0)


#View list of most important variables by product
rf_basic_varimp %>%
  gt() %>% cols_align(align = 'center') %>%
  tab_spanner(label = "Variables (excludes Product)", columns = c('avg_selling_price','feature_or_display_promo', 'seasonality_fourier')) %>%
  cols_label(
    avg_selling_price = 'Avg. Selling Price',
    feature_or_display_promo = 'Promotional Event',
    seasonality_fourier = "Seasonality term"
  ) %>%
  tab_header(
    title = md("**Variable importance scores**"),
    subtitle = md("After combining fourier terms into **`seasonality`** variable")
  ) %>%
  tab_source_note(md("Includes those variables with relative importance scores >= 5   (max = 100)"))



### Create final data set for linear regressions ################



by_product_data_reg_final = tibble()

for(i in seq_along(focus_product_list)){
  data_filtered  = data %>% filter(product == focus_product_list[i]) %>% 
    select(product, date, units, avg_selling_price, feature_or_display_promo)
  
  data_filtered = data_filtered %>% inner_join(fourier_series_df) %>%
    unnest(cols = 'fourier_values') %>%
    select(-date) %>% group_by(product) %>% nest()
  
  by_product_data_reg_final = data_filtered %>% bind_rows(by_product_data_reg_final)
  
}



by_product_data_reg_final %>% filter(product == 'Vanilla diva 18 count ice pops') %>%
  unnest(cols = c('data')) %>% ungroup() %>% select(-product) %>% head() %>% mutate_at(vars(matches('.52')), round_2) %>%
  gt() %>% cols_align(align = 'center') %>% 
  tab_spanner(label = "Fourier terms", 
              columns = c('S1.52' ,'C1.52','S2.52' ,'C2.52','S3.52' ,'C3.52' ,'S4.52' ,'C4.52','S5.52', 'C5.52', 'S6.52' ,'C6.52')) %>%
  tab_header(
    title = md("**Data used for price elasticity modeling**"),
    subtitle = md("Vanilla diva 18 count ice pops; first 6 of 104 observations"))




# 6. The "Good" approach for estimating own Price Elasticity --------

# The mid-point method=====

### Understand the need for interaction between price promo flag and prices ################

feature_display_promo_stats = by_product_data_reg_final %>% unnest(cols = c('data')) %>% 
  group_by(product, feature_or_display_promo) %>% 
  summarise(avg_selling_price = round(mean(avg_selling_price, na.rm = T),2),
            avg_units = round(mean(units, na.rm = T),0),
            num_weeks = n()) %>% ungroup() %>%
  pivot_wider(names_from = feature_or_display_promo, values_from = c(avg_selling_price, avg_units, num_weeks)) %>%
  mutate(promo_units_ratio = round(avg_units_1/avg_units_0,1),
         pct_weeks_on_promotion = num_weeks_1/num_weeks_0)  %>%
  rename(regular_price = avg_selling_price_0,
         promo_price = avg_selling_price_1,
         regular_weekly_units = avg_units_0,
         promo_weekly_units = avg_units_1) %>% select(-num_weeks_1, -num_weeks_0)

feature_display_promo_stats %>%
  gt() %>% cols_align(align = 'center')  %>% 
  fmt_currency(
    columns = vars(regular_price, promo_price),
    currency = "USD"
  ) %>% 
  fmt_number(
    columns = vars(regular_weekly_units, promo_weekly_units),
    decimals = 0, sep_mark = ","
  ) %>%
  fmt_percent(
    columns = vars(promo_units_ratio, pct_weeks_on_promotion),
    decimals = 0
  ) %>% 
  cols_label(
    regular_price = 'Regular Price',
    promo_price = 'Promo Price',
    regular_weekly_units = "Avg. Weekly Units (Regular Week, No Promotion)",
    promo_weekly_units = "Avg. Weekly Units (Promotion)",
    promo_units_ratio = "Promo vs. Non-Promo Week Unit Ratio",
    pct_weeks_on_promotion =  "Pct of Weeks with Promotions"
  ) %>%
  tab_header(
    title = md("**Avg. weekly prices and units during regular vs. feature/display promotional weeks**")) %>%
  tab_source_note(md("Promotional event is defined by `feature_or_display_promo` flag = 1"))





# Build model =====


### Include interaction term between feature/display promotion and price ################
by_product_data_reg_final =  by_product_data_reg_final %>%
  mutate(pe_good_model = purrr::map(data, ~lm(units ~ avg_selling_price*feature_or_display_promo +., data = .x)),
         pe_good_model_coefs = purrr::map(pe_good_model, tidy),
         pe_good_model_summary = purrr::map(pe_good_model, glance),
         pe_good_model_details = purrr::map(pe_good_model, augment))

by_product_data_reg_final

### Model stats ################

pe_good_model_summary = by_product_data_reg_final %>% unnest(pe_good_model_summary) %>%
  select(-data, -pe_good_model, -pe_good_model_coefs)

pe_good_model_summary


pe_good_model_coefs = by_product_data_reg_final %>% unnest(pe_good_model_coefs) %>%
  select(-data, -pe_good_model, -pe_good_model_summary) %>%
  filter(grepl('price|promo', term, ignore.case = T))

pe_good_model_coefs

### Adjust coefficients for interactions ################
pe_good_model_coefs_2 = pe_good_model_coefs %>%
  select(product, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(price_coef_regular = avg_selling_price) %>%
  mutate(price_coef_promotion = price_coef_regular+`avg_selling_price:feature_or_display_promo`)

pe_good_model_coefs_2

# Estimate elasticities =====

pe_good_estimate = pe_good_model_coefs_2 %>%
  select(price_coef_regular, price_coef_promotion) %>%
  inner_join(feature_display_promo_stats) %>% ungroup() %>% 
  mutate(price_elasticity_regular = (regular_price/regular_weekly_units)*price_coef_regular,
         price_elasticity_promo = (promo_price/promo_weekly_units)*price_coef_promotion) %>%
  select(product, price_elasticity_regular, price_elasticity_promo) %>%
  mutate_at(.vars = c('price_elasticity_regular','price_elasticity_promo'), .funs = elast_adjust)




# Model accuracy on training data =====

by_product_data_reg_final <- by_product_data_reg_final %>% 
  mutate(pe_good_model_rmse = purrr::map(pe_good_model_details, rmse_calc)) 


by_product_data_reg_final %>% unnest(cols = c('pe_good_model_rmse'))

# 7. The "Better" approach for estimating own Price Elasticity --------
# The log-log method =====


# Build model =====

#includes interaction term between promo event and price
by_product_data_reg_final =  by_product_data_reg_final %>% 
  mutate(pe_better_model = purrr::map(data, ~lm(log(units+1) ~ . + log(avg_selling_price)*feature_or_display_promo - avg_selling_price, data = .x)),
         pe_better_model_coefs = purrr::map(pe_better_model, tidy),
         pe_better_model_summary = purrr::map(pe_better_model, glance),
         pe_better_model_details = purrr::map(pe_better_model, augment))

by_product_data_reg_final %>% select(-matches('pe_good_model'))

### Model stats ################
pe_better_model_summary = by_product_data_reg_final %>% unnest(pe_better_model_summary) %>%
  select(-data, -pe_better_model, -pe_better_model_coefs)  %>% select(-matches('pe_good_model'))

pe_better_model_summary


pe_better_model_coefs = by_product_data_reg_final %>% unnest(pe_better_model_coefs) %>%
  select(-data, -pe_better_model, -pe_better_model_summary) %>%
  filter(grepl('price|promo', term, ignore.case = T)) %>%  select(-matches('pe_good_model'))

pe_better_model_coefs



# Estimate elasticities =====

pe_better_estimate = pe_better_model_coefs %>%
  select(product, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(price_elasticity_regular = `log(avg_selling_price)`) %>%
  mutate(price_elasticity_promo = price_elasticity_regular + `feature_or_display_promo:log(avg_selling_price)`) %>%
  select(product, price_elasticity_regular, price_elasticity_promo) %>%
  mutate_at(.vars = c('price_elasticity_regular','price_elasticity_promo'), .funs = elast_adjust)

pe_better_estimate

# Model accuracy on training data =====


by_product_data_reg_final <- by_product_data_reg_final  %>% 
  mutate(pe_better_model_rmse = purrr::map(pe_better_model_details, rmse_log)) 

by_product_data_reg_final %>% select(product, pe_good_model_rmse, pe_better_model_rmse) %>%
            unnest(cols = c('pe_good_model_rmse', 'pe_better_model_rmse'))



x= by_product_data_reg_final %>% unnest(pe_better_model_details)



# 8. The "Best" approach for estimating own Price Elasticity --------

# * Create Random Forest model the tidymodels way =====

set.seed(987)
val_set <- validation_split(by_product_data_reg, 
                            strata = product, 
                            prop = 0.8)

cores <- parallel::detectCores()
cores


rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger", num.threads = 6) %>% 
  set_mode("regression")


rf_recipe <- 
  recipe(units ~ ., data = by_product_data_reg)


rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)


rf_mod %>%    
  parameters()



grid_mtry = grid_regular(mtry() %>% range_set(c(floor(sqrt(ncol(by_product_data_reg)-1)), ncol(by_product_data_reg)-1)),
                         levels = 10)

grid_min_n = grid_regular(min_n() %>% range_set(c(2, 10)),
                          levels = 9)


grid_trees = grid_regular(trees() %>% range_set(c(500, 2000)),
                          levels = 5)


grid = grid_mtry %>% crossing(grid_min_n) %>% crossing(grid_trees)

set.seed(126)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))


rf_res %>% 
  show_best(metric = "rmse")


rf_best <- 
  rf_res %>% 
  select_best(metric = "rmse")
rf_best



# Fit final model on entire data set =====

rf_final <- rand_forest(mode = "regression", mtry = 33, min_n = 4, trees = 1250) %>%
  set_engine("ranger", importance = 'permutation') %>%
  fit(
    units  ~ . ,
    data = by_product_data_reg) 



rf_preds = predict(rf_final, by_product_data_reg) 

rf_preds = rf_preds %>% bind_cols(select(by_product_data_reg, product, units)) %>% rename(.fitted = .pred) 

pe_model_best_rmse_by_prod = rf_preds %>% group_by(product) %>% nest() %>% mutate(pe_best_model_rmse = map(data, rmse_calc)) %>%
  select(product, pe_best_model_rmse) %>% unnest(cols = c(pe_best_model_rmse))

pe_model_best_rmse = rf_preds %>% nest() %>% mutate(pe_best_model_rmse = map(data, rmse_calc)) %>% 
  select(pe_best_model_rmse) %>% unnest(cols = c(pe_best_model_rmse)) %>% mutate(product = 'All products')





### Evaluate model performance of RF vs. the 2 Linear Models (Good/Better) ################

mean_units_by_prod = by_product_data_reg_final %>% unnest(cols = c('data')) %>% 
  group_by(product) %>% 
  summarise(avg_units_actuals = round(mean(units, na.rm = T))) %>% ungroup()

mean_units = by_product_data_reg_final %>% unnest(cols = c('data')) %>% ungroup() %>% 
  summarise(avg_units_actuals = round(mean(units, na.rm = T))) %>%  mutate(product = 'All products') %>%
  bind_rows(mean_units_by_prod)



pe_model_good_rmse = by_product_data_reg_final %>% unnest(cols = 'pe_good_model_details') %>%
  select(product, units, .fitted) %>% ungroup() %>% nest() %>% 
  mutate(pe_good_model_rmse = map(data, rmse_calc)) %>% 
  select(pe_good_model_rmse) %>%
  unnest(cols = c(pe_good_model_rmse)) 


pe_model_better_rmse = by_product_data_reg_final %>% unnest(cols = 'pe_better_model_details') %>%
  select(product, `log(units + 1)` , .fitted) %>% ungroup() %>% 
  mutate(units = exp(`log(units + 1)`), .fitted = exp(.fitted)) %>% 
  nest() %>% 
  mutate(pe_better_model_rmse = map(data, rmse_calc)) %>% 
  select(pe_better_model_rmse) %>%
  unnest(cols = c(pe_better_model_rmse))


model_comparison = by_product_data_reg_final %>% unnest(cols = c(pe_better_model_rmse, pe_good_model_rmse)) %>%
  select(product, pe_good_model_rmse, pe_better_model_rmse) %>% inner_join(pe_model_best_rmse_by_prod) %>%
  bind_rows(bind_cols(pe_model_best_rmse, pe_model_good_rmse, pe_model_better_rmse)) %>%
  inner_join(mean_units)


model_comparison %>% ungroup() %>% rename(`Linear regression` =  pe_good_model_rmse, 
                                          `Log-log regression` = pe_better_model_rmse,
                                          `Random forest` = pe_best_model_rmse) %>% mutate_if(is.numeric, round) %>% 
  gt() %>% cols_align(align = 'center') %>%
  fmt_number(
    columns = vars(`Linear regression`, `Log-log regression`, `Random forest` , avg_units_actuals),
    decimals = 0, sep_mark = ","
  ) %>%
  tab_spanner(label = "Price Elasticity Modeling Approach", columns = c('Linear regression','Log-log regression', 'Random forest')) %>%
  cols_label(
    avg_units_actuals = 'Avg. Weekly Units (Actuals)') %>% 
  tab_header(
    title = md("**Model RMSEs**"),
  ) %>%
  tab_source_note(md("Summary of training data RMSEs from linear, log-log regression, and random forest models."))




# Estimate elasticities using data perturbation =====

### Create artificial price points ################

sim_price_intervals = 0.1 #create artificial price points for every row of our training data at $0.10 intervals

prices_sim = data %>% group_by(product) %>% summarise(min_price = min(avg_selling_price, na.rm= T),
                                                      max_price = max(avg_selling_price, na.rm = T)) %>%
  ungroup() %>%
  mutate(price_points = floor((max_price-min_price)/sim_price_intervals),
         max_price2 = min_price + price_points*sim_price_intervals)

prices_sim = setDT(prices_sim)[, .SD[rep(1:.N, price_points+1)]][,
                                                                 price_test := seq(min_price, max_price2, by = sim_price_intervals),
                                                                 by = .(min_price, max_price2)][, .(product, price_test)]



### Run predictions on augmented data ################


data_sim = by_product_data_reg %>% select(-units) %>% mutate(group_number = as.numeric(row.names(by_product_data_reg))) %>% 
  full_join(prices_sim, by = 'product') %>%
  select(product, price_test, everything()) %>%
  select(-avg_selling_price) %>% rename(avg_selling_price = price_test)

data_sim$product <- factor(data_sim$product)

rf_preds_sim = predict(rf_final, data_sim) 

rf_preds_sim = rf_preds_sim %>% bind_cols(data_sim) %>% rename(preds_simulated = .pred)



# Estimate elasticities by Product and Price Interval =====

rf_preds_sim = rf_preds_sim %>% select(-c(superbowl:C6.52)) %>%
  group_by(group_number) %>% 
  mutate(units_chg_pct = preds_simulated/lag(preds_simulated)-1,
         price_chg_pct = avg_selling_price/lag(avg_selling_price)-1,
         price_elasticity_implied = units_chg_pct/price_chg_pct) %>% na.omit() %>%
  mutate(price_elasticity_implied = if_else(price_elasticity_implied > 0, 0 , price_elasticity_implied),
         price_bins_floor = floor(avg_selling_price)) %>% 
  group_by(product, price_bins_floor,feature_or_display_promo) %>%
  summarise(price_elasticity_implied = mean(price_elasticity_implied, na.rm = T)) %>%
  group_by(product, feature_or_display_promo) %>% mutate(elasticity_low_end = quantile(price_elasticity_implied, 0.25)-6*IQR(price_elasticity_implied)) %>% ungroup() %>%
  mutate(price_elasticity_implied = if_else(price_elasticity_implied < elasticity_low_end, elasticity_low_end, price_elasticity_implied)) %>%
  select(-elasticity_low_end) %>% 
  spread(feature_or_display_promo, price_elasticity_implied) %>% 
  rename(price_elasticity_regular = `0`, price_elasticity_promo = `1`)



#understand prevalence of price points

key_price_points = by_product_data_reg %>% mutate(price_bins_floor = floor(avg_selling_price)) %>%
  group_by(product, feature_or_display_promo, price_bins_floor) %>% 
  summarise(weeks = n(),
            units = sum(units)) %>%
  group_by(product, feature_or_display_promo) %>% 
  mutate(pct_weeks = weeks/sum(weeks),
         pct_units = units/sum(units)) %>% ungroup() %>%
  select(-weeks, -units) %>% pivot_wider(names_from = feature_or_display_promo,
                                         values_from = c(pct_weeks,pct_units),
                                         values_fill = list(pct_weeks = 0, pct_units = 0)) %>%
  rename(pct_regular_weeks = pct_weeks_0, pct_promo_weeks = pct_weeks_1,
         pct_regular_units = pct_units_0, pct_promo_units = pct_units_1)




pe_best_estimate  = rf_preds_sim %>% full_join(key_price_points, by = c('product','price_bins_floor'))



# Summary of promo vs. regular price elasticities =====
pe_best_estimate %>% 
  mutate(price_range = paste("$",price_bins_floor, " to $", price_bins_floor+0.99))  %>%
  select(product, price_range, price_elasticity_regular, price_elasticity_promo,
         pct_regular_weeks, pct_regular_units, pct_promo_weeks, pct_promo_units) %>% 
  mutate_at(vars(matches('elasticity')), round_2) %>%
  gt(groupname_col = 'product') %>% cols_align(align = 'center') %>%
  fmt_percent(
    columns = vars(pct_regular_weeks, pct_regular_units, pct_promo_weeks, pct_promo_units),
    decimals = 0
  )  %>%
  cols_label(
    price_elasticity_regular = 'Regular Price Elasticity',
    price_elasticity_promo = 'Promo Price Elasticity',
    pct_regular_weeks = "Pct of Regular Weeks",
    pct_regular_units = "Pct of Regular Units",
    pct_promo_weeks = "Pct of Promo Weeks",
    pct_promo_units =  "Pct of Promo Units"
  ) %>% 
  tab_header(
    title = md("**Promo vs. Regular Price Elasticity Summary**"),
  ) %>%
  tab_source_note(md("Regular weeks = no feature or display support for the product")) %>%
  tab_source_note(md("Promo weeks = those weeks with feature and display support"))




# 9. Summary of the three approaches ------------------------------------------------

# Visualize elasticities at various price points =====

for(i in seq_along(focus_product_list)){
  
  data_filtered =   pe_best_estimate %>% filter(product == focus_product_list[i]) %>%
    mutate(price_range = paste("$",price_bins_floor, " to $", price_bins_floor+0.99)) 
  
  pe_good_estimate_filtered = pe_good_estimate %>% filter(product == focus_product_list[i]) 
  pe_better_estimate_filtered = pe_better_estimate %>% filter(product == focus_product_list[i]) 
  
  offset = 0.15
  offset_good = ifelse( pe_good_estimate_filtered$price_elasticity_regular == 0, -offset,
                        ifelse(pe_good_estimate_filtered$price_elasticity_regular < pe_better_estimate_filtered$price_elasticity_regular,
                               - offset, + offset))
  
  offset_better = ifelse(pe_better_estimate_filtered$price_elasticity_regular == 0, -offset,
                         ifelse(pe_better_estimate_filtered$price_elasticity_regular > pe_good_estimate_filtered$price_elasticity_regular,
                                + offset,  - offset))
  
  y_min = min(data_filtered$price_elasticity_regular, data_filtered$price_elasticity_promo,
              pe_good_estimate_filtered$price_elasticity_promo, pe_good_estimate_filtered$price_elasticity_regular,
              pe_better_estimate_filtered$price_elasticity_promo, pe_better_estimate_filtered$price_elasticity_regular)- 0.5
  
  y_max = max(data_filtered$price_elasticity_regular, data_filtered$price_elasticity_promo,
              pe_good_estimate_filtered$price_elasticity_promo, pe_good_estimate_filtered$price_elasticity_regular,
              pe_better_estimate_filtered$price_elasticity_promo, pe_better_estimate_filtered$price_elasticity_regular) + 0.5
  
  pe_best_plot_cor_regular  <- ggplot(data_filtered, aes(price_range , price_elasticity_regular)) +
    geom_col(aes( fill = price_elasticity_regular),  width = 0.75) + 
    scale_x_discrete(name = 'Regular Price Range') +
    scale_y_continuous(name= 'Elasticity', labels = scales::comma_format() , limits = c(y_min, 0)) +
    
    geom_hline(yintercept=pe_good_estimate_filtered$price_elasticity_regular, 
               linetype = 'solid', color= 'red', size = 1.5) + 
    
    geom_hline(yintercept=pe_better_estimate_filtered$price_elasticity_regular, 
               linetype = 'solid', color= 'darkgreen', size = 1.5) + 
    theme_economist() +    
    ggtitle("Elasticities at Regular Price points") +
    scale_color_economist() +   
    theme(axis.title.y = element_text(color = 'grey23', size=16),
          axis.title.x = element_text(color = 'grey23', size=16),
          axis.text = element_text(size = 14),
          plot.title = element_text(vjust=1, 
                                    colour="grey23",
                                    size = 14,
                                    face="bold"),
          legend.title = element_blank(), legend.position = "none")  + 
    annotate("text", x = c(1.5), 
             y = c(pe_good_estimate_filtered$price_elasticity_regular + offset_good), 
             label = paste(c("Mid-point elasticity = "),round(pe_good_estimate_filtered$price_elasticity_regular,2)),
             size = 5, color = c('red'), fontface = 2) +
    annotate("text", x = c(2.5), 
             y = c(pe_better_estimate_filtered$price_elasticity_regular + offset_better), 
             label = paste(c("Log-log elasticity = "),round(pe_better_estimate_filtered$price_elasticity_regular,2)),
             size = 5, color = c('darkgreen'), fontface = 2)  
  
  offset_good_promo = ifelse( pe_good_estimate_filtered$price_elasticity_promo == 0, -offset,
                              ifelse(pe_good_estimate_filtered$price_elasticity_promo < pe_better_estimate_filtered$price_elasticity_promo,
                                     - offset, + offset))
  
  offset_better_promo = ifelse(pe_better_estimate_filtered$price_elasticity_promo == 0, -offset,
                               ifelse(pe_better_estimate_filtered$price_elasticity_promo > pe_good_estimate_filtered$price_elasticity_promo,
                                      + offset,  - offset))
  
  
  pe_best_plot_cor_promo  <- ggplot(data_filtered, aes(price_range , price_elasticity_promo)) +
    geom_col(aes( fill = price_elasticity_promo),  width = 0.75) + 
    scale_x_discrete(name = 'Promotional Price Range') + 
    scale_y_continuous(name= 'Elasticity', labels = scales::comma_format(), limits = c(y_min, 0)) +
    geom_hline(yintercept=pe_good_estimate_filtered$price_elasticity_promo, 
               linetype = 'solid', color= 'red', size = 1.5) + 
    
    geom_hline(yintercept=pe_better_estimate_filtered$price_elasticity_promo, 
               linetype = 'solid', color= 'darkgreen', size = 1.5) + 
    theme_economist_white() +    
    ggtitle("Elasticities at Promotional Price points") +
    scale_color_economist() +   
    theme(axis.title.y = element_text(color = 'grey23', size=16),
          axis.title.x = element_text(color = 'grey23', size=16),
          axis.text = element_text(size = 14),
          plot.title = element_text(vjust=1, 
                                    colour="brown",
                                    size = 14,
                                    face="bold"),
          legend.title = element_blank(), legend.position = "none")  +
    annotate("text", x = c(1.5), 
             y = c(pe_good_estimate_filtered$price_elasticity_promo+offset_good_promo), 
             label = paste(c("Mid-point elasticity = "),round(pe_good_estimate_filtered$price_elasticity_promo,2)),
             size = 5, color = c('red'), fontface = 2) +
    annotate("text", x = c(2.5), 
             y = c(pe_better_estimate_filtered$price_elasticity_promo+offset_better_promo), 
             label = paste(c("Log-log elasticity = "),round(pe_better_estimate_filtered$price_elasticity_promo,2)),
             size = 5, color = c("darkgreen"), fontface = 2) 
  
  
  #combine the above two graphs together
  figure1 <- ggarrange(pe_best_plot_cor_regular, pe_best_plot_cor_promo ,
                       ncol = 1, nrow = 2)
  
  
  
  
  figure1 <-  annotate_figure(figure1,
                              top = text_grob("", color = "darkgrey",
                                              hjust = 1, x = 1, face = "bold", size = 18),
                              bottom = text_grob("revology.ai", color = "darkgrey",
                                                 hjust = 1, x = 1, face = "bold", size = 11),
                              fig.lab = paste0('Random Forest vs. other approaches for ', focus_product_list[i]), fig.lab.face = 'bold', fig.lab.size = 18,
                              fig.lab.pos = 'top.right'
  )
  
  print(figure1)
  
  
  
}




# References =====

### Exploratory analysis ################

#* https://www.littlemissdata.com/blog/simple-eda ----   


### Tables ################
#* https://gt.rstudio.com/ -----
#* https://blog.rstudio.com/2020/04/08/great-looking-tables-gt-0-2/ ----
  
  
  
### Time series decomposition and forecasting with Prophet ################
#* https://facebook.github.io/prophet/docs/quick_start.html----
#* https://cran.r-project.org/web/packages/prophet/vignettes/quick_start.html----


### Modeling the tidyverse way  ################
#* https://www.tidymodels.org----


###  Linear Regression  ################
#* Interaction terms interpretation: http://www.medicine.mcgill.ca/epidemiology/Joseph/courses/EPIB-621/interaction.pdf ----


###  Price Elasticity modeling resources  ################

#Pricing Analytics by Walter R. Paczkowski
#*  pages: 46 - different demand models and elasticities (linear, linear-log, log-log)----
#* pages: 194 - using decision trees (hospital example) ----

#Mid-point method:
#* https://rpubs.com/cyobero/elasticity ----
#* http://www.salemmarafi.com/code/price-elasticity-with-r/ ----
#* https://quickonomics.com/how-to-calculate-price-elasticities-using-the-midpoint-formula/ ----
#* W. Paczkowski. Pricing Analytics: Models and Advanced Quantitative Techniques for Product Pricing. Routledge,  2019. ----

#Log-log method:
#* http://support.sas.com/resources/papers/proceedings13/425-2013.pdf ----
  


