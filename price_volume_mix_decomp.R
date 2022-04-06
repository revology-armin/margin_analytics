
#██████╗░███████╗██╗░░░██╗░█████╗░██╗░░░░░░█████╗░░██████╗░██╗░░░██╗
#██╔══██╗██╔════╝██║░░░██║██╔══██╗██║░░░░░██╔══██╗██╔════╝░╚██╗░██╔╝
#██████╔╝█████╗░░╚██╗░██╔╝██║░░██║██║░░░░░██║░░██║██║░░██╗░░╚████╔╝░
#██╔══██╗██╔══╝░░░╚████╔╝░██║░░██║██║░░░░░██║░░██║██║░░╚██╗░░╚██╔╝░░
#██║░░██║███████╗░░╚██╔╝░░╚█████╔╝███████╗╚█████╔╝╚██████╔╝░░░██║░░░
#╚═╝░░╚═╝╚══════╝░░░╚═╝░░░░╚════╝░╚══════╝░╚════╝░░╚═════╝░░░░╚═╝░░░

#░█████╗░███╗░░██╗░█████╗░██╗░░░░░██╗░░░██╗████████╗██╗░█████╗░░██████╗
#██╔══██╗████╗░██║██╔══██╗██║░░░░░╚██╗░██╔╝╚══██╔══╝██║██╔══██╗██╔════╝
#███████║██╔██╗██║███████║██║░░░░░░╚████╔╝░░░░██║░░░██║██║░░╚═╝╚█████╗░
#██╔══██║██║╚████║██╔══██║██║░░░░░░░╚██╔╝░░░░░██║░░░██║██║░░██╗░╚═══██╗
#██║░░██║██║░╚███║██║░░██║███████╗░░░██║░░░░░░██║░░░██║╚█████╔╝██████╔╝
#╚═╝░░╚═╝╚═╝░░╚══╝╚═╝░░╚═╝╚══════╝░░░╚═╝░░░░░░╚═╝░░░╚═╝░╚════╝░╚═════╝░



#----PRICE-COST-VOLUME-MIX DECOMPOSITION----

library(easypackages)

mypackages <- c('tidyverse','gt', 'anytime')
packages(mypackages)


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


# 2. Create some fake transactional data to work with-------------------------------------------

# Feel free to import your own data to run a Revenue or Gross Profit$ decomposition

channel = c('Grocery','Online','Convenience')
customers = paste('Customer',c(LETTERS[1:3]))
brands =  c('Good','Better','Best')
products = paste('Product',c(1:3))


cost_table = expand.grid(product = products, brand = brands) %>%
                  mutate(base_cost = rep(c(50, 75, 100), 3), current_cost = rep(c(55, 75, 102), 3)) %>%
                    mutate(brand_cost_factor = if_else(brand == 'Good', 0.95,
                                           if_else( brand == 'Better', 1, 1.05))) %>%
                        mutate(base_cost = base_cost * brand_cost_factor,
                               current_cost = current_cost * brand_cost_factor) 





price_table = expand.grid(product = products, brand = brands)  %>%
              mutate(base_price = rep(c(65, 85, 120),3), current_price = rep(c(65, 82, 130),3))   %>%
                  mutate(brand_price_factor = if_else(brand == 'Good', 0.9,
                                                     if_else( brand == 'Better', 1.05, 1.25))) %>%
                              mutate(base_price = base_price * brand_price_factor,
                                     current_price = current_price * brand_price_factor) 
                            


set.seed(234)
data = expand_grid(channel = channel, customer_segment = customers, brand = brands, product = products) %>%
                            mutate(channel_price_factor = if_else(channel == 'Online', 0.95,
                                                                  if_else(channel == 'Grocery', 1, 1.15)),
                                   customer_price_factor = if_else(grepl("A", customer_segment , ignore.case = F), 0.95,
                                                                  if_else(grepl("B", customer_segment,  ignore.case = F), 1, 1.05))) %>%
                        inner_join(cost_table) %>% inner_join(price_table) %>% 
                            mutate( base_price = base_price * channel_price_factor * customer_price_factor,
                              current_price = current_price * channel_price_factor * customer_price_factor) %>%
                      group_by(channel, customer_segment, brand, product) %>%
                                  mutate(base_units = sample.int(1000,1), current_units = sample.int(1000,1)) %>%
                                                ungroup() %>% select(-contains('_factor')) %>%
                   mutate(base_revenue = base_units * base_price,
                          current_revenue  = current_units * current_price,
                          base_cost_total = base_cost * base_units,
                          current_cost_total = current_cost * current_units,
                          base_gp_per_unit = base_price - base_cost,
                          current_gp_per_unit = current_price - current_cost,
                          base_gross_profit_total = base_gp_per_unit * base_units,
                          current_gross_profit_total = current_gp_per_unit * current_units)
            


               
# 3. Top-down Gross Profit $ (GP$) and Revenue Decomposition -------------------------------------------

# This is the recommended method if you want to peel back the onion on key drivers of your business performance changes and make adjustments                                 


# GP$ performance decomp at Level 1 (Channel) ----

# Decomposing total company GP$ and Revenue changes into price, cost, volume and channel mix impacts

channel_level_decomp = data %>% group_by(channel) %>%
                                  summarise_at(c('base_units','current_units',
                                                'base_revenue', 'current_revenue',
                                                'base_cost_total','current_cost_total',
                                                'base_gross_profit_total','current_gross_profit_total'), sum, na.rm = T) %>%
                              mutate(base_cost_per_unit = base_cost_total / base_units ,
                                     current_cost_per_unit = current_cost_total / current_units,
                                    base_gp_per_unit = base_gross_profit_total / base_units ,
                                     current_gp_per_unit = current_gross_profit_total / current_units,
                                    base_price = base_revenue / base_units ,
                                    current_price = current_revenue / current_units,
                                  base_mix = base_units / sum(base_units),
                                     current_mix = current_units / sum(current_units),
                                    fcst_units = base_mix * sum(current_units),
                                    delta_to_fcst_units = current_units - fcst_units,
                                  gp_mix_impact = (base_gp_per_unit - sum(base_gross_profit_total) / sum(base_units)) * delta_to_fcst_units,
                                  gp_volume_impact = (current_units - base_units) * (sum(base_gross_profit_total) / sum(base_units)),
                                  revenue_mix_impact = (base_price - sum(base_revenue) / sum(base_units)) * delta_to_fcst_units,
                                  revenue_volume_impact = (current_units - base_units) * (sum(base_revenue) / sum(base_units)),
                                  price_impact = (current_price - base_price) * current_units,
                                  cost_impact = -(current_cost_per_unit - base_cost_per_unit) * current_units) %>% ungroup() %>%
                                select(channel, base_gross_profit_total, current_gross_profit_total, base_revenue, current_revenue, 
                                        gp_mix_impact:cost_impact) 


channel_level_decomp_summary =   channel_level_decomp  %>% add_row(channel = 'Company Total',
                                  base_gross_profit_total = sum(channel_level_decomp$base_gross_profit_total),
                                  current_gross_profit_total = sum(channel_level_decomp$current_gross_profit_total),
                                  base_revenue = sum(channel_level_decomp$base_revenue),
                                  current_revenue = sum(channel_level_decomp$current_revenue),
                                  gp_mix_impact = sum(channel_level_decomp$gp_mix_impact),
                                  gp_volume_impact = sum(channel_level_decomp$gp_volume_impact),
                                  revenue_mix_impact = sum(channel_level_decomp$revenue_mix_impact),
                                  revenue_volume_impact = sum(channel_level_decomp$revenue_volume_impact),
                                  price_impact = sum(channel_level_decomp$price_impact),
                                  cost_impact = sum(channel_level_decomp$cost_impact)) %>% arrange(-base_gross_profit_total) %>%
                                  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
                                  mutate_if(is.numeric, ~ round(.x, 0))
                                  
                                       

# Let's visualize Level 1 (Channel) GP $ decomp

channel_level_decomp_summary %>% select(-one_of(c('base_revenue', 'current_revenue', 'revenue_mix_impact', 'revenue_volume_impact'))) %>% 
                                  select(channel, base_gross_profit_total, current_gross_profit_total, gross_profit_change, everything()) %>%
  gt(rowname_col = 'channel') %>% cols_align(align = 'center') %>%
  tab_spanner(label = "GP$ Change Decomposition", columns = c('price_impact', 'cost_impact', 'gp_volume_impact', 'gp_mix_impact')) %>%
  fmt_currency(
    columns = vars(base_gross_profit_total, current_gross_profit_total, gross_profit_change, price_impact, cost_impact, gp_volume_impact, gp_mix_impact),
    currency = "USD",
    decimals = 0
  ) %>% 
  cols_label(
    base_gross_profit_total = 'GP$ in Base Period',
    current_gross_profit_total = 'GP$ in Current Period',
    gross_profit_change  = 'GP$ Change',
    price_impact = 'Price',
    cost_impact= 'Cost',
    gp_volume_impact= 'Volume',
    gp_mix_impact= 'Channel Mix'
  ) %>%
  tab_header(
    title = md("*Price-Cost-Volume-Mix Decomposition of Gross Profit $ Performance*")
  )


# Let's visualize Level 1 (Channel) Revenue decomp

# continue from here with revenue decomp visualization
channel_level_decomp_summary %>% select(-one_of(c('base_revenue', 'current_revenue', 'revenue_mix_impact', 'revenue_volume_impact'))) %>% 
  select(channel, base_gross_profit_total, current_gross_profit_total, gross_profit_change, everything()) %>%
  gt(rowname_col = 'channel') %>% cols_align(align = 'center') %>%
  tab_spanner(label = "GP$ Change Decomposition", columns = c('price_impact', 'cost_impact', 'gp_volume_impact', 'gp_mix_impact')) %>%
  fmt_currency(
    columns = vars(base_gross_profit_total, current_gross_profit_total, gross_profit_change, price_impact, cost_impact, gp_volume_impact, gp_mix_impact),
    currency = "USD",
    decimals = 0
  ) %>% 
  cols_label(
    base_gross_profit_total = 'GP$ in Base Period',
    current_gross_profit_total = 'GP$ in Current Period',
    gross_profit_change  = 'GP$ Change',
    price_impact = 'Price',
    cost_impact= 'Cost',
    gp_volume_impact= 'Volume',
    gp_mix_impact= 'Channel Mix'
  ) %>%
  tab_header(
    title = md("*Price-Cost-Volume-Mix Decomposition of Gross Profit $ Performance*")
  )  
  


# GP$ performance decomp at Level 2 (Customer) ----

# Decomposing channel level GP$ and Revenue changes into price, cost, volume and product mix impacts

# Pick a channel to analyze

channel_filter = 'Grocery'

customer_level_decomp = data %>% filter(channel == channel_filter) %>% group_by(customer_segment) %>%
  summarise_at(c('base_units','current_units',
                 'base_revenue', 'current_revenue',
                 'base_cost_total','current_cost_total',
                 'base_gross_profit_total','current_gross_profit_total'), sum, na.rm = T) %>%
  mutate(base_cost_per_unit = base_cost_total / base_units ,
         current_cost_per_unit = current_cost_total / current_units,
         base_gp_per_unit = base_gross_profit_total / base_units ,
         current_gp_per_unit = current_gross_profit_total / current_units,
         base_price = base_revenue / base_units ,
         current_price = current_revenue / current_units,
         base_mix = base_units / sum(base_units),
         current_mix = current_units / sum(current_units),
         fcst_units = base_mix * sum(current_units),
         delta_to_fcst_units = current_units - fcst_units,
         gp_mix_impact = (base_gp_per_unit - sum(base_gross_profit_total) / sum(base_units)) * delta_to_fcst_units,
         gp_volume_impact = (current_units - base_units) * (sum(base_gross_profit_total) / sum(base_units)),
         revenue_mix_impact = (base_price - sum(base_revenue) / sum(base_units)) * delta_to_fcst_units,
         revenue_volume_impact = (current_units - base_units) * (sum(base_revenue) / sum(base_units)),
         price_impact = (current_price - base_price) * current_units,
         cost_impact = -(current_cost_per_unit - base_cost_per_unit) * current_units) %>% ungroup() %>%
  select(customer_segment, base_gross_profit_total, current_gross_profit_total, base_revenue, current_revenue, 
         gp_mix_impact:cost_impact) 


customer_level_decomp_summary =   customer_level_decomp  %>% add_row(customer_segment = 'All Segments',
                                                                   base_gross_profit_total = sum(customer_level_decomp$base_gross_profit_total),
                                                                   current_gross_profit_total = sum(customer_level_decomp$current_gross_profit_total),
                                                                   base_revenue = sum(customer_level_decomp$base_revenue),
                                                                   current_revenue = sum(customer_level_decomp$current_revenue),
                                                                   gp_mix_impact = sum(customer_level_decomp$gp_mix_impact),
                                                                   gp_volume_impact = sum(customer_level_decomp$gp_volume_impact),
                                                                   revenue_mix_impact = sum(customer_level_decomp$revenue_mix_impact),
                                                                   revenue_volume_impact = sum(customer_level_decomp$revenue_volume_impact),
                                                                   price_impact = sum(customer_level_decomp$price_impact),
                                                                   cost_impact = sum(customer_level_decomp$cost_impact)) %>% arrange(-base_gross_profit_total) %>%
  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
  mutate_if(is.numeric, ~ round(.x, 0))




# Let's visualize Level 2 (Customer) GP $ decomp

customer_level_decomp_summary %>% select(-one_of(c('base_revenue', 'current_revenue', 'revenue_mix_impact', 'revenue_volume_impact'))) %>% 
  select(customer_segment, base_gross_profit_total, current_gross_profit_total, gross_profit_change, everything()) %>%
  gt(rowname_col = 'customer_segment') %>% cols_align(align = 'center') %>%
  tab_spanner(label = "GP$ Change Decomposition", columns = c('price_impact', 'cost_impact', 'gp_volume_impact', 'gp_mix_impact')) %>%
  fmt_currency(
    columns = vars(base_gross_profit_total, current_gross_profit_total, gross_profit_change, price_impact, cost_impact, gp_volume_impact, gp_mix_impact),
    currency = "USD",
    decimals = 0
  ) %>% 
  cols_label(
    base_gross_profit_total = 'GP$ in Base Period',
    current_gross_profit_total = 'GP$ in Current Period',
    gross_profit_change  = 'GP$ Change',
    price_impact = 'Price',
    cost_impact= 'Cost',
    gp_volume_impact= 'Volume',
    gp_mix_impact= 'Customer Mix'
  ) %>%
  tab_header(
    title = md(paste("GP$ Decomposition of ", channel_filter, "channel"))
  )














# 4. Bottoms-up Gross Profit $ and Revenue Decomposition -------------------------------------------

# You use this approach if you want to explain drivers of total company performance


