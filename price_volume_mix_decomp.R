
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

mypackages <- c('tidyverse','gt', 'anytime', 'cowplot', 'magick')
packages(mypackages)


# 1. Create some fake transactional data to work with-------------------------------------------

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
            


               
# 2. Top-down Gross Profit $ (GP$) and Revenue Decomposition -------------------------------------------

# This is the recommended method if you want to peel back the onion on key drivers of your business performance changes and make adjustments  
# You can use this 


# 2-a) GP$ performance decomp at Level 1 (Channel) ----

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
                              mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
                                select(channel, base_gross_profit_total, current_gross_profit_total, base_revenue, current_revenue, 
                                        gp_mix_impact:gross_profit_change)
                                    


channel_level_decomp_summary = channel_level_decomp %>% summarise_if(is.numeric, sum, na.rm = T) %>% 
                      mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
                      mutate(channel = 'Company Total') %>% select(channel, everything()) %>%
                            bind_rows(channel_level_decomp) %>% 
                                  mutate_if(is.numeric, ~ round(.x, 0)) %>%
                    mutate(gross_profit_change = if_else(channel == 'Company Total', gross_profit_change, as.numeric(NA)))
                    
                                  
                                       

# Visual of Level 1 (Channel) GP $ decomp ----

l1_visual <- channel_level_decomp_summary %>% select(-one_of(c('base_revenue', 'current_revenue', 'revenue_mix_impact', 'revenue_volume_impact'))) %>% 
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
    title = md("Price-Cost-Volume-Mix Decomposition of Gross Profit $ Performance")); l1_visual




# 2-b) GP$ performance decomp at Level 2 (Customer Segment) ----

# Decomposing channel level GP$ and Revenue changes into price, cost, volume and product mix impacts by Customer Segment

# Pick a channel to dive deeper
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
  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
  select(customer_segment, base_gross_profit_total, current_gross_profit_total, base_revenue, current_revenue, 
         gp_mix_impact:gross_profit_change)



customer_level_decomp_summary = customer_level_decomp %>% summarise_if(is.numeric, sum, na.rm = T) %>% 
  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
  mutate(customer_segment = 'All Customers') %>% select(customer_segment, everything()) %>%
  bind_rows(customer_level_decomp) %>% 
  mutate_if(is.numeric, ~ round(.x, 0)) %>%
  mutate(gross_profit_change = if_else(customer_segment == 'All Customers', gross_profit_change, as.numeric(NA)))





# Visual of Level 2 (Customer) GP $ decomp ----

l2_visual <- customer_level_decomp_summary %>% select(-one_of(c('base_revenue', 'current_revenue', 'revenue_mix_impact', 'revenue_volume_impact'))) %>% 
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
    gp_mix_impact= 'Product Mix'
  ) %>%
  tab_header(
    title = md(paste("Price-Cost-Volume-Mix Decomposition of ", channel_filter, "channel"))); l2_visual






# 2-c) GP$ performance decomp at Level 3 (Brand) ----

# Decomposing customer level GP$ and Revenue changes into price, cost, volume and product mix impacts by Brand

# Pick a customer segment to dive deeper
customer_filter = 'Customer C'


brand_level_decomp = data %>% filter(channel == channel_filter, customer_segment == customer_filter) %>% group_by(brand) %>%
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
  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
  select(brand, base_gross_profit_total, current_gross_profit_total, base_revenue, current_revenue, 
         gp_mix_impact:gross_profit_change)



brand_level_decomp_summary = brand_level_decomp %>% summarise_if(is.numeric, sum, na.rm = T) %>% 
  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
  mutate(brand = 'All Brands') %>% select(brand, everything()) %>%
  bind_rows(brand_level_decomp) %>% 
  mutate_if(is.numeric, ~ round(.x, 0))  %>%
  mutate(gross_profit_change = if_else(brand == 'All Brands', gross_profit_change, as.numeric(NA)))



# Visual of Level 3 (Brand) GP $ decomp ----

l3_visual <- brand_level_decomp_summary %>% select(-one_of(c('base_revenue', 'current_revenue', 'revenue_mix_impact', 'revenue_volume_impact'))) %>% 
  select(brand, base_gross_profit_total, current_gross_profit_total, gross_profit_change, everything()) %>%
  gt(rowname_col = 'brand') %>% cols_align(align = 'center') %>%
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
    gp_mix_impact= 'Product Mix'
  ) %>%
  tab_header(
    title = md(paste("Price-Cost-Volume-Mix Decomposition of ", customer_filter, " segment"))); l3_visual 






# 2-d) GP$ performance decomp at Level 4 (Product) ----

# Decomposing Brand level GP$ and Revenue changes into price, cost, volume and product mix impacts by Product

# Pick a customer segment to dive deeper
brand_filter = 'Good'


product_level_decomp = data %>% filter(channel == channel_filter, customer_segment == customer_filter, brand == brand_filter) %>% group_by(product) %>%
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
  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
  select(product, base_gross_profit_total, current_gross_profit_total, base_revenue, current_revenue, 
         gp_mix_impact:gross_profit_change)



product_level_decomp_summary = product_level_decomp %>% summarise_if(is.numeric, sum, na.rm = T) %>% 
  mutate(gross_profit_change = gp_mix_impact + gp_volume_impact + price_impact + cost_impact) %>%
  mutate(product = 'All Products') %>% select(product, everything()) %>%
  bind_rows(product_level_decomp) %>% 
  mutate_if(is.numeric, ~ round(.x, 0))   %>%
  mutate(gross_profit_change = if_else(product == 'All Products', gross_profit_change, as.numeric(NA)))




# Visual of Level 4 (Product) GP $ decomp ----

l4_visual <- product_level_decomp_summary %>% select(-one_of(c('base_revenue', 'current_revenue', 'revenue_mix_impact', 'revenue_volume_impact'))) %>% 
  select(product, base_gross_profit_total, current_gross_profit_total, gross_profit_change, everything()) %>%
  gt(rowname_col = 'product') %>% cols_align(align = 'center') %>%
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
    gp_mix_impact= 'Product Mix'
  ) %>%
  tab_header(
    title = md(paste("Price-Cost-Volume-Mix Decomposition of ", brand_filter, " brand"))
  ); l4_visual  







