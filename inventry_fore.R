#-------------------------------------------------------------------------------
# FINAL SCRIPT (v12)
# - Providing specific date format options to fix the join error.
#-------------------------------------------------------------------------------

# Step 1: Load Required Libraries
library(tidyverse)
library(timetk)
library(gt)

# Step 2: Load and Create Input Data

# !!! ACTION REQUIRED: CHECK YOUR CSV AND CHOOSE THE CORRECT FORMAT BELOW !!!
# Delete or comment out the lines that do NOT match your file's date format.

sellout_fcast <- read_csv("F_2.csv") %>%
  mutate(.index = as.Date(.index, format = "%d-%m-%Y"))

# Manually create the retailer demand data frame
retailer_demand_df <- tibble(
  .index = as.Date(c("2023-07-03", "2023-07-10")),
  retailer_demand = c(150, 45)
)

# Step 3: Define the Simulation Function (No changes needed here)
projected_sellin_order <- function(sellout_fcast,
                                   current_inv,
                                   target_method = "weeks_cover",
                                   weeks_cover = 2,
                                   fixed_target = 600) {
  # Get the current week from the sellout forecast
  current_week <- sellout_fcast %>%
    filter(.key == "prediction") %>%
    slice(1) %>%
    select(.index) %>%
    pull()
  
  # Prepare the base tibble
  tibble_base <- sellout_fcast %>%
    filter(.key == "prediction") %>% 
    select(forecast = .value, week = .index) %>%
    left_join(retailer_demand_df, by = c("week" = ".index")) %>%
    replace(is.na(.), 0) %>%
    mutate(
      target_inventory = case_when(
        target_method == "weeks_cover" ~ slidify_vec(
          .x = forecast, .f = sum, .period = weeks_cover,
          .align = "left", .partial = TRUE),
        target_method == "fixed" ~ fixed_target,
        TRUE ~ slidify_vec(
          .x = forecast, .period = 2, .f = sum,
          .align = "left", .partial = TRUE)
      ),
      opening_inventory = 0,
      ending_inventory = 0,
      intransit_t1 = 0,
      order_quantity = 0,
      late_sales = 0,
      horizon = if_else(week <= current_week + lubridate::weeks(2), "execution", "planning")
    ) %>%
    arrange(week)
  
  # Initialize the first row (Current Week)
  tibble_base$opening_inventory[1] = current_inv
  
  # Calculate the projected sell in order row by row (Week by Week)
  for (i in 1:nrow(tibble_base)) {
    
    # Standardized demand logic to be consistent across all weeks
    demand_to_use <- pmax(tibble_base$retailer_demand[i], tibble_base$forecast[i])
    
    if (i == 1) {
      # First week calculations
      tibble_base$intransit_t1[i]      <- 0
      tibble_base$ending_inventory[i]  <- tibble_base$opening_inventory[i] - demand_to_use
      tibble_base$late_sales[i]        <- abs(pmin(0, tibble_base$ending_inventory[i]))
      
    } else {
      # Subsequent weeks
      tibble_base$intransit_t1[i]      <- tibble_base$order_quantity[i - 1]
      tibble_base$opening_inventory[i] <- tibble_base$ending_inventory[i - 1] + tibble_base$intransit_t1[i]
      tibble_base$ending_inventory[i]  <- tibble_base$opening_inventory[i] - demand_to_use
      tibble_base$late_sales[i]        <- abs(pmin(0, tibble_base$ending_inventory[i]))
    }
    
    # Calculate order quantity for all weeks except the last one
    if (i < nrow(tibble_base)) {
      tibble_base$order_quantity[i] = tibble_base$target_inventory[i] - tibble_base$ending_inventory[i]
    } else {
      tibble_base$order_quantity[i] = NA
    }
    
    # Ensure order quantity is not negative
    if (!is.na(tibble_base$order_quantity[i])) {
      tibble_base$order_quantity[i] <- pmax(0, tibble_base$order_quantity[i])
    }
  }
  return(tibble_base)
}

# Step 4: Define the Table Formatting Function (No changes needed)
table_formating_function <- function(input_table, policy_type, policy_value = NULL){
  subtitle_text <- case_when(
    policy_type == "fixed" ~ paste("Distributor Ordering Behavior Based on Fixed Target Inventory Policy (",
                                   policy_value, " cases)"),
    policy_type == "weeks_cover" ~ paste("Distributor Ordering Behavior Based on",
                                         policy_value,
                                         "Weeks Coverage Target Inventory Policy"),
    TRUE ~ "Distributor Ordering Behavior"
  )
  
  input_table %>%
    select(week, forecast, retailer_demand, opening_inventory, intransit_t1, ending_inventory,
           order_quantity, target_inventory, late_sales, horizon) %>%
    gt() %>%
    tab_header(
      title = "Projected Sell-In Order Simulation",
      subtitle = subtitle_text
    ) %>%
    cols_label(
      week = "Week",
      forecast = "Sell-Out Forecast",
      retailer_demand = "Retailer Orders",
      opening_inventory = "Opening Inventory",
      intransit_t1 = "In-Transit Inventory",
      ending_inventory = "Ending Inventory",
      order_quantity = "Sellin Order Quantity",
      target_inventory = "Target Inventory",
      late_sales = "Late Sales",
      horizon = "Planning Horizon"
    ) %>%
    fmt_number(columns = where(is.numeric), decimals = 0) %>%
    tab_style(style = list(cell_text(whitespace = "nowrap")), 
              locations = cells_body(columns = week)) %>%
    cols_align(align = "center", columns = everything()) %>%
    fmt_date(columns = week, date_style = "yMd") %>%
    data_color(columns = horizon, 
               colors = scales::col_factor(palette = c("lightblue", "lightgray"), 
                                           domain = c("execution", "planning"))) %>%
    data_color(columns = late_sales, 
               colors = scales::col_numeric(palette = c("white", "red"), domain = c(0, 1))) %>%
    data_color(columns = target_inventory, 
               colors = scales::col_numeric(palette = c("lightgreen", "lightcoral"), domain = c(0, 600))) %>%
    tab_style(style = list(cell_text(weight = "bold")), 
              locations = cells_column_labels()) %>%
    tab_style(style = list(cell_fill(color = "#F0F0F0"), 
                           cell_text(weight = "bold")), 
              locations = cells_title()) %>%
    tab_footnote(footnote = "Execution: Current + Next weeks with retailer order visibility",
                 locations = cells_column_labels(columns = horizon)) %>%
    tab_footnote(footnote = "Late sales occur when demand exceeds available inventory",
                 locations = cells_column_labels(columns = late_sales)) %>%
    tab_footnote(footnote = "Green: <= 600 cases capacity, Red: > 600 cases",
                 locations = cells_column_labels(columns = target_inventory))
}


# Step 5: Run All Simulations (No changes needed)

sellinorder_fixedinvpolicy_100 <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "fixed",
    fixed_target = 100
  )

sellinorder_fixedinvpolicy_300 <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "fixed",
    fixed_target = 300
  )

sellinorder_fixedinvpolicy_400 <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "fixed",
    fixed_target = 400
  )

sellinorder_fixedinvpolicy_600 <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "fixed",
    fixed_target = 600
  )

# Step 6: Display the Formatted Results (No changes needed)
table_formating_function(
  sellinorder_fixedinvpolicy_400,
  policy_type = "fixed",
  policy_value = 400
)

# --- NEW SIMULATION: 1-Week Coverage Policy ---
sellinorder_weeksofcoverpolicy_1w <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "weeks_cover",
    weeks_cover = 1
  )

sellinorder_weeksofcoverpolicy_2w <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "weeks_cover",
    weeks_cover = 2
  )

sellinorder_weeksofcoverpolicy_3w <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "weeks_cover",
    weeks_cover = 3
  )

sellinorder_weeksofcoverpolicy_4w <-
  projected_sellin_order(
    sellout_fcast = sellout_fcast,
    current_inv = 100,
    target_method = "weeks_cover",
    weeks_cover = 4
  )


# --- NEW TABLE: 1-Week Coverage Policy ---
table_formating_function(
  sellinorder_weeksofcoverpolicy_1w,
  policy_type = "weeks_cover",
  policy_value = 3
)

# Step 5: Run ALL Simulations
# Fixed Target Policies
sim_fixed_100 <- projected_sellin_order(sellout_fcast, 100, "fixed", fixed_target = 100)
sim_fixed_300 <- projected_sellin_order(sellout_fcast, 100, "fixed", fixed_target = 300)
sim_fixed_400 <- projected_sellin_order(sellout_fcast, 100, "fixed", fixed_target = 400)
sim_fixed_600 <- projected_sellin_order(sellout_fcast, 100, "fixed", fixed_target = 600)
# Weeks of Cover Policies
sim_woc_1 <- projected_sellin_order(sellout_fcast, 100, "weeks_cover", weeks_cover = 1)
sim_woc_2 <- projected_sellin_order(sellout_fcast, 100, "weeks_cover", weeks_cover = 2)
sim_woc_3 <- projected_sellin_order(sellout_fcast, 100, "weeks_cover", weeks_cover = 3)
sim_woc_4 <- projected_sellin_order(sellout_fcast, 100, "weeks_cover", weeks_cover = 4)

# ---------------------------------------------------------------------------
# Step 7: Calculate KPIs and Create the Final Comparison Table
# ---------------------------------------------------------------------------

# Create a function to calculate KPIs for any simulation result
calculate_kpis <- function(sim_data, policy_name) {
  
  sim_data %>%
    # Calculate total demand to be used in metrics
    mutate(total_demand = pmax(retailer_demand, forecast)) %>%
    summarise(
      Policy = policy_name,
      Total_late_sales = sum(late_sales),
      Weeks_with_Stockouts = sum(late_sales > 0),
      
      # Calculate Fill Rate
      Fillrate = (sum(total_demand) - sum(late_sales)) / sum(total_demand),
      
      # Calculate Inventory Efficiency metrics
      Avg_Ending_Inventory = mean(ending_inventory[ending_inventory > 0], na.rm = TRUE),
      
      # COGS = Demand that was successfully met
      COGS = sum(total_demand) - sum(late_sales),
      
      # Inventory Turns = COGS / Average Inventory
      Inventory_Turns = COGS / Avg_Ending_Inventory,
      
      # Calculate Capacity metrics
      Weeks_Over_Capacity = sum(opening_inventory > 800) # Capacity set to 800
    )
}

# Apply the function to all simulation results
kpi_list <- list(
  calculate_kpis(sim_fixed_100, "Fixed 100"),
  calculate_kpis(sim_fixed_300, "Fixed 300"),
  calculate_kpis(sim_fixed_400, "Fixed 400"),
  calculate_kpis(sim_fixed_600, "Fixed 600"),
  calculate_kpis(sim_woc_1, "1-Week Cover"),
  calculate_kpis(sim_woc_2, "2-Week Cover"),
  calculate_kpis(sim_woc_3, "3-Week Cover"),
  calculate_kpis(sim_woc_4, "4-Week Cover")
)

# Combine all KPIs into one table
kpi_summary_table <- bind_rows(kpi_list)

# Create the formatted GT table
kpi_summary_table %>%
  gt(rowname_col = "Policy") %>%
  tab_header(
    title = "Inventory Policy Performance Comparison",
    subtitle = "Key Performance Metrics Across Different Ordering Policies"
  ) %>%
  # Group columns
  tab_spanner(label = "Service Performance", columns = c(Total_late_sales, Weeks_with_Stockouts, Fillrate)) %>%
  tab_spanner(label = "Inventory Efficiency", columns = c(Avg_Ending_Inventory, Inventory_Turns)) %>%
  tab_spanner(label = "Capacity Constraints", columns = Weeks_Over_Capacity) %>%
  
  # Format numbers and percentages
  fmt_number(columns = where(is.numeric), decimals = 0) %>%
  fmt_percent(columns = Fillrate, decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "NA") %>%
  
  # Add color scales (lower is better)
  data_color(
    columns = c(Total_late_sales, Weeks_with_Stockouts),
    colors = scales::col_numeric(palette = c("green", "yellow", "red"), domain = NULL)
  ) %>%
  data_color(
    columns = c(Weeks_Over_Capacity),
    colors = scales::col_numeric(palette = c("green", "yellow", "red"), domain = c(0, 5))
  ) %>%
  # Add color scales (higher is better)
  data_color(
    columns = c(Fillrate),
    colors = scales::col_numeric(palette = c("red", "yellow", "green"), domain = c(0.80, 1.0))
  ) %>%
  data_color(
    columns = c(Inventory_Turns),
    colors = scales::col_numeric(palette = c("gray", "lightgreen", "green"), domain = NULL, na.color = "#C8C8C8")
  ) %>%
  
  # Add footnotes
  tab_footnote(footnote = "Lower values indicate better service performance", locations = cells_column_labels(columns = Total_late_sales)) %>%
  tab_footnote(footnote = "Higher fill rates are better (>95% is excellent)", locations = cells_column_labels(columns = Fillrate)) %>%
  tab_footnote(footnote = "Higher inventory turns indicate more efficient inventory usage", locations = cells_column_labels(columns = Inventory_Turns)) %>%
  tab_footnote(footnote = "Zero weeks over capacity is ideal (capacity limit = 800 units)", locations = cells_column_labels(columns = Weeks_Over_Capacity))