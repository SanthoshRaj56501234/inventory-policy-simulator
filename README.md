#  Inventory Policy Simulator

Simulates and compares 8 inventory ordering policies to identify 
the optimal strategy balancing service level and inventory efficiency.

##  Business Problem
Distributors over-order to avoid stockouts, tying up warehouse capacity. 
This tool helps planners choose the right ordering policy by showing 
the trade-off between fill rate and inventory turns.

##  What This Analysis Does
- Simulates Fixed Target policies (100, 300, 400, 600 units)
- Simulates Weeks-of-Cover policies (1, 2, 3, 4 weeks)
- Compares all 8 policies on Fill Rate, Stockouts, Inventory Turns & Capacity

##  Technical Stack
R, tidyverse, timetk, gt

##  Files
| File | Data |
|------|-------------|
| `inventry_fore.R` | 'Forecast.csv' |
