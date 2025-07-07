# Load necessary packages
library(ompr)
library(ompr.roi)
library(ROI)
library(ROI.plugin.glpk)
library(magrittr)

# Define Factory Classes
factory_classes <- data.frame(
  class = c(1, 2, 3),
  min_rate = c(1.0, 0.5, 1.5),   
  max_rate = c(2.0, 2.5, 4.0),   
  weekly_cost_min = c(10000, 25000, 30000), 
  variable_cost = c(180, 130, 250), 
  setup_cost = c(20000, 10000, 5000), 
  available_factories = c(11, 9, 5) 
)

# Define Phases (6 phases)
phases <- data.frame(
  phase = 1:6,
  weeks = c(5, 5, 3, 3, 4, 6),  
  demand_per_hour = c(14, 33, 28, 20, 42, 24)
)

# 20% extra production factor
extra_production_factor <- 1.2

# Define Model with 20% extra production
model <- MIPModel() %>%
  # Decision Variables
  add_variable(x[i, p], i = 1:3, p = 1:6, type = "integer", lb = 0) %>%
  add_variable(y[i, p], i = 1:3, p = 1:6, type = "binary") %>%
  add_variable(z[i, p], i = 1:3, p = 2:6, type = "binary") %>%
  add_variable(prod[i, p], i = 1:3, p = 1:6, type = "continuous", lb = 0) %>% 
  
  # Objective Function: Minimize Total Cost
  set_objective(
    sum_expr(x[i, p] * factory_classes$weekly_cost_min[i] * phases$weeks[p], i = 1:3, p = 1:6) +
      sum_expr((prod[i, p] - x[i, p] * factory_classes$min_rate[i]) * 
                 factory_classes$variable_cost[i] * phases$weeks[p], i = 1:3, p = 1:6) +
      sum_expr(z[i, p] * factory_classes$setup_cost[i], i = 1:3, p = 2:6),
    sense = "min"
  ) %>%
  
  # Constraint 1: Demand Satisfaction
  add_constraint(sum_expr(prod[i, p], i = 1:3) >= phases$demand_per_hour[p] * extra_production_factor, p = 1:6) %>%
  
  # Constraint 2: Production Limits
  add_constraint(prod[i, p] >= x[i, p] * factory_classes$min_rate[i], i = 1:3, p = 1:6) %>%
  add_constraint(prod[i, p] <= x[i, p] * factory_classes$max_rate[i] * extra_production_factor, i = 1:3, p = 1:6) %>%
  
  # Constraint 3: Activation Limits
  add_constraint(x[i, p] <= y[i, p] * factory_classes$available_factories[i], i = 1:3, p = 1:6) %>%
  
  # Constraint 4: Setup Cost Tracking (only applies when p > 1)
  add_constraint(z[i, p] >= y[i, p] - y[i, p - 1], i = 1:3, p = 2:6) %>%
  
  # Constraint 5: Factory Continuity (tracking new setups only)
  add_constraint(x[i, p] - x[i, p - 1] <= z[i, p] * factory_classes$available_factories[i], i = 1:3, p = 2:6) %>%
  
  # Constraint 6: Maximum Factory Limits
  add_constraint(x[i, p] <= factory_classes$available_factories[i], i = 1:3, p = 1:6) 


# Solve the MILP Model using the GLPK solver
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

# Extract the solution
solution <- get_solution(result, x[i, p])
print(solution)

objective_value(result)

# Create a model without the extra production factor
model_no_extra <- MIPModel() %>%
  # Decision Variables
  add_variable(x[i, p], i = 1:3, p = 1:6, type = "integer", lb = 0) %>%
  add_variable(y[i, p], i = 1:3, p = 1:6, type = "binary") %>%
  add_variable(z[i, p], i = 1:3, p = 2:6, type = "binary") %>%
  add_variable(prod[i, p], i = 1:3, p = 1:6, type = "continuous", lb = 0) %>% 
  
  # Objective Function: Minimize Total Cost
  set_objective(
    sum_expr(x[i, p] * factory_classes$weekly_cost_min[i] * phases$weeks[p], i = 1:3, p = 1:6) +
      sum_expr((prod[i, p] - x[i, p] * factory_classes$min_rate[i]) * 
                 factory_classes$variable_cost[i] * phases$weeks[p], i = 1:3, p = 1:6) +
      sum_expr(z[i, p] * factory_classes$setup_cost[i], i = 1:3, p = 2:6),
    sense = "min"
  ) %>%
  
  # Constraint 1: Demand Satisfaction (without the extra production factor)
  add_constraint(sum_expr(prod[i, p], i = 1:3) >= phases$demand_per_hour[p], p = 1:6) %>%
  
  # Constraint 2: Production Limits
  add_constraint(prod[i, p] >= x[i, p] * factory_classes$min_rate[i], i = 1:3, p = 1:6) %>%
  add_constraint(prod[i, p] <= x[i, p] * factory_classes$max_rate[i], i = 1:3, p = 1:6) %>%
  
  # Constraint 3: Activation Limits
  add_constraint(x[i, p] <= y[i, p] * factory_classes$available_factories[i], i = 1:3, p = 1:6) %>%
  
  # Constraint 4: Setup Cost Tracking (only applies when p > 1)
  add_constraint(z[i, p] >= y[i, p] - y[i, p - 1], i = 1:3, p = 2:6) %>%
  
  # Constraint 5: Factory Continuity (tracking new setups only)
  add_constraint(x[i, p] - x[i, p - 1] <= z[i, p] * factory_classes$available_factories[i], i = 1:3, p = 2:6) %>%
  
  # Constraint 6: Maximum Factory Limits
  add_constraint(x[i, p] <= factory_classes$available_factories[i], i = 1:3, p = 1:6)

# Solve the MILP Model without extra production factor using GLPK solver
result_no_extra <- solve_model(model_no_extra, with_ROI(solver = "glpk", verbose = TRUE))

# Extract the solution
solution_no_extra <- get_solution(result_no_extra, x[i, p])
print(solution_no_extra)

objective_value(result_no_extra)

# Now we can calculate the cost difference
cost_difference <- objective_value(result) - objective_value(result_no_extra)
print(cost_difference)



