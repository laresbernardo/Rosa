# Hide/Show navigation tabs function
nav_fun <- function(exec_type, matriz) {
  exec_type <- as.numeric(exec_type)
  nav_list <- matriz[exec_type, ]
  if (nav_list[1] == 1) {
    shinyjs::show(selector = '.navbar-nav a[data-value="Configuration"]')
  } else {
    shinyjs::hide(selector = '.navbar-nav a[data-value="Configuration"]')
  }
  if (nav_list[2] == 1) {
    shinyjs::show(selector = '.navbar-nav a[data-value="Execution"]')
    if (exec_type == 3) {
      shinyjs::hide("run_back")
      shinyjs::show("run_back3")
    }
  } else {
    shinyjs::hide(selector = '.navbar-nav a[data-value="Execution"]')
    if (exec_type == 3) {
      shinyjs::show("run_back")
      shinyjs::hide("run_back3")
    }
  }
  if (nav_list[3] == 1) {
    shinyjs::show(selector = '.navbar-nav a[data-value="Results"]')
    if (exec_type == 4) {
      shinyjs::hide("res_back")
      shinyjs::show("res_back4")
    }
  } else {
    shinyjs::hide(selector = '.navbar-nav a[data-value="Results"]')
    if (exec_type == 4) {
      shinyjs::show("res_back4")
      shinyjs::hide("res_back")
    }
  }
}

dat_next_button <- function(exec_typeype) {
  # Refresh and fresh new model required configurations
  if (exec_typeype %in% c(1, 2)) {
    button <- hover_action_button(
      "dat_next",
      align = "right", "Configuration", icon = icon("forward"), button_animation = "sweep-to-right", icon_animation = "forward",
      style = "margin-top:20px;font-weight:bold;border-color:#F36633;text-padding:-2px;text-align:center;border-radius:5px;border-width:2px"
    )
  }
  # To recreate, straight to Execution
  if (exec_typeype == 3) {
    button <- hover_action_button(
      "dat_next3",
      align = "right", "Execution", icon = icon("forward"), button_animation = "sweep-to-right", icon_animation = "forward",
      style = "margin-top:20px;font-weight:bold;border-color:#F36633;text-padding:-2px;text-align:center;border-radius:5px;border-width:2px; margin-left: 5px;" # Adjust margin-right as needed
    )
  }
  # For dashboard, straight to Results
  if (exec_typeype == 4) {
    button <- hover_action_button(
      "dat_next4",
      align = "right", "Results", icon = icon("forward"), button_animation = "sweep-to-right", icon_animation = "forward",
      style = "margin-top:20px;font-weight:bold;border-color:#F36633;text-padding:-2px;text-align:center;border-radius:5px;border-width:2px"
    )
  }
  return(button)
}
