# Package setup --------------------------------

# Install required packages:
# ! Comment out when deploying to rsconnect
# install.packages("pak
# pak::pak("surveydown-dev/surveydown)

# Load packages
library(surveydown)

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect()

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
  
  previous_user <- function(){
    val <- sd_value("user_description")
    if (is.null(val)) return(FALSE)
    if (val == "aware_previous") return(TRUE)
    return(FALSE)
  }
  
  potential_user <- function(){
    val <- sd_value("user_description")
    if (is.null(val)) return(FALSE)
    if (val %in% c("aware_explore", "aware_unused", "unaware")) return(TRUE)
    return(FALSE)
  }
  
  sd_skip_if(
    input$team_member == 1 ~ "page4",
    previous_user() ~ "page3b",
    potential_user() ~ "page3c"
  )
  
  sd_show_if(
    #page 1 send to screenout if necessary
    
    #page 2 Demographics
    input$role == "other" ~ "role_other",
    input$institution_affil == "other" ~ "institution_affil_other",
    input$team_member == 0 ~ "user_description",
    
    #page 3a Utilization
    "other" %in% input$why_description ~ "why_description_other",
    "consortium" %in% input$why_external ~ "consortia_analysis",
    "consortium" %in% input$why_external ~ "consortia_data",
    "consortium" %in% input$why_external ~ "consortia_affil",
    input$nps_score < 7 ~ "nps_score_explain",
    
    #page3b previous use
    "other" %in% input$why_stopped ~ "why_stopped_other",
    "other" %in% input$what_would_help_previous ~ "what_would_help_previous_other",
    
    #page3c potential use
    "other" %in% input$what_would_help_potential ~ "what_would_help_potential_other",
    
    #page 4 specific use data submission
    input$use_submit_data == 1 ~ "use_submit_raw_summarized",
    input$use_submit_data == 1 ~ "use_submit_satisfaction",
    input$use_submit_satisfaction < 5 ~ "what_would_help_submit",
    
    #page 5 specific use group support
    input$use_support == 1 ~ "use_support_satisfaction",
    input$use_support_satisfaction < 5 ~ "what_would_help_use_support",
    
    #page 6 specific us running analyses
    input$use_interactive == 1 ~ "which_interactive_use_anvil",
    input$use_interactive == 1 ~ "which_interactive_use_separate",
    (input$use_interactive == 1 | input$use_workflows == 1) ~ "whose_data_use",
    "anvil_data" %in% input$whose_data_use ~ "anvil_data_type",
    "anvil_data" %in% input$whose_data_use ~ "anvil_data_control",
    "controlled_access" %in% input$anvil_data_control ~ "controlled_datasets_use",
    "other" %in% input$controlled_datasets_use ~ "controlled_datasets_use_other",
    any(c(input$use_interactive, input$use_workflows, input$services_use) == 1) ~ "analyses_satisfaction",
    input$analyses_satisfaction < 5 ~ "what_would_help_services",
    
    #page 7 group supervision
    input$use_supervise == 1 ~ "use_supervise_questions",
    input$use_supervise == 1 ~ "use_supervise_satisfaction",
    input$use_supervise_satisfaction < 5 ~ "what_would_help_supervise",
    
    #page8 educational support
    input$use_ed == 1 ~ "use_ed_satisfaction",
    input$use_ed_satisfaction < 5 ~ "what_would_help_ed",
    
    #page9 training needs
    input$team_member == 0 ~ "page9",
    input$preferred_learning_other < 4 ~ "preferred_learning_other_specify"
  )

  sd_server(db = db)

}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
