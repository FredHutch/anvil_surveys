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
  
  tool_user <- function(){
    val <- c(sd_value(use_interactive), sd_value(use_workflows), sd_value(services_use))
    if (all(is.null(val))) return(FALSE)
    if (any(val) == 1) return(TRUE)
    return(FALSE)
  }
  
  sd_skip_if(
    #page 1 send to screenout if necessary
    sd_value(consent) == 0 ~ "screenout",
    #user logic
    sd_value(team_member) == 1 ~ "page4",
    previous_user() ~ "page3b",
    potential_user() ~ "page3c"
  )
  
  sd_show_if(
  
    #page 2 Demographics
    sd_value(role) == "other" ~ "role_other",
    sd_value(institution_affil) == "other" ~ "institution_affil_other",
    sd_value(team_member) == 0 ~ "user_description",
    
    #page 3a Utilization
    "other" %in% sd_value(why_description) ~ "why_description_other",
    "consortium" %in% sd_value(why_external) ~ "consortia_analysis",
    "consortium" %in% sd_value(why_external) ~ "consortia_data",
    "consortium" %in% sd_value(why_external) ~ "consortia_affil",
    (sd_is_answered("nps_score") & sd_value(nps_score) < 7) ~ "nps_score_explain",
    
    #page3b previous use
    "other" %in% sd_value(why_stopped) ~ "why_stopped_other",
    "other" %in% sd_value(what_would_help_previous) ~ "what_would_help_previous_other",
    
    #page3c potential use
    "other" %in% sd_value(what_would_help_potential) ~ "what_would_help_potential_other",
    
    #page 4 specific use data submission
    sd_value(use_submit_data) == 1 ~ "use_submit_raw_summarized",
    sd_value(use_submit_data) == 1 ~ "use_submit_satisfaction",
    (sd_is_answered("use_submit_satisfaction") & sd_value(use_submit_satisfaction) < 5) ~ "what_would_help_submit",
    
    #page 5 specific use group support
    sd_value(use_support) == 1 ~ "use_support_satisfaction",
    (sd_is_answered("use_support_satisfaction") & sd_value(use_support_satisfaction) < 5) ~ "what_would_help_use_support",
    
    #page 6 specific us running analyses
    sd_value(use_interactive) == 1 ~ "which_interactive_use_anvil",
    sd_value(use_interactive) == 1 ~ "which_interactive_use_separate",
    (sd_value(use_interactive) == 1 | sd_value(use_workflows) == 1) ~ "whose_data_use",
    "anvil_data" %in% sd_value(whose_data_use) ~ "anvil_data_type",
    "anvil_data" %in% sd_value(whose_data_use) ~ "anvil_data_control",
    "controlled_access" %in% sd_value(anvil_data_control) ~ "controlled_datasets_use",
    "other" %in% sd_value(controlled_datasets_use) ~ "controlled_datasets_use_other",
    tool_user() ~ "analyses_satisfaction",
    (sd_is_answered("analyses_satisfaction") & sd_value(analyses_satisfaction) < 5) ~ "what_would_help_services",
    
    #page 7 group supervision
    sd_value(use_supervise) == 1 ~ "use_supervise_questions",
    sd_value(use_supervise) == 1 ~ "use_supervise_satisfaction",
    (sd_is_answered("use_supervise_satisfaction") & sd_value(use_supervise_satisfaction) < 5) ~ "what_would_help_supervise",
    
    #page8 educational support
    sd_value(use_ed) == 1 ~ "use_ed_satisfaction",
    (sd_is_answered("use_ed_satisfaction") & sd_value(use_ed_satisfaction) < 5) ~ "what_would_help_ed",
    
    #page9 training needs
    sd_value(team_member) == 0 ~ "page9",
    sd_value(preferred_learning_other) < 4 ~ "preferred_learning_other_specify",
    (sd_is_answered("training_satisfaction") & sd_value(training_satisfaction) < 5) ~ "training_satisfaction_barrier",
    (sd_is_answered("training_satisfaction") & sd_value(training_satisfaction) == 5) ~ "training_wanted"
  )

  sd_server(db = db)

}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
