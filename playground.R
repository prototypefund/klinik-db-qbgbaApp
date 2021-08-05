library(DBI)
library(RMariaDB)
library(qbgbaAnalyst)
library(shiny)

# con <- dbConnect(RMariaDB::MariaDB(),
#                  host     = "localhost",
#                  port     = 3306,
#                  username = keyring::key_list("mysql-localhost")[1,2],
#                  password = keyring::key_get("mysql-localhost", "gbadatauser"),
#                  dbname   = "gbadata"
# )


ui <- fluidPage(

)

server <- function(input, output, session) {

}

shinyApp(ui, server)
