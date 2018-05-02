library(shiny)
ui <- shinyUI(fluidPage(
  titlePanel("Donation Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.main=='plot'",
                       selectInput("typeInput","gradient type",choices = c("state","athlete","school","marriage","Greek"),selected="state"),
                       
                       conditionalPanel(
                         condition="input.typeInput=='state'",
                         radioButtons("responseInput","Donation type",choices = c("avg_donation","median_donation"), selected="avg_donation")),
                       
                       conditionalPanel(
                         condition="input.typeInput=='athlete'",
                         radioButtons("responseInput","Donation type",choices = c("median_donation"), selected="median_donation"),
                         radioButtons("variable_input","variables",choices=c("years_AfterFirstDonation","years_sinceGraduating","Total Giving"), selected="years_AfterFirstDonation")),
                       
                       conditionalPanel(
                         condition="input.typeInput=='school'",
                         radioButtons("responseInput","Donation type",choices = c("median_donation"), selected="median_donation"),
                         radioButtons("variable_input","variables",choices=c("years_sinceGraduating"), selected="years_sinceGraduating")),
                       
                       conditionalPanel(
                         condition="input.typeInput=='marriage'",
                         radioButtons("responseInput","Donation type",choices = c("median_donation")),
                         radioButtons("variable_input","variables",choices=c("years_sinceGraduating"))),
                       
                       conditionalPanel(
                         condition="input.typeInput=='Greek'",
                         radioButtons("responseInput","Donation type",choices = c("median_donation")),
                         radioButtons("variable_input","variables",choices=c("years_sinceGraduating")))
                       
      ),
      
      
      conditionalPanel(
        condition="input.main=='model'",
        numericInput("countDegree","countofDegree",1,min=1,max=4),
        selectInput("school","school",choices = c("Farmer","Engineering/Computing","Arts & Science",
                                                  "Education/Health","FA","IS"),selected = "FA"),
        selectInput("year","year",choices = c("Middle Aged","Young","Elder"),selected = "Middle Aged"),
        selectInput("Greek","countofGreek",choices = c("no Greek","Greek"),selected="Greek"),
        selectInput("gender","gender",choices = c("female","male","unknown"),selected="female"),
        selectInput("degree","degree",choices = c("Undergraduate","Graduate","Associate"),selected="Undergraduate"),
        selectInput("athlete","athlete",choices = c("No","Yes"),selected = "No")
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        tabPanel(title="plot", plotOutput("figure")),
        tabPanel(title="model",h3("10-year predictive donation"),h4(verbatimTextOutput("prediction"))),
        id="main"
      )
      
    )
    
  ))
  
)




################# SERVER





library(shiny)
pred_function <- function(degree, athlete, countDegree, school, year, Greek, gender){
  inputdata<-c(degree, athlete, countDegree, school, year, Greek, gender)
  pred_data<-as.data.frame(t(inputdata))
  colnames(pred_data)<-c("Degree.Category","athlete","count.Degree","School", "year", "Greek","Gender")
  pred_data$count.Degree <- as.numeric(pred_data$count.Degree)
  prediction<-predict(final_mod, pred_data)
  return(prediction)
}

server <- shinyServer(
  function(input,output,session)
  {
    observeEvent(input$state, {}  )
    output$figure <- renderPlot({
      if(input$typeInput == "state" & input$responseInput == "avg_donation") {
        plot <- avg.map
        
      }
      else if (input$typeInput == "state" & input$responseInput == "median_donation"){
        plot <- med.map
      }
      else if(input$typeInput == "athlete" & input$variable_input == "Total Giving") {
        plot <- cf
      }
      else if(input$typeInput == "athlete" & input$variable_input == "years_sinceGraduating" & 
              input$responseInput == "median_donation") {
        plot <- yg_med 
      }
      else if(input$typeInput == "athlete" & input$variable_input == "years_AfterFirstDonation" & 
              input$responseInput == "median_donation") {
        plot <- med_dot
      }
      else if(input$typeInput == "school" & input$variable_input == "years_sinceGraduating" & 
              input$responseInput == "median_donation") {
        plot <- med_school
      }
      else if(input$typeInput == "Greek" & input$variable_input == "years_sinceGraduating" & 
              input$responseInput == "median_donation") {
        plot <- med_greek
      }
      else if (input$typeInput == "marriage" & input$variable_input == "years_sinceGraduating" & 
               input$responseInput == "median_donation") {
        plot <- med_marriage
      }
      plot
    })
    
    
    
    
    output$prediction<-renderText({
      
      pred_function(input$degree, input$athlete, input$countDegree, input$school, input$year, input$Greek, input$gender)
    })
  }
)
shinyApp(ui = ui,server = server)