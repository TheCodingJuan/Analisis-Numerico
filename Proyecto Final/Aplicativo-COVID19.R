library(shiny)
library(shinydashboard)
library(deSolve)
library(phaseR)
library(pracma)
library(dplyr)
library(ggplot2)
library(ggthemes)

hallarDivisor <- function(x){
  val <- 1
  repeat{
    temp <- x %/% val
    if(temp >= 0 && temp <= 10){break}
    else{ val <- val*10}
  }
  return(val)
}

sis_model = function (current_timepoint, state_values, parameters)
{
  
  S = state_values [1]        # susceptibles
  I = state_values [2]        # infectious
  
  with ( 
    as.list (parameters),     
    {
      # compute derivatives
      dS = (-beta * S * I) + (gamma * I)
      dI = ( beta * S * I) - (gamma * I)
      
      # combine results
      results = c (dS, dI)
      list (results)
    }
  )
}

sir<-function(tiempo,estado, parametros){
  
  with(as.list(c(estado,parametros)),{
    dS<- -beta*s*i
    dI<- beta*s*i-gamma*i
    dR<- gamma*i
    return(list(c(dS,dI,dR)))
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "Covid-19 Madrid"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("S.I.R Ode", tabName = "SIR", icon = icon("th")),
      menuItem("S.I.R lSode", tabName = "SIRR", icon = icon("th")),
      menuItem("S.I.S lSoda", tabName = "SI", icon = icon("th")),
      menuItem("S.I.S", tabName = "SIS", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "SIR",
              fluidRow(
                box(plotOutput("plot1", height = 500)),
                box(
                  title = "Días",
                  sliderInput("slider1", "Número de días:", 1, 77,77)
                ),
                box(
                  title = "Población",
                  numericInput(inputId = "slider2", label = "Población:", value = 3266126)
                ),
                box(
                  title = "Beta",
                  sliderInput("slider3", "Valor de Beta", 1.4, 2.5, 2.5)
                ),
                box(
                  title = "Gamma",
                  sliderInput("slider4", "Valor de Gamma", 0.1, 2.0 ,2.0)
                ),
                infoBox(title="DESCRIPCIÓN", value = NULL, subtitle = div("Suceptibles = Rojo",br()," Infectados = Verde",br(), "Recuperados = Azul") ,
                        icon = shiny::icon("bar-chart"), color = "light-blue", width = 5,
                        href = NULL, fill =FALSE)
              )
      ),
      tabItem(tabName = "SIRR",
              fluidRow(
                box(plotOutput("plot2", height = 500)),
                box(
                  title = "Días",
                  sliderInput("dias", "Número de días:", 1, 77, 77)
                ),
                box(
                  title = "Población",
                  numericInput( inputId = "poblacion", label = "Población:",value = 3266126)
                ),
                box(
                  title = "Beta",
                  sliderInput(inputId = "beta","Valor de Beta", 1.4,  2.5, 2.5)
                ),
                box(
                  title = "Gamma",
                  sliderInput(inputId = "gamma", "Valor de Gamma", 0.1 , 2.0 , 2.0 )
                ),
                infoBox(title="DESCRIPCIÓN", value = NULL, subtitle = div("Suceptibles = Rojo",br()," Infectados = Verde",br(), "Recuperados = Azul") ,
                        icon = shiny::icon("bar-chart"), color = "light-blue", width = 5,
                        href = NULL, fill =FALSE)
              )
      ),
      tabItem(tabName = "SI",
              fluidRow(
                box(plotOutput("plot3", height = 500)),
                box(
                  title = "Días",
                  sliderInput("diasSI", "Número de días:", 1, 77, 77)
                ),
                box(
                  title = "Población",
                  sliderInput(inputId = "poblacionSI", "Población:",1,3266126,3266126)
                ),
                infoBox(title="DESCRIPCIÓN", value = NULL, subtitle = div("Suceptibles = Azul",br()," Infectados = Rojo") ,
                        icon = shiny::icon("bar-chart"), color = "light-blue", width = 5,
                        href = NULL, fill =FALSE)
                
              )
      ),
      tabItem(tabName = "SIS",
              fluidRow(
                box(plotOutput("plot4", height = 500)),
                box(
                  title = "Días",
                  sliderInput("diasSIS", "Número de días:", 1, 77, 77)
                ),
                box(
                  title = "Población",
                  sliderInput(inputId = "poblacionSIS", "Población:",1,3266126,3266126)
                ),
                infoBox(title="DESCRIPCIÓN", value = NULL, subtitle = div("Suceptibles = Azul",br()," Infectados = Rojo") ,
                        icon = shiny::icon("bar-chart"), color = "light-blue", width = 5,
                        href = NULL, fill =FALSE)
                
              )
      )
      
    )
  )
)



server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    
    divisor <- hallarDivisor(input$slider2)
    
    inic <- c(s = input$slider2/divisor , i = 1 / divisor, r = 0)
    
    parametros <- c(beta = (input$slider3 /10), gamma = (input$slider4 / 10))
    tiempo <- seq(1 , input$slider1, by=1)
    
    out <- ode(y = inic, times = tiempo, func = sir, parms = parametros)
    out <- as.data.frame(out)
    out$time <- NULL
    head(out, 77)
    #grï¿½fica
    matplot(x = tiempo, y = out, type = "l",
            xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR ODE",
            lwd = 1, lty = 1, bty = "l", col = 2:4)
    
  })
  
  output$plot2 <- renderPlot(
    {
      divisor <- hallarDivisor(input$poblacion)
      
      inic <- c(s = input$poblacion/divisor , i = 1 / divisor, r = 0)
      
      parametros <- c(beta = (input$beta /10), gamma = (input$gamma / 10))
      tiempo <- seq(1 , input$dias, by=1)
      
      out <- lsode(y = inic, times = tiempo, func = sir, parms = parametros)
      out <- as.data.frame(out)
      
      susceptibles = out$s
      
      
      
      out$time <- NULL
      head(out, 77)
      #grï¿½fica
      matplot(x = tiempo, y = out, type = "l",
              xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR lSode",
              lwd = 1, lty = 1, bty = "l", col = 2:4)
    }
  )
  
  output$plot3 <- renderPlot(
    {
      contact_rate = 5                    # number of contacts per day by common person
                                          #SRC: https://www.dw.com/es/coronavirus-el-factor-r-y-los-cuatro-par%C3%A1metros-de-contagio/a-53212796
      transmission_probability = 0.3      # transmission probability From COVID-19
      infectious_period = 15 
      beta_value =  contact_rate * transmission_probability
      gamma_value = 1 / infectious_period
      Ro = beta_value / gamma_value
      parameter_list = c (beta = beta_value, gamma = gamma_value)
      X = input$poblacionSI      # susceptible hosts
      Y = 0.00001       # infectious hosts

      N = X + Y 
      initial_values = c (S = X/N, I = Y/N)
      timepoints = seq (1, input$diasSI, by=1)
      output = lsoda (initial_values, timepoints, sis_model, parameter_list)
      # susceptible hosts over time
      plot (S ~ time, data = output, type='l', ylim = c(0,1), col = 'blue', ylab = 'S, I, S', main = 'SIS epidemic') 

      # remain on same frame
      par (new = TRUE)    

      # infectious hosts over time
      plot (I ~ time, data = output, type='l', ylim = c(0,1), col = 'red', ylab = '', axes = FALSE) 
    }
  )
  
  output$plot4 <- renderPlot(
    {
      contact_rate = 5                    # number of contacts per day by common person
      #SRC: https://www.dw.com/es/coronavirus-el-factor-r-y-los-cuatro-par%C3%A1metros-de-contagio/a-53212796
      transmission_probability = 0.3      # transmission probability From COVID-19
      infectious_period = 15 
      beta_value =  contact_rate * transmission_probability
      gamma_value = 1 / infectious_period
      Ro = beta_value / gamma_value
      parameter_list = c (beta = beta_value, gamma = gamma_value)
      X = input$poblacionSIS      # susceptible hosts
      Y = 0.00001       # infectious hosts
      
      N = X + Y 
      initial_values = c (S = X/N, I = Y/N)
      timepoints = seq (1, input$diasSIS, by=1)
      output = ode(initial_values, timepoints, sis_model, parameter_list)
      # susceptible hosts over time
      plot (S ~ time, data = output, type='l', ylim = c(0,1), col = 'blue', ylab = 'S, I, S', main = 'SIS epidemic') 
      
      # remain on same frame
      par (new = TRUE)    
      
      # infectious hosts over time
      plot (I ~ time, data = output, type='l', ylim = c(0,1), col = 'red', ylab = '', axes = FALSE) 
    }
  )
  
}

shinyApp(ui, server)