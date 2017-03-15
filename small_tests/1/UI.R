## ui.R
fluidPage(  
  sliderInput(inputId = "propagation_time",
              label = h5("Observe the propagation status at the specific time "),
              value = 0,
              min = 0,
              max = 20),
  
  plotOutput("image",
             width = 800,
             height = 800
  )
)