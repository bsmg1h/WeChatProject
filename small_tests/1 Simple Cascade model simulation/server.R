##server.R

function(input,output){
  
  result <- simulate(g,pb_matrix)
  
  p <- reactive({node_color(g,result,input$propagation_time)
  })
  
  output$image = renderPlot({
    plot(g, layout = coords,
         vertex.size = 6,
         vertex.label = NA,
         edge.width = 1,
         edge.arrow.size = 0.2,
         edge.arrow.width = 1,
         vertex.color = colorbar[p()])
  })
}