library(shiny)
library(tidyverse)


df <- read_csv("Qualification landscape all levels all sector subject areas all qualification types.csv")


server <- function(input, output, session) {
  filtered_df <- reactive({
    df2 <- df
    if (input$search != "") {
      df2 <- df2 %>% 
        filter(str_detect(Title, regex(input$search, ignore_case = TRUE)))
    }
    df2 %>% 
      filter(Level == input$level | input$level == "All levels") %>% 
      filter(`Sector Subject Area` == input$ssa | input$ssa == "All sector subject areas") %>% 
      filter(`Included in performance tables` == input$pt | input$pt == "All qualifications") %>% 
      filter(input$type == "all" | (Type != 'GCSE' & Type != 'AS/A level')) %>% 
      arrange(desc(Certificates))
  })
  
  plot_df <- reactive({
     filtered_df() %>% 
      group_by(Type) %>% 
      summarise(Certificates = sum(Certificates),
                `Available qualifications` = n()) %>% 
      mutate(Type = fct_reorder(Type, !!sym(input$order))) %>% 
      pivot_longer(cols = -Type) %>% 
      group_by(name) %>% 
      mutate(perc = value / sum(value)) %>% 
      ungroup() %>% 
      mutate(label = paste0(scales::comma(value, accuracy = 1), " (", scales::percent(perc, accuracy = 0.1), ")")) %>% 
      mutate(perc = if_else(name == "Certificates", perc, -perc))
  })
  
  
  output$plot <- renderPlot({
    plot_df() %>% 
      ggplot() +
      geom_col(aes(Type, perc, fill = name)) +
      geom_text(data = filter(plot_df(), perc < 0),
                aes(Type, perc, label = label), hjust = "top", nudge_y = -0.01) +
      geom_text(data = filter(plot_df(), perc >= 0),
                aes(Type, perc, label = label), hjust = "bottom", nudge_y = 0.01) +
      scale_x_discrete(name = "") +
      scale_y_continuous(name = "% share",
                         limits = c(-1, 1), expand = c(0, 0),
                         labels = function(x) paste0(abs(x * 100), "%"),
                         breaks = seq(-1, 1, 0.2)) +
      coord_flip() +
      theme_bw(15) +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
  })
  
  output$table <- DT::renderDT(filtered_df(), rownames = FALSE, options = list(dom = "tip"))
  
  output$filtered_table <- downloadHandler(
    filename = function() { paste0("data-", Sys.Date(), ".csv") },
    content = function(file) { write_csv(filtered_df(), file) }
  )
}