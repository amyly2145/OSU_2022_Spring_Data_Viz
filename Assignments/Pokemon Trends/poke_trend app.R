library(shiny)
library(tidyverse)
library(shinyWidgets)
library(ggrepel)
library(gargle)

# Loading data ----
pokemon <- read.csv("https://gist.githubusercontent.com/armgilles/194bcff35001e7eb53a2a8b441e8b2c6/raw/92200bc0a673d5ce2110aaad4544ed6c4010f687/pokemon.csv")

pokemon <- pokemon %>%  
  pivot_longer(cols = c(`Total`,`HP`, `Attack`, `Defense`, `Sp..Atk`, `Sp..Def`, `Speed`), 
               names_to = "Stat", 
               values_to = "Values") %>% 
  mutate(Stat = recode(Stat, 
                       "Sp..Atk" = "Sp.Atk",
                       "Sp..Def" = "Sp.Def")) %>% 
  filter(!(grepl('Mega|Primal', Name))) %>% 
  mutate(across(c("Type.1", "Type.2"), na_if, "", .names = "{.col}_changed")) %>% 
  rowwise() %>% 
  mutate(Type = paste(sort(c(Type.1_changed, Type.2_changed)), collapse = ", ")) %>% 
  pivot_longer(cols = c(`Type.1`,`Type.2`), 
               names_to = "Type_Split", 
               values_to = "Values2") %>% 
  filter(Values2 != "") %>% 
  select(Name, Generation, Stat, Values, Type, Type_Split, Values2) %>% 
  rename(Type_Combo = Type,
         Type = Values2) %>% 
  # where 0 means there is only 1 type and 1 means there are 2 types.
  mutate(Type = factor(Type, levels = c("Normal","Fire","Water",
                                        "Electric","Grass","Ice",
                                        "Fighting","Poison", "Ground",
                                        "Flying","Psychic","Bug",
                                        "Rock","Ghost","Dragon",
                                        "Dark","Steel","Fairy")))


# ui.R ----
ui <- fluidPage(
  titlePanel("Pokemon Trends"),  # Add a title panel
  fluidRow(style="text-align: center;",
           "Instructions: To start your exploration, select a primary type. \n Then click on the info button below to choose combinations of secondary types to compare to the pure primary types!", 
           tags$hr(style="border-color: black;")), 
  sidebarLayout(  # Make the layout a sidebarLayout
    sidebarPanel(
      selectInput(inputId = "type", 
                  label = "Select Primary Type", 
                  choices = unique(pokemon$Type),
                  selected = "Bug"),
      dropdownButton(label = "Click here to select secondary types",
                     status = "default",
                     width = 400,
                     tooltip = TRUE,
                     icon = icon("info"),
                     tags$label("Choose :"),
                     fluidRow(column(4,wellPanel(checkboxGroupInput("type2a", 
                                                                    label = NULL, 
                                                                    choices = list("Normal" = "Normal",
                                                                                   "Fire" = "Fire",
                                                                                   "Water" = "Water",
                                                                                   "Electric" = "Electric",
                                                                                   "Grass" = "Grass",
                                                                                   "Ice" = "Ice"), 
                                                                    selected = "Normal"))),
                              column(4,wellPanel(checkboxGroupInput("type2b", 
                                                                    label = NULL,
                                                                    choices = list("Fighting" = "Fighting",
                                                                                   "Poison" = "Poison",
                                                                                   "Ground" = "Ground",
                                                                                   "Flying" = "Flying",
                                                                                   "Psychic" = "Psychic",
                                                                                   "Bug" = "Bug")))),
                              column(4,wellPanel(checkboxGroupInput("type2c",
                                                                    label = NULL,
                                                                    choices = list("Rock" = "Rock",
                                                                                   "Ghost" = "Ghost",
                                                                                   "Dragon" = "Dragon",
                                                                                   "Dark" = "Dark",
                                                                                   "Steel" = "Steel",
                                                                                   "Fairy" ="Fairy"))))
                     )),
      (checkboxGroupInput("gen", 
                          h4("Select Generation"), 
                          choices = list("Gen 1" = 1, 
                                         "Gen 2" = 2, 
                                         "Gen 3" = 3, 
                                         "Gen 4" = 4, 
                                         "Gen 5" = 5, 
                                         "Gen 6" = 6), 
                          selected = 1))
      
    ), #end sidebar panel
    
    mainPanel(plotOutput("plot"), br()) # add main panel
    
  ), # end sidepanel layout
  
  fluidRow(column(6,
                  tableOutput("mytable")
  ),
  
  column(6,
         plotOutput("pop_plot"), br())
  
  ) # end fluidrow
) #end fluid page


# server.R ----
server <- function(input, output) {
  
  # make second type reactive
  
  sec_type <- reactive({
    
    c(input$type2a, input$type2b,input$type2c)
    
  }) 

  # create the boxplots for pure types and secondary types  
  output$plot <- renderPlot({ 
    
    #create validate warnings
    validate(
    need(input$gen, 'Please check at least one Generation!'),
    need(sec_type() != '', 'Please select another Secondary Type! \n Note that not all type combinations exist!')
  )
    
    pokemon %>%
      filter(grepl(input$type, Type_Combo)) %>% 
      filter(grepl(paste0("^", input$type, "$|", paste0(sec_type(), collapse="|")), Type_Combo)) %>% 
      mutate(Type_1  = input$type,
             Type_2 = str_match(Type_Combo,(paste0(".*(",paste0(sec_type(), collapse="|"),").*")))[,2]) %>%
      mutate(Plot_Type = ifelse(is.na(Type_2),paste0(input$type), Type_2))  %>% 
      filter(Generation %in% input$gen) %>% 
      ggplot(mapping = aes(y=Values, 
                           x=Plot_Type, 
                           fill = Plot_Type))+
      geom_boxplot(aes(fill = Plot_Type))+
      facet_wrap(~Stat, scales = "free")+
      labs(y =  "Stat Values",
           x = "",
           title = "Distribution of Stats by Pure and Secondary Types")+
      guides(fill=guide_legend(ncol=3, title = "Types"))+
      scale_fill_manual(values=c(Normal = '#A8A77A',
                                 Fire = '#EE8130',
                                 Water = '#6390F0',
                                 Electric = '#F7D02C',
                                 Grass = '#7AC74C',
                                 Ice = '#96D9D6',
                                 Fighting = '#C22E28',
                                 Poison = '#A33EA1',
                                 Ground = '#E2BF65',
                                 Flying = '#A98FF3',
                                 Psychic = '#F95587',
                                 Bug = '#A6B91A',
                                 Rock = '#B6A136',
                                 Ghost = '#735797',
                                 Dragon = '#6F35FC',
                                 Dark = '#705746',
                                 Steel = '#B7B7CE',
                                 Fairy ='#D685AD'))+
      theme(axis.text.x = element_text(angle = 90), 
            legend.position = c(0.5, 0.05), 
            legend.title = element_text(size = 10), 
            legend.text = element_text(size = 10))
    
                              
  })
  
  #create the summary stat table for all generations chosen
  
  output$mytable <- renderTable(
    pokemon %>% filter(grepl(input$type, Type_Combo)) %>% 
      filter(grepl(paste0("^", input$type, "$|", paste0(sec_type(), collapse="|")), Type_Combo)) %>% 
      mutate(Type_1  = input$type,
             Type_2 = str_match(Type_Combo,(paste0(".*(",paste0(sec_type(), collapse="|"),").*")))[,2]) %>%
      mutate(Plot_Type = ifelse(is.na(Type_2),paste0(input$type), Type_2))  %>% 
      filter(Generation %in% input$gen) %>%
      group_by(Stat) %>% 
      summarise("Mean" = mean(Values), 
                "Median" = median(Values),
                "Std. Dev." = sd(Values), 
                "Min" = min(Values),
                "Max" = max(Values))
    )

  
  #create the popularity of type plots
  
  output$pop_plot <- renderPlot( {
    
    #create validate warnings
    validate(
      need(input$gen, 'Please check at least one Generation!'),
      need(sec_type() != '', 'Please select another Secondary Type! \n Note that not all type combinations exist!')
    )
    
    pokemon %>% filter(grepl(input$type, Type_Combo)) %>% 
      filter(grepl(paste0("^", input$type, "$|", paste0(sec_type(), collapse="|")), Type_Combo)) %>% 
      mutate(Type_1  = input$type,
             Type_2 = str_match(Type_Combo,(paste0(".*(",paste0(sec_type(), collapse="|"),").*")))[,2]) %>%
      mutate(Plot_Type = ifelse(is.na(Type_2),paste0(input$type), Type_2))  %>% 
      filter(Generation %in% input$gen, Stat == "Total") %>%
      group_by(Plot_Type) %>% 
      summarize("Count" = n(),
                "Avg_Tot" = mean(Values)) %>% 
      mutate(Color = recode(Plot_Type, 
                            Normal = '#A8A77A',
                            Fire = '#EE8130',
                            Water = '#6390F0',
                            Electric = '#F7D02C',
                            Grass = '#7AC74C',
                            Ice = '#96D9D6',
                            Fighting = '#C22E28',
                            Poison = '#A33EA1',
                            Ground = '#E2BF65',
                            Flying = '#A98FF3',
                            Psychic = '#F95587',
                            Bug = '#A6B91A',
                            Rock = '#B6A136',
                            Ghost = '#735797',
                            Dragon = '#6F35FC',
                            Dark = '#705746',
                            Steel = '#B7B7CE',
                            Fairy ='#D685AD')) %>% 
      ggplot(aes(x=Avg_Tot, color = Color, size = Count))+
      geom_point(aes(y = Count)) + 
      scale_color_identity()+
      labs(y = "Number of Pokemon", 
           title = "Popularity of Types", 
           x="Average Total Stat Value")+ 
      geom_text_repel(aes(y = Count, label=Plot_Type), 
                      box.padding = unit(0.5, "lines"), 
                      size = 3, 
                      color = 'black')+
      theme(axis.text.x = element_text(hjust=0), 
            legend.key.size = unit(0.3, 'cm'), 
            plot.margin=unit(c(0.1,0.1,0.1,0.1), 'cm'),
            legend.justification="right",
            legend.margin=margin(0.1,0.1,0.1,0.1),
            legend.box.margin=margin(-3,-3,-3,-3), 
            legend.direction = "vertical", 
            legend.box = "horizontal")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
