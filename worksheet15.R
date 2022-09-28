library(shiny)
library(ggplot2)


ui = fluidPage(
    #app title
    titlePanel('Gapminder Data'),
    
    #defining the layout
    sidebarLayout(
        
        #layout for the side panel
        sidebarPanel(
            
            #input continents as checkboxes
            checkboxGroupInput('checkbox',
                               'Choose with Continent',
                               choices = levels(gapminder$continent),
                               selected = levels(gapminder$continent)[3:4]),
            
            #input years as a slider
            sliderInput('year',
                        'Select years',
                        min = min(gapminder$year),
                        max = max(gapminder$year),
                        value = c(min(gapminder$year), max(gapminder$year)),
                        step = 6),
            
            #input countries as a drop down box
            selectInput('country',
                        'Select Country for Comparison',
                        choices = unique(gapminder$country),
                        selected = 'France')
        ),
        
        #layout for the main panel
        mainPanel(
            #heading of the main panel
            h3('Life Expectancy and GDP Analysis'),
            
            #defining the plots
            plotOutput('pointplot'),
            textOutput('heading'),
            plotOutput('lineplot')
        )
    )
)

server <- function(input, output){
    
    sub = reactive(
        {
            cont = input$checkbox
            subset(gapminder, gapminder$continent %in% cont)
        }
    )
    
    #plotting the life expectancy vs gdp graph
    output$pointplot = renderPlot({
        # Plot Number 1
        # Scatterplot with size = population
        # and colour = country
        # then changing opacity
        # scale ang log scale
        data = sub()
        ggplot(
            data,
            aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)
        ) +
            geom_point(show.legend = TRUE, alpha = 0.7) +
            scale_color_viridis_d() +
            scale_size(range = c(2, 12)) +
            scale_x_log10() +
            labs(x = "GDP per capita", y = "Life expectancy")
        })
    
    #writing the text 
    output$heading = renderText({
        
    })
}

shinyApp(ui = ui, server = server)