library('shiny')
library('shinydashboard')
library('ggplot2')
library('plyr')
library('dplyr') 
library('tidyverse')
library('readxl')
#install.packages('Cairo')
library('Cairo') 
library('ggpubr')


ui <- dashboardPage(skin = 'green',
  dashboardHeader(title = 'Payment Visualisations', titleWidth = 250),
  dashboardSidebar(width= 250,
    sidebarMenu(
      menuItem('Info', tabName = 'intro'),
      menuItem('Distributional Analysis', tabName = 'plots'),
      menuItem('Cost Curve Analysis', tabName= 'APCC'),
      menuItem('Background Data', tabName= 'data')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
        fluidRow(
          column(width = 12, 
            box(
              title = 'Distribution of Agricultural Gross Margins & Farm Performance',
              width = 12, 
              solidHeader = TRUE,
              status = 'success',
              helpText()
            )
          )
        )
      ),
      tabItem(tabName = 'plots',
        fluidRow(
          column(width = 12,
            box(title = 'Inputs', width=12, status = 'success', solidHeader = TRUE,
              column(width=6,
                radioButtons(
                  'display_var',
                  'Select Variable to Plot:',
                  choices = c(
                    'Agricultural Gross Margins per ha' = 'agriculture.gross.margin.per.ha',
                    'Performance Ratio'= 'performance.ratio'
                  )
                ),
                sliderInput(
                  'bins', 
                  'Select Binwidth:',
                  min = 10,
                  max = 10000,
                  value = 7000
                )
              ),
              column(width = 6,
                selectInput(
                  'group',
                  'Select Farm Grouping:', 
                  choices = c(
                    'None' = FALSE,
                    'Farm Type' = 'type',
                    'Region' = 'gor',
                    'LFA Status' = 'lfa',
                    'Tenancy Type' = 'tenancy',
                    'SLR Farm Size' = 'slrgroup',
                    'AES particpant'= 'agenv'
                  )
                ),
                checkboxInput(inputId = 'split', label = 'Split Histogram into Groups:', value = FALSE),
                checkboxInput(inputId = 'addmedian', label = 'Show Median:', value = FALSE)
              )
            )
          )
        ),

        fluidRow(
          box(title = 'Histogram', status = 'success', solidHeader = TRUE,
            plotOutput('distplot', click='plot_click'),verbatimTextOutput('data1')
          ),
          box(title = 'Cumulative Plot', status = 'success', solidHeader = TRUE, 
            plotOutput('cumplot', click='plot_click'), verbatimTextOutput('data2')
          )
        )
      ),
      tabItem(tabName= 'APCC',
        fluidRow(
          column(width = 12,
            box(width=12,
              title = 'Inputs', status = 'success', solidHeader = TRUE,
              column(width=4,
                checkboxGroupInput(
                  'type',
                  'Select Farm Type(s):', 
                  choices = c(
                    'Cereals',
                    'Dairy',
                    'General cropping',
                    'Horticulture',
                    'LFA Grazing Livestock',
                    'Lowland Grazing Livestock',
                    'Mixed',
                    'Pigs',
                    'Poultry'
                  ),
                  selected = c(
                    'Cereals',
                    'Dairy',
                    'General cropping', 
                    'LFA Grazing Livestock',
                    'Lowland Grazing Livestock',
                    'Mixed'
                  )
                )
              ),
              column(
                width=4,
                checkboxInput(inputId = 'combine', label = 'Combine Curves', value = FALSE),
                sliderInput(
                  'UAAarea', 
                  'Percentage of Holding Entered:',
                  min = 0,
                  max = 1,
                  value = 0.6
                ),
                checkboxInput(
                  inputId = 'percentage',
                  label = 'Show UAA as Percentage of Total',
                  value = FALSE
                )
              )
            )
          )
        ),
        fluidRow(
          box(width=14,
            title = 'Cost Curve by Number of Farms', status = 'success', solidHeader = TRUE,
            column(width = 6,
              plotOutput('numberplot', brush = brushOpts(id = 'plot_brush',resetOnNew = TRUE))
            ),
            column(width = 6,
              plotOutput('numberplot2')
            )
          )
        ),
        fluidRow(
          box(width=14,
            title = 'Cost Curve by UAA', status = 'success', solidHeader = TRUE, 
            column(width = 6,
              plotOutput('areaplot', brush = brushOpts(id = 'plot2_brush',resetOnNew = TRUE))
            ),
            column(width = 6,
              plotOutput('areaplot2')
            )
          )
        )
      ),
      tabItem(tabName= 'data',
        fluidRow(
          column(width = 12,
          box(title = 'Inputs', width=12, status = 'success', solidHeader = TRUE,
          column(width=4,
          checkboxGroupInput('type', 'Select Farm Type(s):', 
          choices = c('Cereals',
          'Dairy',
          'General cropping',
          'Horticulture',
          'LFA Grazing Livestock',
          'Lowland Grazing Livestock',
          'Mixed',
          'Pigs',
          'Poultry'),
          selected = c('Cereals', 'Dairy', 'General cropping', 
          'LFA Grazing Livestock', 'Lowland Grazing Livestock', 'Mixed'))),
          column(width=4,
          selectInput('var1', 'Select Variable for x-axis:', 
          choices = c('Farm Business Income' = 'FBI',
          'Agriculture Gross Margins per ha' = 'agriculture.gross.margin.per.ha',
          'Agriculture Gross Margins' = 'agriculture.gross.margin',
          'Agriculture Income per ha' = 'agriculture.income.per.ha',
          'Agriculture Income' = 'agriculture.income',
          'Performance Ratio'= 'performance.ratio',
          'Farm Size (UAA)'= 'UAA'),
          selected = c('FBI')),

          selectInput('var2', 'Select Variable for y-axis:', 
          choices = c('Farm Business Income' = 'FBI',
          'Agriculture Gross Margins per ha' = 'agriculture.gross.margin.per.ha',
          'Agriculture Gross Margins' = 'agriculture.gross.margin',
          'Agriculture Income per ha' = 'agriculture.income.per.ha',
          'Agriculture Income' = 'agriculture.income',
          'Performance Ratio'= 'performance.ratio',
          'Farm Size (UAA)'= 'UAA'),
          selected= c('agriculture.gross.margin.per.ha')))))
        ),

        fluidRow(
          box(width=8,
            title = 'Correlation', status = 'success', solidHeader = TRUE,
            plotOutput('correlation')
          )
        )
      )
    )
  )
)


server <- function(input, output) {
 
  # extract data
  mydata3 <- read.csv('//ler365fs/fbs2/Users/TASPrototype/Non FBS Staff analysis/Rebecca Marston/Appropriate Point on the Cost Curve/FBS_data_2018.csv')

  # reactive values
  x_reactive <- reactive({
    switch(
      input$display_var, 
      'performance.ratio' = mydata3$performance.ratio,
      'agriculture.gross.margin.per.ha' = mydata3$agriculture.gross.margin.per.ha
    )
  })
  
  filter_var <- reactive(input$type)
  
  #histogram--------------------------------------------------------------------------------------------------
  output$distplot <- renderPlot({
    # set x-axis label depending on the value of display_var
    if (input$display_var == 'performance.ratio') {
      xlabel <- 'Performance Ratio'
    } else if (input$display_var == 'agriculture.gross.margin.per.ha') {
      xlabel <- 'Agriculture Gross Margin (£/ha)'
    }
    
    # set linecolor for median line   
    if (input$addmedian == TRUE) {
      linecolor <- 'red' 
    } else {
      linecolor <- rgb(0,0,0,0)  # colorless
    }
    
    # set variable and groupings for colour 
    p <- ggplot(mydata3, aes_string(input$display_var, color = input$group, fill = input$group))  
    
    # added input$split here and repeated the code with and without a call to facet_wrap
    if (input$split==TRUE) {
      p + geom_histogram( bins = input$bins, alpha=0.5, position='identity') +
        xlab(xlabel) +
        ylab('Number of Farms') +
        geom_vline(aes(xintercept = median(x_reactive())), col = linecolor, linetype= 'dashed') + 
        facet_wrap(~get(input$group)) + 
        if (input$display_var == 'performance.ratio') {
          coord_cartesian(xlim = c(0, 250))
        } else if (input$display_var == 'agriculture.gross.margin.per.ha') {
          coord_cartesian(xlim = c(-5000, 10000))
        }       
    } else {         
      p + geom_histogram( bins = input$bins, alpha=0.5, position='identity') +
        xlab(xlabel) +
        ylab('Number of Farms') +
        geom_vline(aes(xintercept = median(x_reactive())), col = linecolor, linetype= 'dashed') + 
        if (input$display_var == 'performance.ratio') {
          coord_cartesian(xlim = c(0, 250))
        } else if (input$display_var == 'agriculture.gross.margin.per.ha') {
          coord_cartesian(xlim = c(-5000, 10000))
        }
    }
  })
  # data points 
  output$data1 <- renderText({
    xy_str <- function(e) {
      if (is.null(e)) return('NULL\n')
      paste0(
        'x=', round(e$x, 1),  
        ' y=', round(e$y, 1)
      )
    }
    paste0('data points: ', xy_str(input$plot_click))
  })
    
  #cumulative plot---------------------------------------------------------------------------------------
  output$cumplot <- renderPlot({
    if (input$display_var == 'performance.ratio') {
      xlabel <- 'Performance Ratio'} 
    else if (input$display_var == 'agriculture.gross.margin.per.ha') {
      xlabel <- 'Agriculture Gross Margins (£/ha)'}
    
    c <- ggplot(mydata3, aes_string(input$display_var, color = input$group)) 
    c + stat_ecdf(geom = 'line', pad = FALSE) +
      xlab(xlabel) +
      ylab('Cumulative Density') +
      if (input$display_var == 'performance.ratio') {
        coord_cartesian(xlim = c(0, 250))} 
    else if (input$display_var == 'agriculture.gross.margin.per.ha') {
      coord_cartesian(xlim = c(-5000, 10000))} })
  
  # data points
  output$data2 <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return('NULL\n')
      paste0(
        'x=', round(e$x, 1),  
        ' y=', round(e$y, 1)
      )
    }
    paste0('data points: ', xy_str(input$plot_click))
  })
    
  
  # cost curves-------------------------------------------------------------------------------
  # filter by farm type. There | means or, so it is saying either filter on the variable that has been set by ticking the box, or set the filter variable to 'All types'.
  filteredData <- reactive({
    mydata3 %>%
      dplyr::group_by(type) %>% 
      dplyr::filter(type == filter_var()) %>%  
      # order by gross margin 
      dplyr::arrange(agriculture.gross.margin.per.ha) %>%
      # create a rank variable
      dplyr::mutate(percRank = rank(agriculture.gross.margin.per.ha)/length(agriculture.gross.margin.per.ha)) %>%
      # keep track of the cumulative sum
      dplyr::mutate(gmCumsum = cumsum(UAA))
  })
  
  # cost curve by numbers-----------------------------------------------------------------
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$numberplot <- renderPlot({
    #Plot cumulative plot of gross margins and number of farms participating  
    ggplot(data = filteredData() ) +
      geom_line(aes(percRank,agriculture.gross.margin.per.ha, color=type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Agriculture Gross Margins (£/ha)')
  })

  # zoom plot
  output$numberplot2 <- renderPlot({
    ggplot(data = filteredData() ) +
      geom_line(aes(percRank,agriculture.gross.margin.per.ha, color=type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Agriculture Gross Margins (£/ha)') +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
 
  # UAA Curve-----------------------------------------------------------------------
  area_var <- reactive(input$UAAarea)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$areaplot <- renderPlot({
    # Plot a cumulative plot adding up the individual UAA(y) of the ranked farms against the gross margins (x)
    ggplot(data = filteredData() ) +
      geom_line(aes(x= percRank, y= gmCumsum*area_var(), color=type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Cumulative UAA (ha)')
  }) 
  
  # zoom plot
  output$areaplot2 <- renderPlot({
    ggplot(data = filteredData()) +
      geom_line(aes(x=percRank, y=gmCumsum*area_var(), color=type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Cumulative UAA (ha)') +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

  #correlations---------------------------------------------------------------------------------------------------
  # need to fix reactivity and connect to the input controls for filtering farm types (currently connected to cost curve tab)
  x_axis<- reactive(input$var1)
  y_axis<- reactive(input$var2)
  
  output$correlation <- renderPlot({
    ggscatter(
      filteredData(),
      x = 'UAA',
      y = 'agriculture.gross.margin.per.ha', 
      add = 'reg.line',
      conf.int = TRUE, 
      cor.coef = TRUE,
      cor.method = 'pearson'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)