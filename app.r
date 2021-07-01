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


ui <- dashboardPage(
  dashboardHeader(title = 'Payment Visualisations'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'intro'),
      menuItem('Distributional Analysis', tabName = 'plots'),
      menuItem('Cost Curve Analysis', tabName = 'apcc'),
      menuItem('Background Data', tabName = 'data')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro', fluidRow(
        box(width = 12,
          title = 'Distribution of Agricultural Gross Margins & Farm Performance',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          helpText()
        )
      )),
      tabItem(tabName = 'plots', fluidRow(
        box(width = 12,
          title = 'Inputs',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          radioButtons(
            inputId = 'plots_display_var',
            label = 'Select Variable to Plot:',
            choices = c(
              'Agricultural Gross Margins per ha' = 'agriculture.gross.margin.per.ha',
              'Performance Ratio'= 'performance.ratio'
            )
          ),
          sliderInput(
            inputId = 'plots_bins',
            label = 'Select Binwidth:',
            min = 10,
            max = 10000,
            value = 7000
          )
          selectInput(
            inputId = 'plots_group',
            label = 'Select Farm Grouping:',
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
          checkboxInput(
            inputId = 'plots_split',
            label = 'Split Histogram into Groups:',
            value = FALSE
          ),
          checkboxInput(
            inputId = 'plots_addmedian',
            label = 'Show Median:',
            value = FALSE
          )
        ),
        box(width = 12,
          title = 'Histogram',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          plotOutput('plots_distplot', click='plots_click1'),
          verbatimTextOutput('plots_data1')
        ),
        box(width = 12,
          title = 'Cumulative Plot',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          plotOutput('plots_cumplot', click='plot_click'),
          verbatimTextOutput('plots_data2')
        )
      )),
      tabItem(tabName= 'apcc', fluidRow(
        box(width = 12,
          title = 'Inputs',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          checkboxGroupInput(
            inputId = 'apcc_type',
            label = 'Select Farm Type(s):',
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
          ),
          checkboxInput(
            inputId = 'apcc_combine',
            label = 'Combine Curves',
            value = FALSE
          ),
          sliderInput(
            inputId = 'apcc_UAAarea',
            label = 'Percentage of Holding Entered:',
            min = 0,
            max = 1,
            value = 0.6
          ),
          checkboxInput(
            inputId = 'apcc_percentage',
            label = 'Show UAA as Percentage of Total',
            value = FALSE
          )
        ),
        box(width = 12,
          title = 'Cost Curve by Number of Farms',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          column(width = 6,
            plotOutput('apcc_numberplot', brush = brushOpts(id = 'apcc_brush1',resetOnNew = TRUE))
          ),
          column(width = 6,
            plotOutput('apcc_numberplot2')
          )
        ),
        box(width = 12,
          title = 'Cost Curve by UAA',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          column(width = 6,
            plotOutput('apcc_areaplot', brush = brushOpts(id = 'apcc_brush2',resetOnNew = TRUE))
          ),
          column(width = 6,
            plotOutput('apcc_areaplot2')
          )
        )
      ),
      tabItem(tabName= 'data', fluidRow(
        box(width = 12,
          title = 'Inputs',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          checkboxGroupInput(
            inputId = 'data_type',
            label = 'Select Farm Type(s):',
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
          ),
          selectInput(
            inputId = 'data_var1',
            label = 'Select Variable for x-axis:',
            choices = c(
              'Farm Business Income' = 'FBI',
              'Agriculture Gross Margins per ha' = 'agriculture.gross.margin.per.ha',
              'Agriculture Gross Margins' = 'agriculture.gross.margin',
              'Agriculture Income per ha' = 'agriculture.income.per.ha',
              'Agriculture Income' = 'agriculture.income',
              'Performance Ratio'= 'performance.ratio',
              'Farm Size (UAA)'= 'UAA'
            ),
            selected = c('FBI')
          ),
          selectInput(
            inputId = 'data_var2',
            label = 'Select Variable for y-axis:',
            choices = c(
              'Farm Business Income' = 'FBI',
              'Agriculture Gross Margins per ha' = 'agriculture.gross.margin.per.ha',
              'Agriculture Gross Margins' = 'agriculture.gross.margin',
              'Agriculture Income per ha' = 'agriculture.income.per.ha',
              'Agriculture Income' = 'agriculture.income',
              'Performance Ratio'= 'performance.ratio',
              'Farm Size (UAA)'= 'UAA'
            ),
            selected= c('agriculture.gross.margin.per.ha')
          )
        ),
        box(width = 12,
          title = 'Correlation',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          plotOutput('data_correlation')
        )
      )
    )
  )
)


server <- function(input, output) {

  # extract data
  mydata3 <- read.csv('//ler365fs/fbs2/Users/TASPrototype/Non FBS Staff analysis/Rebecca Marston/Appropriate Point on the Cost Curve/FBS_data_2018.csv')

  xy_str <- function(e) {
    if(is.null(e)) return('NULL\n')
    paste0(
      'x=', round(e$x, 1),
      ' y=', round(e$y, 1)
    )
  }


  # reactive values
  x_reactive <- reactive({
    switch(
      input$display_var,
      'performance.ratio' = mydata3$performance.ratio,
      'agriculture.gross.margin.per.ha' = mydata3$agriculture.gross.margin.per.ha
    )
  })

  #histogram--------------------------------------------------------------------------------------------------
  output$plots_distplot <- renderPlot({
    # set x-axis label depending on the value of display_var
    if (input$plots_display_var == 'performance.ratio') {
      xlabel <- 'Performance Ratio'
    } else if (input$plots_display_var == 'agriculture.gross.margin.per.ha') {
      xlabel <- 'Agriculture Gross Margin (£/ha)'
    }

    # set linecolor for median line
    if (input$addmedian == TRUE) {
      linecolor <- 'red'
    } else {
      linecolor <- rgb(0,0,0,0)  # colorless
    }

    # set variable and groupings for colour
    p <- ggplot(
      mydata3,
      aes_string(
        input$plots_display_var,
        color = input$plots_group,
        fill = input$plots_group
      )
    )

    # added input$split here and repeated the code with and without a call to facet_wrap
    if (input$split==TRUE) {
      p + geom_histogram( bins = input$plots_bins, alpha=0.5, position='identity') +
        xlab(xlabel) +
        ylab('Number of Farms') +
        geom_vline(aes(xintercept = median(x_reactive())), col = linecolor, linetype= 'dashed') +
        facet_wrap(~get(input$plots_group)) +
        if (input$plots_display_var == 'performance.ratio') {
          coord_cartesian(xlim = c(0, 250))
        } else if (input$plots_display_var == 'agriculture.gross.margin.per.ha') {
          coord_cartesian(xlim = c(-5000, 10000))
        }
    } else {
      p + geom_histogram( bins = input$plots_bins, alpha=0.5, position='identity') +
        xlab(xlabel) +
        ylab('Number of Farms') +
        geom_vline(aes(xintercept = median(x_reactive())), col = linecolor, linetype= 'dashed') +
        if (input$plots_display_var == 'performance.ratio') {
          coord_cartesian(xlim = c(0, 250))
        } else if (input$plots_display_var == 'agriculture.gross.margin.per.ha') {
          coord_cartesian(xlim = c(-5000, 10000))
        }
    }
  })
  # data points
  output$plots_data1 <- renderText({
    xy_str <- function(e) {
      if (is.null(e)) return('NULL\n')
      paste0(
        'x=', round(e$x, 1),
        ' y=', round(e$y, 1)
      )
    }
    paste0('data points: ', xy_str(input$plots_click2))
  })

  #cumulative plot---------------------------------------------------------------------------------------
  output$plots_cumplot <- renderPlot({

    if (input$plots_display_var == 'performance.ratio') {
      xlabel <- 'Performance Ratio'
    } else if (input$plots_display_var == 'agriculture.gross.margin.per.ha') {
      xlabel <- 'Agriculture Gross Margins (£/ha)'
    }

    c <- ggplot(
      mydata3,
      aes_string(
        input$plots_display_var,
        color = input$plots_group
      )
    )

    c + stat_ecdf(geom = 'line', pad = FALSE) +
      xlab(xlabel) +
      ylab('Cumulative Density') +
      if (input$plots_display_var == 'performance.ratio') {
        coord_cartesian(xlim = c(0, 250))
      } else if (input$plots_display_var == 'agriculture.gross.margin.per.ha') {
        coord_cartesian(xlim = c(-5000, 10000))
      }
  })

  # data points
  output$plots_data2 <- renderText({
    paste0('data points: ', xy_str(input$plots_click))
  })


  # cost curves-------------------------------------------------------------------------------
  # filter by farm type. There | means or, so it is saying either filter on the variable that has been set by ticking the box, or set the filter variable to 'All types'.
  apcc_filteredData <- reactive({
    mydata3 %>%
      dplyr::group_by(type) %>%
      dplyr::filter(type == input$apcc_type) %>%
      # order by gross margin
      dplyr::arrange(agriculture.gross.margin.per.ha) %>%
      # create a rank variable
      dplyr::mutate(percRank = rank(agriculture.gross.margin.per.ha)/length(agriculture.gross.margin.per.ha)) %>%
      # keep track of the cumulative sum
      dplyr::mutate(gmCumsum = cumsum(UAA))
  })

  # cost curve by numbers-----------------------------------------------------------------
  ranges <- reactiveValues(x = NULL, y = NULL)

  output$apcc_numberplot <- renderPlot({
    #Plot cumulative plot of gross margins and number of farms participating
    ggplot(data = filteredData() ) +
      geom_line(aes(percRank, agriculture.gross.margin.per.ha, color=type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Agriculture Gross Margins (£/ha)')
  })

  # zoom plot
  output$apcc_numberplot2 <- renderPlot({
    ggplot(data = filteredData() ) +
      geom_line(aes(percRank,agriculture.gross.margin.per.ha, color=type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Agriculture Gross Margins (£/ha)') +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })

  observe({
    brush <- input$apcc_brush1
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  # UAA Curve-----------------------------------------------------------------------
  area_var <- reactive(input$apcc_UAAarea)
  ranges2 <- reactiveValues(x = NULL, y = NULL)

  output$apcc_areaplot <- renderPlot({
    # Plot a cumulative plot adding up the individual UAA(y) of the ranked farms against the gross margins (x)
    ggplot(data = filteredData() ) +
      geom_line(aes(x = percRank, y = gmCumsum*area_var(), color = type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Cumulative UAA (ha)')
  })

  # zoom plot
  output$apcc_areaplot2 <- renderPlot({
    ggplot(data = filteredData()) +
      geom_line(aes(x = percRank, y = gmCumsum*area_var(), color = type)) +
      xlab('Farm Rank by Gross Margin') +
      ylab('Cumulative UAA (ha)') +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  observe({
    brush <- input$apcc_brush2
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })

  #correlations---------------------------------------------------------------------------------------------------
  apcc_filteredData <- reactive({
    mydata3 %>%
      dplyr::group_by(type) %>%
      dplyr::filter(type == input$data_type) %>%
      # order by gross margin
      dplyr::arrange(agriculture.gross.margin.per.ha) %>%
      # create a rank variable
      dplyr::mutate(percRank = rank(agriculture.gross.margin.per.ha)/length(agriculture.gross.margin.per.ha)) %>%
      # keep track of the cumulative sum
      dplyr::mutate(gmCumsum = cumsum(UAA))
  })

  # need to fix reactivity and connect to the input controls for filtering farm types (currently connected to cost curve tab)
  x_axis <- reactive(input$data_var1)
  y_axis <- reactive(input$data_var2)

  output$data_correlation <- renderPlot({
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
