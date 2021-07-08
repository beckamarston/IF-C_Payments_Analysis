# TODO
# Overall Dashboard: 
# •	Combine APCC and Labour values dashboards
# Cost curve analysis tab:
# •	Add a table to show the deciles of farms gross margins (£/ha) reactive to same levers as the graph



# install.packages(c(
#   'shiny', 'shinydashboard', 'shinyWidgets',
#   'dplyr', 'htmltools',
#   'ggplot2'
# ))
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library('dplyr')
library('htmltools')
library('ggplot2')



{
  data <- readxl::read_excel('//ler365fs/fbs2/Users/TASPrototype/Non FBS Staff analysis/Rebecca Marston/dashboard_data.xlsx') %>%
    dplyr::select(
      'farms',
      # 'uncal',
      # 'weight',
      # 'stratum',
      'Farm Type' = 'type',
      'Region' = 'gor',
      # 'quotatype',
      'slr',  #SLR
      'SLR Farm Size' = 'slrgroup',
      # 'slrgroup2',
      # 'so',
      'Tenancy Type' = 'tenancy',
      # 'jca',
      'LFA Status' = 'lfa',
      # 'totarea',
      # 'orgarea',
      # 'bps',
      'AES particpant' = 'agenv',
      # 'newwt3yr',
      # 'num.pop',
      'Farm Size (UAA)' = 'UAA',
      # 'area.farmed',
      'labour.force',  #Number of Workers on the Farm
      # 'farmer.spouse.AWU',
      # 'FPD.AWU',
      # 'FPD.spouses.AWU',
      # 'unpaid.regular.AWU',
      # 'unpaid.casual.AWU',
      # 'manager.AWU',
      # 'paid.whole.time.AWU',
      # 'paid.part.time.AWU',
      # 'paid.casual.AWU',
      # 'trainee.AWU',
      'AWU',
      # 'AWU.check',
      # 'adjusted.AWU',
      'Farm Business Income' = 'FBI',
      # 'output.from.agriculture',
      # 'farm.business.output',
      # 'farm.business.variable.costs',
      # 'farm.business.fixed.costs',
      # 'BPS.income',
      # 'basic.payment.scheme',
      # 'agri.environment.income',
      # 'agri.environment.payments',
      # 'diversified.income',
      # 'diversified.output',
      'Performance Ratio'= 'performance.ratio',
      # 'total.area',
      # 'agriculture.fixed.costs',
      # 'agriculture.unpaid.labour',
      # 'agriculture.variable.costs',
      'Agriculture Income' = 'agriculture.income',
      'Agriculture Income per ha' = 'agriculture.income.per.ha',
      'Agriculture Gross Margin' = 'agriculture.gross.margin',
      'Agriculture Gross Margin per ha' = 'agriculture.gross.margin.per.ha'
    ) %>%
    dplyr::mutate(`All` = 'All')
  names = names(data)
  names.selected = 'Agriculture Gross Margin per ha'
  # names.selected = 'Performance Ratio'
  farm.types <- unique(data$`Farm Type`)
  farm.types.selected <- c(
    'Dairy',
    'Lowland Grazing Livestock',
    'LFA Grazing Livestock',
    'Cereals',
    # 'Pigs',
    'Mixed',
    # 'Poultry',
    # 'Horticulture',
    'General cropping'
  )
  categories <- c(
    'All',
    'Farm Type',
    'Region',
    'SLR Farm Size',
    'Tenancy Type',
    'LFA Status',
    'AES particpant'
  )
}



ui <- {dashboardPage(
  dashboardHeader(title = 'Payment Visualisations'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'intro_'),
      menuItem('Distributional Analysis', tabName = 'plots_'),
      menuItem('Cost Curve Analysis', tabName = 'apcc_'),
      menuItem('Background Data', tabName = 'data_')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro_', {fluidRow(
        box(width = 12,
          title = 'Distribution of Agricultural Gross Margins & Farm Performance',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          htmltools::includeMarkdown('intro.md')
        )
      )}),
      tabItem(tabName = 'plots_', {fluidRow(
        box(width = 12,
          title = 'Inputs',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          pickerInput('plots_x', 'x-axis', choices = names, select = names.selected),
          selectInput('plots_group', 'Farm Grouping', choices = categories),
          # numericInput( 'plots_bins', 'Number of bins', value = 21, min = 1),
          materialSwitch('plots_split', 'Split Histogram into Groups', value = FALSE, status = 'primary', right = TRUE)
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
          plotOutput('plots_cumplot', click='plots_click2'),
          verbatimTextOutput('plots_data2')
        )
      )}),
      tabItem(tabName = 'apcc_', {fluidRow(
        box(width = 12,
          title = 'Inputs',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          pickerInput('apcc_cats', 'Catagory', choices = categories),
          pickerInput('apcc_catopts', 'None', choices = categories[1], selected = categories[1], options = list(`actions-box`=TRUE), multiple = TRUE),
          sliderInput('apcc_UAAarea', 'Percentage of Holding Entered', min = 0, max = 1, value = 0.6),
          materialSwitch('apcc_percentage', 'Show UAA as Percentage of Total', value = FALSE, status = 'primary', right = TRUE)
        ),
        box(width = 12,
          title = 'Cost Curve by Number of Farms',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          plotOutput('apcc_numberplot')
        ),
        box(width = 12,
          title = 'Cost Curve by UAA',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          plotOutput('apcc_areaplot'),
          tableOutput('apcc_areaplot_table')
        ),
        box(width = 12,
          title = 'Cost Curve by Number of Farms',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          tableOutput('apcc_table')
        )
      )}),
      tabItem(tabName = 'data_', {fluidRow(
        box(width = 12,
          title = 'Inputs',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          pickerInput('data_type', 'Farm Types', choices = farm.types, selected = farm.types.selected, options = list(`actions-box`=TRUE), multiple = TRUE),
          selectInput('data_x', 'x-axis', choices = names),
          selectInput('data_y', 'y-axis', choices = names)
        ),
        box(width = 12,
          title = 'Correlation',
          solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = FALSE,
          plotOutput('data_correlation')
        )
      )})
    )
  )
)}



server <- function(input, output, session) {
  # plots_ Tab
  xy_str <- function(e) {
    if(is.null(e)) {
      'NULL\n'
    } else {
      paste0(
        'x=', round(e$x, 1),
        ' y=', round(e$y, 1)
      )
    }
  }
  output$plots_distplot <- renderPlot({
    p <- data %>%
      dplyr::group_by(!!as.name(input$plots_group)) %>%
      ggplot(aes(
        x = !!as.name(input$plots_x),
        colour = !!as.name(input$plots_group),
        fill = !!as.name(input$plots_group)
      )) +
      # geom_histogram(
      #   bins = input$plots_bins,
      #   alpha = 1 / dplyr::n_distinct(data[[input$plots_group]])
      # ) +
      geom_density(
        alpha = 1 / dplyr::n_distinct(data[[input$plots_group]])
      ) +
      ylab('Number of Farms') +
      coord_cartesian(xlim = c(
        quantile(data[[input$plots_x]], .025),
        quantile(data[[input$plots_x]], .975)
      ))

    if (input$plots_split==TRUE) {
      p <- p +
        theme(legend.position = 'none') +
        facet_wrap(~get(input$plots_group))
    }

    p
  })
  output$plots_data1 <- renderText({
    paste('data points:', xy_str(input$plots_click1))
  })
  output$plots_cumplot <- renderPlot({
    data %>%
      dplyr::group_by(!!as.name(input$plots_group)) %>%
      ggplot(aes(
        x = !!as.name(input$plots_x),
        colour = !!as.name(input$plots_group)
      )) +
      stat_ecdf(
        geom = 'line',
        pad = FALSE
      ) +
      ylab('Cumulative Density') +
      coord_cartesian(xlim = c(
        quantile(data[[input$plots_x]], .025),
        quantile(data[[input$plots_x]], .975)
      ))
  })
  output$plots_data2 <- renderText({
    paste('data points:', xy_str(input$plots_click2))
  })

  # apcc_ Tab
  observeEvent(input$apcc_cats, {
    opts <- data %>% select(input$apcc_cats) %>% unique()
    updatePickerInput(session, 'apcc_catopts', input$apcc_cats, choices = opts, selected = opts)
  })
  apcc_filteredData <- reactive({
    df <- data %>%
      dplyr::filter(
        !!as.name(input$apcc_cats) %in% input$apcc_catopts
      ) %>%
      dplyr::group_by(!!as.name(input$apcc_cats)) %>%
      dplyr::arrange(`Agriculture Gross Margin per ha`) %>%
      dplyr::mutate(
        `Farm Rank by Gross Margin` = rank(`Agriculture Gross Margin per ha`) / length(`Agriculture Gross Margin per ha`),
        `Cumulative UAA (ha)` = input$apcc_UAAarea * cumsum(`Farm Size (UAA)`) / if(input$apcc_percentage){sum(`Farm Size (UAA)`)}else{1}
      )
  })
  output$apcc_numberplot <- renderPlot({
    apcc_filteredData() %>%
      ggplot(aes(
        x = `Farm Rank by Gross Margin`,
        y = `Agriculture Gross Margin per ha`,
        colour = !!as.name(input$apcc_cats)
      )) +
      geom_line()
  })
  output$apcc_areaplot <- renderPlot({
    apcc_filteredData() %>%
      ggplot(aes(
        x = `Farm Rank by Gross Margin`,
        y = `Cumulative UAA (ha)`,
        colour = !!as.name(input$apcc_cats)
      )) +
      geom_line()
  })
  output$apcc_table <- renderTable({
    df <- apcc_filteredData() %>%
      dplyr::mutate(
        `Rank Decile` = dplyr::ntile(`Farm Rank by Gross Margin`, 10)
      ) %>%
      dplyr::group_by(
        !!as.name(input$apcc_cats),
        `Rank Decile`
      ) %>%
      dplyr::summarise(
        `Mean Farm Rank by Gross Margin` = mean(`Farm Rank by Gross Margin`),
        `Mean Agriculture Gross Margin per ha` = mean(`Agriculture Gross Margin per ha`),
        `Mean Cumulative UAA (ha)` = mean(`Cumulative UAA (ha)`),
        `Count` = dplyr::n(),
        .groups = 'drop'
      )
  })
  
  # data_ Tab
  output$data_correlation <- renderPlot({
    data %>%
      dplyr::filter(
        `Farm Type` %in% input$data_type
      ) %>%
      ggplot(aes(
        x = !!as.name(input$data_x),
        y = !!as.name(input$data_y)
      )) +
      geom_point(aes(
        colour = `Farm Type`
      )) +
      geom_smooth()
  })
}



shinyApp(ui, server)
