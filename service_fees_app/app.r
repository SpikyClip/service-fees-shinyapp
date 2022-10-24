library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggpubr)
library(scales)
library(DT)
library(tools)

# Load fonts and set theme
font_add("IBMPlexSans", regular = "IBMPlexSans-Regular.ttf")
font_add("IBMPlexSans-Bold", regular = "IBMPlexSans-Bold.ttf")
font_add("IBMPlexSans-Medium", regular = "IBMPlexSans-Medium.ttf")
showtext_auto()
theme_set(
    theme_ipsum_ps(
        axis_title_size = 11.5,
        axis_text_size = 10,
        grid = F,
        axis = T,
        ticks = F
    )
)

# 
# User Interface
# 
ui = fluidPage(
    navbarPage(
        title = "Service Fee Statistics",
        tabPanel(
            title = "Main",
            # Sidebar contains introduction and settings
            sidebarLayout(
                sidebarPanel = sidebarPanel(
                    h3("Introduction"),
                    htmltools::includeMarkdown("md/intro.md"),
                    h3("Settings"),
                    # Takes in csv
                    fileInput(
                        inputId = "raw_csv",
                        label = "Choose a CSV file",
                        accept = c(".csv", ".xlsx")
                    ),
                    # Standard vs Actual Fees mode
                    radioButtons(
                        inputId = "mode",
                        label = "Plot Standardised or Actual Fees.",
                        choices = c("Standard", "Actual"),
                    ),
                    # Dynamic ui listing possible reference service providers
                    selectInput(
                        inputId = "standard_provider",
                        label = "Reference Service Provider",
                        choices = NULL
                    ),
                    h3("Download"),
                    fluidRow(
                        h4("Template"),
                        downloadButton(
                            outputId = "template_df_dl",
                            label = "Download Template CSV"
                        ),
                    ),
                    fluidRow(
                        h4("Data"),
                        downloadButton(
                            outputId = "proc_df_dl",
                            label = "Download Data"
                        ),
                        downloadButton(
                            outputId = "summ_df_dl",
                            label = "Download Summary Statistics"
                        ),
                    ),
                    fluidRow(
                        h4("Graph"),
                        downloadButton(
                            outputId = "graph_dl",
                            label = "Download Graph"
                        )
                    )

                ),
                # Mainpanel contains plot and summary statistics
                mainPanel = mainPanel(
                    plotOutput("boxplot"),
                    h3("Summary Statistics"),
                    dataTableOutput("summtable")
                )
            )
        ),
        # Additional tab displaying all data for troubleshooting
        tabPanel(
            title = "Table Data",
            dataTableOutput("datatable")
        )
    )
)
# 
# Server Function
# 
server = function(input, output) {


    # When df is updated, update list of possible reference service providers
    observeEvent(
        get_df(),
            {
            service_providers = unique(get_df()$`Service Provider`)
            updateSelectInput(
                inputId = "standard_provider",
                choices = service_providers
                )
            }
        )


    # Get default path if csv is not uploaded, otherwise validate csv and get
    # user submitted path.
    get_input_path = reactive({
            if (!isTruthy(input$raw_csv)) {
                path = "data/physio_fees_raw.csv"
            } else {
                file = input$raw_csv
                ext = file_ext(file$datapath)
                req(file)
                validate(need(ext == "csv", "Please upload a csv file."))
                path = file$datapath
            }
            return(file.path(path))
    })


    # Get input filename for naming output downloads
    get_input_filename = reactive({
        input_filename = get_input_path() %>%
            basename() %>%
            file_path_sans_ext()
        return(input_filename)
    })

    # Get unprocessed dataframe
    get_df = reactive({
        df = read_csv(get_input_path())
        return(df)
    })


    # Process fee rate columns and standardised times, returning processed df.
    get_proc_df = reactive({
        df = get_df()
        standard_provider = input$standard_provider
        # Get standardised times for each service provided by standard_provider
        standard_df = df %>%
            filter(`Service Provider` == standard_provider) %>%
            rename(`Standard Service Duration (mins)` = `Actual Service Duration (mins)`) %>%
            select(`Service Category`, `Standard Service Duration (mins)`)
        # Add standardised times to proc_df by using a left join by Service 
        # Category
        proc_df = df %>%
            left_join(standard_df, by = "Service Category") %>%
            mutate(
                `Rate ($/min)` = `Actual Fee ($)` / `Actual Service Duration (mins)`,
                `Standard Fee ($)` = `Rate ($/min)` * `Standard Service Duration (mins)`,
                across(c(`Service Provider`, `Service Category`), str_to_title),
                across(c(`Rate ($/min)`, `Standard Fee ($)`, `Actual Fee ($)`), round, 2)
            ) %>%
            # Reorganise columns
            select(
                `Service Provider`,
                `Service Category`,
                `Rate ($/min)`,
                `Actual Service Duration (mins)`,
                `Standard Service Duration (mins)`,
                `Actual Fee ($)`,
                `Standard Fee ($)`
            )
        return(proc_df)
    })


    # Get filtered df without NA values (can occur if service providers offer a
    # service that is not offered by the standard provider). Pivot standard and
    # actual fees so it is easier to swap between the two plots.
    get_proc_df_filt = reactive({
        proc_df_filt = get_proc_df() %>% 
            filter(!is.na(`Standard Fee ($)`)) %>%
            pivot_longer(
                cols = c(`Actual Fee ($)`, `Standard Fee ($)`),
                names_to = "Fee Type",
                values_to = "Fee ($)"
            )
        return(proc_df_filt)
    })


    # Get table of summary statistics such as count, mean duration, and mean
    # actual and standard fees.
    get_summ_df = reactive({
        summ_df =
            get_proc_df() %>%
            group_by(`Service Category`) %>%
            summarise(
                `Service Count` = n(),
                `Mean Actual Service Duration (mins)` = mean(`Actual Service Duration (mins)`) %>% round(),
                `Mean Actual Fee ($)` = mean(`Actual Fee ($)`) %>% round(2),
                `Mean Standard Fee ($)` = mean(`Standard Fee ($)`) %>% round(2)
            )
        return(summ_df)
    })


    # Filter summary statistic df of NA values and pivot mean standard and 
    # actual fees so it is easier to swap between the two plots.
    get_summ_df_filt = reactive({
        summ_df_filt = get_summ_df() %>%
            filter(!is.na(`Mean Standard Fee ($)`)) %>%
            pivot_longer(
                cols = c(`Mean Actual Fee ($)`, `Mean Standard Fee ($)`),
                names_to = "Fee Type",
                values_to = "Mean Fee ($)"
            )
        return(summ_df_filt)
    })


    # Generate boxplot based on mode and selected standard_provider
    my_boxplot = reactive({
        # inputs
        standard_provider = input$standard_provider
        mode = input$mode
        # Filter based on Mode
        proc_df_filt = get_proc_df_filt() %>% filter(str_detect(`Fee Type`, mode))
        summ_df_filt = get_summ_df_filt() %>% filter(str_detect(`Fee Type`, mode))

        title = paste(mode, "Fees by Service Category")

        proc_df_filt %>%
            ggplot(aes(x = `Fee ($)`, y = `Service Category`)) +
            # Boxplot and errorbar
            stat_boxplot(geom ='errorbar', width = 0.5, color = "gray45") +
            geom_boxplot(color = "gray45") +
            # Individual and mean points
            geom_point(aes(color = `Service Provider`), alpha = 0.75) +
            geom_point(
                data = summ_df_filt,
                mapping = aes(x = `Mean Fee ($)`),
                color = "orangered"
            ) +
            # Label for mean and standard_provider values.
            geom_label(
                data = subset(proc_df_filt, `Service Provider` == standard_provider),
                mapping = aes(label = dollar(`Fee ($)`), color = `Service Provider`),
                nudge_y = 0.2,
                label.size = 0.1,
                show.legend = F,
                alpha = 0.8
            ) +
            geom_label(
                data = summ_df_filt,
                mapping = aes(
                    x = `Mean Fee ($)`,
                    label = dollar(`Mean Fee ($)`)
                ),
                color = "orangered",
                nudge_y = -0.2,
                label.size = 0.1,
                show.legend = F,
                alpha = 0.8
            ) +
            # Formatting
            scale_color_manual(values=setNames("limegreen", standard_provider)) +
            scale_x_continuous(labels = label_dollar()) +
            theme(axis.title.y=element_text(angle=0)) +
            ggtitle(title)
    })


    # 
    # Server Outputs
    # 
    
    # Internal
    output$datatable = renderDataTable({get_proc_df()})
    output$summtable = renderDataTable({get_summ_df()})
    output$boxplot = renderPlot({my_boxplot()})
    
    # External
    output$template_df_dl = downloadHandler(
        filename = "service_fees_template.csv",
        content = function(file) {
            template_path = file.path("data/service_fees_template.csv")
            file.copy(template_path, file)
        }
    )
    output$proc_df_dl = downloadHandler(
        filename = function() {
            paste0(get_input_filename(), "_processed", ".csv")
        },
        content = function(file) {
            write_csv(get_proc_df(), file)
        }
    )
    output$summ_df_dl = downloadHandler(
        filename = function() {
            paste0(get_input_filename(), "_summary", ".csv")
        },
        content = function(file) {
            write_csv(get_summ_df(), file)
        }
    )
    # output$graph_dl
}

shinyApp(ui = ui, server = server)
