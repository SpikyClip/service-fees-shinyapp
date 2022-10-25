library(shiny)
library(tidyverse)
library(showtext)
library(scales)
library(DT)
library(tools)
library(markdown)

if (Sys.info()[['sysname']] == 'Linux') {
    dir.create('~/.fonts')
    fonts = c(
        "www/IBMPlexSans-Regular.ttf",
        "www/IBMPlexSans-Bold.ttf",
        "www/IBMPlexSans-Medium.ttf"
        )
    file.copy(fonts, "~/.fonts")
    system('fc-cache -f ~/.fonts')
} else if (Sys.info()[['sysname']] == 'Windows') {
    font_add("IBMPlexSans", regular = "IBMPlexSans-Regular.ttf")
    font_add("IBMPlexSans-Bold", regular = "IBMPlexSans-Bold.ttf")
    font_add("IBMPlexSans-Medium", regular = "IBMPlexSans-Medium.ttf")
    showtext_auto()
}

# Load fonts and set theme
theme_set(
    theme(
        # Base
        line = element_line(
            size = 0.25,
            color = "#BFBEBE",
            linetype = 1
        ),
        text = element_text(
            size = 16,
            family = "IBMPlexSans",
            hjust = 0,
            color = "#78797D"
            ),
        title = element_text(
            size = rel(1.5),
            family = "IBMPlexSans-Bold",
            color = "#646466"
            ),
        # Panel
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(30, 30, 30, 30),
        # Axes
        axis.line = element_line(),
        axis.ticks = element_line(),
        axis.title = element_text(
            size = rel(0.8),
            family = "IBMPlexSans-Medium"
            ),
        axis.title.y = element_text(vjust = 1, angle = 0),
        axis.text = element_text(
            size = rel(0.8),
            hjust = 0.5
            ),
        # Legend
        legend.title = element_text(
            size = rel(0.8),
            family = "IBMPlexSans-Medium"
            ),
        legend.key = element_blank()
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
                            label = "Download Template (.CSV)"
                        ),
                    ),
                    fluidRow(
                        h4("Data"),
                        downloadButton(
                            outputId = "proc_df_dl",
                            label = "Download Data (.CSV)"
                        ),
                        downloadButton(
                            outputId = "summ_df_dl",
                            label = "Download Summary Statistics (.CSV)"
                        ),
                    ),
                    fluidRow(
                        h4("Plot"),
                        radioButtons(
                            inputId = "plot_resolution",
                            label = "Resolution",
                            inline = TRUE,
                            choiceNames = c("Retina (320)", "Print (300)", "Screen (72)"),
                            choiceValues = c("retina", "print", "screen")
                        ),
                        radioButtons(
                            inputId = "plot_format",
                            label = "Format",
                            inline = TRUE,
                            choices = c("png", "tiff","jpeg")
                        ),
                        downloadButton(
                            outputId = "plot_dl",
                            label = "Download Plot"
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
            stat_boxplot(geom ='errorbar', width = 0.5, color = "#BFBEBE") +
            geom_boxplot(color = "#BFBEBE") +
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
    # Template CSV
    output$template_df_dl = downloadHandler(
        filename = "service_fees_template.csv",
        content = function(file) {
            template_path = file.path("data/service_fees_template.csv")
            file.copy(template_path, file)
        }
    )
    # Processed data CSV
    output$proc_df_dl = downloadHandler(
        filename = function() {
            paste0(get_input_filename(), "_processed", ".csv")
        },
        content = function(file) {
            write_csv(get_proc_df(), file)
        }
    )
    # Summary Statistics CSV
    output$summ_df_dl = downloadHandler(
        filename = function() {
            paste0(get_input_filename(), "_summary", ".csv")
        },
        content = function(file) {
            write_csv(get_summ_df(), file)
        }
    )
    # Plot Download
    output$plot_dl = downloadHandler(
        filename = function() {
            paste0(get_input_filename(), "_boxplot", ".", input$plot_format)
        },
        content = function(file) {
            ggsave(file,
                   plot = my_boxplot(),
                   device = input$plot_format,
                   bg = "white"
                   )
        }
    )
}

shinyApp(ui = ui, server = server)
