library(tidyverse)
library(hrbrthemes)
library(showtext)
library(ggpubr)
library(scales)

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

ui = fluidPage(
    titlePanel("Service Fee Statistics"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            fileInput(
                inputId = "file1",
                label = "Choose a CSV file",
                accept = c(".csv", ".xlsx")
            ),
            radioButtons(
                inputId = "mode",
                label = "Plot Standardised or Actual Fees.",
                choices = c("Standard", "Actual"),
                )
        ),
        mainPanel = mainPanel(
            plotOutput("boxplot"),
            tableOutput("summtable")
        )
    )
)

server = function(input, output) {
    
    
    get_df = reactive({
        if (is.null(input$file1)) {
            path = "data/physio_fees_raw.csv"
        } else {
            file = input$file1
            ext = tools::file_ext(file$datapath)
            req(file)
            validate(need(ext == "csv", "Please upload a csv file."))
            path = file$datapath
        }
        df = read_csv(path)
        return(df)
    })
    
    
    get_proc_df = reactive({
        df = get_df()
        standard_provider = df[1, "Service Provider"] %>% as.character()
        standard_df = df %>%
            filter(`Service Provider` == standard_provider) %>%
            rename(`Standard Service Duration (mins)` = `Actual Service Duration (mins)`) %>%
            select(`Service Category`, `Standard Service Duration (mins)`)
        
        proc_df = df %>%
            left_join(standard_df, by = "Service Category") %>%
            mutate(
                `Rate ($/min)` = `Actual Fee ($)` / `Actual Service Duration (mins)`,
                `Standard Fee ($)` = `Rate ($/min)` * `Standard Service Duration (mins)`,
                across(c(`Service Provider`, `Service Category`), str_to_title)
            ) %>%
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
    
    my_boxplot = reactive({
        # Filter based on Mode
        mode = input$mode
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
    output$summtable = renderTable({get_summ_df()})
    output$boxplot = renderPlot({my_boxplot()})
}

shinyApp(ui = ui, server = server)
