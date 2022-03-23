# for re-deployment after any local change you might also opt for https://community.rstudio.com/t/edit-app-source-code-in-shinyapps-io-without-redeploying/53720/2

library(tidyverse)
library(shiny)


# Load data
metadat <- readxl::read_excel("Covid_data.xlsx")

dat <- read.table("oscidat.txt", header = TRUE, sep = ",")
# dat %>% str()

# dat %>% select(age) %>% summary()

# OSCI mapping to status
dat2 <- dat %>% mutate(
  status = case_when(
    OSCI == 0 ~ "Fully recovered",
    OSCI %in% c(1:2) ~ "Ambulatory mild disease",
    OSCI %in% c(3:4) ~ "Hospitalized: moderate disease",
    OSCI %in% c(5:7) ~ "Hospitalized: severe disease",
    OSCI == 8 ~ "Deceased",
    is.na(OSCI) ~ "Missing"
  )
  ,
  age_groups = case_when(
    age < 50 ~ "24-50",
    age >= 50 ~ "> 50"
  )
) %>%
  # reorder status level from best to worst clinical status
  mutate(status = fct_relevel(
    as.factor(status),
    "Fully recovered"
  ) ) %>%
  mutate(status = fct_relevel(
    status,
    "Deceased",
    after = 1
  ))


############# APP SECTION #################################
#############

ui <- fluidPage(
  h4("OSCI-based clinical improvement for hospitalized patients with COVID-19 over a 35 days follow-up"),
  h6("Author: Federico Bonofiglio"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("filter",
                   label = "Filter cohoort:",
                   choices = c("yes", "no"),
                   selected = "no"),

      conditionalPanel(condition = "input.filter == 'yes'",
                       selectInput("gender",
                                   label = "Select patient's gender",
                                   choices = unique(dat2$sex),
                                   selected = "Male"),

                       sliderInput("age_range",
                                   label = "Select patient's age range:",
                                   min = min(dat2$age),
                                   max = max(dat2$age),
                                   value = c(25, 50)
                                   )
      ),
      conditionalPanel(condition = "input.filter == 'no'",
                       p("The entire cohoort is shown")),


      radioButtons("plot_scale",
                   label = "Select Y-axis scale of the output plot:",
                   choices = c("Stacked percentage",
                               "Percentage"),
                   selected = "Stacked percentage"),

      # conditionalPanel(condition = "input.plot_scale == 'Stacked percentage'",
      #                  h5(" ")
      #                  ),
      conditionalPanel(condition = "input.plot_scale == 'Percentage'" ,
                        radioButtons("group_panel",
                                     label = "Group by:",
                                     choices = c("status", "arm"),
                                     selected = "status"
                        )
                        )
      ),

    mainPanel(
      plotOutput("mainplot"),

      br(), br(), br(),
      textOutput("plot_comment")
    )
  )

)



server <- function(input, output) {


  rdat <- reactive(
    {
      if (input$filter == "yes")
      {
        dat2 %>%
          filter(age >= input$age_range[1] & age <= input$age_range[2]) %>%
          arrange(day) %>%
          group_by(arm, day, sex) %>%
          count(status) %>%
          mutate(perc = 100*n/sum(n)) %>%
          filter(sex == input$gender)
      }
      else
      {
        dat2 %>%
          arrange(day) %>%
          group_by(arm, day) %>%
          count(status) %>%
          mutate(perc = 100*n/sum(n))
      }

    }
  )

  output$mainplot <- renderPlot(
    {
      if (input$plot_scale == "Stacked percentage")
      {
        ggplot(rdat(),
               aes(x=day, y=perc,
                   fill=status)) +
          facet_wrap(~arm) +
          geom_area(alpha=0.6,
                    size=0.5,
                    colour="black") +
          ylab("Stacked percentage")
      }
      else
      {
        if (input$group_panel == "status")
        {
          ggplot(rdat(),
                 aes(x=day, y=perc,
                     group = status,
                     color=status)) +
            facet_wrap(~arm) +
            geom_line(size = 1.5) +
            ylab("Percentage")
        }
        else
        {
          ggplot(rdat(),
                 aes(x=day, y=perc,
                     group = arm,
                     color=arm)) +
            facet_wrap(~status) +
          geom_line(size = 1.5) +
            ylab("Percentage")
        }

      }
    }
  )

  output$plot_comment <- renderText(
    {
      if (input$plot_scale == "Percentage")
      {
        if (input$group_panel == "status")
          "Note: at each day the status percentages sums up to 100."
        else
          "Note: shown are absolute percentages in each arm (e.g., at each day status percentages do not sum up to 100.)"

      }
      else
        "Note: at each day the status percentages stacks up to 100."

    }
  )
}


shinyApp(ui, server)
