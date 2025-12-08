# dashboardadd.R 

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(broom)

# ---- Load and prep data ----
df_raw <- read_csv("youtube_health_topics_6_keywords.csv")

df <- df_raw %>%
  rename(
    views        = view_count,
    likes        = like_count,
    comments     = comment_count,
    published_at = published_at,
    channel_name = channel_title
  ) %>%
  mutate(
    topic = factor(topic),
    professional = if_else(
      professional,
      "Professional (cardio/diabetes/antidepressants)",
      "Influencer / Wellness (probiotics/hormones/anti-aging)"
    ),
    professional = factor(
      professional,
      levels = c(
        "Professional (cardio/diabetes/antidepressants)",
        "Influencer / Wellness (probiotics/hormones/anti-aging)"
      )
    ),
    like_rate    = likes    / pmax(views, 1),
    comment_rate = comments / pmax(views, 1)
  )

topics_available <- levels(df$topic)

# ---- UI ----
ui <- fluidPage(
  titlePanel("YouTube Healthcare Dashboard"),

  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput(
        "topic",
        "Select topic(s):",
        choices  = topics_available,
        selected = topics_available,
        multiple = TRUE
      ),
      selectInput(
        "prof_group",
        "Topic type:",
        choices = c("All", levels(df$professional)),
        selected = "All"
      ),
      sliderInput(
        "min_views",
        "Minimum views:",
        min   = 0,
        max   = max(df$views, na.rm = TRUE),
        value = 0,
        step  = round(max(df$views, na.rm = TRUE) / 100)
      ),
      selectInput(
        "sort_metric",
        "Sort table by:",
        choices = c(
          "Views"         = "views",
          "Likes"         = "likes",
          "Comments"      = "comments",
          "Like rate"     = "like_rate",
          "Comment rate"  = "comment_rate"
        ),
        selected = "views"
      )
    ),

    mainPanel(
      tabsetPanel(
        # ------------ Home / Instructions tab ------------
        tabPanel(
          "Home",
          br(),
          h3("Welcome to the YouTube Healthcare Dashboard"),
          p("This dashboard allows users to explore engagement patterns, topics, creator types, and statistical insights from more than 1,800 YouTube videos covering six major healthcare topics."),
          p("You can filter by topic, creator type (professional vs influencer), and minimum views, and interactively examine:"),
          tags$ul(
            tags$li("Overall video summary by topic"),
            tags$li("Engagement patterns such as like rate vs comment rate"),
            tags$li("Differences between professional creators and influencers"),
            tags$li("Top-performing videos"),
            tags$li("A statistical model that identifies which factors most strongly predict video popularity")
          ),
          br(),
          h4("Data sources"),
          p("Data were collected using the YouTube Data API across six topics:"),
          tags$ul(
            tags$li("Cardiovascular diseases (professional)"),
            tags$li("Diabetes (professional)"),
            tags$li("Antidepressants (professional)"),
            tags$li("Probiotics (influencer / wellness)"),
            tags$li("Hormone balance (influencer / wellness)"),
            tags$li("Anti-aging (influencer / wellness)")
          ),
          p("Each topic contains approximately 300 videos, totaling around 1,800+ videos.")
        ),

        # ------------ Original tabs ------------
        tabPanel(
          "Overview",
          br(),
          h4("Summary by topic"),
          tableOutput("summary_topic"),
          br(),
          h4("Views distribution by topic and category"),
          plotOutput("plot_views")
        ),
        tabPanel(
          "Engagement",
          br(),
          h4("Like rate vs. Comment rate (by topic and category)"),
          plotOutput("plot_engagement_scatter")
        ),
        tabPanel(
          "Topic comparison",
          br(),
          h4("Average views by topic and category"),
          plotOutput("summary_bar")
        ),
        tabPanel(
          "Video table",
          br(),
          h4("Top videos (filtered)"),
          tableOutput("video_table")
        ),

        # ------------ Drivers of views (regression) ------------
        tabPanel(
          "Drivers of views",
          br(),
          h4("Which factors predict video popularity?"),
          p("We fit a linear regression model (on log10 scale) using likes, comments, topic, and creator type to understand which characteristics are associated with higher view counts."),
          p("Model:  log10(views + 1) ~ log10(likes + 1) + log10(comments + 1) + topic + professional"),
          br(),
          h4("Model coefficients (selected filters)"),
          tableOutput("model_coef_table"),
          br(),
          h4("Effect sizes with 95% confidence intervals"),
          plotOutput("model_coef_plot")
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Filtered data based on user inputs (used by多个tab)
  filtered_df <- reactive({
    d <- df %>%
      filter(
        topic %in% input$topic,
        views >= input$min_views
      )

    if (input$prof_group != "All") {
      d <- d %>% filter(professional == input$prof_group)
    }

    d
  })

  # ---- Summary table by topic & category ----
  output$summary_topic <- renderTable({
    filtered_df() %>%
      group_by(topic, professional) %>%
      summarise(
        n_videos         = n(),
        avg_views        = mean(views, na.rm = TRUE),
        avg_likes        = mean(likes, na.rm = TRUE),
        avg_comments     = mean(comments, na.rm = TRUE),
        avg_like_rate    = mean(like_rate, na.rm = TRUE),
        avg_comment_rate = mean(comment_rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_views)) %>%
      mutate(
        avg_views        = round(avg_views),
        avg_likes        = round(avg_likes),
        avg_comments     = round(avg_comments),
        avg_like_rate    = round(avg_like_rate, 4),
        avg_comment_rate = round(avg_comment_rate, 4)
      )
  })

  # ---- Boxplot of views by topic, faceted by professional vs influencer ----
  output$plot_views <- renderPlot({
    d <- filtered_df()
    if (nrow(d) == 0) return(NULL)

    ggplot(d, aes(x = topic, y = views, fill = professional)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
      scale_y_log10(labels = comma) +
      facet_wrap(~ professional) +
      labs(
        x = "Topic",
        y = "Views (log scale)",
        fill = "Category"
      ) +
      theme_minimal(base_size = 12)
  })

  # ---- Engagement scatter, faceted by professional vs influencer ----
  output$plot_engagement_scatter <- renderPlot({
    d <- filtered_df() %>%
      filter(!is.na(like_rate), !is.na(comment_rate))

    if (nrow(d) == 0) return(NULL)

    ggplot(d, aes(x = like_rate, y = comment_rate, color = topic)) +
      geom_point(alpha = 0.7, size = 3) +
      scale_x_continuous(labels = percent_format(accuracy = 0.01)) +
      scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
      facet_wrap(~ professional) +
      labs(
        x = "Like rate (likes / views)",
        y = "Comment rate (comments / views)",
        color = "Topic"
      ) +
      theme_minimal(base_size = 12)
  })

  # ---- Bar chart: average views by topic & category ----
  output$summary_bar <- renderPlot({
    df %>%
      group_by(topic, professional) %>%
      summarise(
        avg_views = mean(views, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = reorder(topic, avg_views), y = avg_views, fill = professional)) +
      geom_col() +
      scale_y_log10(labels = comma) +
      coord_flip() +
      labs(
        x = "Topic",
        y = "Average views (log scale)",
        fill = "Category"
      ) +
      theme_minimal(base_size = 12)
  })

  # ---- Video-level table ----
  output$video_table <- renderTable({
    metric <- input$sort_metric

    filtered_df() %>%
      arrange(desc(.data[[metric]])) %>%
      select(
        title,
        topic,
        professional,
        views,
        likes,
        comments,
        like_rate,
        comment_rate,
        channel_name,
        published_at
      ) %>%
      mutate(
        like_rate    = round(like_rate, 4),
        comment_rate = round(comment_rate, 4)
      ) %>%
      head(100)
  })

  model_data <- reactive({
    d <- filtered_df() %>%
      filter(
        !is.na(views), views > 0,
        !is.na(likes), likes >= 0,
        !is.na(comments), comments >= 0
      ) %>%
      mutate(
        log_views    = log10(views + 1),
        log_likes    = log10(likes + 1),
        log_comments = log10(comments + 1)
      )

    d
  })

  model_fit <- reactive({
    d <- model_data()
    validate(
      need(nrow(d) >= 30, "Not enough videos under current filters to fit the model. Please relax your filters.")
    )

    lm(log_views ~ log_likes + log_comments + topic + professional, data = d)
  })

  output$model_coef_table <- renderTable({
    fit <- model_fit()

    tidy(fit, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        estimate      = round(estimate, 3),
        std.error     = round(std.error, 3),
        conf.low      = round(conf.low, 3),
        conf.high     = round(conf.high, 3),
        p.value       = signif(p.value, 3)
      )
  })


  output$model_coef_plot <- renderPlot({
    fit <- model_fit()

    coef_df <- tidy(fit, conf.int = TRUE) %>%
      filter(term != "(Intercept)")

    if (nrow(coef_df) == 0) return(NULL)

    ggplot(coef_df,
           aes(x = reorder(term, estimate), y = estimate)) +
      geom_point() +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
      coord_flip() +
      labs(
        x = "Predictor",
        y = "Effect on log10(views + 1)",
        title = "Estimated effects with 95% confidence intervals"
      ) +
      theme_minimal(base_size = 12)
  })
}

# ---- Run app ----
shinyApp(ui = ui, server = server)

