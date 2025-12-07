# app.R

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# ---- Load and prep data ----
df_raw <- read_csv("youtube_health_topics_6_keywords.csv")

df <- df_raw %>%
  # adapt these renames if your column names are slightly different
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
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Filtered data based on user inputs
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
        n_videos        = n(),
        avg_views       = mean(views, na.rm = TRUE),
        avg_likes       = mean(likes, na.rm = TRUE),
        avg_comments    = mean(comments, na.rm = TRUE),
        avg_like_rate   = mean(like_rate, na.rm = TRUE),
        avg_comment_rate= mean(comment_rate, na.rm = TRUE),
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
}

# ---- Run app ----
shinyApp(ui = ui, server = server)