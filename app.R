# ---- BIBLIOTECAS
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(ggsci)
library(broom)
library(DT)

# ---- CONFIGURAÇÕES GLOBAIS
cor_principal <- "#4C72B0"
cor_destaque  <- "#C44E52"
# 
tema_padrao <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.minor = element_blank()
  )
theme_set(tema_padrao)

# ---- CARREGAMENTO DA BASE
url_dataset <- "https://raw.githubusercontent.com/EstevaoMO/analise_rendimento_estudantil/refs/heads/main/student_performance_dataset.csv"

dados_estudantes <- read.csv(url_dataset, stringsAsFactors = FALSE) |>
  mutate(
    parental_education = if_else(parental_education == "None", "Desconhecido", parental_education),
    gender = as.factor(gender),
    parental_education = as.factor(parental_education),
    internet_access = as.factor(internet_access),
    extracurricular_activities = as.factor(extracurricular_activities),
    part_time_job = as.factor(part_time_job),
    final_grade = as.factor(final_grade)
  )

# ---- MODELO DE REGRESSÃO
modelo_regressao <- lm(
  final_exam_score ~ study_time_hours + attendance_percent + sleep_hours +
    parental_education + internet_access + extracurricular_activities +
    part_time_job + previous_grade,
  data = dados_estudantes
)

# ---- FUNÇÕES REUTILIZÁVEIS
criar_histograma <- function(df, coluna, titulo, eixo_x) {
  ggplot(df, aes(x = .data[[coluna]])) +
    geom_histogram(bins = 30, fill = cor_principal, color = "white", alpha = 0.85) +
    labs(title = titulo, x = eixo_x, y = "Frequência")
}

criar_grafico_proporcao <- function(df, coluna, titulo) {
  df |>
    count(.data[[coluna]]) |>
    mutate(prop = n / sum(n)) |>
    ggplot(aes(x = reorder(.data[[coluna]], prop), y = prop, fill = .data[[coluna]])) +
    geom_col(color = "white", show.legend = FALSE) +
    geom_text(aes(label = percent(prop, accuracy = 1)), hjust = -0.15, fontface = "bold", size = 3.5) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.2))) +
    scale_fill_jco() +
    labs(title = titulo, x = NULL, y = "Proporção")
}

criar_dispersao_tendencia <- function(df, x, titulo, eixo_x) {
  ggplot(df, aes(x = .data[[x]], y = final_exam_score)) +
    geom_point(alpha = 0.35, color = cor_principal) +
    geom_smooth(method = "lm", color = cor_destaque, se = TRUE) +
    labs(title = titulo, x = eixo_x, y = "Nota Final")
}

criar_boxplot_categorico <- function(df, coluna, titulo) {
  ggplot(df, aes(x = .data[[coluna]], y = final_exam_score, fill = .data[[coluna]])) +
    geom_boxplot(alpha = 0.7, show.legend = FALSE, outlier.color = cor_destaque) +
    scale_fill_jco() +
    labs(title = titulo, x = NULL, y = "Nota Final")
}

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Desempenho Estudantil"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "visao_geral", icon = icon("chart-pie")),
      menuItem("Exploração Interativa", tabName = "exploracao", icon = icon("magnifying-glass-chart")),
      menuItem("Modelo Preditivo", tabName = "modelo", icon = icon("brain")),
      menuItem("Dados Brutos", tabName = "dados", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---- ABA 1: VISÃO GERAL
      tabItem(tabName = "visao_geral",
              fluidRow(
                valueBoxOutput("box_media_nota", width = 3),
                valueBoxOutput("box_total_alunos", width = 3),
                valueBoxOutput("box_media_estudo", width = 3),
                valueBoxOutput("box_media_frequencia", width = 3)
              ),
              fluidRow(
                box(title = "Distribuição das Notas Finais", width = 6, status = "primary",
                    plotOutput("hist_notas")),
                box(title = "Distribuição das Horas de Estudo", width = 6, status = "primary",
                    plotOutput("hist_estudo"))
              ),
              fluidRow(
                box(title = "Acesso à Internet", width = 3, status = "primary", plotOutput("prop_internet")),
                box(title = "Escolaridade dos Pais", width = 3, status = "primary", plotOutput("prop_escolaridade")),
                box(title = "Atividades Extracurriculares", width = 3, status = "primary", plotOutput("prop_extra")),
                box(title = "Trabalho de Meio Período", width = 3, status = "primary", plotOutput("prop_trabalho"))
              )
      ),
      
      # ---- ABA 2: EXPLORAÇÃO INTERATIVA
      tabItem(tabName = "exploracao",
              fluidRow(
                box(title = "Filtros", width = 3, status = "warning", solidHeader = TRUE,
                    selectInput("var_numerica", "Variável numérica (eixo X):",
                                choices = c("study_time_hours", "attendance_percent",
                                            "sleep_hours", "previous_grade"),
                                selected = "study_time_hours"),
                    selectInput("var_categorica", "Variável categórica:",
                                choices = c("gender", "parental_education", "internet_access",
                                            "extracurricular_activities", "part_time_job"),
                                selected = "gender")
                ),
                box(title = "Nota Final vs. Variável Numérica", width = 4, status = "primary",
                    plotlyOutput("scatter_dinamico")),
                box(title = "Nota Final por Variável Categórica", width = 5, status = "primary",
                    plotlyOutput("box_dinamico"))
              )
      ),
      
      # ---- ABA 3: MODELO PREDITIVO
      tabItem(tabName = "modelo",
              fluidRow(
                box(title = "Simule uma Previsão", width = 4, status = "success", solidHeader = TRUE,
                    sliderInput("input_estudo", "Horas de estudo:", min = 0, max = 12, value = 4, step = 0.5),
                    sliderInput("input_frequencia", "Frequência (%):", min = 0, max = 100, value = 80),
                    sliderInput("input_sono", "Horas de sono:", min = 3, max = 10, value = 7, step = 0.5),
                    sliderInput("input_notaanterior", "Nota anterior:", min = 0, max = 100, value = 70),
                    selectInput("input_escolaridade", "Escolaridade dos pais:",
                                choices = levels(dados_estudantes$parental_education)),
                    selectInput("input_internet", "Acesso à internet:",
                                choices = levels(dados_estudantes$internet_access)),
                    selectInput("input_extra", "Atividades extracurriculares:",
                                choices = levels(dados_estudantes$extracurricular_activities)),
                    selectInput("input_trabalho", "Trabalho de meio período:",
                                choices = levels(dados_estudantes$part_time_job)),
                    actionButton("botao_prever", "Calcular Previsão", icon = icon("calculator"),
                                 class = "btn-success")
                ),
                box(title = "Nota Prevista", width = 3, status = "success", solidHeader = TRUE,
                    height = "180px",
                    h1(textOutput("nota_prevista"), style = "text-align:center; font-weight:bold;")),
                box(title = "Efeito de Cada Variável no Modelo", width = 5, status = "primary",
                    plotOutput("coeficientes_modelo"))
              ),
              fluidRow(
                box(title = "Resumo Estatístico do Modelo", width = 12, status = "primary",
                    verbatimTextOutput("resumo_modelo"))
              )
      ),
      
      # ---- ABA 4: DADOS BRUTOS
      tabItem(tabName = "dados",
              fluidRow(
                box(title = "Base de Dados Completa", width = 12, status = "primary",
                    DTOutput("tabela_dados"))
              )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # ---- VALUE BOXES
  output$box_media_nota <- renderValueBox({
    valueBox(round(mean(dados_estudantes$final_exam_score, na.rm = TRUE), 1),
             "Média das Notas", icon = icon("star"), color = "blue")
  })
  
  output$box_total_alunos <- renderValueBox({
    valueBox(nrow(dados_estudantes), "Total de Alunos", icon = icon("users"), color = "purple")
  })
  
  output$box_media_estudo <- renderValueBox({
    valueBox(round(mean(dados_estudantes$study_time_hours, na.rm = TRUE), 1),
             "Média de Horas de Estudo", icon = icon("book"), color = "green")
  })
  
  output$box_media_frequencia <- renderValueBox({
    valueBox(paste0(round(mean(dados_estudantes$attendance_percent, na.rm = TRUE), 1), "%"),
             "Frequência Média", icon = icon("calendar-check"), color = "yellow")
  })
  
  # ---- VISÃO GERAL
  output$hist_notas  <- renderPlot(criar_histograma(dados_estudantes, "final_exam_score", "Notas Finais", "Nota Final"))
  output$hist_estudo <- renderPlot(criar_histograma(dados_estudantes, "study_time_hours", "Horas de Estudo", "Horas de Estudo"))
  
  output$prop_internet     <- renderPlot(criar_grafico_proporcao(dados_estudantes, "internet_access", NULL))
  output$prop_escolaridade <- renderPlot(criar_grafico_proporcao(dados_estudantes, "parental_education", NULL))
  output$prop_extra        <- renderPlot(criar_grafico_proporcao(dados_estudantes, "extracurricular_activities", NULL))
  output$prop_trabalho     <- renderPlot(criar_grafico_proporcao(dados_estudantes, "part_time_job", NULL))
  
  # ---- EXPLORAÇÃO INTERATIVA
  output$scatter_dinamico <- renderPlotly({
    grafico <- criar_dispersao_tendencia(dados_estudantes, input$var_numerica,
                                         paste("Nota Final vs.", input$var_numerica), input$var_numerica)
    ggplotly(grafico)
  })
  
  output$box_dinamico <- renderPlotly({
    grafico <- criar_boxplot_categorico(dados_estudantes, input$var_categorica,
                                        paste("Nota Final por", input$var_categorica))
    ggplotly(grafico)
  })
  
  # ---- MODELO PREDITIVO
  output$coeficientes_modelo <- renderPlot({
    tidy(modelo_regressao, conf.int = TRUE) |>
      filter(term != "(Intercept)") |>
      mutate(significativo = p.value < 0.05) |>
      ggplot(aes(x = reorder(term, estimate), y = estimate, color = significativo)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      coord_flip() +
      scale_color_manual(values = c("TRUE" = cor_principal, "FALSE" = "gray70")) +
      labs(x = NULL, y = "Efeito estimado na nota final", color = "p < 0.05")
  })
  
  output$resumo_modelo <- renderPrint({
    summary(modelo_regressao)
  })
  
  previsao_reativa <- eventReactive(input$botao_prever, {
    novo_dado <- data.frame(
      study_time_hours = input$input_estudo,
      attendance_percent = input$input_frequencia,
      sleep_hours = input$input_sono,
      previous_grade = input$input_notaanterior,
      parental_education = factor(input$input_escolaridade, levels = levels(dados_estudantes$parental_education)),
      internet_access = factor(input$input_internet, levels = levels(dados_estudantes$internet_access)),
      extracurricular_activities = factor(input$input_extra, levels = levels(dados_estudantes$extracurricular_activities)),
      part_time_job = factor(input$input_trabalho, levels = levels(dados_estudantes$part_time_job))
    )
    valor_bruto <- predict(modelo_regressao, newdata = novo_dado)
    valor_limitado <- pmin(pmax(valor_bruto, 0), 100)
    return(valor_limitado)
  })
  
  output$nota_prevista <- renderText({
    if (input$botao_prever == 0) {
      "—"
    } else {
      paste0(round(previsao_reativa(), 1))
    }
  })
  
  # ---- DADOS BRUTOS
  output$tabela_dados <- renderDT({
    datatable(dados_estudantes, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# RODAR APP
shinyApp(ui, server)