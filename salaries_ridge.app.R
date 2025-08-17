# ==============================================================================
# Dashboard Interactivo de Salarios de Profesores (- Modelo Ridge -)
#
# Descripción: Aplicación Shiny completa con pestañas para análisis exploratorio,
#              comparación flexible de variables y predicción de salarios,
# ==============================================================================

# 1. Carga de Librerías y Datos
# ------------------------------------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(DT) 
library(tidyr) 
library(glmnet) 

# Carga de datos
salaries_df <- read_csv("data/salaries.csv", show_col_types = FALSE)

# 2. Preprocesamiento de Datos
# ------------------------------------------------------------------------------
# Limpieza inicial
if ("...1" %in% names(salaries_df)) {
  salaries_df <- salaries_df %>% select(-`...1`)
}

# Renombrar columnas
salaries_df <- salaries_df %>%
  rename(
    Rango = rank,
    Disciplina = discipline,
    Años_Doctorado = yrs.since.phd,
    Años_Servicio = yrs.service,
    Sexo = sex,
    Salario = salary
  )

# Convertir variables a factor y recodificar
salaries_df <- salaries_df %>%
  mutate(
    Rango = as.factor(Rango),
    Disciplina = as.factor(Disciplina),
    Sexo = as.factor(Sexo),
    Rango = dplyr::recode(Rango,
                          "Prof" = "Catedrático",
                          "AssocProf" = "Prof. Asociado",
                          "AsstProf" = "Prof. Asistente"
    ),
    Sexo = dplyr::recode(Sexo,
                         "Male" = "Hombre",
                         "Female" = "Mujer"
    )
  )

salaries_df$Rango <- factor(salaries_df$Rango,
                            levels = c("Prof. Asistente", "Prof. Asociado", "Catedrático")
)

# 3. Creación del Modelo de Predicción 
# ------------------------------------------------------------------------------
# Preparar los datos para glmnet: matriz de diseño y vector de respuesta
x_vars <- model.matrix(Salario ~ ., data = salaries_df)[, -1]
y_var <- salaries_df$Salario

# Entrenar el modelo Ridge con validación cruzada para encontrar el mejor lambda
prediction_model <- cv.glmnet(
  x_vars, y_var,
  alpha = 0, # alpha = 0 para Ridge
  standardize = TRUE #
)

# Definición de la Interfaz de Usuario (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  
  # Estilos CSS
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap');
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f5f5f5; /* Blanco roto muy suave */
        color: #333333; /* Gris oscuro para el texto principal */
      }
      .well {
        background-color: #f0f0f0; /* Gris muy claro */
        border-radius: 0.75rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        padding: 2rem;
        color: #333333;
      }
      .main-content {
        background-color: #ffffff;
        border-radius: 0.75rem;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1-px rgba(0, 0, 0, 0.06);
        padding: 2rem;
        min-height: 80vh;
        color: #333333;
      }
      .title {
        color: #555555;
        font-weight: bold;
        text-align: center;
      }
      .plot-container {
        border: 1px solid #e0e0e0; /* Borde gris claro */
        border-radius: 0.5rem;
        padding: 1rem;
        background-color: #ffffff;
        margin-bottom: 2rem;
      }
      .prediction-box {
        background-color: #e8f5e9; /* Verde pastel muy claro */
        border-left: 5px solid #66bb6a; /* Verde pastel más saturado */
        padding: 1rem;
        border-radius: 0.5rem;
        margin-top: 1rem;
      }
      .prediction-box h4 {
        margin-top: 0;
        color: #43a047; /* Verde medio */
      }
      .sidebar-section {
        margin-bottom: 2rem;
        border-bottom: 1px solid #cccccc;
        padding-bottom: 1rem;
      }
      .btn-primary {
        background-color: #9575cd; /* Morado pastel */
        border-color: #9575cd;
      }
      .btn-primary:hover {
        background-color: #7e57c2; /* Morado pastel más oscuro */
        border-color: #7e57c2;
      }
    "))
  ),
  
  # Título de la aplicación
  div(class = "title",
      titlePanel("Dashboard de Análisis y Predicción de Salarios")
  ),
  
  # Diseño de la página con un sidebar y un panel principal
  sidebarLayout(
    
    # Panel de la barra lateral para controles
    sidebarPanel(class = "well",
                 
                 # Sección para la predicción del salario
                 div(class = "sidebar-section",
                     h3("Predicción de Salario"),
                     p("Introduce los datos de un profesor para predecir su salario."),
                     
                     # Inputs para la predicción
                     selectInput("pred_rango", "Rango:", levels(salaries_df$Rango)),
                     selectInput("pred_disciplina", "Disciplina:", levels(salaries_df$Disciplina)),
                     selectInput("pred_sexo", "Sexo:", levels(salaries_df$Sexo)),
                     sliderInput("pred_años_servicio", "Años de Servicio:", min = 0, max = 60, value = 10),
                     sliderInput("pred_años_doctorado", "Años desde Doctorado:", min = 0, max = 60, value = 10),
                     
                     # Botón de predicción
                     actionButton("predict_btn", "Calcular Salario", class = "btn-primary"),
                     
                     # Muestra el resultado de la predicción
                     div(class = "prediction-box",
                         h4("Salario Previsto"),
                         textOutput("prediction_output")
                     )
                 )
    ),
    
    # Panel principal con pestañas
    mainPanel(class = "main-content",
              tabsetPanel(
                
                # Pestaña 1: Análisis General (Variables Categóricas)
                tabPanel("Análisis General",
                         h3("Visualizaciones del Análisis Exploratorio"),
                         # Selector para el tipo de gráfico (solo categóricas)
                         selectInput(
                           "plot_type", "Selecciona un Gráfico:",
                           c("Distribución de Salarios por Rango" = "rango_dist",
                             "Distribución de Salarios por Disciplina" = "disciplina_dist",
                             "Distribución de Salarios por Sexo" = "sexo_dist")
                         ),
                         # Área para renderizar el gráfico
                         div(class = "plot-container",
                             plotOutput("selected_plot")
                         ),
                         
                         # Resumen de Datos
                         hr(),
                         h3(textOutput("data_summary_title")),
                         DTOutput("data_summary")
                ),
                
                # Pestaña 2: Análisis Comparativo (entre grupos y continuas) 
                tabPanel("Análisis Comparativo",
                         h3("Análisis de la Relación entre Variables"),
                         fluidRow(
                           column(4,
                                  selectInput("comp_x_var", "Variable en Eje X:",
                                              c("Rango", "Disciplina", "Sexo", "Años_Servicio", "Años_Doctorado")
                                  )
                           ),
                           column(4,
                                  selectInput("comp_y_var", "Variable en Eje Y:",
                                              c("Salario", "Años_Servicio", "Años_Doctorado", "Rango", "Disciplina", "Sexo")
                                  )
                           ),
                           column(4,
                                  selectInput("comp_fill_var", "Variable para Color/Grupo:",
                                              c("Ninguna", "Rango", "Disciplina", "Sexo")
                                  )
                           )
                         ),
                         div(class = "plot-container",
                             plotOutput("flexible_comparative_plot")
                         )
                ),
                
                # Pestaña 3: Resumen del Modelo
                tabPanel("Resumen del Modelo",
                         h3("Resumen del Modelo de Regresión Ridge"),
                         p("Este modelo se utiliza para la predicción de salarios."),
                         verbatimTextOutput("model_summary")
                )
              )
    )
  )
)

# Definición de la Lógica del Servidor (Server)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Definición de la paleta de colores pastel
  custom_palette <- c("#f4c2c2", "#b19cd9", "#89cff0", "#77dd77")
  
  # 1. Lógica para el gráfico de análisis general (categóricas)
  output$selected_plot <- renderPlot({
    req(input$plot_type)
    
    switch(input$plot_type,
           "rango_dist" = {
             ggplot(salaries_df, aes(x = Rango, y = Salario, fill = Rango)) +
               geom_boxplot() + labs(title = "Distribución de Salarios por Rango", x = "Rango", y = "Salario") +
               scale_fill_manual(values = custom_palette) + # Aplicar colores pastel
               theme_minimal() + theme(legend.position = "none")
           },
           "disciplina_dist" = {
             ggplot(salaries_df, aes(x = Disciplina, y = Salario, fill = Disciplina)) +
               geom_boxplot() + labs(title = "Distribución de Salarios por Disciplina", x = "Disciplina", y = "Salario") +
               scale_fill_manual(values = custom_palette) + # Aplicar colores pastel
               theme_minimal() + theme(legend.position = "none")
           },
           "sexo_dist" = {
             ggplot(salaries_df, aes(x = Sexo, y = Salario, fill = Sexo)) +
               geom_boxplot() + labs(title = "Distribución de Salarios por Sexo", x = "Sexo", y = "Salario") +
               scale_fill_manual(values = custom_palette) + # Aplicar colores pastel
               theme_minimal() + theme(legend.position = "none")
           }
    )
  })
  
  # 2. Lógica para el nuevo gráfico flexible de análisis comparativo
  output$flexible_comparative_plot <- renderPlot({
    req(input$comp_x_var, input$comp_y_var)
    
    # Asigna las variables de forma dinámica
    x_var <- input$comp_x_var
    y_var <- input$comp_y_var
    fill_var <- input$comp_fill_var
    
    # Construye el gráfico base
    p <- ggplot(salaries_df, aes_string(x = x_var, y = y_var))
    
    # Si se selecciona una variable para colorear, añádela
    if (fill_var != "Ninguna") {
      p <- p + aes_string(fill = fill_var, color = fill_var) +
        scale_fill_manual(values = custom_palette) + # Aplicar colores pastel
        scale_color_manual(values = custom_palette) # Aplicar colores para los puntos/líneas
    }
    
    # Define el tipo de gráfico según las variables seleccionadas
    x_is_cont <- is.numeric(salaries_df[[x_var]])
    y_is_cont <- is.numeric(salaries_df[[y_var]])
    
    if (x_is_cont && y_is_cont) {
      # Scatter plot para dos variables continuas
      p <- p + geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE)
    } else if (!x_is_cont && y_is_cont) {
      # Boxplot clásico para variable categórica en X
      p <- p + geom_boxplot()
    } else if (x_is_cont && !y_is_cont) {
      # Boxplot con ejes invertidos para variable categórica en Y
      p <- p + geom_boxplot() + coord_flip()
    } else {
      # Dos variables categóricas: cuenta y barplot
      p <- ggplot(salaries_df, aes_string(x = x_var)) + geom_bar(aes_string(fill = fill_var))
    }
    
    # Añade etiquetas y tema
    p <- p + labs(
      title = paste(y_var, "vs.", x_var),
      x = x_var,
      y = y_var
    ) + theme_minimal()
    
    # Renderiza el gráfico
    p
  })
  
  # 3. Lógica para el título del resumen de datos
  output$data_summary_title <- renderText({
    switch(input$plot_type,
           "rango_dist" = "Resumen de Datos por Rango",
           "disciplina_dist" = "Resumen de Datos por Disciplina",
           "sexo_dist" = "Resumen de Datos por Sexo",
           "Resumen Completo del Dataset"
    )
  })
  
  # 4. Lógica para la tabla de resumen (ahora dinámica)
  output$data_summary <- renderDT({
    req(input$plot_type)
    
    variable_resumen <- switch(input$plot_type,
                               "rango_dist" = "Rango",
                               "disciplina_dist" = "Disciplina",
                               "sexo_dist" = "Sexo"
    )
    
    summarized_df <- salaries_df %>%
      group_by(!!sym(variable_resumen)) %>%
      summarize(
        Conteo = n(),
        Salario_Medio = mean(Salario),
        Salario_Mediana = median(Salario)
      ) %>%
      ungroup() %>%
      rename(Variable = 1)
    
    datatable(
      summarized_df,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      class = "display compact"
    )
  })
  
  # 5. Lógica para el resumen del modelo de regresión
  output$model_summary <- renderPrint({
    # Acceder al mejor modelo (con el lambda óptimo)
    best_model <- prediction_model$glmnet.fit
    
    # Extraer los coeficientes para el lambda óptimo
    coefficients <- as.matrix(coef(best_model, s = prediction_model$lambda.min))
    
    # Crear un resumen limpio
    cat("Resumen del Modelo de Regresión Ridge\n\n")
    cat("Lambda (λ) óptimo:", prediction_model$lambda.min, "\n")
    cat("\nCoeficientes del Modelo (en el lambda óptimo):\n")
    print(coefficients)
  })
  
  # 6. Lógica para la predicción de salario
  # Se usa 'tryCatch' para manejar posibles errores al predecir
  observeEvent(input$predict_btn, {
    
    # Intentar ejecutar el código de predicción
    tryCatch({
      # Crear un nuevo data frame con los valores del usuario
      new_data_df <- data.frame(
        Rango = input$pred_rango,
        Disciplina = input$pred_disciplina,
        Sexo = input$pred_sexo,
        Años_Doctorado = input$pred_años_doctorado,
        Años_Servicio = input$pred_años_servicio
      )
      
      # Asegurarse de que los factores tienen los mismos niveles que los datos de entrenamiento
      # Esto es crucial para que model.matrix funcione correctamente.
      new_data_df$Rango <- factor(new_data_df$Rango, levels = levels(salaries_df$Rango))
      new_data_df$Disciplina <- factor(new_data_df$Disciplina, levels = levels(salaries_df$Disciplina))
      new_data_df$Sexo <- factor(new_data_df$Sexo, levels = levels(salaries_df$Sexo))
      
      # Para crear la matriz de datos para la predicción,
      # necesitamos asegurar que tenga la misma estructura que la de entrenamiento.
      # La forma más robusta de hacerlo es combinar temporalmente los datos
      # de entrada con los datos de entrenamiento y luego usar model.matrix.
      temp_df <- rbind(salaries_df %>% select(-Salario), new_data_df)
      
      # Creamos la matriz de diseño de todo el conjunto de datos combinado.
      # La fórmula '~ .' se refiere a todas las variables, por lo que no necesita Salario.
      temp_matrix <- model.matrix(~ ., data = temp_df)[, -1]
      
      # La matriz de datos para la predicción es la última fila de la matriz temporal.
      new_data_matrix <- temp_matrix[nrow(temp_matrix), , drop = FALSE]
      
      # Realizar la predicción
      prediction <- predict(
        prediction_model,
        newx = new_data_matrix,
        s = "lambda.min"
      )
      
      # Mostrar el resultado
      output$prediction_output <- renderText({
        paste0("€", format(round(prediction, 2), nsmall = 2, big.mark = ","))
      })
      
    }, error = function(e) {
      # Si ocurre un error, mostrarlo en la consola y en la interfaz
      warning("Error al realizar la predicción:", e$message)
      output$prediction_output <- renderText({
        paste("Error: ", e$message)
      })
    })
  })
}

# 7. Ejecutar la Aplicación
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
