library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "Badogue Mapas"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("busmap2", height = 500)
           )
    ),
    column(width = 3,
      box(width = NULL, status = "warning",
        p(
          class = "text-muted",
          paste("Nota: As opções do tipo 'Cores de Sentimento' aceitam apenas números inteiros maiores ou iguais a zero.",
                "As opções 'Índice de Sentimento' e 'Variação com o mês anterior' aceitam números reais entre -1 e +1."
          ),
          fileInput('file', 'Escolha o Arquivo EXCEL', accept=c('.xlsx')),
          selectInput("tipolegenda", "Tipo de Legenda",
                      choices = c(
                         "Cores de Sentimento Positivo" = 'corespos',
                         "Cores de Sentimento Negativo" = 'coresneg',
                         "Cores de Sentimento Neutro" = 'coresneu',
                         "Variação do IS com mês anterior" = 'aumentadiminui',
                         "Legenda Típica do IS" = 'positivasnegativas'
                      ),
                      selected = 'positivasnegativas'
          ),
          textAreaInput("mes",label = "Mês referente aos dados",value = "")
        )
      )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
