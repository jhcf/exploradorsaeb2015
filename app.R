source('INPUT_R_MICRODADOS_CENSO-ESCOLAR_2017.r')

# remove os dados das escolas privadas, porque são poucas
#saeb <- saeb[saeb$ID_DEPENDENCIA_ADM!='Privada',]
#nrow(saeb)

dataset <- saeb

# cria a interface com o usuário shiny

ui <- fluidPage(
  
  titlePanel("Explorador do SAEB 2015 - Questionário Diretores - https://github.com/jhcf/exploradorsaeb2015"),
  
  sidebarPanel(
  
    selectInput('col_name', 'Filtrar coluna', c('.', names(dataset))),
    
    selectInput('col_val', 'Valor filtro', choices=c('.')),

    sliderInput('tamanhoAmostra', 'Tamanho da amostra', 
                min=min(1, nrow(dataset)), 
                max=nrow(dataset),
                value=min(1000, nrow(dataset)), 
                step=floor(nrow(dataset)/20),
                round=0),
    
    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),

    selectInput('color', 'Cor', c('.', names(dataset))),
    
#    checkboxInput('jitter', 'Jitter'),
#    checkboxInput('smooth', 'Smooth'),
    
    selectInput('facet_col', 'Facet Vertical', c(None='.', names(dataset))),
    selectInput('facet_row', 'Faceta Horizontal', c(None='.', names(dataset)))
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Mosaic',plotOutput("mosaico")),
      tabPanel('Mosaic (StdRes)',plotOutput("mosaicoStdRes")),
      tabPanel('Mosaic (obs)',plotOutput("mosaicoObserved")),
      tabPanel('Mosaic (exp)',plotOutput("mosaicoExpected")),
      tabPanel('Assoc Plot',plotOutput("assoc")),
      tabPanel('Sieve',plotOutput("sieve")),
      tabPanel('Tabs (stdRes)',plotOutput("tabelasStdRes")),
      tabPanel('Tabs (obs)',plotOutput("tabelasObserved")),
      tabPanel('Tabs (exp)',plotOutput("tabelasExpected")),
      tabPanel('Plot points',plotOutput("ggplot2"))
    )
  )
)

# cria o módulo servidor

server <- function(input, output, session) {
  
  colName <- reactive({
    print('colName <- reactive')
    colName <- input$col_name
  })
  
  observe ({
    print('observe 1')
    colName <- input$col_name
    if (colName != '.') {
      colNum <- which(colnames(dataset)==colName)
      print(paste('observe 1: length(colNum)',colNum))
      if (length(colNum)>0) {
        fatores <- dataset[[colNum]]
        escolhas <- c('.', levels(unique(factor(fatores))))
        updateSelectInput(session,'col_val',
                        label=paste("Filtrar registros com ",colName,' == ?'),
                        choices=escolhas,
                        selected = head(escolhas, 1))
      }
    }
  })  
  
  colVal <- reactive({
    print('colVal <- reactive')
    input$col_val
  })
  
  datasetFiltrado <- reactive({
    print('datasetFiltrado <- reactive')
    colName <- colName()
    if (colName != '.') {
      colVal <- colVal()
      if (colVal != '.') {
        colNum <- which(colnames(dataset)==colName)
        print(paste('datasetFiltrado <- reactive: colNum:',colNum," colVal:",colVal))
        datasetFiltrado <- subset(dataset, dataset[[colNum]]==colVal)
        print(paste('datasetFiltrado : nrowOriginal: ',nrow(dataset),' :nrowFiltrado:',nrow(datasetFiltrado)))
      } else {
        print(paste('datasetFiltrado <- reactive: else 1'))
        datasetFiltrado <- dataset
      }
    } else {
      print(paste('datasetFiltrado <- reactive: else 2'))
      datasetFiltrado <- dataset
    }
    datasetFiltrado
  })
  
  datasetAmostrado <- reactive ({
    print('datasetAmostrado <- reactive')
    dsFiltrado <- datasetFiltrado()
    sliderValue <- input$tamanhoAmostra
    if (nrow(dsFiltrado)<sliderValue) {
      sampleSize <- nrow(dsFiltrado)
    } else {
      sampleSize <- sliderValue
    }
    datasetAmostrado <- dsFiltrado[sample(nrow(dsFiltrado), sampleSize),]
    datasetAmostrado
  })
  
  observe({
    colName <- input$col_name
    colVal <- input$col_val
    print(paste('observe:input$col_name+input$col_val',colName,'+',colVal))
    if (colName != '.' & colVal != '.') {
      nrows <- nrow(datasetFiltrado())
      print(paste('observe:input$col_val:nrows',nrows))
      updateSliderInput(session,'tamanhoAmostra',
                        label = paste('Tamanho da amostra com ',colName,'=',colVal),
                      min=min(1, nrows), max=nrows,
                      step=floor(nrows/20),value=min(1000, nrows))
    }
  })  

#  dataset <- reactive({
#    saeb[sample(nrow(saeb), input$tamanhoAmostra),]
#  })
  
  output$mosaico <- renderPlot({
    print('output$mosaico')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
        tabelaContingencia <-
            structable(as.formula(sprintf("~%s+%s", input$x, input$y)),
            data=dtset)
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$y, input$x)),
                data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetCol, facetRow, input$x, input$y)),
                data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$y, input$x)),
              data=dtset)
    }
    # imprime o gráfico mosaico (biblioteca vcd)
    mosaico <- (mosaic(tabelaContingencia,shade=TRUE,direction = 'v', 
                 labeling=
                   labeling_border(gp_labels = gpar(fontsize=9),
                                   rot_labels = c(0,90,90,0),
                                   just_labels = "right",
                                   tl_varnames = c(TRUE,FALSE)))
          )
    #print(mosaico)
    
  }, height=700)

  output$mosaicoStdRes <- renderPlot({
    print('output$mosaicoStandardResiduals')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s", input$x, input$y)),
                   data=dtset)
#      tabelaContingencia <-
#        structable(as.formula(sprintf("~%s+%s", 'ID_UF', 'ID_DEPENDENCIA_ADM')),
#                   data=saeb)
#textplot(chisq.test(tabelaContingencia)$stdres,cex = c(0.7))
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$y, input$x)),
                     data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetCol, facetRow, input$x, input$y)),
                     data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$y, input$x)),
                   data=dtset)
    }
    # imprime o gráfico mosaico (biblioteca vcd)
    mosaico <- (mosaic(tabelaContingencia,gp=shading_Friendly2,direction = 'v', 
                 labeling=
                   labeling_residuals(gp_labels = gpar(fontsize=9),
                                      rot_labels = c(0,90,90,0),
                                      just_labels = "right",
                                      tl_varnames = c(TRUE,FALSE))
                )
    )
    #textplot(mosaico)
    
  }, height=700)

  output$mosaicoExpected <- renderPlot({
    print('output$mosaicoExpected')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s", input$x, input$y)),
                   data=dtset)
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$y, input$x)),
                     data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetCol, facetRow, input$x, input$y)),
                     data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$y, input$x)),
                   data=dtset)
    }
    # imprime o gráfico mosaico (biblioteca vcd)
    mosaico <- (mosaic(tabelaContingencia,type="expected",gp=shading_Friendly2,direction = 'v', 
                 labeling=
                   labeling_values(value_type = c('expected'),
                                   gp_labels = gpar(fontsize=9),
                                   rot_labels = c(0,90,90,0),
                                   just_labels = "right",
                                   tl_varnames = c(TRUE,FALSE))
    )
    )
    #print(mosaico)
    print(mosaico)    
  }, height=700)

    
  output$mosaicoObserved <- renderPlot({
    print('output$mosaicoExpected')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s", input$x, input$y)),
                   data=dtset)
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$y, input$x)),
                     data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetCol, facetRow, input$x, input$y)),
                     data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$y, input$x)),
                   data=dtset)
    }
    # imprime o gráfico mosaico (biblioteca vcd)
    mosaico <- (mosaic(tabelaContingencia,type="observed",gp=shading_Friendly2,direction = 'v', 
                 labeling=
                   labeling_values(value_type = c('observed'),
                                  gp_labels = gpar(fontsize=6),
                                   rot_labels = c(0,90,90,0),
                                   just_labels = "right",
                                   tl_varnames = c(TRUE,FALSE))
                 )
    )
    #print(mosaico)
    
  }, height=700)

  output$assoc <- renderPlot({
    print('output$assoc')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s", input$y, input$x)),
                   data=dtset)
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$x, input$y)),
                     data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetRow, facetCol, input$y, input$x)),
                     data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$x, input$y)),
                   data=dtset)
    }
    # imprime o gráfico mosaico (biblioteca vcd)
    mosaico <- (assoc(tabelaContingencia, direction = 'v',
#          gp=shading_Friendly2,direction = 'v', 
                labeling=
                  labeling_residuals(gp_labels = gpar(fontsize=9),
                     rot_labels = c(0,90,90,0),
                     just_labels = "right",
                     tl_varnames = c(TRUE,FALSE))
              )
#                 labeling=
#                   labeling_border(gp_labels = gpar(fontsize=9),
#                                   rot_labels = c(0,90,90,0),
#                                   just_labels = "right",
#                                   tl_varnames = c(TRUE,FALSE)))
    )
    #print(mosaico)
    
  }, height=700)

  output$sieve <- renderPlot({
    print('output$assoc')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s", input$y, input$x)),
                   data=dtset)
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$x, input$y)),
                     data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetRow, facetCol, input$y, input$x)),
                     data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$x, input$y)),
                   data=dtset)
    }
    # imprime o gráfico mosaico (biblioteca vcd)
    crivo <- (sieve(tabelaContingencia, direction = 'h'))
          #          gp=shading_Friendly2,direction = 'v', 
          #                 labeling=
          #                   labeling_border(gp_labels = gpar(fontsize=9),
          #                                   rot_labels = c(0,90,90,0),
          #                                   just_labels = "right",
          #                                   tl_varnames = c(TRUE,FALSE)))
    
    #print(crivo)
    
  }, height=700)

  output$tabelasStdRes <- renderPlot({
      print('output$tabelasStdRes')
      # cria a tabela de contingência com as colunas selecionadas
      dtset <- datasetAmostrado()
      facetRow <- input$facet_row
      facetCol <- input$facet_col
      if (facetRow == '.' & facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s", input$x, input$y)),
                     data=dtset)
        #      tabelaContingencia <-
        #        structable(as.formula(sprintf("~%s+%s", 'ID_UF', 'ID_DEPENDENCIA_ADM')),
        #                   data=saeb)
      } else if (facetRow != '.') {
        if (facetCol == '.') {
          tabelaContingencia <-
            structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$y, input$x)),
                       data=dtset)
        } else {
          tabelaContingencia <-
            structable(as.formula(sprintf("~%s+%s+%s+%s", facetCol, facetRow, input$x, input$y)),
                       data=dtset)
        }
      } else { #facetCol != '.'
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$y, input$x)),
                     data=dtset)
      }
      textplot(round(chisq.test(tabelaContingencia)$stdres,2),cex = c(0.7))
      # imprime o gráfico mosaico (biblioteca vcd)
#      mosaico <- (mosaic(tabelaContingencia,gp=shading_Friendly2,direction = 'v', 
#                         labeling=
#                           labeling_residuals(gp_labels = gpar(fontsize=9),
#                                              rot_labels = c(0,90,90,0),
#                                              just_labels = "right",
#                                              tl_varnames = c(TRUE,FALSE))
#      )
#      )
      #textplot(mosaico)
      
    }, height=1000)


  output$tabelasExpected <- renderPlot({
    print('output$tabelasExpected')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s", input$y, input$x)),
                   data=dtset)
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$x, input$y)),
                     data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetRow, facetCol, input$y, input$x)),
                     data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$x, input$y)),
                   data=dtset)
    }
#    tabelaContingencia <-
#               structable(as.formula(sprintf("~%s+%s+%s+%s", 'ID_UF', 'ID_DEPENDENCIA_ADM','ID_LOCALIZACAO','Q001_SEXO')),
#                          data=saeb)
#    residualsPlot <- (textplot(round(chisq.test(tabelaContingencia)$stdres,digits=2),cex = c(1)))
#    observedPlot <- (textplot(chisq.test(tabelaContingencia)$observed,cex = c(1)))
#    expectedPlot <- (textplot(chisq.test(tabelaContingencia)$expected,cex = c(1)))
#    print(residualsPlot)
#    print(observedPlot)
#    print(expectedPlot)
#    # imprime o gráfico mosaico (biblioteca vcd)
#    print(sieve(tabelaContingencia, direction = 'h'))
    #          gp=shading_Friendly2,direction = 'v', 
    #                 labeling=
    #                   labeling_border(gp_labels = gpar(fontsize=9),
    #                                   rot_labels = c(0,90,90,0),
    #                                   just_labels = "right",
    #                                   tl_varnames = c(TRUE,FALSE)))
    
    # imprime o gráfico mosaico (biblioteca vcd)
    mosaicoExpected <- (mosaic(tabelaContingencia,type="expected",gp=shading_Friendly2,direction = 'v', 
                       labeling=
                         labeling_values(value_type = c('expected'),
                                         gp_labels = gpar(fontsize=9),
                                         rot_labels = c(0,90,90,0),
                                         just_labels = "right",
                                         tl_varnames = c(TRUE,FALSE))))
    #print(mosaico)
    textplot(mosaicoExpected)    
    
  }, height=700,width = 700)
  
  output$tabelasObserved <- renderPlot({
    print('output$tabelasExpected')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    facetRow <- input$facet_row
    facetCol <- input$facet_col
    if (facetRow == '.' & facetCol == '.') {
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s", input$y, input$x)),
                   data=dtset)
    } else if (facetRow != '.') {
      if (facetCol == '.') {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s", facetRow, input$x, input$y)),
                     data=dtset)
      } else {
        tabelaContingencia <-
          structable(as.formula(sprintf("~%s+%s+%s+%s", facetRow, facetCol, input$y, input$x)),
                     data=dtset)
      }
    } else { #facetCol != '.'
      tabelaContingencia <-
        structable(as.formula(sprintf("~%s+%s+%s", facetCol, input$x, input$y)),
                   data=dtset)
    }
    #    tabelaContingencia <-
    #               structable(as.formula(sprintf("~%s+%s+%s+%s", 'ID_UF', 'ID_DEPENDENCIA_ADM','ID_LOCALIZACAO','Q001_SEXO')),
    #                          data=saeb)
    #    residualsPlot <- (textplot(round(chisq.test(tabelaContingencia)$stdres,digits=2),cex = c(1)))
    #    observedPlot <- (textplot(chisq.test(tabelaContingencia)$observed,cex = c(1)))
    #    expectedPlot <- (textplot(chisq.test(tabelaContingencia)$expected,cex = c(1)))
    #    print(residualsPlot)
    #    print(observedPlot)
    #    print(expectedPlot)
    #    # imprime o gráfico mosaico (biblioteca vcd)
    #    print(sieve(tabelaContingencia, direction = 'h'))
    #          gp=shading_Friendly2,direction = 'v', 
    #                 labeling=
    #                   labeling_border(gp_labels = gpar(fontsize=9),
    #                                   rot_labels = c(0,90,90,0),
    #                                   just_labels = "right",
    #                                   tl_varnames = c(TRUE,FALSE)))
    
    # imprime o gráfico mosaico (biblioteca vcd)
    mosaicoObserved <- (mosaic(tabelaContingencia,type="observed",gp=shading_Friendly2,direction = 'v', 
                               labeling=
                                 labeling_values(value_type = c('observed'),
                                                 gp_labels = gpar(fontsize=9),
                                                 rot_labels = c(0,90,90,0),
                                                 just_labels = "right",
                                                 tl_varnames = c(TRUE,FALSE))))
    #print(mosaico)
    textplot(mosaicoObserved)    
    
  }, height=700,width = 700)
  
  output$ggplot2 <- renderPlot({
    dtset <- datasetAmostrado()
    print('output$mosaico')
    # cria a tabela de contingência com as colunas selecionadas
    p <- ggplot(dtset, aes_string(x=input$x, y=input$y)) + geom_point() 

    p <- p + scale_y_discrete(limits = rev(levels(dtset[[input$y]])))
  
    if (input$color != '.')
      p <- p + aes_string(color=input$color)
  
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
  
    #if (input$jitter)
    p <- p + geom_jitter()
    #if (input$smooth)
    #  p <- p + geom_smooth()
  
    print(p)
  
  }, height=700)

}

# Executa a aplicação shiny 
shinyApp(ui = ui, server = server)
