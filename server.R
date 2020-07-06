library(tidyverse)
library(shiny)
library(cowplot)

source("helper.R")
shinyServer(function(input, output, session) {
  valeurs <- reactiveValues()
  observe({
    inFile <- input$dataload
    if (is.null(inFile))
      return(NULL)
    else{
      load(inFile$datapath)
      updateNumericInput(session, "nbparam", value = input$nbparam)
      updateNumericInput(session, "baseline", value = input$baseline)
      updateTextInput(session, "xlabel", value = input$xlabel)
      updateRadioButtons(session, "format", selected = input$format)
    }
    valeurs[["input"]] <- input
  })
  output$ui_variable <- renderUI({
    nbparam <- input$nbparam
    if (!is.null(input$dataload)){
      input <- valeurs[["input"]]
    }
    if (!is.na(nbparam)){
      r <- tagList(p("Nom de la variable"),
      lapply(1:nbparam, function(i) {
        isolate({textInput(paste0("variable", i), "", value = ifelse(!is.null(input[[paste0("variable",i)]]), input[[paste0("variable",i)]], ""))})
      }))
    }
  })
  output$ui_minvar <- renderUI({
    nbparam <- input$nbparam
    if (!is.null(input$dataload)){
      input <- valeurs[["input"]]
    }
    if (!is.na(nbparam)){
      r <- tagList(p("Valeur minimale"),
      lapply(1:nbparam, function(i) {
        isolate({textInput(paste0("minvar", i), "", value = ifelse(!is.null(input[[paste0("minvar",i)]]), input[[paste0("minvar",i)]], ""))})
      }))
    }
  })
  output$ui_maxvar <- renderUI({
    nbparam <- input$nbparam
    if (!is.null(input$dataload)){
      input <- valeurs[["input"]]
    }
     if (!is.na(nbparam)){
       r <- tagList(p("Valeur maximale"),
       lapply(1:nbparam, function(i) {
         isolate({textInput(paste0("maxvar", i), "", value = ifelse(!is.null(input[[paste0("maxvar",i)]]), input[[paste0("maxvar",i)]], ""))})
       }))
     }
  })
  output$ui_minICER <- renderUI({
    nbparam <- input$nbparam
    if (!is.null(input$dataload)){
      input <- valeurs[["input"]]
    }
     if (!is.na(nbparam)){
       r <- tagList(p("ICER minimum"),
       lapply(1:nbparam, function(i) {
         isolate({numericInput(paste0("minICER", i), "", value = ifelse(!is.null(input[[paste0("minICER",i)]]), input[[paste0("minICER",i)]], ""))})
       }))
     }
  })
  output$ui_maxICER <- renderUI({
    nbparam <- input$nbparam
    if (!is.null(input$dataload)){
      input <- valeurs[["input"]]
    }
     if (!is.na(nbparam)){
       r <- tagList(p("ICER maximum"),
       lapply(1:nbparam, function(i) {
         isolate({numericInput(paste0("maxICER", i), "", value = ifelse(!is.null(input[[paste0("maxICER",i)]]), input[[paste0("maxICER",i)]], ""))})
     }))
     }
  })
  output$ui_unit <- renderUI({
    nbparam <- input$nbparam
    if (!is.null(input$dataload)){
      input <- valeurs[["input"]]
    }
     if (!is.na(nbparam)){
       r <- tagList(p("Unité"),
       lapply(1:nbparam, function(i) {
         isolate({textInput(paste0("unit", i), "", value = ifelse(!is.null(input[[paste0("unit",i)]]), input[[paste0("unit",i)]], ""))})
       }))
     }
  })
#### Je n'ai rien trouvé de plus élégant pour changer d'onglet à la fin du calcul ##
  fin <-  eventReactive(input$start, {updateTabsetPanel(session, "main", selected = "Graphique")})
  observe(fin())
    start <- eventReactive({
      input$start
      input$choixPalette
    }, 
    {
    #  if (!is.null(input$dataload))
    #    input <- valeurs[["input"]]
    tmp <- data.frame(variable = sapply(1:input$nbparam, function(i) input[[paste0("variable",i )]]), 
                      minvar = sapply(1:input$nbparam, function(i) input[[paste0("minvar",i )]]),
                      maxvar = sapply(1:input$nbparam, function(i) input[[paste0("maxvar",i )]]),
                      minICER = sapply(1:input$nbparam, function(i) input[[paste0("minICER",i )]]),
                      maxICER = sapply(1:input$nbparam, function(i) input[[paste0("maxICER",i )]]),
                      unit = sapply(1:input$nbparam, function(i) input[[paste0("unit",i )]]))
    xlabel <- input$xlabel
    if (input$format == "Français"){
      big.mark <- " "
      decimal.mark <- ","
      currency.sep <- " "
      currency.pos <- "after"
    } else {
      big.mark <- ","
      decimal.mark <- "."
      currency.sep <- ""
      currency.pos <- "before"
    }
    baseline <- input$baseline
    tmp$delta = tmp$maxICER - tmp$minICER
    print(tmp)
    tmp <- tmp[order(tmp$delta, decreasing = TRUE),]
    maxICER <- max(tmp$maxICER)
    minICER <- min(tmp$minICER) 
    deltamax = maxICER - minICER
    tmin <- tmp[, c(1, 2, 4, 6, 7)]
    names(tmin) <- c("variable", "valuevar", "valueICER", "unit", "delta")
    tmin$Level = "Min"
    
    tmax <- tmp[, c(1, 3, 5, 6, 7)]
    names(tmax) <- c("variable", "valuevar", "valueICER", "unit", "delta")
    tmax$Level = "Max"
    t <- rbind(tmin, tmax)
    t$variable <- factor(t$variable, levels=tmp$variable[order(tmp$delta,decreasing=F)])

    
    t$valuevar2 <- ifelse(t$unit %in% c("%", "percent"), percent(t$valuevar), 
                          ifelse(t$unit %in% currencies, currency.format(t$valuevar, t$unit, currency.sep, currency.pos, big.mark, decimal.mark), 
                                 ifelse(t$unit != "", paste(formatE(t$valuevar, big.mark = big.mark, decimal.mark = decimal.mark), t$unit), formatE(t$valuevar, big.mark = big.mark, decimal.mark = decimal.mark))))
    
    breaks <- pretty(-baseline:max(pretty(deltamax)))
    
    #breaks <- pretty(-baseline:max(pretty(deltamax))+baseline)
    
    palette <- input$choixPalette
    print(palette)
    col <- if (grepl("Set", palette)) {
      suppressWarnings(RColorBrewer::brewer.pal(9, name = palette))
    } else {
      c("#000000", "#a8a8a8")
    }
    
    l = nrow(t)/2
    graph <- ggplot(t, aes(variable, valueICER-baseline, fill=Level)) +  geom_bar(position="identity", stat="identity", width=0.7) + coord_flip() + scale_y_continuous(breaks=breaks-baseline, labels=formatE(breaks, big.mark, decimal.mark), expand=c(0.2, 0.2))
    if (palette != "N&B"){
      graph <- graph + scale_fill_brewer(palette = palette)
    } else {
      graph <- graph + scale_fill_grey()
    }
    graph <- graph + ylab(xlabel) + xlab("") + guides(fill=FALSE)
    valeurs[['graph']] <-  graph +  geom_text(aes(label=valuevar2),vjust=0.5, hjust = c(rep(1.2, l),rep(-0.2, l)), colour=c(rep(col[2], l),rep(col[1], l)), size=4) 
    valeurs[['download']] <-  graph + theme_bw() + geom_text(aes(label=valuevar2),vjust=0.5, hjust = c(rep(1.2, l),rep(-0.2, l)), colour=c(rep(col[2], l), rep(col[1], l)), size=3) 
    valeurs[['tab']] <- "Graphique"
    print(valeurs$graph + theme_bw(base_size=16))
    #print(ggdraw(switch_axis_position(valeurs[['graph']] + theme_bw(base_size=16), axis="x")))
    #valeurs[['download']] <- ggdraw(switch_axis_position(valeurs[['download']] + theme_bw() + theme(text=element_text(size=10)), axis="x"))   
  })
  
  output$graph <- renderPlot({
    start()
  })
  output$downloadImg <- downloadHandler(
      filename = function() {
        paste0('img-', Sys.Date(), paste0('.', input$typegraph))
      },
      content = function(file) {
      if (input$typegraph %in% c("pdf", "eps"))
        ggsave(file, plot=valeurs[['download']], height = ifelse(input$nbparam<4, input$nbparam, 4), width=ifelse(input$nbparam<3, 6,8), dpi = input$nbDPI, encoding = "ISOLatin9.enc")
      else if (input$typegraph == "tiff")
        ggsave(file, plot=valeurs[['download']], height = ifelse(input$nbparam<4, input$nbparam, 4), width=ifelse(input$nbparam<3, 6,8), dpi = input$nbDPI, compression = "lzw")
      else
        ggsave(file, plot=valeurs[['download']], height = ifelse(input$nbparam<4, input$nbparam, 4), width=ifelse(input$nbparam<3, 6,8), dpi = input$nbDPI)
      }
    )
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.RData')
    },
    content = function(file) {
      save(input, file=file)
    }
  )
  output$erreurs <- renderUI({
    nbparam <- input$nbparam
    if (!is.na(nbparam) & !is.na(input$baseline)){
      r <- tagList(
      lapply(1:nbparam, function(i) {
        if (!is.null(input[[paste0("maxICER",i)]]) & !is.null(input[[paste0("minICER",i)]])){
          if (!is.na(input[[paste0("maxICER",i)]]) & !is.na(input[[paste0("minICER",i)]])){
            if (input$baseline > input[[paste0("maxICER",i)]] | input$baseline < input[[paste0("minICER",i)]])
              p(paste("L'intervalle de l'ICER de la variable", i, "ne comporte pas la baseline"))
          }
        }
      }
      ))
    }
  })
})

