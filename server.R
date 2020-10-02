library(shiny)
library(RSQLite)
library(stringr)
library('SPEI')
library(ggplot2)
library('zoo')
library(Kendall)
library(DT)
library(trend)
library("ggpubr")



shinyServer(function(input, output) {
  conn <- dbConnect(RSQLite::SQLite(), './Data/test.db')
  v<- function(freq) {
    ist <-  input$Vector
    
    query = str_interp("SELECT t1.YEAR,t1.MONTH,t1.PRCP,t3.TMAX,t4.TMIN ,t2.TMED from(SELECT d.YIL as YEAR  ,d.AY as MONTH  ,d.value as  PRCP from data d where d.Istasyon_No = ${ist} and d.var LIKE '%YAGIS%') t1
    JOIN (SELECT d.YIL as YEAR,d.AY as MONTH,d.value as  TMED from data d where d.Istasyon_No = ${ist} and d.var = 'ORTALAMA_SICAKLIK_°C') t2
    ON t1.YEAR = t2.YEAR and t1.MONTH = t2.MONTH JOIN (SELECT d.YIL as YEAR  ,d.AY as MONTH  ,d.value as  TMAX from data d where d.Istasyon_No = ${ist} and d.var = 'MAKSIMUM_SICAKLIK_C' ) t3
    ON t1.YEAR = t3.YEAR and t1.MONTH = t3.MONTH JOIN (SELECT d.YIL as YEAR  ,d.AY as MONTH  ,d.value as  TMIN from data d where d.Istasyon_No = ${ist} and d.var = 'MINIMUM_SICAKLIK_C') t4
    ON t1.YEAR = t4.YEAR and t1.MONTH = t4.MONTH")
    conn <- dbConnect(RSQLite::SQLite(), './Data/test.db')
    data = dbGetQuery(conn, query)
    
    query = str_interp("SELECT DISTINCT d.Enlem from data d where d.Istasyon_No = ${ist};")
    lat = dbGetQuery(conn, query)
    
    
    if (input$PET == 'Hargreaves'){
      data$PET <- hargreaves(Tmin=data$TMIN, Tmax=data$TMAX, lat = as.numeric(lat))
    } else {
      data$PET <- thornthwaite(Tave=data$TMED, lat = as.numeric(lat))
    }
    
    # print(input$PET)
    
    
    
    data$BAL <- data$PRCP-data$PET
    
    datats <- ts(data[,-c(1,2)], end=c(2020,8), frequency=12)
    
    if(input$Index == 'SPEI'){
      spei1 <- spei(datats[,'BAL'], freq,distribution = input$Dist)
    } else {
      spei1 <- spi(datats[,'PRCP'], freq,distribution = input$Dist)
    }
    
    data$Date <- as.yearmon(paste(data$YEAR, data$MONTH), "%Y %m")
    
    query = str_interp("SELECT t1.*,t2.Total_Precipitation FROM 
(SELECT d.YIL ,AVG(value) as Mean_Temperature from data d where d.Istasyon_No = ${ist} and d.var = 'ORTALAMA_SICAKLIK_°C' group by d.Istasyon_No,d.YIL) t1
JOIN
(SELECT d.YIL ,SUM(value) as Total_Precipitation from data d where d.Istasyon_No = ${ist} and d.var LIKE '%YAGIS%' group by d.Istasyon_No,d.YIL) t2
ON t1.YIL = t2.YIL")
    datay = dbGetQuery(conn, query)
    
    results <- list()
    results$spei1 <- spei1
    results$data <- data
    results$datay <- datay
    
    return(results)  
  }
  
  output$main_plot <- 
    renderPlot({
      
      # spei1 <- v(input$Freq)$spei1
      # res <- data.frame(as.matrix(spei1$fitted), date=time(spei1$fitted))
      # res$year <- trunc(res$date)
      # res$month <- (res$date - res$year) * 12 + 1
      # res$Date <- as.yearmon(paste(res$year, res$month), "%Y %m")
      # 
      # res <- res %>% 
      #   mutate(mycolor = ifelse(Series.1>0, "type1", "type2"))
      # 
      
      
      plot(v(input$Freq)$spei1,main=str_interp('Station ${input$Vector}, ${input$Index}'),xlab="Year")
    })
  
  
  output$pet_plot <- 
    renderPlotly({
      
      data = v(input$Freq)$data
      f <- ggplot(data)  + 
        geom_bar(aes(x=Date, y=PET),stat="identity", fill="tan1", alpha=0.5,colour="#00AFBB")+
        scale_y_continuous("Potential evapotranspiration [mm]")
      
      fig <- ggplotly(f)
      fig
    }
    
    )
  
  output$data_plot <- 
    renderPlotly({
      
      data = v(input$Freq)$data
      
      tr <- MannKendall(data$TMED)
      tau <- formatC(tr$tau[1], digits = 5, format = "f")
      pside <- formatC(tr$sl[1], digits = 5, format = "f")
      
      
      st <- "There is no trend present in the data."
      if (pside < 0.05){
        st <- str_interp("There is no trend present in the data \
                         tau = ${tau} pvalue = ${pside}")
      }
      else{
        if (tau > 0)
        {
          st <- str_interp("There is positive trend \
                         tau = ${tau} pvalue = ${pside}")
        }
        else
        {
          st <- str_interp("There is negative trend \
                         tau = ${tau} pvalue = ${pside}")
        }
      }
      
      tr_p <- MannKendall(data$PRCP)
      tau_p <- formatC(tr_p$tau[1], digits = 5, format = "f")
      pside_p <- formatC(tr_p$sl[1], digits = 5, format = "f")
      
      
      if (pside_p < 0.05){
        st_p <- str_interp("There is no trend present in the data \
                         tau = ${tau_p} pvalue = ${pside_p}")
      }
      else{
        if (tau_p > 0)
        {
          st_p <- str_interp("There is positive trend \
                         tau = ${tau_p} pvalue = ${pside_p}")
        }
        else
        {
          st_p <- str_interp("There is negative trend \
                         tau = ${tau_p} pvalue = ${pside_p}")
        }
      }
      
      # print(st)
      
      q1 <- ggplot(data) +
        geom_line(aes(Date, TMED), group = 1,color="red", size=1) +
        geom_ribbon(aes(x = Date, ymax = TMAX, ymin = TMIN), alpha = 0.6, fill = "skyblue")+
        scale_y_continuous("Mean Temperature [°C]") + 
        annotate("text",  x=Inf, y = Inf, label = st, vjust=1, hjust=1)
      
      
      q2 <- ggplot(data)  + 
        geom_bar(aes(x=Date, y=PRCP),stat="identity", fill="tan1", colour="sienna3")+
        scale_y_continuous("Precipitation [mm]") + 
        annotate("text",  x=Inf, y = Inf, label = st_p, vjust=1, hjust=1)
      
      # gridExtra::grid.arrange(q1, q2, ncol = 1)
      ply1 <- ggplotly(q1)
      ply2 <- ggplotly(q2)
      
      subplot(ply1, ply2,nrows = 2)
      
      # subplot(q1, q2, nrows = 2, margin = 0.04, heights = c(0.6, 0.4))
      
      
      #plot(v(input$Freq),main=str_interp('Station ${input$Vector}, ${input$Index}'),xlab="Year")
    })
  
  output$summary <- renderPrint({
    summary(v(input$Freq)$spei1)
  })
  
  output$ysummary <- DT::renderDataTable({
    
    # v(input$Freq)$datay
    v(input$Freq)$datay
    
  })
  
  output$mymap <- renderLeaflet({
    
    topoData <-  rgdal::readOGR("./Data/st.geojson")
    
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 35, lat = 38, zoom = 6) %>% 
      addMarkers(data = topoData, label = paste0(topoData$Istasyon_No),labelOptions = labelOptions(noHide = T))
  })
  
  output$mary <- renderPlot({
    df = v(input$Freq)
    data = df$data
    dataspi = df$spei1
    dataspi <- data.frame(as.matrix(dataspi$fitted))
    
    p1 <- ggdensity(data, x = "PRCP", fill = "red") +
      scale_x_continuous(limits = c(-1, 50)) +
      stat_overlay_normal_density(color = "red", linetype = "dashed")
    
    p2 <- ggqqplot(data, x = "PRCP", fill = "red")
    
    p3 <- ggdensity(data, x = "TMED", fill = "red") +
      scale_x_continuous(limits = c(-1, 50)) +
      stat_overlay_normal_density(color = "red", linetype = "dashed")
    
    p4 <- ggqqplot(data, x = "TMED", fill = "red")
    
    p5 <- ggdensity(dataspi, x = "Series.1", fill = "red") +
      scale_x_continuous(limits = c(-1, 50)) +
      stat_overlay_normal_density(color = "red", linetype = "dashed")
    
    p6 <- ggqqplot(dataspi, x = "Series.1", fill = "red")
    
    p7 <- ggarrange(p1, p2, p3,p4,p5,p6,
                    labels = c("A", "B","C","D","E","F"),
                    ncol = 2, nrow = 3)
    
    p7
  })
  
  output$at <- renderText({
    df <- v(input$Freq)
    data <- df$data
    
    
    p <- shapiro.test(data$PRCP)
    W <- as.numeric(p[1][1])
    P <- as.numeric(p[2][1])
    
    p <- shapiro.test(data$TMED)
    Wt <- as.numeric(p[1][1])
    Pt <- as.numeric(p[2][1])
    
    sp <- sens.slope(data$PRCP,conf.level = 0.95)
    sp_t <- str_interp("Sen's slope = ${as.numeric(sp$estimates)} for precipitation with %95 confidence interval")
    
    st <- sens.slope(data$TMED,conf.level = 0.95)
    st_t <- str_interp("Sen's slope = ${as.numeric(st$estimates)} for temperature  %95 confidence interval")
    
    tr <- MannKendall(data$TMED)
    tau <- formatC(tr$tau[1], digits = 5, format = "f")
    pside <- formatC(tr$sl[1], digits = 5, format = "f")
    
    if (pside < 0.05){
      st_m <- str_interp("There is no trend present in temperature data based on Mann-Kendall test\
                         tau = ${tau} pvalue = ${pside}")
    }
    else{
      if (tau > 0)
      {
        st_m <- str_interp("There is positive trend in temperature data based on Mann-Kendall test\
                         tau = ${tau} pvalue = ${pside}")
      }
      else
      {
        st_m <- str_interp("There is negative trend in temperature data based on Mann-Kendall test\
                         tau = ${tau} pvalue = ${pside}")
      }
    }
    
    tr_p <- MannKendall(data$PRCP)
    tau_p <- formatC(tr_p$tau[1], digits = 5, format = "f")
    pside_p <- formatC(tr_p$sl[1], digits = 5, format = "f")
    
    
    if (pside_p < 0.05){
      st_p <- str_interp("There is no trend present in precipitation data based on Mann-Kendall test \
                         tau = ${tau_p} pvalue = ${pside_p}")
    }
    else{
      if (tau_p > 0)
      {
        st_p <- str_interp("There is positive trend in precipitation data based on Mann-Kendall test \
                         tau = ${tau_p} pvalue = ${pside_p}")
      }
      else
      {
        st_p <- str_interp("There is negative trend in precipitation data based on Mann-Kendall test\
                         tau = ${tau_p} pvalue = ${pside_p}")
      }
    }
    
    
    data <- df$spei1
    p <- shapiro.test(data$fitted)
    Ws <- as.numeric(p[1][1])
    Ps <- as.numeric(p[2][1])
    
    
    st1 <- str_interp("Shapiro-Wilk normality test for precipitation \
   W = ${W} pvalue = ${P} \ ")
    
    st2 <- str_interp("Shapiro-Wilk normality test for temperature \
   W = ${Wt} pvalue = ${Pt} \ ")
    
    st3 <- str_interp("Shapiro-Wilk normality test for spi \
    W = ${Ws} pvalue = ${Ps} \ ")
    
    addr = paste(st1,  st2, st3, sp_t,st_t,st_p,st_m, sep="\n")
    print(addr[1])
    
    
    
  })
  
  
  output$total <- renderText({
    at <- v(input$Freq)$datay
    m <- mean(at$Mean_Temperature)
    s <- mean(at$Total_Precipitation) 
    
    paste("Mean Temperature :", m ," C", " Mean Total Precipitation : " , s, " mm")
    
  })
  
  output$totalplot <- renderPlotly({
    df <- v(input$Freq)$datay
    # q1 <- ggplot(df,aes(x=YIL)) +
    #   geom_bar(aes(y= Total_Precipitation), fill="tan1", alpha=0.5,colour="#00AFBB",size=1,stat = "identity") +
    #   geom_line(aes(y= Mean_Temperature*25),color="red", size=1) +
    #   scale_fill_brewer(palette = "RdBu") +
    #   scale_y_continuous("Total Precipitation, mm",sec.axis = sec_axis(~./25, name="Mean Temperature [°C]"))
    # 
    # fig <- ggplotly(q1)
    # fig       
    fig <- plot_ly(df)
    fig <- fig %>% add_trace(x = ~YIL, y = ~Total_Precipitation, type = 'bar', name = 'Precipitation',
                             marker = list(color = '#C9EFF9'),
                             hoverinfo = "text",
                             text = ~paste(Total_Precipitation, ' mm'))
    fig <- fig %>% add_trace(x = ~YIL, y = ~Mean_Temperature, type = 'scatter', mode = 'lines', name = 'Temperature', yaxis = 'y2',
                             line = list(color = '#45171D'),
                             hoverinfo = "text",
                             text = ~paste(Mean_Temperature, '°C'))
    
    fig <- fig %>% layout(title = 'Yearly Total Precipitation and Mean Temperature ',
                          xaxis = list(title = ""),
                          yaxis = list(side = 'left', title = 'Total Precipitation , mm', showgrid = FALSE, zeroline = FALSE),
                          yaxis2 = list(side = 'right', overlaying = "y", title = 'Temperature in degrees C', showgrid = FALSE, zeroline = FALSE))
    
    fig
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Index, ".csv", sep = "")
    },
    content = function(file) {
      
      spei1 <- v(input$Freq)$spei1
      res <- data.frame(as.matrix(spei1$fitted), date=time(spei1$fitted))
      res$year <- trunc(res$date)
      res$month <- (res$date - res$year) * 12 + 1
      
      write.csv(res, file, row.names = FALSE)
    }
  )
  
})