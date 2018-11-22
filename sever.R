forestfires <- read.csv('data/forestfires.csv')
freqofday <- read.csv('data/area_day.csv')
chifreqofday <- read.csv('data/area_day1.csv')
freqofmonth <- read.csv('data/area_month.csv')
datacorrelation <- read.csv('data/forestfiresCR.csv')
titleread <- read.csv('data/titlehy.csv')
r = read.csv('data/rain_more0.csv')

m = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
d = c("mon","tue","wed","thu","fri","sat","sun")


freqofmonth$EnumMonth = sprintf("%02d",match(freqofmonth$month,m))
freqofday$EnumDay = sprintf("%02d",match(freqofday$day,d))
forestfires$EnumMonth = sprintf("%02d",match(forestfires$month,m))
forestfires$EnumDay = sprintf("%02d",match(forestfires$day,d))
chifreqofday$EnumDay = sprintf("%02d",match(chifreqofday$day,d))

function(input, output) {
  
  #some data manipulation to derive the values of KPI boxes
  # total.temp <- sum(forestfires$temp)
  top.month <- forestfires %>% group_by(month) %>% summarise(value = max(temp)) %>% filter(value==max(value))
  prof.day <- forestfires %>% group_by(day) %>% summarise(value = max(temp)) %>% filter(value==max(value))
  # tal.freq <- sum(freqofmonth$freq)
  top.months <- freqofmonth %>% group_by(month) %>% summarise(value = max(freq)) %>% filter(value == max(value))
  # total.dfreq <- sum(freqofday$freq)
  top.days <- freqofday %>% group_by(day) %>% summarise(value = max(freq)) %>% filter(value == max(value))
  # total.dwind <- sum(forestfires$wind)
  top.mwind <- forestfires %>% group_by(month) %>% summarise(value = max(wind)) %>% filter(value == max(value))
  top.dwind <- forestfires %>% group_by(day) %>% summarise(value = max(wind)) %>% filter(value == max(value))
  top.mISI <- forestfires %>% group_by(month) %>% summarise(value = max(ISI)) %>% filter(value == max(value))
  top.dISI <- forestfires %>% group_by(day) %>% summarise(value = max(ISI)) %>% filter(value == max(value))
  top.mRH <- forestfires %>% group_by(month) %>% summarise(value = max(RH)) %>% filter(value == max(value))
  top.dRH <- forestfires %>% group_by(day) %>% summarise(value = max(RH)) %>% filter(value == max(value))


  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(top.month$value, format="d", big.mark=',')
      ,paste('Temp max month:',top.month$month)
      ,icon = icon("calendar",lib = "font-awesome")
      ,color = "purple")
    
    
  })

  output$value2 <- renderValueBox({
    valueBox(
      formatC(prof.day$value, format="d", big.mark=',')
      ,paste('Temp max day:',prof.day$day)
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "yellow")
    
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(top.mwind$value, format="d", big.mark=',')
      ,paste('Wind max month:',top.mwind$month)
      ,icon = icon("calendar",lib = "font-awesome")
      ,color = "purple")
    
    
  })
  output$value4 <- renderValueBox({
    valueBox(
      formatC(top.dwind$value, format="d", big.mark=',')
      ,paste('Wind max day:',top.dwind$day)
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "yellow")
    
  })
  output$value5 <- renderValueBox({
    valueBox(
      formatC(top.mISI$value, format="d", big.mark=',')
      ,paste('ISI max month:',top.mISI$month)
      ,icon = icon("calendar",lib = "font-awesome")
      ,color = "purple")
    
    
  })
  output$value6 <- renderValueBox({
    valueBox(
      formatC(top.dISI$value, format="d", big.mark=',')
      ,paste('ISI max day:',top.dISI$day)
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "yellow")
    
  })
  output$value7 <- renderValueBox({
    valueBox(
      formatC(top.mRH$value, format="d", big.mark=',')
      ,paste('RH max month:',top.mRH$month)
      ,icon = icon("calendar",lib = "font-awesome")
      ,color = "purple")
    
    
  })
  output$value8 <- renderValueBox({
    valueBox(
      formatC(top.dRH$value, format="d", big.mark=',')
      ,paste('RH max day:',top.dRH$day)
      ,icon = icon("sun",lib = "font-awesome")
      ,color = "yellow")
  
  })
  
  output$value9 <- renderValueBox({
    valueBox(
      formatC(top.months$value, format="d", big.mark=',')
      ,paste('max fire freq month:',top.months$month)
      ,color = "navy"
      ,icon = icon("calendar",lib = "font-awesome"))
    
    
  })
  
  output$value10 <- renderValueBox({
    valueBox(
      formatC(top.days$value, format="d", big.mark=',')
      ,paste('max fire freq day:',top.days$day)
      ,color = "teal"
      ,icon = icon("sun", lib = "font-awesome"))
    
    
  })

  
  #creating the plotOutput Temp by month
  
  output$Tempbymonth <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumMonth),], 
           aes(x=EnumMonth, y=temp, fill=factor(month))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Temp of Month") + 
      xlab("Month") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Temp of Month") + labs(fill = "month")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:12)),
                       labels=paste(m))
    })
  
  #creating the plotOutput wind by day
  
  output$Windbyday <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumDay),], 
           aes(x=EnumDay, y=wind, fill=factor(day))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Wind of Day") + 
      xlab("day") + theme(legend.position="bottom" 
                          ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Wind of Day") + labs(fill = "day")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:7)),
                       labels=paste(d))
  })
  
  output$Windbymonth <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumMonth),], 
           aes(x=EnumMonth, y=wind, fill=factor(month))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Wind of Month") + 
      xlab("Month") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Wind of Month") + labs(fill = "month")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:12)),
                       labels=paste(m))
  })
  
  #creating the plotOutput Temp by day
  
  output$Tempbyday <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumDay),], 
           aes(x=EnumDay, y=temp, fill=factor(day))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Temp of Day") + 
      xlab("day") + theme(legend.position="bottom" 
                          ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Temp of Day") + labs(fill = "day")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:7)),
                       labels=paste(d))
  })
  
  output$ISImonth <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumMonth),], 
           aes(x=EnumMonth, y=ISI, fill=factor(month))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("ISI of Month") + 
      xlab("Month") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("ISI of Month") + labs(fill = "month")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:12)),
                       labels=paste(m))
  })
  
  output$ISIday <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumDay),], 
           aes(x=EnumDay, y=ISI, fill=factor(day))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("ISI of Day") + 
      xlab("day") + theme(legend.position="bottom" 
                          ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("ISI of Day") + labs(fill = "day")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:7)),
                       labels=paste(d))
  })
  
  #creating the plotOutput RH by month
  
  output$RHmonth <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumMonth),], 
           aes(x=EnumMonth, y=RH, fill=factor(month))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("RH of Month") + 
      xlab("Month") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("RH of Month") + labs(fill = "month")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:12)),
                       labels=paste(m))
  })
  
  #creating the plotOutput RH by day
  
  output$RHday <- renderPlot({
    ggplot(data = forestfires[order(forestfires$EnumDay),], 
           aes(x=EnumDay, y=RH, fill=factor(day))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("RH of Day") + 
      xlab("day") + theme(legend.position="bottom" 
                          ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("RH of Day") + labs(fill = "day")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:7)),
                       labels=paste(d))
  })
  #creating the plotOutput freq forestfires by month
  
  output$freqbymonth <- renderPlot({
    ggplot(data = freqofmonth[order(freqofmonth$EnumMonth),],
           aes(x=EnumMonth, y=freq,fill=factor(month)),colorspaces("#9aafc7")) +
      geom_bar(position = "dodge", stat = "identity") + ylab("frequency") +
      xlab("month") + theme(legend.position = "bottom",
                            plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Freq fire of Month") + labs(fill = "month")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:12)),
                       labels=paste(m))
  })
  
  output$freqbyday <- renderPlot({
    ggplot(data = freqofday,
           aes(x=EnumDay, y=freq,fill=factor(day))) +
      geom_bar(position = "dodge", stat = "identity") + ylab("frequency") +
      xlab("day") + theme(legend.position = "bottom",
                          plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Freq fire of day") + labs(fill = "day")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:7)),
                       labels=paste(d))
  })
  
  output$firebymonth <- renderPlot({
    ggplot(data = freqofmonth[order(freqofmonth$EnumMonth),],
           aes(x=EnumMonth, y=total,fill=factor(month))) +
      geom_bar(position = "dodge", stat = "identity") + ylab("Statistical Probability") +
      xlab("month") + theme(legend.position = "bottom",
                            plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Statistical Probability of fire each Month") + labs(fill = "month")+
      scale_x_discrete(breaks= paste(sprintf("%02d", 1:12)),
                       labels=paste(m))
  })
  output$hypothesisout <- renderPlot({
    t_dist = rt (100000,247)
    rain = r$rain
    xbar = mean(rain)
    s = sd(rain)
    mu0 = 0.1
    n = 8
    alpha = 0.5
    t0 = qt(.05,13)
    t = -(xbar-0)/(s/sqrt(n))
    
    ggplot(data.frame(x = t_dist)) + geom_density(aes(x=x),fill = 'pink')+
      geom_vline(xintercept = mean (t_dist) + t0,color ='blue' )+
      geom_vline(xintercept = mean (t_dist) + t,color ='red' )
  })
  
  output$n_var <- renderText({
    paste("N :", sum(freqofday$freq), sep=" ")
  }
  )
  output$df_var <- renderText(
    paste("DF :",length(freqofday$day)-1, sep=" ")
  )
  output$chisq_var <- renderText(
    {
      obs <- freqofday$freq
      
      SdObs = obs - mean(obs)
      VarObs = SdObs^2
      x = sum(VarObs)/mean(obs)
      
      paste("Chisq :", x, sep=" ")
    }
  )
  output$a_var <- renderText(
    paste("Alpha :",input$alpha/100, sep=" ")
  )
  
  output$v_var <- renderText({
    obs <- freqofday$freq
    
    SdObs = obs - mean(obs)
    VarObs = SdObs^2
    x = sum(VarObs)/mean(obs)
    t = x + mean (obs)
    paste("Value:", t, sep=" ")
  })
  
  output$oePlot <- renderPlot({
    x = chifreqofday[order(chifreqofday$EnumDay),]
    day <- x$day
    Yresult <- x$freq
    Xresult <- x$freq*0 + mean(x$freq)
    
    nyx <- data.frame(day, Yresult, Xresult)
    nyxlong <- melt(nyx, id=c("day"))
    
    # make the plot
    ggplot(nyxlong) +
      geom_bar(aes(x = day, y = value, fill = variable), 
               stat="identity", position = "dodge", width = 0.7) +
      scale_fill_manual("", values = c("tomato","turquoise"),
                        labels = c(" Observed", " Expected")) +
      labs(x="\nDay",y="Count\n") +
      theme_bw(base_size = 14) +
      ylim(0,50)
  })  
  output$chisqPlot <- renderPlot({
    
    obs <- chifreqofday$freq
    
    SdObs = obs - mean(obs)
    VarObs = SdObs^2
    x = sum(VarObs)/mean(obs)
    
    
    pos1 = qchisq(input$alpha/100 , 6)
    pos2 = qchisq(1 - input$alpha/100 , 6)
    t = mean (obs) 
    v = seq(-10,30,length.out = 1000) 
    
    
    d = data.frame(
      vx = v + t,
      vy = dchisq(v,6)
    )
    chisq_var = (sum(VarObs)/mean(obs))+ mean(obs)
    ggplot() +
      geom_vline(xintercept = pos1+t, color = "red") +
      geom_vline(xintercept = pos2+t, color = "blue") +
      geom_line(data = d, aes(x = vx, y = vy)) + 
      geom_area(data = d, aes(x = ifelse(vx > pos1+t & vx < pos2+t, vx, pos1+t),y = vy), alpha = .5, fill = "green") + 
      geom_area(data = d, aes(x = ifelse(vx < pos1+t, vx, pos1+t),y = vy), alpha = .5, fill = "red") +
      geom_area(data = d, aes(x = ifelse(vx > pos2+t, vx, pos2+t),y = vy), alpha = .5, fill = "red") +
      geom_text(aes(label = paste(sprintf("%2.2f",pos1+t)), x = pos1+t + 1, y = 0.15, color = "lower bound" )) + 
      geom_text(aes(label = paste(sprintf("%2.2f",pos2+t)), x = pos2+t + 1, y = 0.14, color = "upper bound")) + 
      ylim(0,0.15) + xlim(37,60) + xlab("Value") + ylab("Prob")+
      geom_point(aes(chisq_var,0),colours="red", size=3)+
      geom_text(aes(label = paste(sprintf("%2.2f",chisq_var)), x = chisq_var, y = 0.01))
    
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- forestfires
    if (input$mon != "All") {
      data <- data[data$month == input$mon,]
    }
    if (input$day != "All") {
      data <- data[data$day == input$day,]
    }
    data
  }))
  selectedDataCR <- reactive({
    datacorrelation[ c(input$xcorre, input$ycorre)]
  })
  vxcor <- reactive({
    datacorrelation[ c(input$xcorre)]
  })
  vycor <- reactive({
    datacorrelation[ c(input$ycorre)]
  })
  
  output$valuecor <- renderValueBox({
    valueBox(
      formatC("", format="d", big.mark=',')
      ,paste('Value of correlation between X and Y :',cor(vxcor(),vycor()))
      ,color = "yellow")
  })
  output$plotcorrelation <- renderPlot({
    plot(selectedDataCR(),
         col="blue", pch = 20, cex = 1)
  })
  
  
}


