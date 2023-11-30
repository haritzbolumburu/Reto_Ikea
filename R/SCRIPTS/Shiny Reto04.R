# Seleccion de datos ------------------------------------------------------


conexionIKEA<-dbConnect(odbc(),"RETO 4")
#Mostrar tablas BD
dbListTables(conexionIKEA)

ikea<-dbGetQuery(conexionIKEA,"select * from muebles;" )

ikea[ikea$product_category_1_name == ikea[1, 'product_category_1_name'], 'product_category_1_name'] <- 'Childrens'
ikea[ikea$product_category_2_name == ikea[99, 'product_category_2_name'], 'product_category_2_name'] <- 'Childrens storage'
ikea[ikea$product_category_3_name == ikea[99, 'product_category_3_name'], 'product_category_3_name'] <- 'Childrens small storage'
ikea[ikea$product_category_3_name == ikea[133, 'product_category_3_name'], 'product_category_3_name'] <- 'Childrens storage furniture'

no_socio <- filter(ikea, membership_id == "")
socio <- filter(ikea, !membership_id == "")


# Carga de datos especificos para el calendario ---------------------------

consulta_calendario<-dbGetQuery(conexionIKEA,"select count(transaction_id) Transaccion, date(transaction_timestamp) Fecha from muebles
group by day(transaction_timestamp)
order by date(transaction_timestamp);" )

consulta_calendario
class(consulta_calendario)
class(consulta_calendario$Transaccion)
class(consulta_calendario$Fecha)

Transacciones <- c(29482,NA,16957,16068,16659,15447,18882,38019,NA,27154,18125,16597,14593,18327,37054,NA,17894,15618,15751,25704,17547,31490,NA,24061,18639,15544,16602,16520,33543,NA)
#no es posible cambiar de integer64 a integer por lo que se copian los numeros en un vector nuevo

colores_calor <- c("#fffae5", "#fff6cc", "#fff2b2", "#ffee99", "#ffe97f", "#ffe566", "#ffe14c", "#ffdd32", "#ffd819", "#ffd400", "#ffea00", "#ffdd00", "#ffd000", "#ffc300", "#ffb700", "#ffaa00", "#ffa200", "#ff9500", "#ff8800", "#ff7b00", "#ff5400", "#ff4800", "#dc2f02", "#d00000", "#9d0208")

Transacciones <- as.character(Transacciones)

# Limpieza de los datos ---------------------------------------------------


ikea$store <- as.factor(ikea$store)
ikea$product_category_1_name <- as.factor(ikea$product_category_1_name)
ikea$product_category_2_name <- as.factor(ikea$product_category_2_name)
ikea$product_category_3_name <- as.factor(ikea$product_category_3_name)


#no_socio<-read.csv("no_socios.csv",sep=",")
#socio<-read.csv("socios.csv",sep=",")
#ikea<-read.csv("ikea.csv",sep=",")

levels(ikea$product_category_1_name)
dia<-format(as.Date(ikea$transaction_timestamp,"%Y-%m-%d"))
ikea2<-cbind(ikea,dia)
paleta<-c("#fff23e", "royalblue3")
tipo_cliente<-ifelse(ikea$membership_id == "","No socio","Socio")

ikea2<-cbind(ikea2,tipo_cliente)
no_socio$dia<-format(as.Date(no_socio$transaction_timestamp,"%Y-%m-%d"))
socio$dia<-format(as.Date(socio$transaction_timestamp,"%Y-%m-%d"))

categorias <- select(ikea, c('product_category_1_name', 'product_category_2_name', 'product_category_3_name'))
#categorias<-as.data.frame(cbind(ikea$product_category_1_name,ikea$product_category_2_name,ikea$product_category_3_name),stringsAsFactors = T)
colnames(categorias)<-c("Categoria_1","Categoria_2","Categoria_3")
ikea2$dia<-as.Date(ikea2$dia)

no_socio$dia<-as.Date(no_socio$dia)
ikea2$tipo_cliente<-as.factor(ikea2$tipo_cliente)
ikea2$store<-as.factor(ikea2$store)
ikea2$product_category_1_name<-as.factor(ikea2$product_category_1_name)
ikea2$product_category_2_name<-as.factor(ikea2$product_category_2_name)
ikea2$product_category_3_name<-as.factor(ikea2$product_category_3_name)



# ui y server -------------------------------------------------------------

ui<-fluidPage(
  theme = bs_theme(version = version_default(), bootswatch = "yeti"),
  navbarPage(
    titlePanel(title=div(img(src="https://1000marcas.net/wp-content/uploads/2019/12/IKEA-Logo-1.png", height="15%", width= "15%", "VERDE OSCURO"))),
    tabPanel(
      "Calendario",
      plotOutput(outputId = "calendario")
    ),
    tabPanel(
      "Distribuciones",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("fecha",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
          pickerInput("tienda",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),
          pickerInput("tipo_cliente","Seleccione el tipo de cliente",choices=levels(ikea2$tipo_cliente), selected=levels(ikea2$tipo_cliente), multiple = T,options=list("actions-box"=TRUE)),
          submitButton("Cargar datos", icon("cloud"))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Socios/No Socios",plotlyOutput("pie")),
            tabPanel("Tienda elegida",plotlyOutput("pie2")),
          )
        )
      )
    ),
    navbarMenu(
      "Categorias",
      tabPanel(
        "Categoria 1",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("fechacat1",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
            pickerInput("tiendacat1",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),
            pickerInput("categoria_1","Seleccione el nombre de la categoria_1:",choices=levels(ikea2$product_category_1_name),selected =levels(ikea2$product_category_1_name) ,multiple = T,options=list("actions-box"=TRUE)),
            submitButton("Cargar datos", icon("cloud"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Cantidad vendida", plotlyOutput("barras")),
              tabPanel("Gasto", plotlyOutput("grafico"))
            )
          )
        )
      ),
      tabPanel(
        "Categoria 2",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("fechacat2",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
            pickerInput("tiendacat2",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),
            pickerInput("categoria_2","Seleccione el nombre de la categoria_2:",choices=levels(ikea2$product_category_2_name),selected =levels(ikea2$product_category_2_name) ,multiple = T,options=list("actions-box"=TRUE)),
            submitButton("Cargar datos", icon("cloud"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Cantidad vendida", plotlyOutput("barras2")),
              tabPanel("Gasto", plotlyOutput("grafico2"))
            )
          )
        )
      ),
      tabPanel(
        "Categoria 3",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("fechacat3",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
            pickerInput("tiendacat3",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),
            pickerInput("categoria_3","Seleccione el nombre de la categoria_3:",choices=levels(ikea2$product_category_3_name),selected =levels(ikea2$product_category_3_name) ,multiple = T,options=list("actions-box"=TRUE)),
            submitButton("Cargar datos", icon("cloud"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Cantidad vendida", plotlyOutput("barras3")),
              tabPanel("Gasto", plotlyOutput("grafico3"))
            )
          )
        )
      )
    ),
    navbarMenu(
      "Evolucion",
      tabPanel(
        "Categoria 1",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("fechaevo1",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
            pickerInput("tiendaevo1",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),
            pickerInput("categoria_11","Seleccione el nombre de la categoria_1:",choices=levels(ikea2$product_category_1_name),selected =levels(ikea2$product_category_1_name) ,multiple = T,options=list("actions-box"=TRUE)),
            submitButton("Cargar datos", icon("cloud"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Cantidad vendida", plotOutput("lineacant")),
              tabPanel("Ventas", plotOutput("lineadin"))
            )
          )
        )
      ),
      tabPanel(
        "Categoria 2",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("fechaevo2",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
            pickerInput("tiendaevo2",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),
            pickerInput("categoria_22","Seleccione el nombre de la categoria_2:",choices=levels(ikea2$product_category_2_name),selected =levels(ikea2$product_category_2_name) ,multiple = T,options=list("actions-box"=TRUE)),
            submitButton("Cargar datos", icon("cloud"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Cantidad vendida", plotOutput("lineacant2")),
              tabPanel("Ventas", plotOutput("lineadin2"))
            )
          )
        )
      ),
      tabPanel(
        "Categoria 3",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("fechaevo3",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
            pickerInput("tiendaevo3",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),
            pickerInput("categoria_33","Seleccione el nombre de la categoria_3:",choices=levels(ikea2$product_category_3_name),selected =levels(ikea2$product_category_3_name) ,multiple = T,options=list("actions-box"=TRUE)),
            submitButton("Cargar datos", icon("cloud"))
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Cantidad vendida", plotOutput("lineacant3")),
              tabPanel("Ventas", plotOutput("lineadin3"))
            )
          )
        )
      )
    ),
    tabPanel(
      "Productos mas vendidos",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("fechaboxplot",label="Seleciona el rango de fechas",start="2019-06-01",end="2019-06-30",max="2019-06-30",format = "yyyy-mm-dd"),
          pickerInput("tiendaboxplot",label="Seleccione una tienda",choices=levels(ikea2$store),selected=levels(ikea2$store), multiple = T,options=list("actions-box"=TRUE)),         
          submitButton("Cargar datos", icon("cloud"))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Categoria 1",plotlyOutput("boxplot1")),
            tabPanel("Categoria 2",plotlyOutput("boxplot2")),
            tabPanel("Categoria 3",plotlyOutput("boxplot3"))
          )
        )
      )
    )
  )
)



server<-function(input,output){
  tabla<-reactive({
    filter(ikea2,
           store %in% input$tienda&
             dia>=input$fecha[1] & dia<=input$fecha[2])
  })
  tabla2<-reactive({
    filter(ikea2,
           tipo_cliente %in% input$tipo_cliente&
             dia>=input$fecha[1] & dia<=input$fecha[2])
  })
  tabla3<-reactive({
    filter(ikea2,
           store==input$tiendacat1&
             dia>=input$fechacat1[1] & dia<=input$fechacat1[2]&
             product_category_1_name==input$categoria_1)
  })
  tabla4<-reactive({
    filter(ikea2,
           store==input$tiendacat2&
             dia>=input$fechacat2[1] & dia<=input$fechacat2[2]&
             product_category_2_name==input$categoria_2)
  })
  tabla5<-reactive({
    filter(ikea2,
           store==input$tiendacat3&
             dia>=input$fechacat3[1] & dia<=input$fechacat3[2]&
             product_category_3_name==input$categoria_3)
  })
  tabla6<-reactive({
    filter(ikea2,
           store==input$tiendaevo1&
             dia>=input$fechaevo1[1] & dia<=input$fechaevo1[2]&
             product_category_1_name==input$categoria_11)
  })
  tabla7<-reactive({
    filter(ikea2,
           store==input$tiendaevo2&
             dia>=input$fechaevo2[1] & dia<=input$fechaevo2[2]&
             product_category_2_name==input$categoria_22)
  })
  tabla8<-reactive({
    filter(ikea2,
           store==input$tiendaevo3&
             dia>=input$fechaevo3[1] & dia<=input$fechaevo3[2]&
             product_category_3_name==input$categoria_33)
  })
  tabla9<-reactive({
    filter(ikea2,
            store==input$tiendaboxplot&
             dia>=input$fechaboxplot[1] & dia<=input$fechaboxplot[2])
  })
  
  
  output$barras<-renderPlotly({
    plot_ly(tabla3(),y=~product_category_1_name,x=~qty,color=~tipo_cliente,colors=c("#27408B","#FFFF00"),alpha=0.75,type="bar")%>%
      layout(barmode="stack",title="Cantidad vendida de cada Categoria 1",
             xaxis=list(
               title="Cantidad vendida"
             ),yaxis=list(title="Categoria 1"))
    
  })
  output$barras2<-renderPlotly({
    plot_ly(tabla4(),y=~product_category_2_name,x=~qty,color=~tipo_cliente,colors=c("#27408B","#FFFF00"),alpha=0.75,type="bar")%>%
      layout(barmode="stack",title="Cantidad vendida de cada Categoria 2",
             xaxis=list(
               title="Cantidad vendida"
             ),yaxis=list(title="Categoria 2"))
  })
  
  output$barras3<-renderPlotly({
    plot_ly(tabla5(),x=~product_category_3_name,y=~qty,color=~tipo_cliente,colors=c("#27408B","#FFFF00"),alpha=0.75,type="bar")%>%
      layout(barmode="stack",title="Cantidad vendida de cada Categoria 3",
             yaxis=list(
               title="Cantidad vendida"
             ),
             xaxis=list(title="Categoria 3"))
  })
  output$grafico<-renderPlotly({
    plot_ly(tabla3(),color=~tipo_cliente,y=~sales,x=~product_category_1_name,type="bar",colors=c("#27408B","#FFFF00"))%>%
      layout(
        title="Gasto por cliente de cada Categoria 1",
        xaxis=list(
          title="Categoria 1"
        ),yaxis=list(
          title="Gasto"
        )
      )
  })
  output$grafico2<-renderPlotly({
    plot_ly(tabla4(),color=~tipo_cliente,y=~sales,x=~product_category_2_name,type="bar",colors=c("#27408B","#FFFF00"))%>%
      layout(
        title="Gasto por cliente de cada Categoria 2",
        xaxis=list(
          title="Categoria 2"
        ),yaxis=list(
          title="Gasto"
        )
      )
  })
  output$grafico3<-renderPlotly({
    plot_ly(tabla5(),color=~tipo_cliente,y=~sales,x=~product_category_3_name,type="bar",colors=c("#27408B","#FFFF00"))%>%
      layout(
        title="Gasto por cliente de cada Categoria 3",
        xaxis=list(
          title="Categoria 3"
        ),yaxis=list(
          title="Gasto"
        )
      )
  })
  output$pie<-renderPlotly({
    plot_ly(tabla(),labels=~tipo_cliente, type="pie",hole=0.5,marker=list(colors=c("#FFFF00", "#27408B")) )%>%
      layout(
        title="Porcentaje de clientes socio o no socios"
      )
  })
  output$pie2<-renderPlotly({
    plot_ly(tabla2(),labels=~store, type="pie",hole=0.5,marker=list(colors=c("#FFFF00", "#27408B")) )%>%
      layout(
        title="Porcentaje de la tienda a la que van los clientes"
      )
  })
  output$calendario<-renderPlot({
    calendR(year=2019,
            month = 6,
            start = "M",
            title.size = 40,
            special.days = Transacciones,
            gradient = TRUE,
            low.col = "white",
            special.col = colores_calor,
            lunar = T,
            lunar.col = "gray30",
            lunar.size = 3,
            text = Transacciones,
            text.pos = 1:30,
            text.size = 10,
            text.col = "blue",
            font.family = "mono",
            bg.col = "white",
            day.size = 5)
  }, height = 500)
  
  output$lineacant <- renderPlot({
    evolucion1.1 <- tabla6() %>%
      group_by(store, dia) %>%
      summarise(suma_cantidad=sum(qty))
    
    ggplot(evolucion1.1, aes(x=day(dia), y=suma_cantidad, group = store, colour = store)) +
      geom_line(size = 1) +
      geom_point(size=3, shape = 21, fill="white") +
      theme_minimal() +
      scale_x_continuous(
        breaks = seq(1, 29, by = 1)
      ) +
      theme(
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Dia del mes") +
      ylab("Cantidad de articulos") +
      ggtitle('Cantidad de articulos comprados por dia') +
      theme(plot.title = element_text(size = 20)) +
      labs(
        group = "Tienda",
        colour = "Tienda"
      ) +
      scale_color_manual(values = c("#FFFF00", "#27408B"))
    
  })
  
  output$lineacant2 <- renderPlot({
    evolucion2.1 <- tabla7() %>%
      group_by(store, dia) %>%
      summarise(suma_cantidad=sum(qty))
    
    ggplot(evolucion2.1, aes(x=day(dia), y=suma_cantidad, group = store, colour = store)) +
      geom_line(size = 1) +
      geom_point(size=3, shape = 21, fill="white") +
      theme_minimal() +
      scale_x_continuous(
        breaks = seq(1, 29, by = 1)
      ) +
      theme(
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Dia del mes") +
      ylab("Cantidad de articulos") +
      ggtitle('Cantidad de articulos comprados por dia') +
      theme(plot.title = element_text(size = 20)) +
      labs(
        group = "Tienda",
        colour = "Tienda"
      ) +
      scale_color_manual(values = c("#FFFF00", "#27408B"))
    
  })
  
  output$lineacant3 <- renderPlot({
    evolucion3.1 <- tabla8() %>%
      group_by(store, dia) %>%
      summarise(suma_cantidad=sum(qty))
    
    ggplot(evolucion3.1, aes(x=day(dia), y=suma_cantidad, group = store, colour = store)) +
      geom_line(size = 1) +
      geom_point(size=3, shape = 21, fill="white") +
      theme_minimal() +
      scale_x_continuous(
        breaks = seq(1, 29, by = 1)
      ) +
      theme(
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Dia del mes") +
      ylab("Cantidad de articulos") +
      ggtitle('Cantidad de articulos comprados por dia') +
      theme(plot.title = element_text(size = 20)) +
      labs(
        group = "Tienda",
        colour = "Tienda"
      ) +
      scale_color_manual(values = c("#FFFF00", "#27408B"))
    
  })
  
  output$lineadin <- renderPlot({
    evolucion1.2 <- tabla6() %>%
      group_by(store, dia) %>%
      summarise(suma_ventas=sum(sales))
    
    ggplot(evolucion1.2, aes(x=day(dia), y=suma_ventas, group = store, colour = store)) +
      geom_line(size = 1) +
      geom_point(size=3, shape = 21, fill="white") +
      theme_minimal() +
      scale_x_continuous(
        breaks = seq(1, 29, by = 1)
      ) +
      theme(
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Dia del mes") +
      ylab("Ventas") +
      ggtitle('Ventas por dia') +
      theme(plot.title = element_text(size = 20)) +
      labs(
        group = "Tienda",
        colour = "Tienda"
      ) +
      scale_color_manual(values = c("#FFFF00", "#27408B"))
    
  })
  
  output$lineadin2 <- renderPlot({
    evolucion2.2 <- tabla7() %>%
      group_by(store, dia) %>%
      summarise(suma_ventas=sum(sales))
    
    ggplot(evolucion2.2, aes(x=day(dia), y=suma_ventas, group = store, colour = store)) +
      geom_line(size = 1) +
      geom_point(size=3, shape = 21, fill="white") +
      theme_minimal() +
      scale_x_continuous(
        breaks = seq(1, 29, by = 1)
      ) +
      theme(
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Dia del mes") +
      ylab("Ventas") +
      ggtitle('Ventas por dia') +
      theme(plot.title = element_text(size = 20)) +
      labs(
        group = "Tienda",
        colour = "Tienda"
      ) +
      scale_color_manual(values = c("#FFFF00", "#27408B"))
    
  })
  
  output$lineadin3 <- renderPlot({
    evolucion3.2 <- tabla8() %>%
      group_by(store, dia) %>%
      summarise(suma_ventas=sum(sales))
    
    ggplot(evolucion3.2, aes(x=day(dia), y=suma_ventas, group = store, colour = store)) +
      geom_line(size = 1) +
      geom_point(size=3, shape = 21, fill="white") +
      theme_minimal() +
      scale_x_continuous(
        breaks = seq(1, 29, by = 1)
      ) +
      theme(
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Dia del mes") +
      ylab("Ventas") +
      ggtitle('Ventas por dia') +
      theme(plot.title = element_text(size = 20)) +
      labs(
        group = "Tienda",
        colour = "Tienda"
      ) +
      scale_color_manual(values = c("#FFFF00", "#27408B"))
     
  })
  
  #BOXPLOTS CON CATEGORIAS POR ENCIMA DEL PERCENTIL 80
  
  output$boxplot1 <- renderPlotly({
    boxplot1.1 <- tabla9() %>%
      group_by(product_category_1_name) %>%
      summarise(sales = mean(sales)) %>%
      arrange(desc(sales))
    
    boxplot2.1 <- boxplot1.1[boxplot1.1$sales > quantile(boxplot1.1$sales, 0.8),]
    
    boxplot3.1 <- tabla9()[tabla9()$product_category_1_name == boxplot2.1$product_category_1_name,]
    
    ggplot(boxplot3.1, aes(x = product_category_1_name, y = sales, color = product_category_1_name)) +
      geom_boxplot() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Categorias de tipo 1") +
      ylab("Ventas") +
      ggtitle("Categorias mas vendidas") +
      theme(plot.title = element_text(size = 20))
  })
  
  #BOXPLOTS CON CATEGORIAS POR ENCIMA DEL PERCENTIL 90
  
  output$boxplot2 <- renderPlotly({
    boxplot1.2 <- tabla9() %>%
      group_by(product_category_2_name) %>%
      summarise(sales = mean(sales)) %>%
      arrange(desc(sales))
    
    boxplot2.2 <- boxplot1.2[boxplot1.2$sales > quantile(boxplot1.2$sales, 0.9),]
    
    boxplot3.2 <- tabla9()[tabla9()$product_category_2_name == boxplot2.2$product_category_2_name,]
    
    ggplot(boxplot3.2, aes(x = product_category_2_name, y = sales, color = product_category_2_name)) +
      geom_boxplot() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Categorias de tipo 2") +
      ylab("Ventas") +
      ggtitle("Categorias mas vendidas") +
      theme(plot.title = element_text(size = 20))
  })
  
  #BOXPLOTS CON CATEGORIAS POR ENCIMA DEL PERCENTIL 95
  
  output$boxplot3 <- renderPlotly({
    boxplot1.3 <- tabla9() %>%
      group_by(product_category_3_name) %>%
      summarise(sales = mean(sales)) %>%
      arrange(desc(sales))
    
    boxplot2.3 <- boxplot1.3[boxplot1.3$sales > quantile(boxplot1.3$sales, 0.95),]
    
    boxplot3.3 <- tabla9()[tabla9()$product_category_3_name == boxplot2.3$product_category_3_name,]
    
    ggplot(boxplot3.3, aes(x = product_category_3_name, y = sales, color = product_category_3_name)) +
      geom_boxplot() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45),
        panel.grid.minor.x = element_blank()
      ) +
      xlab("Categorias de tipo 3") +
      ylab("Ventas") +
      ggtitle("Categorias mas vendidas") +
      theme(plot.title = element_text(size = 20))
  })
}

shinyApp(ui=ui,server=server)
