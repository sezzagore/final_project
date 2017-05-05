
# I load the required libraries
require(ggplot2) 
require(tidyr)
require(dplyr)
require(readr) 
require(foreign) 
require(lubridate) 
require(zipcode)
require(noncensus) 
require(stringr) 
require(shiny)
require(lazyeval)
require(DT)
require(plotly)

load("shiny_data.Rdata") # this is my data as constructed in the document

shinyServer(function(input, output) {
 
  
# Building the map
# first step is to create a reactive data sets
data01 <- reactive({
  aux1 <- df[year(df$inc_date)>=input$range[1] & year(df$inc_date)<=input$range[2] & df$region == input$state ,]
  aux1 %>%
    group_by(county) %>%
    summarise(casualties=n(), pop =round(mean(population)), c_ht_p=round(casualties*100000/pop))
})  

data02 <- reactive({
  aux2 <- df[year(df$inc_date)>=input$range[1] & year(df$inc_date)<=input$range[2],]
  aux2$state_pop <- as.numeric(aux2$state_pop)
  aux2$region <- tolower(aux2$region)
  aux2 %>%
    group_by(region) %>%
    summarise(casualties=n(), pop = mean(state_pop), c_ht_p=round(casualties*100000/pop))
}) 
  
# materialfor map and function to ditch axes in the plot
states <- map_data("state")
county <- map_data("county") 
# both from ggplot 2 
# using stackover flow I found a way to remove the axes
ditch_the_axes <- theme(axis.text = element_blank(),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(),
                        panel.border = element_blank(),
                        panel.grid = element_blank(),
                        axis.title = element_blank())

# I make the plot
output$map01 <- renderPlot({
    
if(input$state == "All"){

mdata <-inner_join(states, data02(), by = "region")

base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) +
        geom_polygon(color = "black", fill = "grey")

mytitle <- paste("Total Fire Casualties in the US over the period", input$range[1],"-",input$range[2])

mapUS <- base + coord_fixed(1.3) +
  geom_polygon(data = mdata, aes(fill = c_ht_p), color = "white") +
  scale_fill_distiller(palette = "Spectral", name = "Casualties per 100 000") +
  geom_polygon(color = "white", fill = NA) +
  theme_bw() + # This gets rid of the grey grid 
  ditch_the_axes +
  ggtitle(mytitle)

  return(mapUS)
}
    
if(input$state != "All"){   
state_df <- states %>%
  filter(region == tolower(input$state)) 

counties_df <- county %>%
  filter(region == tolower(input$state))

mdata <-inner_join(counties_df, data01(), by = c("subregion"="county"))

base <- ggplot(data = state_df, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "grey")

mytitle <- paste("Total Fire Casualties in", input$state, "over the period", input$range[1],"-", input$range[2])

themap01 <- base + coord_fixed(1.3) +
  geom_polygon(data = mdata, aes(fill = c_ht_p), color = "white") +
  scale_fill_distiller(palette = "Spectral", name = "Casualties per 100 000") +
  geom_polygon(color = "white", fill = NA) +
  theme_bw() +  # This gets rid of the grey grid 
  ditch_the_axes +
  ggtitle(mytitle)
    
return(themap01)
}
  })

# I construct table 01 based on the same data as above 
output$table01 <- DT::renderDataTable(DT::datatable({ # see shiny cheatsheet
if(input$state == "All"){
  current_data <- data02()
} else{
  current_data <- data01() 
  current_data <- current_data[!is.na(current_data$county),]
  current_data <- current_data[!is.na(current_data$pop),]
}
    current_data
   },# some options for the rendering of the table 
   rownames=FALSE,
   colnames = c(ifelse(input$state == "All",'State','County'), 'Casualties',"2010 Population", "Cas./Pop. * 100'000"),
   options = list(searching = FALSE)
  ))
  
# first step is to create a reactive data set for the plot
data03 <- reactive({
  
  aux3 <- df[df$region %in% input$state2,]
  aux3$year <- year(aux3$inc_date)
  aux3 %>%
    group_by(region, year) %>%
    summarise(casualties=n(), pop = round(mean(state_pop)), c_ht_p=round(casualties*100000/pop))
})

 output$map02 <- renderPlotly({ # I use plotly here to make the graph dynamic 

    
  if(!input$adjust){ # if the reader chooses to not adjust per population
    thistitle <- paste("Comparing casualties over time")
  base <- ggplot(data03(), aes(x = year, y = casualties, group = region, col = region)) +
      scale_y_continuous(name = "Casualties") # I want continuous axes
  }
  if(input$adjust){  # if the reader chooses to adjust per population
    thistitle <- paste("Comparing casualties per 100000 people over time")
    base <- ggplot(data03(), aes(x = year, y = c_ht_p, group = region, col = region)) +
      scale_y_continuous(name = "Casualties per 100000") # I want continuous axes
  }
  
  plot02 <- base +
    geom_line() +
    scale_x_continuous(breaks = unique(data03()$year)) +
    scale_color_discrete(name = "State") +
    ggtitle(thistitle)
  
  ggplotly(plot02)
                       
  })
 
# Create a reactive data set based on the characteristics of the victims 
 
 
 data04 <- reactive({
   if(input$allstates) return(df[!is.na(df$gender),])
   aux4 <- df[df$region == input$state4,]
   aux4 <- aux4[!is.na(df$gender),]
 })
 # This is inspired by what was covered in class about the diamonds data set
 output$trendPlot <- renderPlot({
   
   if (input$x == "age"){
     facet <- paste0(input$facet_row, '~ .')
     posn_d <- position_dodge(width=.8)
     p <-ggplot(data04(), aes_string(x = "age", fill=input$color)) +
       geom_histogram(position=posn_d, alpha=.6)
     if (facet != '.~ .') p <- p + facet_grid(facet, scales="free")
     return(p)
   }
   facet <- paste0(input$facet_row, '~ .')
   posn_d <- position_dodge(width=.5)
   p <-ggplot(data04(), aes_string(x = input$x, fill=input$color)) +
     geom_bar(position=posn_d, alpha=.6)
   if (facet != '.~ .') p <- p + facet_grid(facet, scales="free")
   
   p 
 })
  
# I create a reactive data set for times of the year i.e. per month on average and per weekday on average
 data06 <- reactive({
   
   if(input$state6 == "All"){
     df$month <- factor(months(df$inc_date, abbreviate = TRUE), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
     df$weekday <- factor(weekdays(df$inc_date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    df <- df[!is.na(df$month),] # I noticed there was a column of NA - so I got rid of it
    df <- df[!is.na(df$weekday),] # I noticed there was a column of NA - so I got rid of it
     return(df)
   } 
   df <- df[df$region == input$state6,]
   df$month <- factor(months(df$inc_date, abbreviate = TRUE), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
   df$weekday <- factor(weekdays(df$inc_date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
   df <- df[!is.na(df$month),]
   df <- df[!is.na(df$weekday),]
   df
 })
 
 output$time <- renderPlot({
   if(input$button1 == "Month"){
    data <- data06() %>% 
       group_by(month) %>%
       summarise(count = n()) 
    p <- ggplot(data = data, aes(x = month, y = count)) +geom_bar(alpha = 0.7, stat = "identity")
   return(p)
    }
   if(input$button1 == "Weekday"){
     data <- data06() %>%
       group_by(weekday) %>%
       summarise(count = n())
     p <- ggplot(data = data, aes(x = weekday, y = count)) +geom_bar(alpha = 0.7, stat = "identity")
     return(p)
     }
  
 })
 
# first step is to make all the inputs reactive to the button
 
 
 state5 <- eventReactive(input$button, {input$state5})
 type2 <- eventReactive(input$button, {input$type2})
 range21 <- eventReactive(input$button, {input$range2[1]})
 range22 <- eventReactive(input$button, {input$range2[2]})
 sub2 <- eventReactive(input$button, {input$sub2})
 allstates2 <- eventReactive(input$button, {input$allstates2})
 allages2 <- eventReactive(input$button, {input$allages2})
 allsev2 <- eventReactive(input$button, {input$allsev2})
 
 # I make a reactive function for the data with  3 conditions 
 # I have 8 different cases as a result (2^3 cases)
 
 data05 <- reactive({
   if (allstates2()) {
     if (allages2()) {
       if (allsev2()) {
         return(df) # no selection is made 
       } else{
         return(df[df$sev %in% type2(),]) # selection for severity 
       }
     } else {
       if (allsev2()) {
         return(df[df$age>= range21() & df$age<= range22() , ]) # selection for age
       } else{
         return(df[df$age>= range21() & df$age<= range22() & df$sev %in% type2(),]) # selection for age and severity
       }
     }
   }
   
   if (!allstates2()) {
     if (allages2()) {
       if (allsev2()) {
         return(df[df$region == state5(), ]) # select region/ state only
       } else{
         return(df[df$region == state5() & df$sev %in% type2(),]) # select state and severity
       }
     } else {
       if (allsev2()) {
         return(df[df$region == state5() & df$age>= range21() & df$age<= range22() , ]) # select state and age
       } else{
         return(df[df$region == state5() & df$age>= range21() & df$age<= range22() & df$sev %in% type2(),]) # select all of them 
       }
     }
   }
   
 })
 
 output$tt <- renderPrint({
   # note : \n means change the line
   cat("In the population of\n") # I use the cat to send text to the console 
   print(ifelse(allstates2(), "all states", state5())) # print does more or less the same thing as cat
   cat("do\n")
   print(ifelse(sub2() == "r", "Black and White individuals", "Female and Male individuals"))
   cat("aged from\n")
   print(paste(range21(), "to", range22()))
   cat("have the same probability of having an injury of kind\n")
   if (allsev2()){
     print("any")
   } else {
     print(type2())
   }
   
   cat("\n")
   # start of test
   if(sub2() == "r") { # if the sample is about race, I split amoung the race and count them
     a <- data05() %>%
       filter(race == "Black") %>% # split for Black victims 
       summarise(count=n())
     n_Black <- a$count
     a <- data05() %>%
       filter(race == "White") %>% # split for White victims 
       summarise(count=n()) 
     n_White <- a$count
     n_Total <- n_Black + n_White 
     test <- prop.test(c(n_Black, n_White), c(n_Total, n_Total)) # here is the actual proportionality test
     if (test$p.value < .05){ # I used ageneric value of alpha, alpha = 0.05
       answer <- "---> No!" # they dont have the same probability 
     } else {
       answer <-"---> Yes!" # I cannot reject the null
     }
     output <- list(test=test, answer=answer) # I want the test and the clear answer so I make a list
     return(output)
   } 
   # Here I carry out the same test for gender , male and female
   if(sub2() == "g") {
     a <- data05() %>%
       filter(gender == "Female") %>%
       summarise(count=n())
     n_Female <- a$count
     a <- data05() %>%
       filter(gender == "Male") %>%
       summarise(count=n()) 
     n_Male <- a$count
     n_Total <- n_Female + n_Male 
     test <- prop.test(c(n_Female, n_Male), c(n_Total, n_Total))
     if (test$p.value < .05){
       answer <- "---> No!"
     } else {
       answer <-"---> Yes!" 
     }
     output <- list(test=test, answer=answer)
     return(output)
   }
   
 })
}) # end of server