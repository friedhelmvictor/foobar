#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(RSQLite)
library(shiny)
library(shinyjs)
library(data.table)
library(DT)
library(ggplot2)

con = dbConnect(drv=RSQLite::SQLite(), dbname="telegramPnD.sqlite")

#for (table in dbListTables(con)) {
#  assign(table, as.data.table(dbReadTable(con, table)))
#}
#override channels casting channel_id to text because its too big
channels <- as.data.table(dbGetQuery(con, "SELECT CAST(id AS TEXT) AS id, name FROM channels;"))
messages <- as.data.table(dbGetQuery(con, "SELECT CAST(channel_id AS TEXT) AS channel_id, message_id, text, media, timestamp FROM messages;"))
channelmembers <- as.data.table(dbGetQuery(con, "SELECT crawl_time, CAST(id AS TEXT) AS id, membercount FROM channelmembers;"))
flagmaxtimes <- as.data.table(dbGetQuery(con, "SELECT CAST(flags.channel_id AS TEXT) AS channel_id, max(timestamp) AS maximumt FROM flags LEFT JOIN messages ON flags.channel_id = messages.channel_id AND flags.message_id = messages.message_id GROUP BY flags.channel_id;"))

# filter messages to only show since last label
messages <- merge(messages, flagmaxtimes, on="channel_id", all.x = T)[, maximumt := ifelse(is.na(maximumt), 0, maximumt)]
messages <- messages[timestamp > maximumt]

channelmembers[, crawl_time := as.POSIXct(crawl_time, origin="1970-01-01")]
messages[, timestamp := as.POSIXct(timestamp, origin="1970-01-01")]
channelIdToName <- channels[, list(id, name)]

# Define UI for application that draws a histogram
ui <- fluidPage(
   useShinyjs(),
  
   # Application title
   titlePanel("Pump and Dump groups"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         uiOutput("channelSelect"),
         tableOutput("channelStats")
      ),
      mainPanel(
        fluidRow(
          column(width = 5, plotOutput("memberTimeseries")),
          column(width = 4, uiOutput("image")),
          column(width = 3, verbatimTextOutput('x4'),
                 tags$hr(),
                 shinyjs::disabled(textInput("flag_message_id", "Message ID", "")),
                 shinyjs::disabled(textInput("flag_channel_id", "Channel ID", "")),
                 textInput("flag_symbol", "Symbol", ""),
                 selectInput("flag_exchange", "Exchange", choices=c("Cryptopia.co.nz", "Yobit.net", "Binance", "Bittrex", "BitMex", "Binance delayed"), selected="Binance"),
                 
                 actionButton("flagpumpbutton", "Flag as pump"),
                 actionButton("flagsignalbutton", "Flag as signal"))#,
          
                 #actionButton("delete", "Delete"))
        ),
        DTOutput("messageList")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   
   output$channelSelect <- renderUI({
     #namesdf <- dbGetQuery(con, "SELECT name, id from channels")
     nameoptions <- setNames(channelIdToName$id, channelIdToName$name)
     print(nameoptions)
     selectInput("channel_name",
                 "Channel",
                 nameoptions)
   })
   
   output$channelStats <- renderTable({
     messageCountTotal <- messages[, list(total = .N), by=list(id=channel_id)]
     messageCountLastWeek <- messages[timestamp > Sys.time() - as.difftime(7, unit="days"), list("7d" = .N), by=list(id=channel_id)]
     messageCountLast24hr <- messages[timestamp > Sys.time() - as.difftime(1, unit="days"), list("24h" = .N), by=list(id=channel_id)]
     
     latestMemberCount <- channelmembers[, list(members=tail(memberCount, 1)), by=id]
     
     merged <- Reduce(function(...) merge(..., on="id", all=T), list(messageCountTotal, messageCountLastWeek, messageCountLast24hr, latestMemberCount, channelIdToName))
     as.data.table(merged)[, list(id, name, total, `7d`, `24h`, members)]
   })
   
   output$memberTimeseries <- renderPlot({
     ggplot(channelmembers[id == input$channel_name]) + geom_line(aes(x=crawl_time, y=memberCount))
   })
   
   reactiveMessages <- reactive({
     messages[channel_id == input$channel_name, list(message_id, text, media, timestamp)][order(rank(timestamp))]
     })
   
   #output$messageList <- renderDataTable(datatable(data.table(1:10,10:1), options = list(order = list(3, "desc"))))
   output$messageList <- DT::renderDataTable(reactiveMessages(), selection = 'single')
   
   UpdateInputs <- function(data, session) {
     updateTextInput(session, "flag_message_id", value = unname(data["flag_message_id"]))
     updateTextInput(session, "flag_channel_id", value = unname(data["flag_channel_id"]))
     updateTextInput(session, "flag_symbol", value = unname(data["flag_symbol"]))
     #updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
     #updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
   }

   
   
   
   output$x4 = renderPrint({
     s = input$messageList_rows_selected
     if (length(s)) {
       data <- reactiveMessages()
       messageId <- as.numeric(data[s,1])
       messageText <- as.character(data[s, 2])
       binanceSymbols <- c("ADA", "ADX", "AE", "AGI", "AION", "AMB", "APPC", "ARDR", "ARK", "ARN", "AST", "BAT", "BCC", "BCD", "BCN", "BCPT", "BLZ", "BNB", "BNT", "BQX", "BRD", "BTC", "BTG", "BTM", "BTS", "CDT", "CHAT", "CLOAK", "CMT", "CND", "CTR", "CVC", "DASH", "DATA", "DENT", "DGD", "DLT", "DNT", "DOCK", "EDO", "ELC", "ELF", "ENG", "ENJ", "EOS", "ETC", "ETH", "EVX", "FUEL", "FUN", "GAS", "GNT", "GO", "GRS", "GTO", "GVT", "GXS", "HC", "HCC", "HOT", "HSR", "ICN", "ICX", "INS", "IOST", "IOTA", "IOTX", "KEY", "KMD", "KNC", "LEND", "LINK", "LLT", "LOOM", "LRC", "LSK", "LTC", "LUN", "MANA", "MCO", "MDA", "MFT", "MOD", "MTH", "MTL", "NANO", "NAS", "NAV", "NCASH", "NEBL", "NEO", "NPXS", "NULS", "NXS", "OAX", "OMG", "ONT", "OST", "PAX", "PHX", "PIVX", "POA", "POE", "POLY", "POWR", "PPT", "QKC", "QLC", "QSP", "QTUM", "RCN", "RDN", "REP", "REQ", "RLC", "RPX", "SALT", "SC", "SKY", "SNGLS", "SNM", "SNT", "STEEM", "STORJ", "STORM", "STRAT", "SUB", "SYS", "THETA", "TNB", "TNT", "TRIG", "TRX", "TUSD", "VEN", "VET", "VIA", "VIB", "VIBE", "WABI", "WAN", "WAVES", "WINGS", "WPR", "WTC", "XEM", "XLM", "XMR", "XRP", "XVG", "XZC", "YOYO", "ZEC", "ZEN", "ZIL", "ZRX")
       symbol <- c(intersect(sapply(stringr::str_extract_all(messageText, "\\b[A-Z]+\\b"), paste), binanceSymbols), "UNKNOWN")[1]
       UpdateInputs(data.frame("flag_message_id"=messageId, "flag_channel_id"=input$channel_name, "flag_symbol"=symbol), session = session)
       print(messageId)
       
     }
   })
   
   observeEvent(input$flagpumpbutton, {
     query <- paste0("INSERT OR IGNORE INTO flags(channel_id, message_id, type, symbol, exchange) VALUES(",input$flag_channel_id,
                     ", ",input$flag_message_id,
                     ", 'pump'",
                     ", '",input$flag_symbol,"'",
                     ", '",input$flag_exchange,"')")
     showNotification(query)
     dbSendQuery(con, query)
   })
   
   observeEvent(input$flagsignalbutton, {
     query <- paste0("INSERT OR IGNORE INTO flags(channel_id, message_id, type, symbol, exchange) VALUES(",input$flag_channel_id,
                     ", ",input$flag_message_id,
                     ", 'signal'",
                     ", '",input$flag_symbol,"'",
                     ", '",input$flag_exchange,"')")
     showNotification(query)
     dbSendQuery(con, query)
   })
   
   
   image_url <- reactive({
     shiny::validate(
       shiny::need(input$flag_channel_id, "Select a channel!"),
       shiny::need(input$flag_message_id, "Select a mesage!")
     )
     paste0(input$flag_channel_id,"/",input$flag_channel_id,"_media_",input$flag_message_id,".jpg")
   })
   
   output$image <- renderUI({
     tags$img(src = image_url(), height="400px", width="400px")
   })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

