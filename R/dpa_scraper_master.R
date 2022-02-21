#' Attempt to construct DF of all DPA scrapers and return it
#'
#' @return dataframe of all the DPA scrapers with attributes
#'
dpa_scraper_master = function(){

  #DPA scraper master list


  # set up ------------------------------------------------------------------


  s.paths = list.files(path = "code/BT-Lumiere scrapers/scrapers", pattern = "R",  full.names = T) %>%
    c()

  dsp.names = str_extract(s.paths, "(?<=/)[^/]+(?=\\.)")

  scraper.master = data.frame(
    scraper.path = s.paths,
    display.name = dsp.names,
    multiple.urls = F,
    main.url = "",
    specialised.scraper = F,
    stock.data.file = "",
    scraper.legal.area = "",
    bid.stem = "",
    stringsAsFactors = F
  )

  rm(s.paths, dsp.names)

  scraper.master$last.check = Date(nrow(scraper.master))
  scraper.master$most.recent = Date(nrow(scraper.master))


  errorifications = c()

  start.vars = ls()
  # main loop ---------------------------------------------------------------


  for(scraper.path in scraper.master$scraper.path){

    #load the code as a string
    #could use 'source()' but I don't want to run the codes here
    #scraper.code = paste(readLines(scraper.path), collapse = "\\n")

    scraper.code = readLines(scraper.path, warn = F)

    Encoding(scraper.code) <- "UTF8"

    #get rdata filename
    stock.data.line = scraper.code[grepl("stock.+\\.rdata", scraper.code, ignore.case = T)][1]
    stock.data.path = str_extract(stock.data.line, "(?<=\").+\\.[Rr]data")
    scraper.master$stock.data.file[scraper.master$scraper.path == scraper.path] = stock.data.path


    ###check for a 'main.url'
    if(any(grepl("^main\\.url", scraper.code))){

      #finds the first line that starts with 'main.url...', and parses it.

      tryCatch(expr = {
        m.url.line = scraper.code[grepl("^main\\.url", scraper.code)][1]

        if(is.na(m.url.line)){
          m.url <<- ""
        }else{
          eval(parse(text = m.url.line))
          m.url <<- main.url
        }

      },
      error = function(e){
        message("couldn't find a main.url")
        m.url <<- ""
      }
      )

      #this block catches when the main URL is there, but the rx can't extract it
      #(e.g. when main.url is saved over multiple lines, the rx engine CANNOT
      #handle this)

      if(nchar(m.url)==0){
        tryCatch(
          expr = {
            m.url <- str_extract(scraper.code, gtabastiat::regex_url) %>%
              purrr::discard(is.na)
            m.url <<- m.url[1]

            message("using a URL I found somewhere in the code")
          },
          error = function(e) {
            message("couldn't find ANY url")
          }
        )
      }
      main.url = m.url
      # 'main.url' now in the local env so it can be referenced...

      scraper.master$main.url[scraper.master$scraper.path == scraper.path] = main.url
    } else {
      scraper.master$multiple.urls[scraper.master$scraper.path == scraper.path] = T

      #this block picks up one of several URLs to save in the spreadsheet
      tryCatch(
        expr = {
          mult.url <<- str_extract(scraper.code, gtabastiat::regex_url) %>%
            purrr::discard(is.na) %>%
            paste(sep = "\n",collapse = "\n")


          message("using a URL I found somewhere in the code")
        },
        error = function(e) {
          message("couldn't find ANY url")
        }
      )

      scraper.master$main.url[scraper.master$scraper.path == scraper.path] = mult.url

    }

    # get the legal area
    if(any(grepl("legal\\.area", scraper.code))){

      legal.area.line = scraper.code[grepl("legal\\.area", scraper.code)][1]

      eval(parse(text = legal.area.line))

      scraper.master$scraper.legal.area[scraper.master$scraper.path == scraper.path] = scraper.legal.area

    }

    #(try to) load the stock data
    tryCatch(
      {
        load(stock.data.path)

        if (exists("last.check")) {
          scraper.master$last.check[scraper.master$scraper.path == scraper.path] = last.check
          rm(last.check)
        }

        if(exists("table.stock")){
          message(paste("loaded:", stock.data.path))

          if("act.date" %in% colnames(table.stock)){
            most.recent = max(table.stock$act.date, na.rm = T)
          }
          scraper.master$most.recent[scraper.master$scraper.path == scraper.path] = most.recent

          scraper.master$bid.stem[scraper.master$scraper.path == scraper.path] = gsub(pattern = "\\d+$",
                                                                                      replacement = "",
                                                                                      x=table.stock$bid[1])

          rm(table.stock)
        }
      },
      error=function(cond){
        message(paste("[err]: problem loading stock data for", scraper.path))
        errorifications %>% c(scraper.path) ->> errorifications

      }
    )




  }





  return(scraper.master)



}
