### script para fazer web scrapping de avaliações de empresas
### Autor: Gabriel Lima Gomes - Brasil - Dezembro 2017.


#### **** WEB SCRAPPING REVIEWS **** ####

### FUNÇÃO PARA CAPTURAR REVISÕES DO SITE Love Mondays
reviewsLoveMondays <- function(){
  print('reviews Love Mondays')
  urls     <- c('https://www.lovemondays.com.br/trabalhar-na-ifood/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-nubank/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-ibm/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-microsoft/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-google/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-oracle/avaliacoes/pagina/') #1
  
  reviewDF <- data.frame()
  
  for(urlPage in urls){
    tryCatch({
      ## READ THE FIRST PAGE TO GET THE COMPANY'S NAME AND TOTAL OF PAGES
      webPage  <- read_html(paste(urlPage,'1',sep = '') )
      company  <- webPage %>% html_nodes('.lm-CompanyHeader-companyName') %>% html_text()
      
      ## RECUPERANDO O TOTAL DE AVALIA??ES
      totalRew   <- as.numeric(webPage %>% html_nodes('.lm-Tabs-default--companyHeader .lm-Tabs-default-item.is-active') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .))
      if(!identical(totalRew,numeric(0)) & !is.na(totalRew) ){
        totalPages <- round(totalRew/10)  
      }else{
        totalPages <- 1
      }

      for( page in 1:totalPages){
        cat('\n\n ==> Page: ',page)
        urlPage2 <- paste(urlPage,page, sep = '')
        cat('\t',urlPage2)
        
        webPage <- read_html(urlPage2)
        nodes   <- webPage %>% html_nodes('section .lm-List-default .lm-List-default-row')
        
        ids     <- nodes %>% html_nodes('.lm-List-item-header-title a') %>% html_attr('href') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .)
        ratings <- c()
        for(review in nodes){
          rating <- 5 * (review %>% html_nodes('.lm-RatingStar-starContainer div:nth-child(2).lm-RatingStar-starContainer-starsActive') %>% 
                           html_attrs() %>% .[[1]] %>% unlist(.) %>% paste(.,collapse = ' ') %>% 
                           gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) %>% as.numeric(.) / 100)
          ratings <- c(ratings,rating)
        }## END review in nodes
        
        reviews <- scrapReviews(nodes = nodes,tagDate = '.lm-Company-dateTime-label', tagTitle = '.lm-List-item-header-title',
                                tagStatus = '.reviewer ',tagLocation = '.lm-List-item-contributions',
                                tagRecommend = '.lm-Review-contribution p:nth-child(3)',tagPros = '.lm-Review-contribution p:nth-child(1)',
                                tagCons = '.lm-Review-contribution p:nth-child(2)',tagAdvice = 'ni' ,
                                company = company,ids = ids, ratings = ratings)
        
        reviewDF <- rbind(reviewDF,reviews)
        fwrite(reviewDF, paste('data/reviews/reviewLoveM-',Sys.Date(),'.csv',sep = ''))
      }## END FOR totalPages
    }, error = function(e){
      print(paste('ERROR IN FOR URLS: ', e , sep = ' ') )
    })
  }#END FOR urls
  cat("\n ***** END REVIEWS LOVE MONDAYS ****")
  return(reviewDF)
}

reviewsGlassDoor <- function(){
  print('reviewsGlassDoor')
  urls  <- c('https://www.glassdoor.com/Reviews/Dell-Reviews-E1327_P',  
             'https://www.glassdoor.com/Reviews/IBM-Reviews-E354_P',
             'https://www.glassdoor.com/Reviews/Microsoft-Reviews-E1651_P',
             'https://www.glassdoor.com/Reviews/Google-Reviews-E9079_P',
             'https://www.glassdoor.com/Reviews/Oracle-Reviews-E1737_P',
             'https://www.glassdoor.com/Reviews/Tesla-Reviews-E43129_P')
  reviewDF <- data.frame()
  
  
  for(urlPage in urls){
    tryCatch({
      webPage  <- read_html(paste(urlPage,'1','.htm',sep = ''))
      company  <- webPage %>% html_nodes('.module.filterableContents .h2') %>% html_text()
      
      totalRew   <- as.numeric(webPage %>% html_nodes('.padTopSm.margRtSm.margBot.minor') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .))
      
      if(!identical(totalRew,numeric(0)) & !is.na(totalRew) ){
        totalPages <- round(totalRew/10)  
      }else{
        totalPages <- 1
      }
      
      ## LOOP TO READ EACH PAGE
      for( page in 1:totalPages){
        cat('\n\n==> Page: ',page)
        urlPage2 <- paste(urlPage,page,'.htm', sep = '')
        cat('\t',urlPage2)
        
        webPage <- read_html(urlPage2) ## SCRAPPING THE PAGE
        nodes   <- webPage %>% html_nodes('.empReview') ## GET NODES THAT CONTAIN THE REVIEWS
        
        ## RECUPERANDO OS IDS DAS AVALIA??ES
        ids <- nodes %>% html_attr('id') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) #html_attr('id') 
        
        ## RECUPERANDO RATING DAS AVALIA??ES
        ratings <- nodes %>% html_nodes('.rating span') %>% html_attr('title') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) %>% as.numeric(.) / 10
        
        reviews <- scrapReviews(nodes = nodes,tagDate = '.date', tagTitle = '.tbl .h2.summary',
                                tagStatus = '.tbl.reviewMeta .authorJobTitle.middle',tagLocation = '.tbl.reviewMeta .authorLocation.middle',
                                tagRecommend = '.recommends .tightLt:nth-child(1)',tagOutlook = '.recommends .tightLt:nth-child(2)',
                                tagCeo = '.recommends .tightLt:nth-child(3) .showDesk',tagPros = '.pros.mainText',
                                tagCons = '.cons.mainText',tagAdvice = '.adviceMgmt' ,
                                company = company,ids = ids, ratings = ratings)
        
        reviewDF <- rbind(reviewDF,reviews)
        fwrite(reviewDF, paste('data/reviews/reviewGlassD-',Sys.Date(),'.csv',sep = ''))
      }## END FOT totalPages
    }, error = function(e){
      print(paste('ERROR IN FOR URLS: ', e , sep = ' ') )
    })
  }## END FOR urls
  cat("\n ***** END REVIEWS GLASSDOOR ****")
  return(reviewDF)
}

reviewsIndeed <- function(){
  print('reviewsIndeed')
  urls <- c('https://www.indeed.com.br/cmp/Oracle/reviews',
            'https://www.indeed.com.br/cmp/Ifood/reviews',
            'https://www.indeed.com.br/cmp/IBM/reviews',
            'https://www.indeed.com.br/cmp/Microsoft/reviews',
            'https://www.indeed.com.br/cmp/Google/reviews') #1
  reviewDF <- data.frame()
  
  urlPage <- urls
  for(urlPage in urls){
    tryCatch({
      ## READ THE FIRST PAGE TO GET THE COMPANY'S NAME AND TOTAL OF PAGES
      webPage  <- read_html(paste(urlPage,'?start=0',sep = '') )
      company  <- webPage %>% html_nodes('.cmp-company-name') %>% html_text()
      
      ## RECUPERANDO O TOTAL DE AVALIA??ES
      totalRew   <- as.numeric(webPage %>% html_nodes('.cmp-filter-result') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .))
      
      if(!identical(totalRew,numeric(0)) & !is.na(totalRew) ){
        totalPages <- round(totalRew/21)  
      }else{
        totalPages <- 1
      }
      
      for( page in 0:totalPages){
        cat('\n\n==> Page: ',page)
        urlPage2 <- paste(urlPage,'?start=',as.character(page*20), sep = '')
        cat('\t',urlPage2)
        
        webPage <- read_html(urlPage2)
        nodes   <- webPage %>% html_nodes('.cmp-review-container')
        ids     <- nodes %>% html_nodes('.cmp-review') %>% html_attr('data-tn-entityid') #%>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .)
        ratings <- nodes %>% html_nodes('.cmp-ratings meta') %>% html_attr('content')
        
        reviews <- scrapReviews(nodes = nodes,tagDate = '.cmp-review-date-created', tagTitle = '.cmp-review-title',
                                tagStatus = '.cmp-reviewer-job-title',tagLocation = '.cmp-reviewer-job-location',
                                tagRecommend = 'ni',tagPros = '.cmp-review-pros',
                                tagCons = '.cmp-review-cons',tagAdvice = 'ni' ,
                                company = company,ids = ids, ratings = ratings)
        
        reviewDF <- rbind(reviewDF,reviews)
        fwrite(reviewDF, paste('data/reviews/reviewIndeed-',Sys.Date(),'.csv',sep = ''))
      }## END FOR totalPages
      Sys.sleep(3)
    }, error = function(e){
      print(paste('ERROR IN FOR URLS: ', e , sep = ' ') )
    })
  }#END FOR urls
  cat("\n ***** END REVIEWS INDEED ****")
  
  return(reviewDF)
}



### FUNÇÃO PARA REALIZAR WEB SCRAPPING DE AVALIA??ES SOBRE EMPRESAS. 
## PARAMETROS: 
# nodes = NODES QUE CONTEM AS AVAILA?OES. OS NODES S?O PASSADOS COMO PARAMETRO AP?S J? REALIZAR O WEB SCRAPPING NA CLASSE PRINCIPAL(MAIN)
# tagDate = TAG(CLASSE/ID) PARA RECUPERAR A DATA DA AVALIA??O
# tagTitle = TAG(CLASSE/ID) PARA RECUPERAR O T?TULO DA AVALIA??O
# tagStatus = TAG(CLASSE/ID) PARA RECUPERAR O STATUS DO AVALIA??O (CARGO, EMPREGADO, EX-EMPREGADO ETC)
# tagLocation = TAG(CLASSE/ID) PARA RECUPERAR A LOCALIZA??O DE ONDE FOI FEITA A AVALIA??O
# tagRecommend = TAG(CLASSE/ID) PARA RECUPERAR SE O AVALIADOR RECOMENDA OU N?O A EMPRESA
# tagOutlook = TAG(CLASSE/ID) PARA RECUPERAR SE PERSPECTIVA ? BOA, INTERMEDIARIA OU RUIM (glassdoor)
# tagCeo = TAG(CLASSE/ID) PARA RECUPERAR SE APROVA O CEO DA EMPRESA
# tagPros = TAG(CLASSE/ID) PARA RECUPERAR AVALIA??O POSITIVA (PR?S DA EMPRESA)
# tagCons = = TAG(CLASSE/ID) PARA RECUPERAR AVALIA??O NEGATIVA (CONTRAS DA EMPRESA)
# tagAdvice = TAG(CLASSE/ID) PARA RECUPERAR MENSAGEM PARA CONSELHOS
# company = NOME DA EMPRESA QUE SER? RECUPERADO NA CLASSE PRINCIPAL 
# ids = IDS RECUPERADOS NA CLASSE PRINCIPAL E SER?O INSERIDOS NO FINAL NO DATAFRAME. (Os ids s?o passados como parametro, porque s?o 
# inseridos na p?gina de diferentes formas, inclusive passando como atributo da tag, assim, sendo necess?rio outra metodologia para recupera-los)
# ratings = RATING RECUPERADOS NA CLASSE PRINCIPAL E SER?O INSERIDOS NO FINAL NO DATAFRAME. (Os rating s?o passados como parametro, porque s?o 
# inseridos na p?gina de diferentes formas, inclusive passando como atributo da tag, assim, sendo necess?rio outra metodologia para recupera-los)

scrapReviews <- function(nodes = '',tagDate = 'ni', tagTitle = 'ni',tagStatus = 'ni', tagLocation = 'ni',tagRecommend = 'ni',
                         tagOutlook = 'ni', tagCeo = 'ni', tagPros = 'ni', tagCons = 'ni', tagAdvice = 'ni', company = '', ids = '', ratings = ''){
  
  reviewDF <- data.frame()
  i <- 1
  tryCatch({
    for(review in nodes){
      #id    <- review %>% html_nodes('.lm-List-item-header-title a') %>% html_attr('href') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .)
      cat('\n\t id = ', ids[i]); i <- i+1;
      
      date  <- review %>% html_nodes(tagDate) %>% html_text()
      date <- ifelse(identical(date, character(0)), 'NI', date )
      
      title <- review %>% html_nodes(tagTitle) %>% html_text()
      title <- ifelse(identical(title, character(0)), 'NI', title )
      
      status   <- review %>% html_nodes(tagStatus) %>% html_text()
      status <- ifelse(identical(status, character(0)), 'NI', status )
      
      location <- review %>% html_nodes(tagLocation) %>% html_text()
      location <- ifelse(identical(location, character(0)), 'NI', location )
      
      recommend <- review %>% html_nodes(tagRecommend) %>% html_text()
      recommend <- ifelse(identical(recommend, character(0)), 'NI', recommend )
      
      outlook <- review %>% html_nodes(tagOutlook) %>% html_text()
      outlook <- ifelse(identical(outlook, character(0)), 'NI', outlook )
      
      ceo <- review %>% html_nodes(tagCeo) %>% html_text()
      ceo <- ifelse(identical(ceo, character(0)), 'NI', ceo )
      
      pros <- review %>% html_nodes(tagPros) %>% html_text()
      pros <- ifelse(identical(pros, character(0)), 'NI', pros )
      
      cons <- review %>% html_nodes(tagCons) %>% html_text()
      cons <- ifelse(identical(cons, character(0)), 'NI', cons )
      
      adviceManag <- review %>% html_nodes(tagAdvice) %>% html_text()
      adviceManag <- ifelse(identical(adviceManag, character(0)), 'NI', adviceManag )
      
      dateCollect <- Sys.Date()
      
      dataFrame <- data.frame(company, date, title,dateCollect = dateCollect, status, location, recommend, outlook, 
                              ceo, pros, cons, adviceManag)
      
      reviewDF <- rbind(reviewDF, dataFrame)
      fwrite(reviewDF, 'reviews.csv')
    }## END FOR NODES
  }, error = function(e){
    print(paste('ERROR IN FOR NODES: ', e , sep = ' ') )
  })
  reviewDF$id     <- ids
  reviewDF$rating <- ratings
  
  return(reviewDF)
}

#### **** PREPROCESS REVIEWS DATA **** ####
cleanReviewGlassD <- function(dados){
  dadosClean <- dados
  
  dadosClean$companyClean <- gsub('Reviews','',dadosClean$company)
  
  print("Clean Date")
  i <- 1
  dadosClean$date <- as.character(dadosClean$date)
  dadosClean$date <- sapply(dadosClean$date, function(x){
    if(grepl('seconds',x)){
      x <- format(as.Date(dadosClean[i,]$dateCollect),'%b %d, %Y')
    }
    i <<- i + 1
    x
  })
  
  dadosClean$dateClean <- lubridate::mdy(as.character(dadosClean$date))
  
  print("Clean Title")
  dadosClean$title <- gsub('\"','', dadosClean$title)
  
  dadosClean$city  <- clearCity(dadosClean$location)
  dadosClean$state <- clearState(dados$location)  
  
  print("Clean Recommend")
  dadosClean$recommend <- sapply(dadosClean$recommend, function(x){
    if(x == "Doesn't Recommend"){
      x <- 'No'
    }else if( x == "Recommends"){
      x <- 'Yes'
    }else{
      x <- 'NI'
    }
    x
  })
  
  print("Clean Outlook")
  dadosClean$outlook <- sapply(dados$outlook, function(x){
    x <- gsub('Outlook','',x)
    if(grepl('of',x)){
      x <- 'NI'
    }
    x
  })
  
  print("Clean Ceo")
  dadosClean$ceo <- sapply(dadosClean$ceo, function(x){
    x <- gsub('of','',x)
    x
  })
  
  print("Clean Position")
  dadosClean$position <- sapply(dadosClean$status, function(x){
    ini <- regexpr('-',x)[[1]] + 1
    if(ini > 0){
      x <- trimws(substr(x,ini,str_length(x)))  
    }
    x  
  })
  
  print("Clean Status")
  dadosClean$status <-sapply(dadosClean$status, function(x){
    fim <- regexpr('-',x)[[1]] - 1
    if(fim > 0){
      x <- trimws(substr(x,0,fim))  
    }
    x
  })
  
  print("Clean ID")
  dadosClean$id   <- paste('gd_',dadosClean$id,sep = '')
  dadosClean$site <- 'glassdoor'
  cols <- c("id","company","dateClean","dateCollect","rating","city","state","title","status","position","recommend","pros","cons","adviceManag","outlook","ceo","site")
  
  dadosClean <- delDup(dadosClean)
  dadosClean <- dadosClean %>% select_(.dots = cols)
  #fwrite(dadosClean,'data/reviewGlassD.csv') 
  return(dadosClean)
}## END cleanReviewGlassD function


cleanReviewIndeed <- function(dados){
  dadosClean       <- dados
  
  print("Clean Date")
  dadosClean$dateClean <- lubridate::dmy(as.character(dadosClean$date))
  dadosClean$city  <- clearCity(dadosClean$location)
  
  print("Clean State")
  dadosClean$state <- sapply(dadosClean$location, function(x){
    x <- gsub('\n','',x)
    x <- gsub('[[:digit:]]','',x)
    pos <- regexpr('\\,',x)[1]
    if(pos > 0){
      x   <- substr(x, pos+1, pos+3)
    }else{
      x <- 'ni'
    }
    x <- gsub("\\W+", "", x) # remove double space
    x
  })
  
  print("Clean Comment")
  dadosClean$pros <- gsub('Prós','',dadosClean$pros)#sapply(dadosClean$pros, function)
  dadosClean$cons <- gsub('Contras','',dadosClean$cons)#sapply(dadosClean$pros, function)
  dadosClean$adviceManag <- gsub('Conselhos para presidência:','',dadosClean$cons)
  
  print("Clean Position")
  dadosClean$position <- sapply(dadosClean$status,function(x){
    fim <- gregexpr('Ex-Funcionário|Funcionário Atual',x)[[1]]-1
    if(fim > 0){
      x <- substr(x,0,fim-1)
      x <- ifelse(x == '','NI',x)
    }
    x
  })
  
  print("Clean Status")
  dadosClean$status <- sapply(dadosClean$status,function(x){
    if(grepl('Ex-Funcionário',x)){
      x <- 'Ex-funcionário'
    }else{
      x <- 'Funcionário'
    }
    x
  })
  dadosClean$site <- 'indeed'
  dadosClean$id <- paste('in_',dadosClean$id,sep = '')
  
#  cols <-c("id","company","dateClean","rating","city","state","title","status","recommend","pros","cons","adviceManag","outlook","ceo","site")
  cols <- c("id","company","dateClean","dateCollect","rating","city","state","title","status","position","recommend","pros","cons","adviceManag","outlook","ceo","site")
  
  #fwrite(dadosClean,'data/reviewIndeed.csv')
  dadosClean <- delDup(dadosClean)
  dadosClean <- dadosClean %>% select_(.dots = cols)
  
  return(dadosClean)
}## END cleanReviewIndeed function

cleanReviewLoveM <- function(dados){
  dadosClean <- dados
  
  print("Clean Date")
  dadosClean             <- convertDate(dadosClean)
  dadosClean$dateClean   <- lubridate::ymd(dadosClean$dateClean)
  
  print("Clean Company")
  dadosClean$company     <- gsub('\n','',dadosClean$company)
  dadosClean$city        <- clearCity(dadosClean$location)
  dadosClean$state       <- clearState(dadosClean$location)
  
  print("Clean Advice")
  dadosClean$adviceManag <- sapply(dadosClean$recommend, function(x){
    if(grepl('conselhos',x, ignore.case = T)){
      x
    }else{
      x <- 'NI'
    }
  })
  
  print("Clean Recommend")
  dadosClean$recommend <- sapply(dadosClean$recommend, function(x){
    if(grepl('conselhos',x, ignore.case = T)){
      x <- 'NI'
    }else{
      x <- gsub('Recomenda a empresa:','',x)
      x <- gsub('\n','',x)
      x
    }
  })
  print("Clean Comments")
  dadosClean$pros <- gsub('Prós:','',dadosClean$pros)#sapply(dadosClean$pros, function)
  dadosClean$cons <- gsub('Contras:','',dadosClean$cons)#sapply(dadosClean$pros, function)
  dadosClean$adviceManag <- gsub('Conselhos para presidência:','',dadosClean$cons)
  
  print("Clean Position")
  dadosClean$position <- sapply(dadosClean$status,function(x){
    fim <- gregexpr('Ex-funcionário',x)[[1]]-1
    if(fim > 0){
      x <- substr(x,0,fim)
      x <- ifelse(x == '','NI',x)
    }
    x
  })
  
  print("Clean Status")
  dadosClean$status <- sapply(dadosClean$status,function(x){
    if(grepl('Ex-funcionário|saiu',x)){
      x <- 'Ex-funcionário'
    }else{
      x <- 'Funcionário'
    }
    x
  })
  
  print("Clean ID")
  dadosClean$id <- paste('lm_',dadosClean$id,sep = '')
  dadosClean$site <- 'love mondays'
  cols <- c("id","company","dateClean","dateCollect","rating","city","state","title","status","position","recommend","pros","cons","adviceManag","outlook","ceo","site")
  
  dadosClean <- dadosClean %>% select_(.dots = cols)

  #fwrite(dadosClean,'data/reviewLoveM.csv')
  return(dadosClean)
} ## END cleanReviewLoveM function

### TRANSFORMAÇÕES NECESSÁRIAS PARA SEREM APLICADAS NA BASE DE DADOS FINAL
cleanReviewFinal <- function(dados){
  #load('data/reviews.RData')
  #reviewClean <- reviews
  reviewClean <- dados #fread('data/reviewFinal.csv',encoding = 'UTF-8')

  reviewClean[,c('pros','cons','title')] <- cleanText(reviewClean,c('pros','cons','title'),
                                                      stopWords = c('reviews','pra','empresa','\\?','trabalho','ser','muito','ponto','positivo','negativo',
                                                                    'can','like','make','many',
                                                                    stopwords('en'),stopwords('sp') )
                                                      ,specialChar = T)

  print("Clean Recommend")
  reviewClean$recommend <- gsub('Sim','Yes',reviewClean$recommend)
  reviewClean$recommend <- gsub('Não','No',reviewClean$recommend)
  
  print("Convert to String")
  reviewClean <- data.frame(apply(reviewClean, 2, function(x){
    x <- as.character(x) 
  }),stringsAsFactors = F)
  
  cols <- c('id','dateClean','dateCollect','rating','city','state','status','position','recommend','adviceManag','outlook',
            'ceo','site','title','pros','cons','company')
  
  print("Select Columns")
  reviewClean <- reviewClean %>% select_(.dots = cols)
  #fwrite(dados,'data/reviewFinal.csv')
  return(reviewClean)
}



#dados <- fread('data/reviewFinal.csv')
#dados[,c('title', 'pros', 'cons')] <- reviewClean[,c('title', 'pros', 'cons')]
#fwrite(dados,'data/reviewFinal.csv')


