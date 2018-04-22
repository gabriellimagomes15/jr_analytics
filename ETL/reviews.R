### script para fazer web scrapping de vagas de emprego e avaliações de empresas
### Autor: Gabriel Lima Gomes - Dezembro 2017.


#### **** WEB SCRAPPING REVIEWS **** ####


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
      
      dataFrame <- data.frame(company, date, title,dataCollect = dateCollect, status, location, recommend, outlook, 
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
  
  i <- 1
  dadosClean$date <- sapply(dadosClean$date, function(x){
    if(grepl('seconds',x)){
      x <- format(as.Date(dadosClean[i,]$dateCollect),'%b %d, %Y')
    }
    i <<- i + 1
    x
  })
  dadosClean$dateClean <- lubridate::mdy(as.character(dadosClean$date))

  dadosClean$title <- gsub('\"','', dadosClean$title)
  dadosClean$city  <- clearCity(dadosClean$location)
  dadosClean$state <- clearState(dados$location)  
  
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
  dadosClean$outlook <- sapply(dados$outlook, function(x){
    x <- gsub('Outlook','',x)
    if(grepl('of',x)){
      x <- 'NI'
    }
    x
  })
  dadosClean$ceo <- sapply(dadosClean$ceo, function(x){
    x <- gsub('of','',x)
    x
  })
  
  dadosClean$position <- sapply(dadosClean$status, function(x){
    ini <- regexpr('-',x)[[1]] + 1
    if(ini > 0){
      x <- trimws(substr(x,ini,str_length(x)))  
    }
    x  
  })
  dadosClean$status <-sapply(dadosClean$status, function(x){
    fim <- regexpr('-',x)[[1]] - 1
    if(fim > 0){
      x <- trimws(substr(x,0,fim))  
    }
    x
  })
  
  dadosClean$id <- paste('gd_',dadosClean$id,sep = '')
  dadosClean$site <- 'glassdoor'
  cols <-c("id","company","dateClean","dateCollect","rating","city","state","title","status","position","recommend","pros","cons","adviceManag","outlook","ceo","site")
  
  dadosClean <- dadosClean %>% select_(.dots = cols)
  #fwrite(dadosClean,'data/reviewGlassD.csv') 
  return(dadosClean)
}## END cleanReviewGlassD function

cleanReviewIndeed <- function(dados){
  dadosClean       <- dados
  dadosClean$dateClean <- lubridate::dmy(as.character(dadosClean$date))
  dadosClean$city  <- clearCity(dadosClean$location)
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
  
  dadosClean$pros <- gsub('Prós','',dadosClean$pros)#sapply(dadosClean$pros, function)
  dadosClean$cons <- gsub('Contras','',dadosClean$cons)#sapply(dadosClean$pros, function)
  dadosClean$adviceManag <- gsub('Conselhos para presidência:','',dadosClean$cons)
  
  dadosClean$position <- sapply(dadosClean$status,function(x){
    fim <- gregexpr('Ex-Funcionário|Funcionário Atual',x)[[1]]-1
    if(fim > 0){
      x <- substr(x,0,fim-1)
      x <- ifelse(x == '','NI',x)
    }
    x
  })
  
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
  cols <-c("id","company","dateClean","dateCollect","rating","city","state","title","status","position","recommend","pros","cons","adviceManag","outlook","ceo","site")
  
  dadosClean <- dadosClean[,..cols]
  #fwrite(dadosClean,'data/reviewIndeed.csv')
  return(dadosClean)
}## END cleanReviewIndeed function

cleanReviewLoveM <- function(dados){
  dadosClean <- dados
  dadosClean       <- convertDate(dados)
  dadosClean$dateClean <- lubridate::ymd(dadosClean$dateClean)
  dadosClean$company <- gsub('\n','',dadosClean$company)
  dadosClean$city  <- clearCity(dadosClean$location)
  dadosClean$state <- clearState(dadosClean$location)
  dadosClean$adviceManag <- sapply(dadosClean$recommend, function(x){
    if(grepl('conselhos',x, ignore.case = T)){
      x
    }else{
      x <- 'NI'
    }
  })
  dadosClean$recommend <- sapply(dadosClean$recommend, function(x){
    if(grepl('conselhos',x, ignore.case = T)){
      x <- 'NI'
    }else{
      x <- gsub('Recomenda a empresa:','',x)
      x <- gsub('\n','',x)
      x
    }
  })
  dadosClean$pros <- gsub('Prós:','',dadosClean$pros)#sapply(dadosClean$pros, function)
  dadosClean$cons <- gsub('Contras:','',dadosClean$cons)#sapply(dadosClean$pros, function)
  dadosClean$adviceManag <- gsub('Conselhos para presidência:','',dadosClean$cons)
  
  dadosClean$position <- sapply(dadosClean$status,function(x){
    fim <- gregexpr('Ex-funcionário',x)[[1]]-1
    if(fim > 0){
      x <- substr(x,0,fim)
      x <- ifelse(x == '','NI',x)
    }
    x
  })
  
  dadosClean$status <- sapply(dadosClean$status,function(x){
    if(grepl('Ex-funcionário|saiu',x)){
      x <- 'Ex-funcionário'
    }else{
      x <- 'Funcionário'
    }
    x
  })
  
  dadosClean$id <- paste('lm_',dadosClean$id,sep = '')
  dadosClean$site <- 'love mondays'
  cols <-c("id","company","dateClean","dateCollect","rating","city","state","title","status","position","recommend","pros","cons","adviceManag","outlook","ceo","site")
  
  
  dadosClean <- dadosClean[,cols]
  #fwrite(dadosClean,'data/reviewLoveM.csv')
} ## END cleanReviewLoveM function


cleanReviewFinal <- function(){
  load('data/reviews.RData')
  reviewClean <- reviews

  reviewClean[,c('pros','cons','title')] <- cleanText(reviewClean,c('pros','cons','title'),
                                                      stopWords = c('reviews','pra','empresa','�','trabalho','ser','muito','ponto','positivo','negativo',
                                                                    'can','like','make','many',
                                                                    stopwords('en'),stopwords('sp') )
                                                      ,specialChar = T)

  fwrite(dados,'data/reviewFinal.csv')  
}


dados <- fread('data/reviewFinal.csv')
dados[,c('title', 'pros', 'cons')] <- reviewClean[,c('title', 'pros', 'cons')]
fwrite(dados,'data/reviewFinal.csv')






















