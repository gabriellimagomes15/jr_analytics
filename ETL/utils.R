### SCRIPT COM INSTALAÇÃO DOS PACOTES UTILZIADOS E FUNÇÕES
### Gabriel Lima Gomes - Brasil - 2018

## R file with differents libraries and Functions to make analysis and text mining about
packages <- c("rvest","plyr","data.table","tm","stringr","stringdist","dplyr","tidytext","bit64","stringr","lubridate")

for(pkg in packages){
  print(pkg)
  if(!require(pkg, character.only = T)){
    install.packages(pkg)
  }
  require(pkg)
}


#### **** COLLECT DATA **** ####
#### >>>> Jobs Vacancies (vagas de emprego) ####

## Fun??o para fazer web scrapping na p?gina vagas de emprego do glassdoor
scrapGlassDoor <- function(urls = '', totalPages = 10){
  urlRoot <- 'https://www.glassdoor.com'
  #urls    <- c('https://www.glassdoor.com/Job/canada-data-scientist-jobs-SRCH_IL.0,6_IN3_KO7,21_IP','https://www.glassdoor.com/Job/us-data-scientist-jobs-SRCH_IL.0,2_IN1_KO3,17_IP')
  
  ## Loop para identificar aumtomaticamente o pa?s 
  country <- sapply(urls, function(x){
    final <- gregexpr('-',x)[[1]][1] - 1
    x <- substr(x,31,final)
    x
  })
  urlDF   <- data.frame(urls, country, stringsAsFactors = F)
  dataDF  <- data.frame()
  
  for(i in 1:dim(urlDF)[1]){
    urlPage    <- urlDF$urls[i]
    
    ## LENDO P?GINA E IDENTIFICANDO QUANTOS REGISTROS EXISTEM PARA IDENTIFICAR AUTOMATICAMENTE A QUANTIDADE DE PAGINAS QUE SER?O LIDAS
    urlPage2 <- paste(urlPage,'1','.htm', sep = '')
    page     <- read_html(urlPage2)
    totalReg <- as.integer(page %>% html_nodes('.jobsCount') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .)) 
    
    if(!identical(totalReg,integer(0) ) & !is.na(totalReg) ){
      totalPages <- (round(totalReg/30))
    }
    cat('Total Pages = ', totalPages,  ' - ', urlPage, '\n\n')
    
    tryCatch({
      ## FOR PARA LER AS P?GINAS COM TODOS AS VAGAS...TO READ THE PAGES WITH ALL JOB POST. TotalPages+1 is because the first page doesn't work with this url
      for( pag in 1:totalPages){
        cat('\n == > Page = ', pag)
        urlPage2 <- paste(urlPage,pag,'.htm', sep = '')
        cat(' ',urlPage2)
        page     <- read_html(urlPage2)
        links    <- page %>% html_nodes('.jlGrid.hover') %>% html_nodes('.jl .flexbox a') %>% html_attr('href')
        
        tryCatch({    
          ## FOR PARA LER A P?GINA COM TODOS OS DETALHES DA VAGA
          for(href in links){
            url        <- paste(urlRoot, href, sep = '')
            cat('\n \t --', url, '\t')
            pageJob    <- read_html(url)
            
            id <- pageJob %>% html_nodes('.jobViewHeader') %>% html_attr('id')
            id <- ifelse(identical(id,character(0)), '--', id)
            
            positJob   <- pageJob %>% html_nodes('.empInfo.tbl h2') %>% html_text()
            positJob   <- ifelse(identical(positJob,character(0)), 'NI', positJob)
            
            company    <- pageJob %>% html_nodes('.empInfo .ib') %>% html_text() %>% .[1]
            company    <- ifelse(is.na(company) , 'NI', company)
            
            city_state <- pageJob %>% html_nodes('.empInfo .subtle.ib') %>% html_text()
            city_state <- ifelse(identical(city_state,character(0)), 'NI', city_state)
            
            date <- pageJob %>% html_nodes('.empLinks .minor') %>% html_text()
            date <- ifelse(identical(date,character(0)), 'NI', date)
            
            descrip <- pageJob %>% html_nodes('.jobDescriptionContent') %>% html_text()
            descrip <- ifelse(identical(descrip,character(0)), 'NI', descrip)
            
            dateCollect <- Sys.Date()
            
            data   <- data.frame(id, positJob, company, city_state, date, descrip, dateCollect, country = urlDF$country[i], url)
            dataDF <- rbind.fill(dataDF,data)
            
            print('Saving data set')
            #fwrite(dataDF, paste('data/Glassdoor',Sys.Date(),'.csv', sep = '') )
            fwrite(dataDF, paste('data/glassDoor',urlDF$country[i],'.csv',sep = '') )
            
            Sys.sleep(4) # DELAY
          }## END FOR LINKS
        }, error = function(e){
          print(paste('ERROR READ LINKS: ', e , sep = ' ') )
        })
      }## END FOR TOTALPAGE
    }, error = function(e){
      print(paste('ERROR: ', e , sep = ' ') )
    })
  }## END FOR URLPAGE
  return(dataDF)
}


scrapJobsVac <- function(pageJob = '', id = 'ni', tagPosit= 'ni', tagCompany= 'ni', tagCityState= 'ni', tagDate= 'ni', 
                         tagDescrip= 'ni', country= 'ni', url= 'ni'){
  #data <- data.frame()
  
  #id <- pageJob %>% html_nodes('.jobViewHeader') %>% html_attr('id')
  #id <- ifelse(identical(id,character(0)), '--', id)
  
  positJob   <- pageJob %>% html_nodes(tagPosit) %>% html_text()
  positJob   <- ifelse(identical(positJob,character(0)), 'NI', positJob)
  
  company    <- pageJob %>% html_nodes(tagCompany) %>% html_text() %>% .[1]
  company    <- ifelse(is.na(company) , 'NI', company)
  
  city_state <- pageJob %>% html_nodes(tagCityState) %>% html_text()
  city_state <- ifelse(identical(city_state,character(0)), 'NI', city_state)
  
  date <- pageJob %>% html_nodes(tagDate) %>% html_text()
  date <- ifelse(identical(date,character(0)), 'NI', date)
  
  descrip <- pageJob %>% html_nodes(tagDescrip) %>% html_text()
  descrip <- ifelse(identical(descrip,character(0)), 'NI', descrip)
  
  dateCollect <- Sys.Date()
  
  data <- data.frame(id, positJob, company, city_state, date, descrip, dateCollect, country = country, url = url)
  
  return(data)
}


#### >>>> COMPANIES REVIEWS (Avalia??o das Empresas) ####
scrapReviewGlass <- function(urlPage = ''){
  if(urlPage == ''){
    stop('Informe uma URL')
  }
  print('Scrapping Reviews Glassdoor')
  ## DATA FRAME TO RETURN THE RESULT
  reviewDF   <- data.frame()
  
  ## READ THE FIRST PAGE TO GET THE COMPANY'S NAME AND TOTAL OF PAGES
  webPage  <- read_html(paste(urlPage,'1','.htm',sep = ''))
  company  <- webPage %>% html_nodes('.module.filterableContents .h2') %>% html_text()
  
  totalRew   <- as.numeric(webPage %>% html_nodes('.padTopSm.margRtSm.margBot.minor') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .))
  totalPages <- round(totalRew/10)
  
  tryCatch({
    ## LOOP TO READ EACH PAGE
    for( page in 1:totalPages){
      cat('==> Page: ',page)
      urlPage2 <- paste(urlPage,page,'.htm', sep = '')
      cat('\t',urlPage2)
      
      webPage <- read_html(urlPage2) ## SCRAPPING THE PAGE
      nodes   <- webPage %>% html_nodes('.empReview') ## GET NODES THAT CONTAIN THE REVIEWS
      
      review <- nodes[1]
      tryCatch({
        ## LOOP TO READ EACH NODE OF THE REVIEWS
        for(review in nodes){
          
          #O ID FICA EM UM ATRIBUTO DO ELEMENTO HTML, ENT?O, EST? PEGANDO TODOS OS ATRIBUTOS DO ELEMENTO ONTE EST? O ID E CONVERTENDO PARA VETOR CHAR
          id    <- review %>% html_attrs() %>% unlist(.) %>% paste(.,collapse = ' ') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) #html_attr('id') 
          cat('\n\t id = ', id)
          
          #O RATING FICA EM UM ATRIBUTO DO ELEMENTO HTML, ENT?O, EST? PEGANDO TODOS OS ATRIBUTOS DO ELEMENTO ONTE EST? O ID E CONVERTENDO PARA VETOR CHAR
          rating <- review %>% html_nodes('.rating span') %>% html_attrs() %>% unlist(.) %>% paste(.,collapse = ' ')
          
          #.date.subtle.small
          date  <- review %>% html_nodes('.date') %>% html_text() # html_nodes('.floatLt .date') %>% html_attr('datetime')
          date <- ifelse(identical(date, character(0)), 'NI', date )
          
          title <- review %>% html_nodes('.tbl .h2.summary') %>% html_text()
          title <- ifelse(identical(title, character(0)), 'NI', title )
          
          status   <- review %>% html_nodes('.tbl.reviewMeta .authorJobTitle.middle ') %>% html_text()
          status <- ifelse(identical(status, character(0)), 'NI', status )
          
          location <- review %>% html_nodes('.tbl.reviewMeta .authorLocation.middle') %>% html_text()
          location <- ifelse(identical(location, character(0)), 'NI', location )
          
          recommend <- review %>% html_nodes('.recommends .tightLt:nth-child(1)') %>% html_text()
          recommend <- ifelse(identical(recommend, character(0)), 'NI', recommend )
          
          outlook   <- review %>% html_nodes('.recommends .tightLt:nth-child(2)') %>% html_text()
          outlook <- ifelse(identical(outlook, character(0)), 'NI', outlook )
          
          ceo <- review %>% html_nodes('.recommends .tightLt:nth-child(3) .showDesk') %>% html_text()
          ceo <- ifelse(identical(ceo, character(0)), 'NI', ceo )
          
          pros <- review %>% html_nodes('.pros.mainText') %>% html_text()
          pros <- ifelse(identical(pros, character(0)), 'NI', pros )
          
          cons <- review %>% html_nodes('.cons.mainText') %>% html_text()
          cons <- ifelse(identical(cons, character(0)), 'NI', cons )
          
          adviceManag <- review %>% html_nodes('.adviceMgmt') %>% html_text()
          adviceManag <- ifelse(identical(adviceManag, character(0)), 'NI', adviceManag )
          
          dateCollect <- Sys.Date()
          
          dataFrame <- data.frame(id, rating, company, dateCollect, date, title, status, location, recommend, outlook, ceo, pros, cons, adviceManag)
          
          reviewDF <- rbind(reviewDF, dataFrame)
          fwrite(reviewDF,'data/reviews.csv')
        }## END FOR NODES
      }, error = function(e){
        print(paste('ERROR IN FOR NODES: ', e , sep = ' ') )
      })
    }## END FOR TOTALPAGES
  }, error = function(e){
    print(paste('ERROR IN FOR TOTALPAGES: ', e , sep = ' ') )
  })
  return(reviewDF)
}


### FUN??O PARA REALIZAR WEB SCRAPPING DE AVALIA??ES SOBRE EMPRESAS. 
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


#### **** TRANSFORM / PRE-PROCESS **** ####
#### >>>> Jobs Vacancies (vagas de emprego) ####

getSkills <- function(data,column = ''){
  print('Get Skills')
  vocSkill   <- read.table('data/dic/dictonarySkills.txt',sep = '\n', stringsAsFactors = F)
  skillFinal <- c()
  apply(data, 1, function(x){
    skill  <- c('')
    words  <- unlist(strsplit(x[column], ' '))
    for(token in words){
      a <- 1-stringdist(tolower(token), vocSkill$V1, method = 'jw')
      if( max(a)  > 0.9){
        skill <- paste(skill, vocSkill[which.max(a),])
      }
    }
    skillFinal <<- c(skillFinal, skill)
  })
  return(skillFinal)
}


getEducation <- function(data,column = ''){
  print('Get Education')
  vocEduc <- read.table('data/dic/education.txt',sep = '\n')
  eduFinal <- c()
  apply(data, 1, function(x){
    educ  <- c('')
    words <- unlist(strsplit(x[column], ' '))
    for(token in words){
      a <- 1-stringdist(tolower(token), vocEduc$V1, method = 'jw')
      if( max(a)  > 0.9){
        educ <- paste(educ, vocEduc[which.max(a),])
      }
    }
    eduFinal <<- c(eduFinal, educ)
  })
  return(eduFinal)
}

getLanguage <- function(data,column = ''){
  print('Get Languages')
  vocLang <- read.table('data/dic/languages.txt',sep = '\n', stringsAsFactors = F)
  langFinal <- c()
  apply(data, 1, function(x){
    lang  <- c('')
    words <- unlist(strsplit(x[column], ' '))
    for(token in words){
      a <- 1-stringdist(tolower(token), vocLang$V1, method = 'jw')
      if( max(a)  > 0.9){
        lang <- paste(lang, vocLang[which.max(a),])
      }
    }
    langFinal <<- c(langFinal, lang)
  })
  return(langFinal)
}

clearPostJob <- function(data, column = ''){
  print('Clear Posit Jobs')
  #data <- dadosClean
  #column = "positJob"
  data <- data %>% 
            select_(.dots = column)
  postJobClean <- sapply(data, function(x){
          x <- gsub("[[:punct:]]", " ", x) # remove punctuation
          x <- gsub("[[:digit:]]", " ", x) # remove numbers
          x <- gsub("http\\w+", " ", x)    # remove html links
          x <- gsub("\\W"," ", x) # remove not word
          x <- gsub("[ \t]{2,}", " ", x) # remove double space
          x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
          x
  })  
  return(postJobClean)
}


clearCompany <- function(data, column = ''){
  print('Clear Company')
  data <- data %>% select_(.dots = column)
  companyClean <- sapply(data, function(x){
    x <- gsub("[[:punct:]]", " ", x) # remove punctuation
    x <- gsub("[[:digit:]]", " ", x) # remove numbers
    x <- gsub("^[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x
  })
  return(companyClean)
}

clearCity <- function(data, column = ''){
  print('Clear City')
  #data <- data %>% select_(.dots = column)
  
  cityClean <- sapply(data, function(x){
    x <- gsub("--", 'NI',x)
    x <- gsub("–", '',x) ## CLEAR ESPECIAL CHAR IN LINUX
    x <- gsub('^A\\sa\\W+A|^a\\W+A','',iconv( enc2native(x), to = "ASCII//TRANSLIT")) ##CLEAR ESPECIAL CHAR IN WINDOWS
    x <- gsub('^A\\sa\\W+A|^a\\W+A','',x)
    x <- gsub("[ \t]{2,}", "", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    #x <- gsub(",\\s+\\w+","", x) # 
    x <- gsub("\\,\\W+(\\w+).*$","", x)
    x <- gsub("^\\W+|\\W+$","", x) # remove not word
    if(x == ''){
      x <- 'ni'
    }
    x
  })
  return(cityClean)
}


clearState <- function(data){
  print('Clear State')
  #data <- data %>% select_(.dots = column)
  
  stateClean <- sapply(data, function(x){
    x <- gsub('\n','',x)
    pos <- regexpr('\\,',x)[1]
    if(pos > 0){
      x   <- substr(x, pos+1, str_length(x))
    }else{
      x <- 'ni'
    }
    x <- gsub("\\W+", "", x) # remove double space
    x
  })
  return(stateClean)
}


clearDate <- function(data){
  print('Clear Date')
  dados2 <- data.frame(t(apply(data, 1, function(x){
    if (grepl('days', x['date'])){
      days  <- as.numeric( gsub("([0-9]+).*$", "\\1", x['date']) )
      x['dateClean'] <- as.character(as.Date(x['dateCollect']) - days)
      
    }else{
      x['dateClean'] <- x['dateCollect']
    }
    x
  })
  ),stringsAsFactors = F
  )
  return(dados2)
}

# >>>> END (FIM) Jobs Vacancies (vagas de emprego) ####
#### >>>> REVIEWS COMPANIES (avalia??es das empresas) ####
convertDate <- function(data){
  return <- data.frame(t( apply(data,1,function(x){
    if(grepl('dias|dias',x['date']) ){
      days <- as.numeric(stringr::str_extract( x['date'], '[[:digit:]]+')) #as.numeric( gsub("^.*([0-9]+).*$", "\\1", x['date']) )
      x['dateClean'] <- as.character( as.Date(x['dateCollect']) - days)
    
    }else if(grepl('meses|mes',x['date'] )){
      days <- as.numeric(stringr::str_extract( x['date'], '[[:digit:]]+')) * 30 #as.numeric( gsub("^.*([0-9]+).*$", "\\1", x['date']) ) * 30
      x['dateClean'] <- as.character( as.Date(x['dateCollect']) - days)
    
    }else if(grepl('anos|anos',x['date'] )){
      days <- as.numeric(stringr::str_extract( x['date'], '[[:digit:]]+')) * 365 #as.numeric( gsub("^.*([0-9]+).*$", "\\1", x['date']) ) * 365
      x['dateClean'] <- as.character( as.Date(x['dateCollect']) - days)
    
    }else{
      x['dateClean'] <- x['dateCollect']
    }
    #x['dateClean'] <- lubridate::ymd(as.character(x['dateClean']) )
    x
  })), stringsAsFactors = F)
  return(return)
}


# >>>> END (FIM) REVIEWS COMPANIES (avalia??es das empresas) ####





## Function to clear comments. Need be a data frame
## Obs: Para eliminar a duplicidade de registro, será utilizada a primeira coluna do parametro 'column';
cleanText <- function(data,column = '', stopWords = "",stemming = F,specialChar = F,delDup = F){
  print('Clean Data')
  if(column[1] == ''){
    warning('ATENÇÃO!!!!\n A coluna (atributo) selecionada para limpeza é: ', colnames(data)[1])
    column <- colnames(data)[1]
  }
  if(class(data)[1] == "list"){
    dataClear = do.call("rbind", lapply(data, as.data.frame));
  }else{
    dataClear <- data
  }
  #commentsDF = subset(commentsDF, select = c(text));
  # Text Cleasing
  #textDF <- dados
  #column <- col
  #stopWords <- ''
  
  dataClear <- dataClear %>%
                select_(.dots = column)
  
  print("Clean Text")
  retorno  <- lapply(dataClear,function(x){
    x <- gsub('http.* *', '', x)
    x <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", x)
    x <- gsub(":", "", x)
    x <- gsub("@\\w+", "", x)       # remove at people
    x <- gsub("[[:punct:]]", "", x) # remove punctuation
    x <- gsub("[[:digit:]]", "", x) # remove numbers
    x <- gsub("http\\w+", "", x)    # remove html links
    x <- gsub("\\W"," ", x) # remove not word
    x <- gsub("[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x <- tolower(x)
    x <- removeWords(x,c(stopWords,stopwords("pt"))) ## remove stop words
    x <- trimws(x)
    x
  });
  
  # remove special character
  if(specialChar){
    print("Clean Special Character")
    retorno <- lapply(retorno,function(x){iconv( enc2native(x), to = "ASCII//TRANSLIT") })
  }
  
  if(stemming){
    print("Clean Stemming")
    retorno <- lapply(retorno,function(x){stemDocument(x,language = "portuguese")})
  }
  
  # verificar quantas colunas. Se o retorno vai ser convertido em dataframe ou um vetor
  if(length(column) > 1){
    retorno <- data.frame(retorno, stringsAsFactors = F)
  }else{
    retorno <- unlist(retorno)
  }
  
  if(delDup){
    # Removing Duplicate Rows
    warning('\n ===> ATENÇÃO!!!\n Será verificado a duplicidade de registros na coluna : ', toupper(column[1]),'\n')
    
    data <- retorno %>% select_(column[1])
    
    retorno$DuplicateFlag <- duplicated(data);
    retorno <- subset(retorno, retorno$DuplicateFlag =="FALSE");
    retorno <- subset(retorno, select = -c(DuplicateFlag))  
  }
  
  dataClear <- retorno
  
  return(dataClear)
}

#### **** DICTONARY OF SKILL IN DATA SCIENCE **** ####
dictonarySkills <- function(){
  skills <- c()
  url    <- 'http://www.datascienceglossary.org/#algorithm'
  page   <- read_html(url)
  skil   <- page %>% html_nodes('.row .col-md-3') %>% html_text()
  skills <- c(skills,skil)
  
  url    <- 'http://bigdata-madesimple.com/big-data-a-to-zz-a-glossary-of-big-data-terminology/'
  page   <- read_html(url)
  skil   <- page %>% html_nodes('.ptb60 strong') %>% html_text()
  skills <- c(skills,skil)
  
  write(skills, 'dictonarySkills.txt')
  
  a <- read.table('t.txt',sep = '\n', stringsAsFactors = F)
  skills <- c(skills,a$V1)
  
  write(skills, 'dictonarySkills.txt')
  skills <- tolower(skills)
  skills <- unique(skills)
  
  write(skills, 'dictonarySkills.txt')
}

### FUN??O PARA ELIMINAR REGISTROS DUPLICADOS
delDup <- function(data){
  # Removing Duplicate tweets and Removing null line
  data[,"DuplicateFlag"] = duplicated(data$id);
  data = subset(data, data$DuplicateFlag =="FALSE");
  data = subset(data, select = -c(DuplicateFlag))
  
  return((data))
}

