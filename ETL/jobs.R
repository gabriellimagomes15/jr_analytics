### script para fazer web scrapping de vagas de emprego e avalia??es de empresas
### Autor: Gabriel Lima Gomes - Dezembro 2017.


#### **** WEB SCRAPPING JOBS VACANCIES **** ####


### Fun??o para fazer web scrapping de vagas de emprego do site glassdoor.com
jobsGlassDoor <- function(){
  ## URL RAIZ PARA ACESSAS A LINKS SECUNDÁRIOS DENTRO DA PÁGINA.
  urlRoot <- 'https://www.glassdoor.com'
  
  ## URLs DE CONSULTAS DE VAGAS DE EMPREGO
    # cargo: data scientist; Pa??s: Canada, US e Brazil
    # a url é fornecida até o parametro que indica a página '_IP', no script as páginas são inseridas dinâmicamente
  urls    <- c('https://www.glassdoor.com/Job/canada-data-scientist-jobs-SRCH_IL.0,6_IN3_KO7,21_IP',
               'https://www.glassdoor.com/Job/us-data-scientist-jobs-SRCH_IL.0,2_IN1_KO3,17_IP',
               'https://www.glassdoor.com/Job/brazil-data-scientist-jobs-SRCH_IL.0,6_IN36_KO7,21_IP')
  
  ## Loop para identificar aumtomaticamente o pa??s 
  country <- sapply(urls, function(x){
    final <- gregexpr('-',x)[[1]][1] - 1
    x <- substr(x,31,final)
    x
  })
  
  ## DATA FRAME PARA ARMAZENAR AS INFORMAÇÕES DE CONSULTAS
  urlDF   <- data.frame(urls, country, stringsAsFactors = F)
  dataDF  <- data.frame()## DATA FRAME QUE IRÁ ARMAZENAS OS DADOS FINAIS
  
  ## FOR PARA LOOP NAS URLs DE CONSULTAS FORNECIDAS
  for(i in 1:dim(urlDF)[1]){
    urlPage <- urlDF$urls[i]
    
    ## LENDO PÁGINA INICIAL E IDENTIFICANDO QUANTOS REGISTROS EXISTEM, ASSIM, IDENTIFICAR AUTOMATICAMENTE A QUANTIDADE DE PAGINAS QUE SERÃO LIDAS
    urlPage2 <- paste(urlPage,'1','.htm', sep = '')
    page     <- read_html(urlPage2)
    totalReg <- as.integer(page %>% html_nodes('.jobsCount') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .)) 
    
    if(!identical(totalReg,integer(0) ) & !is.na(totalReg) ){
      totalPages <- (round(totalReg/30))
    }else{
      totalPages <- 10
    }
    cat('\n\n ****Total Pages = ', totalPages,  ' - ', urlPage, '\n\n')
    
    tryCatch({
      ## FOR PARA LER AS P?GINAS COM TODOS AS VAGAS...TO READ THE PAGES WITH ALL JOB POST.
      for( pag in 1:totalPages){
        cat('\n == > Page = ', pag)
        urlPage2 <- paste(urlPage,pag,'.htm', sep = '')
        cat(' ',urlPage2)
        page     <- read_html(urlPage2)
        ## CAPTURANDO OS LINKS DE TODAS AS VAGAS DA RESPECTIVAS PÁGINA
        links    <- page %>% html_nodes('.jlGrid.hover') %>% html_nodes('.jl .flexbox a') %>% html_attr('href')
        
        tryCatch({    
          ## FOR PARA LER A PÁGINA COM TODOS OS DETALHES DA VAGA
          for(href in links){
            url <- paste(urlRoot, href, sep = '')
            cat('\n \t --', url, '\t')
            
            pageJob <- read_html(url)
            
            ## EXTRAINDO O ID DA VAGA. O ID ESTÁ INSERIDO NO ATRIBUTO 'ID' DA TAG
            id <- pageJob %>% html_nodes('.jobViewHeader') %>% html_attr('id')
            id <- ifelse(identical(id,character(0)), 'NI', id)
            
            ## CHAMANDO FUNÇÃO PARA EXTRAIR INFORMAÇÕES DA VAGA
            data <- scrapJobsVac(pageJob = pageJob, id = id,tagPosit = '.empInfo.tbl h2',tagCompany = '.empInfo .ib',
                                 tagCityState = '.empInfo .subtle.ib',tagDate = '.empLinks .minor',
                                 tagDescrip = '.jobDescriptionContent', country = urlDF$country[i], url = url)
            
            dataDF <- rbind.fill(dataDF,data)
            
            pathSave <- paste('data/jobs/jobsGlassDoor',urlDF$country[i],Sys.Date(),'.csv',sep = '')
            print(paste('Saving data set in ',pathSave) )
            
            fwrite(dataDF,pathSave)
            
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
}## END jobsGlassDoor function

### Função para fazer web scrapping de vagas de emprego do site lovemondays.com.br
jobsLoveM <- function(){
  ## URL RAIZ PARA ACESSAS A LINKS SECUNDÁRIOS DENTRO DA PÁGINA.
  urlRoot <- 'https://www.lovemondays.com.br'
  
  ## URLs DE CONSULTAS DE VAGAS DE EMPREGO
    # cargo: data analyst; Pa??s: Brasil
    # a url é fornecida até o parametro que indica a página '_IP', no script as páginas são inseridas dinâmicamente
  urls    <- c('https://www.lovemondays.com.br/pesquisa/vaga/pagina/1?external_job_city_id=&external_job_city_name=&q=Data+Scientist')
            #c('https://www.lovemondays.com.br/pesquisa/vaga/pagina/1?external_job_city_id=&external_job_city_name=&q=Data+Analyst')
  country <- c('brazil')
  urlDF   <- data.frame(urls, country, stringsAsFactors = F)
  dataDF  <- data.frame()
  
  
  for(i in 1:dim(urlDF)[1]){
    urlPage <- urlDF$urls[i]
    
    page <- read_html(urlPage) 
    totalPages <- page %>% html_nodes('.lm-Pagination-list-item.is-last a') %>% html_attr('href') %>% 
                      gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) %>% as.numeric(.)
    print(totalPages)
    if(identical(totalPages,integer(0) ) | is.na(totalPages) ){
      totalPages <- 5
    }
    cat('\n\n ****Total Pages = ', totalPages,  ' - ', urlPage, '\n\n')
    
    tryCatch({
      ## FOR PARA LER AS PÁGINAS COM TODOS AS VAGAS...TO READ THE PAGES WITH ALL JOB POST.
      for( pag in 1:totalPages){
        cat('\n == > Page = ', pag)
        
        ## INSERINDO O NÚMERO DA PÁGINA DINAMICAMENTE. 
        urlPage2 <- str_replace(urlPage, '[[:digit:]]',as.character(pag) )
        cat(' ',urlPage2)
        page     <- read_html(urlPage2)
        links    <- page %>% html_nodes('.lm-List-default-row.lm-List-jobs-row a') %>% html_attr('href')
        
        tryCatch({    
          ## FOR PARA LER A PÁGINA COM TODOS OS DETALHES DA VAGA
          for(href in links){
            url <- paste(urlRoot, href, sep = '')
            cat('\n \t --', url, '\t')
            
            pageJob <- read_html(url)
            
            ## Bloco para recuperar o ID da vaga. O ID ESTÁ JUNTO COM O TÍTULO DA VAGA NO SEGUINTE FORMATO '(#id)'
            textId <- pageJob %>% html_nodes('title') %>% html_text() %>% trimws(.)
            idJob <- stringr::str_extract(string = textId, pattern = "#[[:digit:]]+")
            
            ## BLOCO PARA RECUPERAR O CARGO, NOME DA EMPRESA E LOCALIZAÇÃO
             # ESSAS INFORMAÇÕES ESTÃO INSERIDAS TODAS JUNTAS, SEPARADAS COM '<br>' E/OU PELA PALAVRA 'em', ESSA TAG FOI SUBSTITUIDA POR '\n'
             # Foi necesário adotar essa estratégia por causa da estrutura da página.
            header <- pageJob %>% html_nodes('header section h1') %>% gsub("<br>"," \n", .) %>% str_split(.,'\n|em')
            
            
            ## BLOCO PARA RECUPERAR A DATA DE POSTAGEM DA VAGA
            json <- pageJob %>% html_nodes("[type='application/ld+json']") %>% html_text()
            d    <- json[which(grepl("datePosted",json))]
            date <- stringr::str_extract(string = d,pattern = "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}") #EXTRAINDO A DATA QUE ESTÁ NO FORMATO 'YYYY-MM-DD'
            
            ## CHAMANDO FUNÇÃO PARA CAPTURAR OS DADOS DA VAGA
            data <- scrapJobsVac(pageJob = pageJob,id = idJob, tagDescrip = ".company-job-description", 
                                 country = urlDF$country[i], url = url)
            
            ## INSERINDO NO DF OS VALORES EXTRIDOS FORA DA FUNÇÃO scrapJobsVac
            data$date       <- date
            data$positJob   <- header[[1]][2]
            data$company    <- header[[1]][3]
            data$city_state <- header[[1]][4]
            
            dataDF <- rbind.fill(dataDF,data)
            
            cat('\n Saving data set in data/jobsLM')
            #fwrite(dataDF, paste('data/Glassdoor',Sys.Date(),'.csv', sep = '') )
            fwrite(dataDF, paste('data/jobs/jobsLM',urlDF$country[i],Sys.Date(),'.csv',sep = '') )
            
            pathSave <- paste('data/jobsLM','urlDF$country[i]',Sys.Date(),'.csv',sep = '')
            print(paste('Saving data set in ',pathSave) )
            
            fwrite(dataDF,pathSave)
            
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
}## END jobsLoveM function


jobsIndeed <- function(){
  urlRoot <- 'https://www.indeed.com.br/viewjob'
  urls    <- c('https://www.indeed.com.br/empregos?q=data+scientist&l=Brazil&start=',
               'https://www.indeed.com.br/empregos?q=cientista+de+dados&l=Brazil&start=')
  
  ## Loop para identificar aumtomaticamente o pa??s da consulta 
  country <- sapply(urls, function(x){
    ini   <- gregexpr('l=',x)[[1]][1] + 2
    final <- gregexpr('&start',x)[[1]][1] - 1
    x <- substr(x,ini,final)
    x
  })
  
  urlDF   <- data.frame(urls, country, stringsAsFactors = F)
  dataDF  <- data.frame()
  
  for(i in 1:dim(urlDF)[1]){
    urlPage <- urlDF$urls[i]
    
    ## LENDO PÁGINA E IDENTIFICANDO QUANTOS REGISTROS EXISTEM PARA IDENTIFICAR AUTOMATICAMENTE A QUANTIDADE DE PAGINAS QUE SERÃO LIDAS
    urlPage2 <- paste(urlPage,'0', sep = '')
    page     <- read_html(urlPage2)
    
    totalReg <- page %>% html_nodes('#searchCount') %>% html_text() %>% stringr::str_extract(., "de [[:digit:]]+") %>% 
                  stringr::str_extract(.,"[[:digit:]]+") %>% as.numeric(.)
    
    if(!identical(totalReg,integer(0) ) & !is.na(totalReg) ){
      totalPages <- (round(totalReg/10))
    }else{
      totalPages <- 5
    }
    cat('\n\n ****Total Pages = ', totalPages,  ' - ', urlPage, '\n\n')
    
    tryCatch({
      ## FOR PARA LER AS PÁGINAS COM TODOS AS VAGAS.
        # NESSE CASO AS PÁGINAS SÃO DIFINIDAS PELO O INICIO DO REGISTRO QUE SERÁ MOSTRADO QUE SÃO DE 10 EM 10 REGISTROS POR PÁGINAS
      for( pag in 0:totalPages){
        cat('\n == > Page = ', pag)
        urlPage2 <- paste(urlPage,pag*10, sep = '')
        cat(' ',urlPage2)
        
        page  <- read_html(urlPage2)
        links <- page %>% html_nodes('.row.result h2 a') %>% html_attr("href")
        
        tryCatch({    
          ## FOR PARA LER A PÁGINA COM TODOS OS DETALHES DA VAGA
          for(href in links){
            ## INSERINDO O SUFIXO PARA LER A PÁGINA DA VAGA
            sufixo  <- substr(href, regexpr('\\?',href), str_length(href))
            url  <- paste(urlRoot,sufixo, sep = '')
            
            cat('\n \t --', url, '\t')
            
            pageJob <- read_html(url)
            
            idJob <- stringr::str_extract(url,"jk=\\w+")
            idJob <- ifelse(idJob == "", 'NI', idJob)
            
            data <- scrapJobsVac(pageJob = pageJob, id = idJob,tagPosit = '[data-tn-component="jobHeader"] .jobtitle',
                                 tagCompany = '[data-tn-component="jobHeader"] .company',
                                 tagCityState = '[data-tn-component="jobHeader"] .location',tagDate = '.result-link-bar .date',
                                 tagDescrip = '#job_summary', country = urlDF$country[i], url = url)
            
            dataDF <- rbind.fill(dataDF,data)

            pathSave <- paste('data/jobs/jobsIndeed',urlDF$country[i],Sys.Date(),'.csv',sep = '')
            print(paste('Saving data set in ',pathSave) )
            
            fwrite(dataDF,pathSave)
            
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
}## END jobsIndeed function

#### **** PREPROCESS JOBS VACANCIES DATA **** ####

cleanJobsGlassDoor <- function(dados){
  dadosClean <- dados
  dadosClean <- delDup(dadosClean)
  
  dadosClean$descClean <- cleanText(data = dadosClean,column = 'descrip', stopWords = stopwords('en'))
  
  
  ## Extraindo do campo 'descrip' da vaga dados sobre skill, education e language, requeridos para a vaga.
  dadosClean$skills     <- getSkills(dadosClean,column = 'descClean')
  dadosClean$education  <- getEducation(dadosClean,'descClean')
  dadosClean$language   <- getLanguage(dadosClean,'descClean')
  
  ## Limpando o campo 'positJob' 
  dadosClean$positClean <- clearPostJob(dadosClean,'positJob')
  
  print("Clear Company")
  ## Limpando o campo 'company' 
  if (length(dadosClean[is.na(dadosClean$company),]$company) > 0)
    dadosClean[is.na(dadosClean$company),]$company <- 'NI'
  
  dadosClean$companyClean <- clearCompany(dadosClean,'company')
  
  ## Separando 'city' e 'state' do campo 'city_state'
  dadosClean$cityClean    <- clearCity(dadosClean$city_state)
  dadosClean$cityClean    <- clearCity(dadosClean$cityClean)
  
  dadosClean$stateClean   <- clearState(dadosClean$city_state)
  
  dadosClean$dateClean    <- ""
  dadosClean              <- clearDate(data = dadosClean)
  
  print("Clean Canada Cities")
  ## GET THE PROVINCE OF THE CANADA
  ## CAPTURANDO A PROVINCIAS DAS CIDADES DO CANADA E ESTADOS DO BRASIL. É NECESSÁRIO PORQUE NO ANUNCIO DA VAGA SÓ TEM A CIDADE PARA ESTES PAISES
  canadaCity           <- data.frame(jsonlite::fromJSON('data/cities/canadaCities.json'), stringsAsFactors = F)
  colnames(canadaCity) <- c('city','prov')
  
  dadosClean[dadosClean$country == 'canada',]$stateClean <- unlist( sapply(dadosClean[dadosClean$country == 'canada',]$cityClean, function(x){
    r <- canadaCity[tolower(canadaCity$city) == tolower(x),'prov']
    if( length(r) > 0){
      r[1] 
    }else{
      x
    }
  }) )
  
  print("Clean Brazil Cities")
  brazilCity <- fread('data/cities/brasilCities.csv',encoding = 'UTF-8') 
  dadosClean[dadosClean$country == 'brazil',]$stateClean <- unlist( sapply(dadosClean[dadosClean$country == 'brazil',]$cityClean, function(x){
    r <- brazilCity[tolower( iconv( enc2native(brazilCity$city), to = "ASCII//TRANSLIT")  ) == tolower(x),'state']
    if( length(r) > 0){
      r[1] 
    }else{
      x
    }
  }) )
  
  
  print("Repace Prov/States")
  ### Cidades/estados que devem passar por transformação para inserir a sigla da provincia
  city_replace <- data.frame( city = c('Fort Saint John','RiviA?re-du-Loup','ByWard Market',
                                 'New Brunswick', 'Saguenay', 'Ontario',"Alberta",
                                 "FairfaxVA"),
                              sig = c('BC','QC','ON','NB','QC','ON','AB','VA'),stringsAsFactors = F)
  dadosClean[is.na(dadosClean)] <- 'NI'
  
  apply(city_replace, 1, function(x){
    if (dim(dadosClean[dadosClean$stateClean == x['city'],])[1] > 0)
      dadosClean[dadosClean$stateClean == x['city'],]$stateClean <<- x['sig']
  })
  
  
  #dadosClean[dadosClean$stateClean == 'Fort Saint John',]$stateClean <- 'BC'
  #dadosClean[dadosClean$stateClean == 'RiviA?re-du-Loup',]$stateClean <- 'QC'
  #dadosClean[dadosClean$stateClean == 'ByWard Market',]$stateClean <- 'ON'
  #dadosClean[dadosClean$stateClean == 'New Brunswick',]$stateClean <- 'NB'
  #dadosClean[dadosClean$stateClean == 'Saguenay',]$stateClean <- 'QC'
  #dadosClean[dadosClean$stateClean == 'Ontario',]$stateClean <- 'ON'
  
  dadosClean$site <- "glassdoor"
  dadosClean      <- dadosClean[,c('id','positClean','companyClean','dateClean', 'dateCollect','descClean','skills','education','language',
                                   'cityClean','stateClean','country','url',"site")]
  
  #data.table::fwrite(dadosClean, 'data/jobsGlassDClean.csv')
  return(dadosClean)
}


cleanJobsLoveMond <- function(dados){
  #dados <- fread('data/jobsLMBrasil2018-02-09.csv',encoding = 'UTF-8')
  dadosClean    <- dados
  
  dadosClean$descClean <- cleanText(data = dadosClean,column = 'descrip', stopWords = stopwords('en'))
  
  ## Extraindo do campo 'descrip' da vaga dados sobre skill, education e language, requeridos para a vaga.
  dadosClean$skills     <- getSkills(dadosClean,column = 'descClean')
  dadosClean$education  <- getEducation(dadosClean,'descClean')
  dadosClean$language   <- getLanguage(dadosClean,'descClean')
  
  print("Clean Position")
  ## Limpando o campo 'positJob' 
  dadosClean$positClean <- sapply(dadosClean$positJob, function(x){
    x <- gsub("[[:punct:]]", " ", x) # remove punctuation
    x <- gsub("[[:digit:]]", " ", x) # remove numbers
    x <- gsub("http\\w+", " ", x)    # remove html links
    x <- gsub("\\W"," ", x) # remove not word
    x <- gsub("[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x <- gsub("Vaga de", "", x)
    x
  }) 
  
  print("Clean Company")
  ## Limpando o campo 'company' 
  if (length(dadosClean[is.na(dadosClean$company),]$company) > 0)
    dadosClean[is.na(dadosClean$company),]$company <- 'NI'
  dadosClean$companyClean <- companyClean <- sapply(dadosClean$company, function(x){
    x <- gsub("[[:punct:]]", " ", x) # remove punctuation
    x <- gsub("[[:digit:]]", " ", x) # remove numbers
    x <- gsub("^[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x
  })
  
  print("Clean City")
  ## Separando 'city' e 'state' do campo 'city_state'
  dadosClean$cityClean <- sapply(dadosClean$city_state, function(x){
    x <- gsub("[ \t]{2,}", "", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x <- gsub("\\-\\W+(\\w+).*$","", x)
    x <- gsub("^\\W+|\\W+$","", x) # remove not word
    if(x == ''){
      x <- 'NI'
    }
    x
  })
  
  ## Separando 'state' do campo 'city_state'
  dadosClean$stateClean <- sapply(dadosClean$city_state, function(x){
    x <- gsub('\n','',x)
    pos <- regexpr('\\-',x)[1]
    if(pos > 0){
      x   <- substr(x, pos+1, str_length(x))
    }else{
      x <- 'NI'
    }
    x <- gsub("\\W+", "", x) # remove double space
    x
  })
  
  dadosClean$dateClean <- dadosClean$date
  
  dadosClean$site <- "loveMondays"
  dadosClean      <- dadosClean[,c('id','positClean','companyClean','dateClean', 'dateCollect','descClean','skills','education','language',
                              'cityClean','stateClean','country','url',"site")]
  dadosClean <- delDup(dadosClean)
  
  #fwrite(dadosClean,'data/jobsLoveMClean.csv')
  
  return(dadosClean)
}


cleanJobsIndeed <- function(dados){
  #dados <- fread('data/jobsIndeedBrasil2018-02-10.csv', encoding = 'UTF-8')
  dadosClean    <- dados
  
  dadosClean$descClean <- cleanText(data = dadosClean,column = 'descrip', stopWords = stopwords('en'))
  
  ## Extraindo do campo 'descrip' da vaga dados sobre skill, education e language, requeridos para a vaga.
  dadosClean$skills     <- getSkills(dadosClean,column = 'descClean')
  dadosClean$education  <- getEducation(dadosClean,'descClean')
  dadosClean$language   <- getLanguage(dadosClean,'descClean')
  
  print("Clean Position")
  ## Limpando o campo 'positJob' 
  dadosClean$positClean <- sapply(dadosClean$positJob, function(x){
    x <- gsub("[[:punct:]]", " ", x) # remove punctuation
    x <- gsub("[[:digit:]]", " ", x) # remove numbers
    x <- gsub("http\\w+", " ", x)    # remove html links
    x <- gsub("\\W"," ", x) # remove not word
    x <- gsub("[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x <- gsub("Vaga de", "", x)
    x
  }) 
  
  ## Limpando o campo 'company' 
  if (length(dadosClean[is.na(dadosClean$company),]$company) > 0)
    dadosClean[is.na(dadosClean$company),]$company <- 'NI'
  
  print("Clean Company")
  dadosClean$companyClean <- sapply(dadosClean$company, function(x){
    x <- gsub("[[:punct:]]", " ", x) # remove punctuation
    x <- gsub("[[:digit:]]", " ", x) # remove numbers
    x <- gsub("^[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x
  })
  
  print("Clean city")
  ## Separando 'city' e 'state' do campo 'city_state'
  dadosClean$cityClean <- sapply(dadosClean$city_state, function(x){
    x <- gsub("[ \t]{2,}", "", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x <- gsub("\\,\\W+(\\w+).*$","", x)
    x <- gsub("^\\W+|\\W+$","", x) # remove not word
    if(x == ''){
      x <- 'NI'
    }
    x
  })
  
  print("Clean state")
  ## Separando 'state' do campo 'city_state'
  dadosClean$stateClean <- sapply(dadosClean$city_state, function(x){
    x <- gsub('\n','',x)
    pos <- regexpr('\\,',x)[1]
    if(pos > 0){
      x   <- substr(x, pos+1, str_length(x))
    }else{
      x <- 'NI'
    }
    x <- gsub("\\W+", "", x) # remove double space
    x
  })
  
  dadosClean$dateClean <- ""
  dadosClean         <- convertDate(dadosClean)
  dadosClean$site    <- "indeed"
  
  dadosClean <- dadosClean[,c('id','positClean','companyClean','dateClean', 'dateCollect','descClean','skills','education','language',
                              'cityClean','stateClean','country','url',"site")]
  
  #fwrite(dadosClean,'data/jobsIndeedClean.csv')
  dadosClean <- delDup(dadosClean)
  
  return(dadosClean)
  
} ## END cleanJobsIndeed function


cleanJobsFinal <- function(data){
  #data    <- data.table::fread('data/jobsFinal.csv',encoding = 'UTF-8')
  
  data[data$country == 'brazil']$state <- paste(data[data$country == 'brazil']$state,'_br',sep = '')
  data[grepl('NI',data$state)]$state <- 'NI'
  data$language <- gsub('espanhol','spanish',data$language)
  data$education <- gsub('bacharelado','bachelor',data$education)
  data$education <- gsub('mestre','master',data$education)
  
  data$language <- sapply(data$language, function(x){
    arrayLang <- unlist( str_split(x,' ') )
    uniqueLang <- paste(unique(arrayLang),collapse = ' ')
    uniqueLang
  })
  
  data$education <- sapply(data$education, function(x){
    x <- unlist( str_split(x,' ') )
    x <- paste(unique(x),collapse = ' ')
    x
  })
  data$city <- gsub('S\\?o Paulo','São Paulo',data$city)
  data$city <- gsub('Sao Paulo','São Paulo',data$city)
  
  #fwrite(data,'data/jobsFinal.csv')
  return(data)
}
