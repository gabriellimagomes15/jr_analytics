#path <- 'E:/Curso Cientista de Dados/Projeto Final/analysisFacebook/jobsAnalysis'
#path <- 'E:/jobsAnalysis'

p <- 1
path <- c('C:/Users/gabri/Documents/GitHub/jr_analytics/','~/Documents/jr_analytics/','G:/GitHub/jr_analytics')
setwd(path[p])
source(paste(path[p],'/ETL/utils.R', sep = '') )
source(paste(path[p],'/ETL/jobs.R', sep = '') )
source(paste(path[p],'/ETL/reviews.R', sep = '') )
source(paste(path[p],'/ETL/dataMining.R', sep = '') )
#source(paste(path,'/ETL/dataMining.R', sep = '') )






#### **** MAIN **** ####

#### >>>> JOBS VACANCIES ####
jobsIndeed      <- fread("data/jobs/jobsIndeedBrasil2018-04-24.csv", encoding = 'UTF-8') # jobsIndeed()
jobsIndeedClean <- #fread("data/jobs/jobsIndeedClean2018-04-24.csv", encoding = 'UTF-8') #
                  cleanJobsIndeed(jobsIndeed)
#tab <- table(jobsIndeedClean$stateClean) ###
#fwrite(jobsIndeedClean,paste('data/jobs/jobsIndeedClean',Sys.Date(),'.csv',sep = "") )

jobsLoveMond      <- fread("data/jobs/jobsLMBrasil2018-04-24.csv", encoding = 'UTF-8') #jobsLoveM()
jobsLoveMondClean <- #fread("data/jobs/jobsLoveMondClean2018-04-24.csv", encoding = 'UTF-8') #
                    cleanJobsLoveMond(jobsLoveMond)
#tab <- table(jobsLoveMondClean$stateClean) ###
#fwrite(jobsLoveMondClean,paste('data/jobs/jobsLoveMondClean',Sys.Date(),'.csv',sep = "") )

jobsGlassDoor   <- rbind(a,b,c)

jobsGlassDClean <- #fread("data/jobs/jobsGlassDClean2018-04-25.csv", encoding = 'UTF-8') #
  cleanJobsGlassDoor(jobsGlassDoor)
#tab <- table(jobsGlassDClean$stateClean) ###
#View(tab)
#fwrite(jobsGlassDClean,paste('data/jobs/jobsGlassDClean',Sys.Date(),'.csv',sep = "") )

mergeDados <- rbind(jobsGlassDClean,jobsIndeedClean,jobsLoveMondClean)
jobsFinal  <- fread("data/jobsFinal.csv", encoding = 'UTF-8')

colnames(mergeDados) <- colnames(jobsFinal)
mergeDadosClean      <- cleanJobsFinal(mergeDados)

table(jobsLoveMondClean$country)

#tab <- table(mergeDadosClean$language) ###
#View(tab)

jobsFinal <- rbind(mergeDadosClean, jobsFinal)
jobsFinal <- delDup(jobsFinal)
jobsFinal <- cleanJobsFinal(jobsFinal)
jobsFinal$country <- gsub("Brasil","brazil",jobsFinal$country)
#fwrite(jobsFinal,paste('data/jobsFinal',Sys.Date(),'.csv',sep = "") )
fwrite(jobsFinal,'data/jobsFinal.csv')


table(jobsFinal$city)

#print(getwd())
#url <- c('https://www.glassdoor.com/Job/canada-data-scientist-jobs-SRCH_IL.0,6_IN3_KO7,21_IP','https://www.glassdoor.com/Job/us-data-scientist-jobs-SRCH_IL.0,2_IN1_KO3,17_IP')
#url <- 'https://www.glassdoor.com/Job/brazil-data-scientist-jobs-SRCH_IL.0,6_IN36_KO7,21_IP'

#collect(url)

#dados <- data.table::fread('data/jobsGlassDoor.csv', stringsAsFactors = F, encoding = 'UTF-8')
#load('data/jobsGlassDoor.RData')
#dadosClean <- preProcess(dados)
#fwrite(dadosClean,'data/dadosFinal2.csv')
#clusterDM()

#### >>>> COMPANIES' REVIEWS  ####
#reviewsLoveMondays() ## SCRAPPING AVALIA??ES NO SITE LOVE MONDAYS
#reviewsGlassDoor() ## SCRAPPING AVALIA??ES NO GLASSDOOR
#transGlassDoor() ## TRANSFORMA??ES AVALIA??ES DO SITE GLASSDOOR
#transLoveMond() ## TRANSFORMA??ES AVALIA??ES DO SITE LOVE MONDAYS

#reviewsIndeed()


  