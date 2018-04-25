#path <- 'E:/Curso Cientista de Dados/Projeto Final/analysisFacebook/jobsAnalysis'
#path <- 'E:/jobsAnalysis'

p <- 3
path <- c('C:/Users/gabri/Documents/GitHub/jr_analytics/','~/Documents/jr_analytics/','G:/GitHub/jr_analytics')
setwd(path[p])
source(paste(path[p],'/ETL/utils.R', sep = '') )
source(paste(path[p],'/ETL/jobs.R', sep = '') )
source(paste(path[p],'/ETL/reviews.R', sep = '') )
source(paste(path[p],'/ETL/dataMining.R', sep = '') )
#source(paste(path,'/ETL/dataMining.R', sep = '') )






#### **** MAIN **** ####

#### >>>> JOBS VACANCIES ####
jobsIndeed      <- jobsIndeed()
jobsIndeedClean <- cleanJobsIndeed(jobsIndeed)
#tab <- table(jobsIndeedClean$stateClean) ###
fwrite(jobsIndeedClean,paste('data/jobs/jobsIndeedClean',Sys.Date(),'.csv',sep = "") )

jobsLoveMond      <- jobsLoveM()
jobsLoveMondClean <- cleanJobsLoveMond(jobsLoveMond)
#tab <- table(jobsLoveMondClean$stateClean) ###
fwrite(jobsLoveMondClean,paste('data/jobs/jobsLoveMondClean',Sys.Date(),'.csv',sep = "") )

jobsGlassDoor <- jobsGlassDoor()
jobsGlassDClean <- cleanJobsGlassDoor(jobsGlassDoor)
tab <- table(jobsGlassDClean$stateClean) ###
fwrite(jobsGlassDClean,paste('data/jobs/jobsGlassDClean',Sys.Date(),'.csv',sep = "") )





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


  