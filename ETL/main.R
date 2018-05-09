### SCRIPT PRINCIPAL PARA EXECUTAR WEB SCRAPING, PRE-PROCESSAMENTO E DATA MINING
### Gabriel Lima Gomes - Brasil - 2018

#path <- 'E:/Curso Cientista de Dados/Projeto Final/analysisFacebook/jobsAnalysis'
#path <- 'E:/jobsAnalysis'

p <- 1
path <- c('C:/Users/gabri/Documents/GitHub/jr_analytics/','~/Documents/jr_analytics/','G:/GitHub/jr_analytics')
setwd(path[p])
source(paste(path[p],'/ETL/utils.R', sep = '') )
source(paste(path[p],'/ETL/jobs.R', sep = '') )
source(paste(path[p],'/ETL/reviews.R', sep = '') )
source(paste(path[p],'/ETL/dataMining.R', sep = '') )






#### **** MAIN **** ####

#### >>>> JOBS VACANCIES ####

#### WEB SCRAPING ####
jobsIndeed      <- jobsIndeed()
jobsLoveMond      <- jobsLoveM()
jobsGlassDoor   <- jobsGlassDoor()

#### PRE PROCESS ####
jobsIndeedClean <- cleanJobsIndeed(jobsIndeed)
#tab <- table(jobsIndeedClean$stateClean) ###
fwrite(jobsIndeedClean,paste('data/jobs/jobsIndeedClean',Sys.Date(),'.csv',sep = "") )

jobsLoveMondClean <- cleanJobsLoveMond(jobsLoveMond)
#tab <- table(jobsLoveMondClean$stateClean) ###
fwrite(jobsLoveMondClean,paste('data/jobs/jobsLoveMondClean',Sys.Date(),'.csv',sep = "") )

jobsGlassDClean <- cleanJobsGlassDoor(jobsGlassDoor)
#tab <- table(jobsGlassDClean$stateClean) ###
#View(tab)
#fwrite(jobsGlassDClean,paste('data/jobs/jobsGlassDClean',Sys.Date(),'.csv',sep = "") )

mergeDados <- rbind(jobsGlassDClean,jobsIndeedClean,jobsLoveMondClean)
jobsFinal  <- fread("data/jobsFinal.csv", encoding = 'UTF-8') ### DADOS ATUAIS 
fwrite(jobsFinal,paste('data/jobsFinal',Sys.Date(),'.csv',sep = "") ) ### BACKUP DA BASE ANTIGA

colnames(mergeDados) <- colnames(jobsFinal)
mergeDadosClean      <- cleanJobsFinal(mergeDados)


jobsFinal <- rbind(mergeDadosClean, jobsFinal) ### MERGE DAS BASES DE DADOS (ANTIGA E NOVA)
jobsFinal <- delDup(jobsFinal)
jobsFinal <- cleanJobsFinal(jobsFinal)

fwrite(jobsFinal,'data/jobsFinal.csv')
save(jobsFinal, file = 'data/jobs.RDATA')

#### CLUSTER DATA MINING ####
dados    <- data.table::fread("data/jobsFinal.csv")
clusters <- clusterJobs(dados, c('position', 'skills', 'education', 'language','state','country'))
fwrite(clusters, "data/clusterReq.csv")


#### >>>> COMPANIES' REVIEWS  ####
#### WEB SCRAPING ####
revLoveM <- reviewsLoveMondays() ## SCRAPPING AVALIA합ES NO SITE LOVE MONDAYS
revGlass <- reviewsGlassDoor() ## SCRAPPING AVALIA합ES NO GLASSDOOR
revIndeed<- reviewsIndeed()

#### PRE PROCESS ####
revGlassDClean <- cleanReviewGlassD(revGlass) ## TRANSFORMA??ES AVALIA합ES DO SITE GLASSDOOR
revLoveMClean  <- cleanReviewLoveM(revLoveM) ## TRANSFORMA??ES AVALIA합ES DO SITE LOVE MONDAYS
revIndeedClean <- cleanReviewIndeed(revIndeed)

mergeReview <- rbind(revGlassDClean, revLoveMClean, revIndeedClean)
reviewFinal <- fread("data/reviewFinal.csv", encoding = 'UTF-8') ### DADOS ATUAIS 
fwrite(reviewFinal,paste('data/reviewFinal',Sys.Date(),'.csv',sep = "") ) ### BACKUP DA BASE ANTIGA

mergeReviewClean <- cleanReviewFinal(mergeReview)
colnames(mergeReviewClean) <- colnames(reviewFinal)


reviewFinal <- rbind(mergeReviewClean, reviewFinal) ### MERGE DAS BASES DE DADOS (ANTIGA E NOVA)
reviewFinal <- delDup(reviewFinal)

fwrite(reviewFinal,'data/reviewFinal.csv')
save(reviewFinal, file = 'data/reviewFinal.RDATA')

