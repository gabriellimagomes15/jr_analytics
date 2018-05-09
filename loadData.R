### SCRIPT PARA LER DADOS EM .RDATA E SALVAR EM .CSV
### Gabriel Lima Gomes - Brasil - 2018

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 0){
  stop("Por favor, indique o diretorio do projeto")
}
setwd(args)

print("Carregando dados JOBS")
load("data/jobs.RDATA")

print("Gravando dados JOBS")
write.csv(jobsFinal,"data/jobsFinal.csv",quote = T,row.names = F,col.names = T)

print("Carregando dados REVIEWS")
load("data/reviewFinal.RDATA")

print("Gravando dados REVIEWS")
write.csv2(reviewFinal,"data/reviewFinal.csv",quote = T,row.names = F,col.names = T)

d <- fread("data/jobsFinal.csv")
e <- fread("data/reviewFinal.csv")
