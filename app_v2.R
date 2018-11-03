#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#--------------------------------------------------------
#  PROGRAMA:                                                                                                      
#           app.R
#--------------------------------------------------------
#  DESCRIÇÃO:
#           PROGRAMA PARA TABULAÇÃO CRUZADA DOS DADOS DO SAEB DIRETOR 2015
#--------------------------------------------------------
#  AUTOR:                                                                                                      
#           Jorge H C Fernandes (jhcf@unb.br)
#           https://github.com/jhcf/exploradorsaeb2015
#--------------------------------------------------------

#------------------------------------------------------------------------
# Obs:                                                                                                                    
#   Para execução é necessário salvar que o arquivo TS_DIRETOR.csv esteja 
#   no mesmo diretório em que este script esteja salvo.	                  
#------------------------------------------------------------------------

#--------------------
# Intalação dos pacotes necessários, se não estiverem instalados
#--------------------

if(!require(data.table)){install.packages('data.table')}
library(data.table)

if(!require(shiny)){install.packages('shiny')}
library(shiny)

if(!require(gmodels)){install.packages('gmodels')}
library(gmodels)

if(!require(vcd)){install.packages('vcd')}
library(vcd)

if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)

# Carga dos microdados

saeb <- data.table::fread(input='TS_DIRETOR.csv',
                          integer64='character',
                          skip=0,  #Ler do inicio
                          nrow=-1, #Ler todos os registros
                          na.strings = "", 
                          showProgress = TRUE)

#---------------------------
# Formata os rótulos das respostas, com base na descrição dos metadados
#---------------------------

saeb$ID_UF <- 
  factor(saeb$ID_UF, 
         levels = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
         labels = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE','BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')
  )

saeb$ID_DEPENDENCIA_ADM <-
  factor(saeb$ID_DEPENDENCIA_ADM,
         levels=c(1,2,3,4),
         labels=c('Federal','Estadual','Municipal','Privada'))


saeb$ID_LOCALIZACAO <- factor(saeb$ID_LOCALIZACAO,levels=c( 1, 2),
                              labels=c(
                                'Urbana',
                                'Rural'))

saeb$TX_RESP_Q001 <- factor(saeb$TX_RESP_Q001,levels=c('A','B'),
                            labels=c(
                              'Masculino',
                              'Feminino'))

saeb$TX_RESP_Q002 <- factor(saeb$TX_RESP_Q002,levels=c('A','B','C','D','E','F'),
                            labels=c(
                              'Até 24 anos',
                              'De 25 a 29 anos',
                              'De 30 a 39 anos',
                              'De 40 a 49 anos',
                              'De 50 a 54 anos',
                              '55 anos ou mais'))

saeb$TX_RESP_Q003 <- factor(saeb$TX_RESP_Q003,levels=c('A','B','C','D','E','F','G'),
                            labels=c(
                              'Branco(a)',
                              'Pardo(a)',
                              'Preto(a)',
                              'Amarelo(a)',
                              'Indígena',
                              'Não quero declarar',
                              'Não sei'))

saeb$TX_RESP_Q004 <- factor(saeb$TX_RESP_Q004,levels=c('A','B','C','D','E','F','G','H','I'),
                            labels=c(
                              'Menos que o Ensino Médio (antigo 2º grau)',
                              'Ensino Médio - Magistério (antigo 2º grau)',
                              'Ensino  Médio - Outros (antigo 2º grau)',
                              'Ensino Superior - Pedagogia',
                              'Ensino Superior - Curso Normal Superior',
                              'Ensino Superior - Licenciatura em Matemática',
                              'Ensino Superior - Licenciatura em Letras',
                              'Ensino Superior - Outras Licenciaturas',
                              'Ensino Superior - Outras áreas'))

saeb$TX_RESP_Q005 <- factor(saeb$TX_RESP_Q005,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Há menos de 2 anos',
                              'De 2 a 7 anos',
                              'De 8 a 14 anos',
                              'De 15 a 20 anos',
                              'Há mais de 20 anos'))

saeb$TX_RESP_Q006 <- factor(saeb$TX_RESP_Q006,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não concluí o ensino superior',
                              'Privada',
                              'Pública Federal',
                              'Pública Estadual',
                              'Pública Municipal'))

saeb$TX_RESP_Q007 <- factor(saeb$TX_RESP_Q007,levels=c('A','B','C','D'),
                            labels=c(
                              'Não concluí o ensino superior',
                              'Presencial',
                              'Semipresencial',
                              'A distância'))

saeb$TX_RESP_Q008 <- factor(saeb$TX_RESP_Q008,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não fiz ou não completei curso de pós-graduação',
                              'Atualização ou Aperfeiçoamento (mínimo 180 horas)',
                              'Especialização (mínimo de 360 horas)',
                              'Mestrado',
                              'Doutorado'))

saeb$TX_RESP_Q009 <- factor(saeb$TX_RESP_Q009,levels=c('A','B','C','D','E','F'),
                            labels=c(
                              'Não fiz ou não completei curso de pós-graduação',
                              'Educação, enfatizando alfabetização',
                              'Educação, enfatizando linguística e/ou letramento',
                              'Educação, enfatizando educação matemática',
                              'Educação - outras ênfases',
                              'Outras áreas que não a Educação'))

saeb$TX_RESP_Q010 <- factor(saeb$TX_RESP_Q010,levels=c('A','B','C','D','E','F','G','H','I','J','K'),
                            labels=c(
                              'Até R$ 788,00',
                              'Entre R$ 788,01 e R$ 1.182,00',
                              'Entre R$ 1.182,01 e R$ 1.576,00',
                              'Entre R$ 1.576,01 e R$ 1.970,00',
                              'Entre R$ 1.970,01 e R$ 2.364,00',
                              'Entre R$ 2.364,01 e R$ 2.758,00',
                              'Entre R$ 2.758,01 e R$ 3.152,00',
                              'Entre R$ 3.152,01 e R$ 3.940,00',
                              'Entre R$ 3.940,01 e R$ 5.516,00',
                              'Entre R$ 5.516,01 e R$ 7.788,00',
                              'R$ 7.788,01 ou mais'))

saeb$TX_RESP_Q011 <- factor(saeb$TX_RESP_Q011,levels=c('A','B','C'),
                            labels=c(
                              'Sim, na área de educação',
                              'Sim, fora da área de educação',
                              'Não'))

saeb$TX_RESP_Q012 <- factor(saeb$TX_RESP_Q012,levels=c('A','B','C','D','E','F','G','H','I','J','K'),
                            labels=c(
                              'Até R$ 788,00',
                              'Entre R$ 788,01 e R$ 1.182,00',
                              'Entre R$ 1.182,01 e R$ 1.576,00',
                              'Entre R$ 1.576,01 e R$ 1.970,00',
                              'Entre R$ 1.970,01 e R$ 2.364,00',
                              'Entre R$ 2.364,01 e R$ 2.758,00',
                              'Entre R$ 2.758,01 e R$ 3.152,00',
                              'Entre R$ 3.152,01 e R$ 3.940,00',
                              'Entre R$ 3.940,01 e R$ 5.516,00',
                              'Entre R$ 5.516,01 e R$ 7.788,00',
                              'R$ 7.788,01 ou mais'))

saeb$TX_RESP_Q013 <- factor(saeb$TX_RESP_Q013,levels=c('A','B','C','D'),
                            labels=c(
                              'Mais de 40 horas',
                              '40 horas',
                              'De 20 a 39 horas',
                              'Menos de 20 horas'))

saeb$TX_RESP_Q014 <- factor(saeb$TX_RESP_Q014,levels=c('A','B','C','D','E','F','G'),
                            labels=c(
                              'Concurso público apenas',
                              'Eleição apenas',
                              'Indicação apenas',
                              'Processo seletivo apenas',
                              'Processo seletivo e Eleição',
                              'Processo seletivo e Indicação',
                              'Outra forma'))

saeb$TX_RESP_Q015 <- factor(saeb$TX_RESP_Q015,levels=c('A','B','C','D','E','F','G','H'),
                            labels=c(
                              'Nunca',
                              'Menos de um ano',
                              '1-2 anos',
                              '3-5 anos',
                              '6-10 anos',
                              '11-15 anos',
                              '16-20 anos',
                              'Mais de 20 anos'))

saeb$TX_RESP_Q016 <- factor(saeb$TX_RESP_Q016,levels=c('A','B','C','D','E','F','G'),
                            labels=c(
                              'Menos de um ano',
                              '1-2 anos',
                              '3-5 anos',
                              '6-10 anos',
                              '11-15 anos',
                              '16-20 anos',
                              'Mais de 20 anos'))

saeb$TX_RESP_Q017 <- factor(saeb$TX_RESP_Q017,levels=c('A','B','C','D','E','F','G'),
                            labels=c(
                              'Menos de um ano',
                              '1-2 anos',
                              '3-5 anos',
                              '6-10 anos',
                              '11-15 anos',
                              '16-20 anos',
                              'Mais de 20 anos'))

saeb$TX_RESP_Q018 <- factor(saeb$TX_RESP_Q018,levels=c('A','B','C','D','E','F','G'),
                            labels=c(
                              'Menos de um ano',
                              '1-2 anos',
                              '3-5 anos',
                              '6-10 anos',
                              '11-15 anos',
                              '16-20 anos',
                              'Mais de 20 anos'))

saeb$TX_RESP_Q019 <- factor(saeb$TX_RESP_Q019,levels=c('A','B'),
                            labels=c(
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q020 <- factor(saeb$TX_RESP_Q020,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não participei',
                              'Sim, e não houve impacto',
                              'Sim, e houve um pequeno impacto',
                              'Sim, e houve um impacto moderado',
                              'Sim, e houve um grande impacto'))

saeb$TX_RESP_Q021 <- factor(saeb$TX_RESP_Q021,levels=c('A','B'),
                            labels=c(
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q022 <- factor(saeb$TX_RESP_Q022,levels=c('A','B','C'),
                            labels=c(
                              'Não gostaria de ter participado',
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q023 <- factor(saeb$TX_RESP_Q023,levels=c('A','B','C'),
                            labels=c(
                              'Não gostaria de ter participado',
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q024 <- factor(saeb$TX_RESP_Q024,levels=c('A','B','C'),
                            labels=c(
                              'Não gostaria de ter participado',
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q025 <- factor(saeb$TX_RESP_Q025,levels=c('A','B','C'),
                            labels=c(
                              'Não gostaria de ter participado',
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q026 <- factor(saeb$TX_RESP_Q026,levels=c('A','B'),
                            labels=c(
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q027 <- factor(saeb$TX_RESP_Q027,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não foram organizadas atividades de formação continuada',
                              'Poucos professores',
                              'Um pouco menos da metade dos professores',
                              'Um pouco mais da metade dos professores',
                              'Quase todos ou todos os professores'))

saeb$TX_RESP_Q028 <- factor(saeb$TX_RESP_Q028,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Menor ou igual a 25%',
                              'De 26% a 50%',
                              'De 51% a 75%',
                              'De 76% a 90%',
                              'De 91% a 100%'))

saeb$TX_RESP_Q029 <- factor(saeb$TX_RESP_Q029,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não existe Conselho Escolar',
                              'Nenhuma vez',
                              'Uma vez',
                              'Duas vezes',
                              'Três vezes ou mais'))

saeb$TX_RESP_Q030 <- factor(saeb$TX_RESP_Q030,levels=c('A','B','C','D','E','F','G'),
                            labels=c(
                              'Não existe Conselho Escolar',
                              'Professores, funcionários, alunos e pais/responsáveis',
                              'Professores, funcionários e pais/responsáveis',
                              'Professores, alunos e pais/responsáveis',
                              'Professores, funcionários e alunos',
                              'Professores e pais/responsáveis',
                              'Outros'))

saeb$TX_RESP_Q031 <- factor(saeb$TX_RESP_Q031,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não existe Conselho de Classe',
                              'Nenhuma vez',
                              'Uma vez',
                              'Duas vezes',
                              'Três vezes ou mais'))

saeb$TX_RESP_Q032 <- factor(saeb$TX_RESP_Q032,levels=c('A','B','C','D','E','F','G','H'),
                            labels=c(
                              'Não sei como foi desenvolvido',
                              'Não existe Projeto Pedagógico',
                              'Utilizando-se um modelo pronto, sem discussão com a equipe escolar',
                              'Utilizando-se um modelo pronto, mas com discussão com a equipe escolar',
                              'Utilizando-se um modelo pronto, porém com adaptações, sem discussão com a equipe escolar',
                              'Utilizando-se um modelo pronto, porém com adaptações e com discussão com a equipe escolar',
                              'Elaborou-se um modelo próprio, mas não houve discussão com a equipe escolar',
                              'Elaborou-se um modelo próprio e houve discussão com a equipe escolar'))

saeb$TX_RESP_Q033 <- factor(saeb$TX_RESP_Q033,levels=c('A','B','C'),
                            labels=c(
                              'Sim',
                              'Não',
                              'Não sei'))

saeb$TX_RESP_Q034 <- factor(saeb$TX_RESP_Q034,levels=c('A','B','C'),
                            labels=c(
                              'Sim',
                              'Não',
                              'Minha escola não participou'))

saeb$TX_RESP_Q035 <- factor(saeb$TX_RESP_Q035,levels=c('A','B','C'),
                            labels=c(
                              'Sim',
                              'Não',
                              'Meu município não participou'))

saeb$TX_RESP_Q036 <- factor(saeb$TX_RESP_Q036,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q037 <- factor(saeb$TX_RESP_Q037,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Prova de seleção',
                              'Sorteio',
                              'Local de moradia',
                              'Prioridade por ordem de chegada',
                              'Outro critério'))

saeb$TX_RESP_Q038 <- factor(saeb$TX_RESP_Q038,levels=c('A','B','C','D'),
                            labels=c(
                              'Após o processo de matrícula, a escola ainda tinha vagas disponíveis',
                              'A procura por vaga na escola preencheu todas as vagas oferecidas',
                              'A procura por vaga na escola foi um pouco maior que as vagas oferecidas',
                              'A procura por vaga na escola superou em muito o número de vagas oferecidas'))

saeb$TX_RESP_Q039 <- factor(saeb$TX_RESP_Q039,levels=c('A','B','C','D','E','F'),
                            labels=c(
                              'Homogeneidade quando à idade (alunos com a mesma idade)',
                              'Homogeneidade quanto ao rendimento escolar (alunos com rendimento similar)',
                              'Heterogeneidade quanto à idade (alunos com idades diferentes)',
                              'Heterogeneidade quanto ao redimento escolar (alunos com rendimentos diferentes)',
                              'Outro critério',
                              'Não houve critério'))

saeb$TX_RESP_Q040 <- factor(saeb$TX_RESP_Q040,levels=c('A','B','C','D','E','F','G','H','I','J'),
                            labels=c(
                              'Preferência dos professores',
                              'Escolha dos professores, de acordo com a pontuação por tempo de serviço e formação',
                              'Professores experientes com turmas de aprendizagem mais rápida',
                              'Professores experientes com turmas de aprendizagem mais lenta',
                              'Manutenção do professor com a mesma turma',
                              'Revezamento dos professores entre as séries',
                              'Sorteio das turmas entre os professores',
                              'Atribuição pela direção da escola',
                              'Outro critério',
                              'Não houve critério'))

saeb$TX_RESP_Q041 <- factor(saeb$TX_RESP_Q041,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não há ação, embora exista o problema',
                              'Não há ação, porque nesta escola não há esse tipo de problema',
                              'Sim, mas com resultados ainda insatisfatórios',
                              'Sim, com resultados satisfatórios',
                              'Sim, mas ainda não avaliamos o resultado'))

saeb$TX_RESP_Q042 <- factor(saeb$TX_RESP_Q042,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não há ação, embora exista o problema',
                              'Não há ação, porque nesta escola não há esse tipo de problema',
                              'Sim, mas com resultados ainda insatisfatórios',
                              'Sim, com resultados satisfatórios',
                              'Sim, mas ainda não avaliamos o resultado'))

saeb$TX_RESP_Q043 <- factor(saeb$TX_RESP_Q043,levels=c('A','B'),
                            labels=c(
                              'Não',
                              'Sim'))

saeb$TX_RESP_Q044 <- factor(saeb$TX_RESP_Q044,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q045 <- factor(saeb$TX_RESP_Q045,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q046 <- factor(saeb$TX_RESP_Q046,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q047 <- factor(saeb$TX_RESP_Q047,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q048 <- factor(saeb$TX_RESP_Q048,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q049 <- factor(saeb$TX_RESP_Q049,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q050 <- factor(saeb$TX_RESP_Q050,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q051 <- factor(saeb$TX_RESP_Q051,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q052 <- factor(saeb$TX_RESP_Q052,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q053 <- factor(saeb$TX_RESP_Q053,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q054 <- factor(saeb$TX_RESP_Q054,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q055 <- factor(saeb$TX_RESP_Q055,levels=c('A','B','C','D'),
                            labels=c(
                              'Nunca',
                              'Algumas vezes',
                              'Frequentemente',
                              'Sempre ou quase sempre'))

saeb$TX_RESP_Q056 <- factor(saeb$TX_RESP_Q056,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Nenhum',
                              'Entre 1 e 5 alunos',
                              'Entre 6 e 10 alunos',
                              'Entre 11 e 20 alunos',
                              'Mais de 20 alunos'))

saeb$TX_RESP_Q057 <- factor(saeb$TX_RESP_Q057,levels=c('A','B','C'),
                            labels=c(
                              'Não',
                              'Sim, mas pouco adequada',
                              'Sim, suficientemente adequada'))

saeb$TX_RESP_Q058 <- factor(saeb$TX_RESP_Q058,levels=c('A','B','C'),
                            labels=c(
                              'Não possui sala de recursos',
                              'Sim, mas com poucos recursos',
                              'Sim, com recursos suficientes'))

saeb$TX_RESP_Q059 <- factor(saeb$TX_RESP_Q059,levels=c('A','B','C'),
                            labels=c(
                              'Não',
                              'Sim, mas apenas em uma área/deficiência',
                              'Sim, em mais de uma área/deficiência'))

saeb$TX_RESP_Q060 <- factor(saeb$TX_RESP_Q060,levels=c('A','B','C'),
                            labels=c(
                              'Não',
                              'Sim, mas em número insuficiente',
                              'Sim, em número suficiente'))

saeb$TX_RESP_Q061 <- factor(saeb$TX_RESP_Q061,levels=c('A','B','C'),
                            labels=c(
                              'Não',
                              'Sim, mas em número insuficiente',
                              'Sim, em número suficiente'))

saeb$TX_RESP_Q062 <- factor(saeb$TX_RESP_Q062,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Inexistente',
                              'Ruim',
                              'Razoável',
                              'Bom',
                              'Ótimo'))

saeb$TX_RESP_Q063 <- factor(saeb$TX_RESP_Q063,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Inexistente',
                              'Ruim',
                              'Razoável',
                              'Bom',
                              'Ótimo'))

saeb$TX_RESP_Q064 <- factor(saeb$TX_RESP_Q064,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Inexistente',
                              'Ruim',
                              'Razoável',
                              'Bom',
                              'Ótimo'))

saeb$TX_RESP_Q065 <- factor(saeb$TX_RESP_Q065,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Inexistente',
                              'Ruim',
                              'Razoável',
                              'Bom',
                              'Ótimo'))

saeb$TX_RESP_Q066 <- factor(saeb$TX_RESP_Q066,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Inexistente',
                              'Ruim',
                              'Razoável',
                              'Bom',
                              'Ótimo'))

saeb$TX_RESP_Q067 <- factor(saeb$TX_RESP_Q067,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q068 <- factor(saeb$TX_RESP_Q068,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q069 <- factor(saeb$TX_RESP_Q069,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q070 <- factor(saeb$TX_RESP_Q070,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q071 <- factor(saeb$TX_RESP_Q071,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q072 <- factor(saeb$TX_RESP_Q072,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q073 <- factor(saeb$TX_RESP_Q073,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q074 <- factor(saeb$TX_RESP_Q074,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q075 <- factor(saeb$TX_RESP_Q075,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q076 <- factor(saeb$TX_RESP_Q076,levels=c('A','B','C','D'),
                            labels=c(
                              'Não',
                              'Sim, pouco',
                              'Sim, moderadamente',
                              'Sim, muito'))

saeb$TX_RESP_Q077 <- factor(saeb$TX_RESP_Q077,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q078 <- factor(saeb$TX_RESP_Q078,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q079 <- factor(saeb$TX_RESP_Q079,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q080 <- factor(saeb$TX_RESP_Q080,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q081 <- factor(saeb$TX_RESP_Q081,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q082 <- factor(saeb$TX_RESP_Q082,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q083 <- factor(saeb$TX_RESP_Q083,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q084 <- factor(saeb$TX_RESP_Q084,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q085 <- factor(saeb$TX_RESP_Q085,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q086 <- factor(saeb$TX_RESP_Q086,levels=c('A','B','C','D','E'),
                            labels=c(
                              'Não sei',
                              'Foi escolhido de forma participativa pelos professores',
                              'Foi escolhido por somente alguns membros da equipe escolar',
                              'Foi escolhido por órgãos externos à escola',
                              'Foi escolhido de outra maneira'))

saeb$TX_RESP_Q087 <- factor(saeb$TX_RESP_Q087,levels=c('A','B','C'),
                            labels=c(
                              'Sim',
                              'Não',
                              'Não sei'))

saeb$TX_RESP_Q088 <- factor(saeb$TX_RESP_Q088,levels=c('A','B','C'),
                            labels=c(
                              'Sim',
                              'Não',
                              'Não sei'))

saeb$TX_RESP_Q089 <- factor(saeb$TX_RESP_Q089,levels=c('A','B','C'),
                            labels=c(
                              'Sim',
                              'Não',
                              'Não sei'))

saeb$TX_RESP_Q090 <- factor(saeb$TX_RESP_Q090,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q091 <- factor(saeb$TX_RESP_Q091,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q092 <- factor(saeb$TX_RESP_Q092,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q093 <- factor(saeb$TX_RESP_Q093,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q094 <- factor(saeb$TX_RESP_Q094,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q095 <- factor(saeb$TX_RESP_Q095,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q096 <- factor(saeb$TX_RESP_Q096,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q097 <- factor(saeb$TX_RESP_Q097,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q098 <- factor(saeb$TX_RESP_Q098,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q099 <- factor(saeb$TX_RESP_Q099,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q100 <- factor(saeb$TX_RESP_Q100,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q101 <- factor(saeb$TX_RESP_Q101,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q102 <- factor(saeb$TX_RESP_Q102,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q103 <- factor(saeb$TX_RESP_Q103,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q104 <- factor(saeb$TX_RESP_Q104,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q105 <- factor(saeb$TX_RESP_Q105,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q106 <- factor(saeb$TX_RESP_Q106,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q107 <- factor(saeb$TX_RESP_Q107,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q108 <- factor(saeb$TX_RESP_Q108,levels=c('A','B'),
                            labels=c(
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q109 <- factor(saeb$TX_RESP_Q109,levels=c('A','B','C'),
                            labels=c(
                              'Não há aula de ensino religioso',
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q110 <- factor(saeb$TX_RESP_Q110,levels=c('A','B','C'),
                            labels=c(
                              'Não há aula de ensino religioso',
                              'Sim',
                              'Não'))

saeb$TX_RESP_Q111 <- factor(saeb$TX_RESP_Q111,levels=c('A','B'),
                            labels=c(
                              'Não há aula de ensino religioso',
                              'Sim'))
# renomeia os nomes das colunas, para facilitar o entendimento da tabulação cruzada

names(saeb)[8] <- 'Q001_SEXO'
names(saeb)[9] <- 'Q002_FAIXA.ETÁRIA'
names(saeb)[10] <- 'Q003_RAÇA.COR'
names(saeb)[11] <- 'Q004_MAIS.ALTA.ESCOLARIDADE'
names(saeb)[12] <- 'Q005_TEMPO.ESCOLARIDADE'
names(saeb)[13] <- 'Q006_TIPO.INSTIT.GRAU'
names(saeb)[14] <- 'Q007_CURSO.SUPERIOR'
names(saeb)[15] <- 'Q008_PÓS.GRAD'
names(saeb)[16] <- 'Q009_ÁREA.PÓS.GRAD'
names(saeb)[17] <- 'Q010_SALÁRIO.BRUTO'
names(saeb)[18] <- 'Q011_OUTRA.ATIVIDADE'
names(saeb)[19] <- 'Q012_SALÁRIO.TOTAL'
names(saeb)[20] <- 'Q013_CARGA.HORÁRIA'
names(saeb)[21] <- 'Q014_COMO.ASSUMIU'
names(saeb)[22] <- 'Q015_TEMPO.ANTERIOR.TRABALHO'
names(saeb)[23] <- 'Q016_ANOS.EM.DIREÇÃO'
names(saeb)[24] <- 'Q017_ANOS.NESTA.DIREÇÃO'
names(saeb)[25] <- 'Q018_ANOS.EM.EDUCAÇÃO'
names(saeb)[26] <- 'Q019_ATIV.DESENV.PROF.2.ANOS'
names(saeb)[27] <- 'Q020_IMPACTO.DE.ATIV.DESENV.PROF.2.ANOS'
names(saeb)[28] <- 'Q021_GOSTARIA.DE.PARTICIPAR.DE.MAIS.ATIV.DESENV.PROF'
names(saeb)[29] <- 'Q022_IMPEDIM.PART.ATIV.DESENV.PROF.1'
names(saeb)[30] <- 'Q023_IMPEDIM.PART.ATIV.DESENV.PROF.2'
names(saeb)[31] <- 'Q024_IMPEDIM.PART.ATIV.DESENV.PROF.3'
names(saeb)[32] <- 'Q025_IMPEDIM.PART.ATIV.DESENV.PROF.4'
names(saeb)[33] <- 'Q026_ORGANIZOU.ATIV.FORM.CONTIN.NA.ESCOLA'
names(saeb)[34] <- 'Q027_QTD.DOCENTES.PARTICIPANTES.ATIV.FORM.CONTIN.'
names(saeb)[35] <- 'Q028_QTD.DOCENTES.ESTÁVEIS'
names(saeb)[36] <- 'Q029_QTD.REUNIÕES.CONSELHO.ESCOLAR.NESTE.ANO'
names(saeb)[37] <- 'Q030_QUEM.PARTICIP.CONSELHO.ESCOLAR'
names(saeb)[38] <- 'Q031_QTD.REUNIÕES.CONSELHO.CLASSE.NESTE.ANO.ESCOLA'
names(saeb)[39] <- 'Q032_ELAB.PROJ.PEDAGÓGICO.NESTE.ANO.ESCOLA'
names(saeb)[40] <- 'Q033_PROVA.BRASIL.2011'
names(saeb)[41] <- 'Q034_CONHECE.RESULT.SAEB.DE.2011.DESTA.ESCOLA'
names(saeb)[42] <- 'Q035_CONHECE.RESULT.SAEB.DE.2011.DO.MUNICÍPIO'
names(saeb)[43] <- 'Q036_CONHECE.RESULT.SAEB.DE.2011.DO.ESTADO'
names(saeb)[44] <- 'Q037_CRITÉRIO.ADMINSSAO.ALUNOS.NESTE.ANO'
names(saeb)[45] <- 'Q038_SITUAÇÃO.OFERTA.DE.VAGAS'
names(saeb)[46] <- 'Q039_CRITÉRIO.FORMAÇÃO.TURMAS'
names(saeb)[47] <- 'Q040_CRITÉRIO.ATRIBUIÇÃO.TURMAS.A.PROFESSORES'
names(saeb)[48] <- 'Q041_AÇÃO.REDUÇÃO.TAXA.ABANDONO'
names(saeb)[49] <- 'Q042_AÇÃO.REDUÇÃO.TAXAS.REPROVAÇÃO'
names(saeb)[50] <- 'Q043_AÇÃO.REFORÇO.ESCOLAR.APRENDIZ.'
names(saeb)[51] <- 'Q044_FREQ.DISCUSS.COM.PROF.MELHOR.ENSINO.E.APRENDIZ.'
names(saeb)[52] <- 'Q045_PROF.CONVERSA.COM.ALUNOS.PARA.TENTAR.SOLUC.PROBL'
names(saeb)[53] <- 'Q046_PAIS.SÃO.AVISADOS.DAS.FALTAS.DOS.ALUNOS'
names(saeb)[54] <- 'Q047_PAIS.SÃO.CHAM.REUNIÃO.CONV.FALTAS.DOS.ALUNOS'
names(saeb)[55] <- 'Q048_PAIS.SÃO.CHAM.INDIVID.CONV.FALTAS.DOS.ALUNOS'
names(saeb)[56] <- 'Q049_ESCOLA.ENVIA.ALGUÉM.CONV.SOBRE.FALTAS.DOS.ALUNOS'
names(saeb)[57] <- 'Q050_ATIV.EXTRACURRICULARES.ESPORTE.NESTE.ANO.ESCOLA'
names(saeb)[58] <- 'Q051_ATIV.EXTRACURRICULARES.ARTES.NESTE.ANO.ESCOLA'
names(saeb)[59] <- 'Q052_PROJETOS.TEMÁTICOS.NESTE.ANO.ESCOLA'
names(saeb)[60] <- 'Q053_EVENTOS.PARA.COMUNIDADE.NESTE.ANO.ESCOLA'
names(saeb)[61] <- 'Q054_ESPAÇO.EVENTOS.PROMOV.PELA.COMUN.NESTE.ANO.ESCOLA'
names(saeb)[62] <- 'Q055_COMUNIDADE.TRAB.VOLUNTÁRIO.ESCOLA'
names(saeb)[63] <- 'Q056_QTD.ESTUDANTES.DEFICIÊNCIA.NECESSID.ESPEC'
names(saeb)[64] <- 'Q057_INFRAESTRUTURA.PESSOAS.DEFICIÊNCIA.NECESSID.ESPEC'
names(saeb)[65] <- 'Q058_RECURSOS.MULTIFUNCIONAIS.ATENDIM.EDUC.ESPECIALIZ.'
names(saeb)[66] <- 'Q059_FORMAÇÃO.TRAB.COM.ESTUD.COM.DEFIC.NECESSID.ESPEC'
names(saeb)[67] <- 'Q060_PROF.FORM.ESPECIF.ESTUD.DEFIC.NECESSID.ESPEC'
names(saeb)[68] <- 'Q061_FUNC.FORM.ESPECIF.ESTUD.DEFIC.NECESSID.ESPEC'
names(saeb)[69] <- 'Q062_MERENDA.ESCOL.REC.FINANCEIROS'
names(saeb)[70] <- 'Q063_MENDA.ESCOL.QTD.ALIMENTOS'
names(saeb)[71] <- 'Q064_MERENDA.ESCOL.QUALID.ALIMENTOS'
names(saeb)[72] <- 'Q065_MERENDA.ESCOL.ESPAÇO.COZINHAR'
names(saeb)[73] <- 'Q066_MERENDA.ESCOL.DISPONIBILID.FUNCIONÁRIOS'
names(saeb)[74] <- 'Q067_DIFIC.FUNC.ESCOLA.INSUFIC.REC.FINANCEIROS'
names(saeb)[75] <- 'Q068_DIFIC.FUNC.ESCOLA.INEXIST.PROF.DISCIPL.SERIES'
names(saeb)[76] <- 'Q069_DIFIC.FUNC.ESCOLA.CARÊNC.PESSOAL.ADMINISTRAT.'
names(saeb)[77] <- 'Q070_DIFIC.FUNC.ESCOLA.CARÊNC.PESSOAL.APOIO.PEDAGÓG'
names(saeb)[78] <- 'Q071_DIFIC.FUNC.ESCOLA.FALTA.RECURSOS.PEDAGÓG.'
names(saeb)[79] <- 'Q072_DIFIC.FUNC.ESCOLA.INTERRUP.ATIV.ESCOLARES'
names(saeb)[80] <- 'Q073_DIFIC.FUNC.ESCOLA.ALTO.ÍNDICE.FALTAS.PROFESS.'
names(saeb)[81] <- 'Q074_DIFIC.FUNC.ESCOLA.ALTO.ÍNDICE.FALTAS.ALUNOS'
names(saeb)[82] <- 'Q075_DIFIC.FUNC.ESCOLA.ALTA.ROTATIV.CORPO.DOCENTE'
names(saeb)[83] <- 'Q076_DIFIC.FUNC.ESCOLA.INDISCIPLINA.DOS.ALUNOS'
names(saeb)[84] <- 'Q077_DIFIC.EXERCIC.CARGO.DIR.INTERFEREN.ATORES.EXTERN'
names(saeb)[85] <- 'Q078_DIFIC.EXERCIC.CARGO.DIR.APOIO.INSTÂNC.SUPERIORES'
names(saeb)[86] <- 'Q079_DIFIC.EXERCIC.CARGO.DIR.TROCA.INFORMA.OUTROS.DIR'
names(saeb)[87] <- 'Q080_DIFIC.EXERCIC.CARGO.DIR.APOIO.COMUNIDADE'
names(saeb)[88] <- 'Q081_APOIO.FINANCEIRO.GOV.FEDERAL'
names(saeb)[89] <- 'Q082_APOIO.FINANCEIRO.GOV.ESTADUAL'
names(saeb)[90] <- 'Q083_APOIO.FINANCEIRO.GOV.MUNICIPAL'
names(saeb)[91] <- 'Q084_APOIO.FINANCEIRO.EMPRESAS.E.DOADORES'
names(saeb)[92] <- 'Q085_USOU.GUIA.LIVROS.DIDATICOS.MEC'
names(saeb)[93] <- 'Q086_COMO.ESCOLHA.LIVROS.DIDATICOS.OCORREU'
names(saeb)[94] <- 'Q087_LIVROS.CHEGARAM.EM.TEMPO.HÁBIL'
names(saeb)[95] <- 'Q088_LIVROS.FALTA.LIVROS.ALUNOS'
names(saeb)[96] <- 'Q089_LIVROS.ESCOLHIDOS.FORAM.RECEBIDOS'
names(saeb)[97] <- 'Q090_AGRESSÃO.VERBAL.FISICA.DE.ALUNOS.A.PROF.E.FUNC'
names(saeb)[98] <- 'Q091_AGRESSÃO.VERBAL.FISICA.DE.ALUNOS.A.ALUNOS'
names(saeb)[99] <- 'Q092_VOCÊ.FOI.VÍTIMA.DE.ATENTADO.À.VIDA'
names(saeb)[100] <- 'Q093_VOCÊ.FOI.AMEAÇADO.POR.ALUNO'
names(saeb)[101] <- 'Q094_VOCÊ.FOI.VÍTIMA.DE.FURTO.SEM.VIOLÊNCIA'
names(saeb)[102] <- 'Q095_VOCÊ.FOI.VÍTIMA.DE.ROUBO.COM.VIOLÊNCIA'
names(saeb)[103] <- 'Q096_ALUNOS.SOB.EFEITO.DE.BEBIDA.ALCOÓLICA'
names(saeb)[104] <- 'Q097_ALUNOS.SOB.EFEITO.DROGAS.ILÍCITAS'
names(saeb)[105] <- 'Q098_ALUNOS.COM.ARMA.BRANCA'
names(saeb)[106] <- 'Q099_ALUNOS.COM.ARMA.FOGO'
names(saeb)[107] <- 'Q100_TEMÁTICA.PROJETOS.VIOLÊNCIA'
names(saeb)[108] <- 'Q101_TEMÁTICA.PROJETOS.USO.DE.DROGAS'
names(saeb)[109] <- 'Q102_TEMÁTICA.PROJETOS.RACISMO'
names(saeb)[110] <- 'Q103_TEMÁTICA.PROJETOS.MACHISMO.E.HOMOFOBIA'
names(saeb)[111] <- 'Q104_TEMÁTICA.PROJETOS.BULLYING'
names(saeb)[112] <- 'Q105_TEMÁTICA.PROJETOS.SEXUALID.GRAVIDEZ.ADOLESC'
names(saeb)[113] <- 'Q106_TEMÁTICA.PROJETOS.DESIGUALDADES.SOCIAIS'
names(saeb)[114] <- 'Q107_TEMÁTICA.PROJETOS.DIVERSIDADE.RELIGIOSA'
names(saeb)[115] <- 'Q108_TEMÁTICA.PROJETOS.MEIO.AMBIENTE'
names(saeb)[116] <- 'Q109_ENSINO.RELIG.PRESENÇA.OBRIGATÓRIA'
names(saeb)[117] <- 'Q110_ENSINO.RELIG.RELIGIÃO.ESPECÍFICA'
names(saeb)[118] <- 'Q111_ENSINO.RELIG.ATIV.ESTUD.NÃO.QUEIRAM.PARTICIP'

# remove as colunas que não possuem dados que façam sentido para uma tabulação cruzada
saeb <- saeb[,c("ID_PROVA_BRASIL","ID_ESCOLA","IN_PREENCHIMENTO_QUESTIONARIO","ID_MUNICIPIO"):=NULL]


dataset <- saeb

# cria a interface com o usuário shiny

ui <- fluidPage(
  
  titlePanel("Explorador do SAEB 2015 - Questionário Diretores - https://github.com/jhcf/exploradorsaeb2015"),
  
  sidebarPanel(
  
    selectInput('col_name', 'Filtrar coluna', c('None', names(dataset))),
    
    selectInput('col_val', 'Valor filtro', choices=c('None')),

    sliderInput('tamanhoAmostra', 'Tamanho da amostra', 
                min=min(1, nrow(dataset)), 
                max=nrow(dataset),
                value=min(1000, nrow(dataset)), 
                step=floor(nrow(dataset)/20),
                round=0),
    
    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]])#,
  ),
  
  mainPanel(
               plotOutput("mosaico")
  )
)

# cria o módulo servidor

server <- function(input, output, session) {
  
  colName <- reactive({
    print('colName <- reactive')
    colName <- input$col_name
  })
  
  observe ({
    print('observe 1')
    colName <- input$col_name
    if (colName != 'None') {
      colNum <- which(colnames(dataset)==colName)
      print(paste('observe 1: length(colNum)',colNum))
      if (length(colNum)>0) {
        fatores <- dataset[[colNum]]
        escolhas <- c('None', levels(unique(factor(fatores))))
        updateSelectInput(session,'col_val',
                        label=paste("Filtrar registros com ",colName,' == ?'),
                        choices=escolhas,
                        selected = head(escolhas, 1))
      }
    }
  })  
  
  colVal <- reactive({
    print('colVal <- reactive')
    input$col_val
  })
  
  datasetFiltrado <- reactive({
    print('datasetFiltrado <- reactive')
    colName <- colName()
    if (colName != 'None') {
      colVal <- colVal()
      if (colVal != 'None') {
        colNum <- which(colnames(dataset)==colName)
        print(paste('datasetFiltrado <- reactive: colNum:',colNum," colVal:",colVal))
        datasetFiltrado <- subset(dataset, dataset[[colNum]]==colVal)
        print(paste('datasetFiltrado : nrowOriginal: ',nrow(dataset),' :nrowFiltrado:',nrow(datasetFiltrado)))
      } else {
        print(paste('datasetFiltrado <- reactive: else 1'))
        datasetFiltrado <- dataset
      }
    } else {
      print(paste('datasetFiltrado <- reactive: else 2'))
      datasetFiltrado <- dataset
    }
    datasetFiltrado
  })
  
  datasetAmostrado <- reactive ({
    print('datasetAmostrado <- reactive')
    dsFiltrado <- datasetFiltrado()
    sliderValue <- input$tamanhoAmostra
    if (nrow(dsFiltrado)<sliderValue) {
      sampleSize <- nrow(dsFiltrado)
    } else {
      sampleSize <- sliderValue
    }
    datasetAmostrado <- dsFiltrado[sample(nrow(dsFiltrado), sampleSize),]
    datasetAmostrado
  })
  
  observe({
    colName <- input$col_name
    colVal <- input$col_val
    print(paste('observe:input$col_name+input$col_val',colName,'+',colVal))
    if (colName != 'None' & colVal != 'None') {
      nrows <- nrow(datasetFiltrado())
      print(paste('observe:input$col_val:nrows',nrows))
      updateSliderInput(session,'tamanhoAmostra',
                        label = paste('Tamanho da amostra com ',colName,'=',colVal),
                      min=min(1, nrows), max=nrows,
                      step=floor(nrows/20),value=min(1000, nrows))
    }
  })  

#  dataset <- reactive({
#    saeb[sample(nrow(saeb), input$tamanhoAmostra),]
#  })
  
  output$mosaico <- renderPlot({
    print('output$mosaico')
    # cria a tabela de contingência com as colunas selecionadas
    dtset <- datasetAmostrado()
    colName <- input$col_name
    colVal <- input$col_val
    if (colVal != 'None') {
        tabelaContingencia <-
          xtabs(as.formula(sprintf("~%s+%s", input$x, input$y)),
            data=dtset)
    } else if (colName != 'None') {
      tabelaContingencia <-
        xtabs(as.formula(sprintf("~%s+%s+%s", input$x, input$y, colName)),
              data=dtset)
    } else {
      tabelaContingencia <-
        xtabs(as.formula(sprintf("~%s+%s", input$x, input$y)),
              data=dtset)
    }
    # imprime o gráfico mosaico (biblioteca vcd)
    print(mosaic(tabelaContingencia,gp=shading_Friendly,direction = 'v', 
                 labeling=
                   labeling_border(gp_labels = gpar(fontsize=9),
                                   rot_labels = c(0,90,90,0),
                                   just_labels = "right",
                                   tl_varnames = c(TRUE,FALSE)))
          )
    
  }, height=700)
  
}

# Executa a aplicação shiny 
shinyApp(ui = ui, server = server)

