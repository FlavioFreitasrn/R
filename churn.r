#Ajuda a localizar o camiho do arquivo que será analisado
arquivo <- file.choose() # nolint
#exibe o caminho
#print(arquivo) # nolint

#Importando e carregando o arquivo da analise
dfchurn <- read.csv(arquivo,
                    sep = ";",
                    na.strings = " ",
                    stringsAsFactors = TRUE)

#Iniciando a analise exploratoria

#cONHECENDO A ESTRUTURA DA DATAFRAME
head(dfchurn)
#Sumarizando o df
summary(dfchurn)

#Alterando nome das colunas
colnames(dfchurn) <- c("Id", "Score", "Estado", "Genero", "Idade",
                        "Patrimonio", "Saldo", "Produto",
                        "PossuiCartaoCredito", "Ativo", "Salario", "Saiu")


#Analisando tabela Estado
eixo_contador <- table(dfchurn$Estado)
barplot(eixo_contador,
        main = "Estados",
        xlab = "Estados"
)
#preencher com a moda, RS
dfchurn[!dfchurn$Estado %in% c("RS", "SC", "PR"), ]$Estado <- "RS"
dfchurn$Estado <- factor(dfchurn$Estado)

#Analisando tabela Genero
eixo_contador <- table(dfchurn$Genero)
barplot(eixo_contador,
        main = "Genero",
        xlab = "Genero"
)
#Corrigindo nomenclatura e removendo a campos vazios
dfchurn[dfchurn$Genero %in% c("F", "Fem"), ]$Genero <- "Feminino"
dfchurn[dfchurn$Genero %in% c("M"), ]$Genero <- "Masculino"
dfchurn[dfchurn$Genero == "", ]$Genero <- "Masculino"
dfchurn$Genero <- factor(dfchurn$Genero)

#Analisando tabela Idade
eixo_contador <- table(dfchurn$Idade)
boxplot(eixo_contador,
        main = "Idade",
        xlab = "Idade"
)
#Substituindo a idade =140  e menor que 13 pela mediana
dfchurn[dfchurn$Idade > 130 | dfchurn$Idade < 13 , ]$Idade <- median(dfchurn$Idade, na.rm = TRUE) # nolint

#tratar salários
summary(dfchurn$Salario)
#ver mediana
median(dfchurn$Salario, na.rm = TRUE)
#atribuir mediana a NAs
dfchurn[is.na(dfchurn$Salario), ]$Salario <- median(dfchurn$Salario, na.rm = TRUE) # nolint

#buscar NAS em salario para checar
dfchurn[!complete.cases(dfchurn$Salario), ]

#buscar duplicados pelo ID
dfchurn[duplicated(dfchurn$Id), ]
dfchurn <- dfchurn[-c(82), ]
dfchurn[dfchurn$Id == x$Id, ]

#outliers, criando um parametro com desvio padrão
desv <- sd(dfchurn$Salario, na.rm = TRUE)
desv

dfchurn[dfchurn$Salario >= 2 * desv, ]$Salario
#outra forma, resultado semelhante, mas sem os NAs
boxplot(dfchurn$Salario)
boxplot(dfchurn$Salario, outline = FALSE)
x <- boxplot(dfchurn$Salario)$out
x
#atualizamos todos para mediana
median(dfchurn$Salario)
dfchurn[dfchurn$Salario >= 2 * desv, ]$Salario <- median(dfchurn$Salario)
#checamos se sairam os outliers
dfchurn[dfchurn$Salario >= 2 * desv, ]$Salario



#valores faltantes NAs
dfchurn[!complete.cases(dfchurn), ]

#GRAFICOS
library(plotly)

#GRAFICOS ESTADOS
eixox_estados <- levels(dfchurn$Estado)
eixoy_contador <- table(dfchurn$Estado)
#Grafico 1
plt_estados <- plot_ly(x = eixox_estados,
                        y = eixoy_contador,
                        name = "Estado",
                        type = "bar"
                        )

plt_estados

#Grafico 2
eixox_estados <- levels(dfchurn$Estado)
eixoy_contador <- table(dfchurn$Estado)
text <- c('27% market share', '24% market share', '19% market share') # nolint
data <- data.frame(eixox_estados, eixoy_contador, text)

plt_estados1 <- plot_ly(data,
                        x = ~eixox_estados,
                        y = ~eixoy_contador,
                        type = "bar",
                        text = eixoy_contador,
                        textposition = "auto",
                        marker = list(color = "rgb(158,202,225)",
                                        line = list(color = "rgb(8,48,107)",
                                        width = 1.5)
                                    )
                        )
plt_estados1 <- plt_estados1 %>% layout(title = "Estados",
                                        xaxis = list(title = ""),
                                        yaxis = list(title = "")
                                        )

plt_estados1


#GRAFICOS GENEROS
eixox_generos <- levels(dfchurn$Genero)
eixoy_contador <- table(dfchurn$Genero)
text <- c('27% market share', '24% market share', '19% market share') # nolint
data <- data.frame(eixox_generos, eixoy_contador, text)

plt_genero <- plot_ly(data,
                        x = ~eixox_generos,
                        y = ~eixoy_contador,
                        type = "bar",
                        text = eixoy_contador,
                        textposition = "auto",
                        marker = list(color = "rgb(158,202,225)",
                                        line = list(color = "rgb(8,48,107)",
                                        width = 1.5)
                                    )
                        )
plt_genero <- plt_genero %>% layout(title = "Generos",
                                        xaxis = list(title = ""),
                                        yaxis = list(title = "")
                                        )

plt_genero

#GRAFICOS IDADE
#BOXPLOT
eixox_idade <- dfchurn$Idade
plt_idade <- plot_ly(
                y = ~eixox_idade,
                type = "box",
                boxpoints = "all",
                jitter = 0.3,
                pointpos = -1.8
                )

plt_idade

#Histograma
library(plotly)

eixox_idade <- dfchurn$Idade
fig <- plot_ly(x = ~eixox_idade, type = "histogram")

fig



eixox_idade <- dfchurn$Idade
eixoy_contador <- table(dfchurn$Idade)
text <- c('27% market share', '24% market share', '19% market share') # nolint
data <- data.frame(eixox_idade, eixoy_contador, text)

plt_idade <- plot_ly(data,
                        x = ~eixox_idade,
                        y = ~eixoy_contador,
                        type = "bar",
                        text = eixoy_contador,
                        textposition = "auto",
                        marker = list(color = "rgb(158,202,225)",
                                        line = list(color = "rgb(8,48,107)",
                                        width = 1.5)
                                    )
                        )
plt_idade <- plt_idade %>% layout(title = "Idade",
                                        xaxis = list(title = ""),
                                        yaxis = list(title = "")
                                        )

plt_idade
