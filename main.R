# Script para realizar uma busca no site de venda Estante Virtual e salvar 
# os resultados em um arquivo .csv

# Data de criação: 2022-12-24

# Autor: João Paulo Gomes Ricotta (ricotta.jpgomes@gmail.com)
# Obs: Esse código também pode ser encontrado no repositório
#      https://github.com/ricotta-jpgomes/estante_virtual



##### 01. Configurando o ambiente de execução do projeto #####

# Definindo o diretório de trabalho (especifique o seu diretório quando executar)
setwd(dir = "C:\\Users\\JP_Ri\\Documents\\R_projects\\estante_virtual")

# Importando dependências
library(tidyverse) 
library(data.table) 
library(stringr) 
library(rvest) 
library(xml2) 
library(readr)
library(dplyr)

##### 02. Definindo URL base e funções auxiliares #####

# URL base
url_base <- "https://www.estantevirtual.com.br" # Vamos realizar uma busca na Estante Virtual

# Funções auxiliares:
# 2.1. Função que faz a busca digitada pelo usuário e salva as informações no .csv
search_process <- function(search_url, page_number = 1) {
  
  
  cat(paste("##### PÁGINA", page_number, "#####\n"))
  raw_page <- read_html(search_url)
  
  items <- raw_page %>%
    html_node('[class="wrapper-livros__resultados livros"]') %>%
    # Na minha inspeção das páginas de resultados de busca, percebi 
    # que as div contendo informações sobre os livros possuem duas 
    # classes. Então, para pegar todas elas independente da classe, 
    # não deixando nenhuma informação para trás, bolei essa xpath 
    # expression para passar como argumento na função html_nodes():
    html_nodes(xpath='//descendant::div[@class="livro" or @class="livro exibe-desagrupado"]')

  # Chamando a função auxiliar process_results()
  infos <- result_process(items)
  
  # Salvando os dados da página
  path <- "C:\\Users\\JP_Ri\\Google Drive\\Datasets\\livros_info.csv" # substitua aqui com o caminho para o seu arquivo
  
  if(!file.exists(path)){
    # especifique o caminho para o seu próprio arquivo em 'path'
    # se esse arquivo ainda não existir, vamos simplesmente
    # gerar esse arquivo salvando as informações obtidas
    write_delim(infos, path, quote = "needed")
  }  else {
    write_delim(infos, path, append=TRUE, quote = "needed")
    # caso esse arquivo já exista, vamos fazer a gravação utilizando a mesma função, mas
    # especificando o argumento append com TRUE, ou seja, os dados serão salvos na 
    # sequência do que a gente já tem.
  }
  
  # Acessando a próxima página de resultados
  next_button <- raw_page %>%
    html_node('[class="paginador pg-footer"]') %>%
    html_node('[aria-label="proxima"]')
  
  status <- next_button %>%
    html_attr('class')
  
  next_page_number <- page_number + 1
  
  # O botão de next na última página de resultados possui essa classe especificada
  # no condicional abaixo, a cada iteração, se a classe do botão next (definida na 
  # variável 'status') for diferente, vamos chamar recursivamente a função search_process
  if (status != 'next nuxt-link-exact-active nuxt-link-active desativado') {
    next_page_url <- next_button %>%
      html_attr('href') %>%
      paste(url_base, ., sep = '')
    
    search_process(next_page_url, page_number = next_page_number)
  }
}

# Função que recebe uma lista de elementos HTML (no caso, a lista contendo cada resultado de 
# de busca na Estante Virtual) e extrai as informações de cada elemento em um dataframe
result_process <- function(elements){
  
  # Vou inicializar seis vetores vazios, que ao longo dos loops serão preenchidos com as
  # informações relativas a cada resultado de busca daquela página
  lvr_titulo <- vector() # O título da obra
  lvr_autor <- vector() # O autor
  lvr_valor <- vector() # O valor mínimo do livro
  lvr_link <- vector() # a url para as ofertas relativas àquele livro
  
  page_results <- length(elements)
  
  for(i in seq_len(page_results)){
    print(paste("Extraindo resultado", i, "de", page_results, "..."))
    # TITULO
    titulo <- elements[i] %>%
      html_node('[class="titulo-autor"]') %>%
      html_node('h2') %>%
      html_text() %>%
      # Vamos limpar os espaços com o str_trim()
      str_trim()
    
    # Adicionando o dado final ao nosso vetor de títulos.
    lvr_titulo <- append(lvr_titulo, titulo) # nessa linha eu pego a informação extraída e adiciono ao meu vetor.
    
    # AUTOR
    autor <- elements[i] %>%
      html_node('[class="titulo-autor"]') %>%
      html_node('[itemprop="author"]') %>%
      html_text() %>%
      # Vamos limpar os espaços com o str_trim()
      str_trim()
    
    # Adicionando o dado final ao nosso vetor de autores.
    lvr_autor <- append(lvr_autor, autor)
    
    # VALOR
    valor <- elements[i] %>%
      html_node('[class="precos"]') %>%
      html_node('[class="preco"]') %>%
      html_text()
    
    # Adicionando o dado final ao nosso vetor de valores.
    lvr_valor <- append(lvr_valor, valor)
    
    # LINK PARA AS OFERTAS
    link <- elements[i] %>%
      html_node('[class=ver-livros]') %>%
      html_node('a') %>%
      html_attr('href') %>%
      paste(url_base, ., sep='')
    
    # Adicionando o dado final ao nosso vetor de links.
    lvr_link <- append(lvr_link, link)
    
    # Definindo uma pausa breve entre cada loop
    Sys.sleep(.3)
  }
  
  processed_infos <- data.frame(lvr_titulo, lvr_autor, lvr_valor, lvr_link)
  return(processed_infos)
}

##### 03. Pegando informações do usuário e chmando funções
cat("Digite abaixo seu termo de busca: ")
search <- readLines(n = 1) %>%
  # substituindo eventuais espaços para não quebrar a requisição
  str_replace_all(' ', '%20') %>% 
  paste(url_base, '/busca?q=', ., sep='')

search_process(search)

