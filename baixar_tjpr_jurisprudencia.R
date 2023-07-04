#Definindo os parâmetros
pesquisa <- "airbnb"
diretorio <- ""



#URL de base
url <- "https://portal.tjpr.jus.br/jurisprudencia/publico/pesquisa.do?actionType=pesquisar"

# #Encontrando os parâmetros da lista de query

# abjutils::chrome_to_body("backURL: 
# postCampo: 
# tmp: 
# criterioPesquisa: airbnb
# processo: 
# acordao: 
# idRelator: 
# nomeRelator: 
# idOrgaoJulgadorSelecao: 
# nomeOrgaoJulgador: 
# idComarca: 
# nomeComarca: 
# idClasseProcessual: 
# descricaoClasseProcessualHidden: 
# descricaoClasseProcessual: 
# idAssunto: 
# descricaoAssuntoHidden: 
# descricaoAssunto: 
# dataJulgamentoInicio: 
# dataJulgamentoFim: 
# dataPublicacaoInicio: 
# dataPublicacaoFim: 
# idLocalPesquisa: 1
# ambito: -1
# idsTipoDecisaoSelecionados: -1
# segredoJustica: pesquisar com
# iniciar: Pesquisar")

#Definindo a query
query <- list(
  "backURL" = "",
  "postCampo" = "",
  "tmp" = "",
  "criterioPesquisa" = pesquisa,
  "processo" = "",
  "acordao" = "",
  "idRelator" = "",
  "nomeRelator" = "",
  "idOrgaoJulgadorSelecao" = "",
  "nomeOrgaoJulgador" = "",
  "idComarca" = "",
  "nomeComarca" = "",
  "idClasseProcessual" = "",
  "descricaoClasseProcessualHidden" = "",
  "descricaoClasseProcessual" = "",
  "idAssunto" = "",
  "descricaoAssuntoHidden" = "",
  "descricaoAssunto" = "",
  "dataJulgamentoInicio" = "",
  "dataJulgamentoFim" = "",
  "dataPublicacaoInicio" = "",
  "dataPublicacaoFim" = "",
  "idLocalPesquisa" = "99",
  "ambito" = "-1",
  "idsTipoDecisaoSelecionados" = "-1",
  "segredoJustica" = "pesquisar com",
  "iniciar" = "Pesquisar",
  "pageNumber" = "1")

#Achar o número de páginas
n_paginas <- url |>
  httr::GET(query = query) |>
  httr::content() |>
  xml2::xml_find_first("//*[@id='navigator']/div[2]") |>
  xml2::xml_text(trim = TRUE) |>
  stringr::str_extract("\\d+")

#Percorrendo as páginas
purrr::walk(1:n_paginas, purrr::possibly(~{
  
  #Criando a hora para escrever o arquivo

  hora <- Sys.time() %>% 
    stringr::str_replace_all("\\D","_")
  
  #Escrevendo o arquivo a partir do parâmetro diretório da função
  
  arquivo <- diretorio %>% 
    file.path(glue::glue("{hora}_pagina_{.x}.html"))

  #Convertendo o inteiro para string para por na query
  pg <- toString(.x)
  
  #Inserindo a pagina na query
  query$pageNumber <- pg
  
  #Escrevendo o arquivo
  httr::GET(url, 
            query = query,
            httr::write_disk(arquivo, overwrite = FALSE))
}, otherwise = NULL))


# A fazer -----------------------------------------------------------------

purrr::walk(1:n_paginas, purrr::possibly(~{
  
  hora <- Sys.time() %>% 
    stringr::str_replace_all("\\D","_")
  
  arquivo <- diretorio %>% 
    file.path(glue::glue("{hora}_pagina_{.x}.html"))
  
  pstr <- toString(.x)
  
  parseada <- structure(
    list(
      scheme="https",
      hostname="esaj.tjsp.jus.br",
      port=NULL,
      path="cpopg/trocarPagina.do",
      query=list(
        paginaConsulta=pstr,
        conversationId="",
        cbPesquisa=pesquisa,
        dadosConsulta.valorConsulta=dadosConsulta,
        cdForo=foro),
      params=NULL,
      fragment=NULL,
      username=NULL,
      password=NULL), class="url")
  
  url <- httr::build_url(parseada)
  
  httr::GET(url, httr::write_disk(arquivo, overwrite = FALSE))
}, otherwise=NULL))



# Função ------------------------------------------------------------------

baixar_tjpr_jurisprudencia <- function(pesquisa, diretorio = ".", dataInicio = "", dataFim = ""){
  
  #URL de base
  url <- "https://portal.tjpr.jus.br/jurisprudencia/publico/pesquisa.do?actionType=pesquisar"
  
  #Definindo a query
  query <- list(
    "backURL" = "",
    "postCampo" = "",
    "tmp" = "",
    "criterioPesquisa" = pesquisa,
    "processo" = "",
    "acordao" = "",
    "idRelator" = "",
    "nomeRelator" = "",
    "idOrgaoJulgadorSelecao" = "",
    "nomeOrgaoJulgador" = "",
    "idComarca" = "",
    "nomeComarca" = "",
    "idClasseProcessual" = "",
    "descricaoClasseProcessualHidden" = "",
    "descricaoClasseProcessual" = "",
    "idAssunto" = "",
    "descricaoAssuntoHidden" = "",
    "descricaoAssunto" = "",
    "dataJulgamentoInicio" = dataInicio,
    "dataJulgamentoFim" = dataFim,
    "dataPublicacaoInicio" = "",
    "dataPublicacaoFim" = "",
    "idLocalPesquisa" = "99",
    "ambito" = "-1",
    "idsTipoDecisaoSelecionados" = "-1",
    "segredoJustica" = "pesquisar com",
    "iniciar" = "Pesquisar",
    "pageNumber" = "1")
  
  #Achar o número de páginas
  n_resultados <- url |>
    httr::GET(query = query) |>
    httr::content() |>
    xml2::xml_find_first("//*[@id='navigator']/div[2]") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_extract("\\d+") |>
    as.integer()
  
  n_paginas <- (n_resultados%/%10)+1
    
  #Percorrendo as páginas
  purrr::walk(1:n_paginas, purrr::possibly(~{
    
    #Criando a hora para escrever o arquivo
    
    hora <- Sys.time() %>% 
      stringr::str_replace_all("\\D","_")
    
    #Criando o termo para escrever o arquivo
    termo_limpo <- janitor::make_clean_names(pesquisa)
    
    #Escrevendo o arquivo a partir do parâmetro diretório da função
    
    arquivo <- diretorio %>% 
      file.path(glue::glue("{hora}_pagina_{.x}_{termo_limpo}.html"))
    
    #Convertendo o inteiro para string para por na query
    pg <- toString(.x)
    
    #Inserindo a pagina na query
    query$pageNumber <- pg
    
    #Escrevendo o arquivo
    httr::GET(url, 
              query = query,
              httr::write_disk(arquivo, overwrite = FALSE))
  }, otherwise = NULL))
}





