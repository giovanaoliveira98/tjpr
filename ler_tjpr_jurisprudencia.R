library(dplyr)

# doc <- arquivos[8]
# 
# doc <- doc %>%
#   xml2::read_html(encoding = "UTF-8")
# 
# table <- doc %>%
#   rvest::html_table()
# 
# df <- table[[3]]
# 
# links <- doc %>%
#   xml2::xml_find_all("*//a[@target='_blank']/@href") %>% 
#   xml2::xml_text(trim=TRUE)


#Esse código só funciona para os termos que TÊM RESULTADO
ler_tjpr_jurisprudencia <- function(diretorio="."){
  
  arquivos <- list.files(diretorio, full.names = TRUE)
  
  pb <- progress::progress_bar$new(total=length(arquivos))
  
  func <- function(doc){
    doc <- doc %>%
      xml2::read_html(encoding = "UTF-8")
    
    table <- doc %>%
      rvest::html_table()
    
    df <- table[[3]]
      
    links <- doc %>%
      xml2::xml_find_all("*//a[@target='_blank']/@href") %>% 
      xml2::xml_text(trim=TRUE) %>% 
      paste0("https://portal.tjpr.jus.br", .)
    
    tibble::tibble(df, links)
  }
  df <- purrr::map_dfr(arquivos, func)
}

df_resultado_links <- ler_tjpr_jurisprudencia(diretorio="data-raw")

df_resultado_links$links <- writexl::xl_hyperlink(unlist(df_resultado_links$links))

writexl::write_xlsx(df_resultado_links, "data/primeiradf.xlsx")

