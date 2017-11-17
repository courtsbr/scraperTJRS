#' Busca livre nos diários judiciais do TJRS
#'
#' Busca processos nos DJE's do TJRS com a opção de "busca livre" no link:
#' http://www.tjrs.jus.br/busca/?tb=dj
#'
#' @param palavra string com o texto que será procurado. Não é case-sensitive.
#' @param session boolean indicando se a resposta será um objeto "response" do httr ou uma "html_session" do rvest.
#' @return  A sessão ou resposta obtida quando se submete o form de busca livre.
#' @seealso O form preenchido nessa função também pode ser usado pra fazer busca processual nos DJE's.
#' @examples
#' busca_livre_tjrs('santa maria') %>%
#' xml2::read_html() %>%
#' rvest::html_nodes('a[ctype="c"]') %>%
#' rvest::html_text()

busca_livre_tjrs <- function(word,ed = '',session = F){
  URL_pesq <- "http://www.tjrs.jus.br/busca/?tb=dj"
  s <- rvest::html_session(URL_pesq)

  form <- s %>%
    rvest::html_form() %>%
    dplyr::first() %>%
    rvest::set_values('q' = word,
                      'edicao' = ed,
                      'requiredfields' = paste0("NumeroEdicao:",ed),
                      'busca' = 'livre',
                      'site'='dag-completo')

  r <- s %>%
    rvest::submit_form(form)

  if(session){return(r)}else{return(r$response)}
}

#' Rapagem de todas as ocorrências de um texto no DJE's do TJRS
#'
#' Raspa todos os matchs de uma busca em
#' http://www.tjrs.jus.br/busca/?tb=dj
#'
#' @param palavra string com o texto que será procurado. Não é case-sensitive.
#' @return Retorna um data_frame contendo as edições e as páginas dos matchs encontrados.
#' @seealso Da página não é possível raspar o parágrafo do match, mas é possível raspar exatamente a página em que ocorre o pareamento. Não importa quantos matches tenham ocorrido, ele só te mostra, no máximo, 1000.
#' @examples
#' matches_busca_livre_tjrs('bolota')

matches_busca_livre_tjrs <- function(word, ed = ''){

  s <- busca_livre_tjrs(word, ed ,T)

  links <- rvest::html_nodes(s, "a")
  text <- rvest::html_text(links)

  link_proxima_pagina <- sum(text %in% 'Próximo')

  matches <- list()

  matches[[1]] <- s %>%
    rvest::html_nodes('a[class="result-title-link"]') %>%
    rvest::html_attr('href')

  i = 2

  while(link_proxima_pagina){

    s  %<>%
      rvest::follow_link('Próximo')

    matches[[i]] <- s %>%
      rvest::html_nodes('a[class="result-title-link"]') %>%
      rvest::html_attr('href')

    links <- rvest::html_nodes(s, "a")
    text <- rvest::html_text(links)

    i = i + 1
    link_proxima_pagina <- sum(text %in% 'Próximo')
  }

  return(matches)
}

#' Download de DJE do TJRS
#'
#' Faz a requisição para download do PDF de DJE do TJRS
#'
#' @param caderno Número inteiro que representa o caderno a ser baixado. Dependendo da época esse número tem interpretações diferentes. Veja as vignettes para mais informações.
#' @param edicao Número da edição. A primeira edição disponível é a de número 3424, de agosto de 2006. Aparentemente é contínuo, veja as vignettes para mais informações.
#' @param path caminho p/ salvar o pdf
#'

download_pdf_tjrs <- dplyr::failwith(NA, function(link, path){
  g <- httr::GET(link, httr::write_disk(path, overwrite = T))
  g
})

#' Download Diário Oficial
#'
#' Faz download de um diário oficial a partir de sua edição e caderno
#'
#' @param caderno Número inteiro que representa o caderno a ser baixado. Dependendo da época esse número tem interpretações diferentes. Veja as vignettes para mais informações.
#' @param edicao Número da edição. A primeira edição disponível é a de número 3424, de agosto de 2006. Aparentemente é contínuo, veja as vignettes para mais informações.
#' @param path caminho p/ salvar o pdf
#'
#' @note atualmente os arquivos são salvos no formato:
#' tjpr_dje_data_numDiario.pdf dentro da pasta especificada
#'
#' @return retorna TRUE se o download foi bem realizado de acordo
#' com a função verify_download.
#'
#' @export

download_diario_oficial_tjrs <- dplyr::failwith(F, function(data, nome_caderno, link, pasta){
  arq <- sprintf("%stjrs_dje_%s_%s.pdf",pasta, data, nome_caderno)
  download_pdf_tjrs(link, arq)
  verify_download(arq)
})

#' Verificar download
#'
#' Verifica se o arquivo foi baixado corretamente.
#' TODO: adicionar mais verificações. Atualmente é só pelo
#' tamanho do arquivo.
#'
#' @param path caminho do arquivo que acabou de ser baixado
#'
verify_download <- function(path){
  file.size(path) > 500
}

#' Lista cadernos e links disponíveis
#'
#' Fornece uma lista dos diários e links disponíveis de um determinado par "mes-ano".
#'
#' @param mesano String no formato "mês-ano". Apenas os diários de 06-2006 em diante estão disponíveis.
#' @return Um data_frame em que cada linha representa um caderno. Contém a data de publicação do caderno, o link de download e o nome do caderno.
lista_dje_mes <- function(mesano){

  u <- sprintf('http://www.tjrs.jus.br/servicodag/?callback=edicoesPeriodo&tipo=periodo&periodo=%s&_=1467218025980',mesano)

httr::GET(u) %>%
  xml2::read_html() %>%
  rvest::html_node('p') %>%
  rvest::html_text() %>%
  stringi::stri_extract_all(regex = '(?<=").*?(?=")') %>%
  dplyr::first() %>%
  grep('Label|id|Url',.,invert = T, value = T) %>%
  dplyr::data_frame() %>%
  setNames('value') %>%
  dplyr::mutate(tipo = ifelse(grepl('[-]',value),'data','nome_caderno'),
                tipo = ifelse(grepl('http://',value,fixed = T),'tipo_e_edição',tipo),
                id = cumsum(tipo == 'data')) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(data = value[1]) %>%
  dplyr::filter(tipo != 'data') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id2 = cumsum(tipo == 'nome_caderno')) %>%
  dplyr::group_by(id2) %>%
  dplyr::mutate(link = stringi::stri_extract_all(regex = 'tp=[0-9]{1}&ed=[0-9]{4}', value[2], simplify = T),
                link = sprintf('http://www3.tjrs.jus.br/servicos/diario_justica/download_edicao.php?%s',link)) %>%
  dplyr::filter(tipo == 'nome_caderno') %>%
  dplyr::ungroup() %>%
  dplyr::select(data,link,tipo,value) %>%
  reshape2::dcast(data+link ~ tipo)
}

#' Procurar um índice num arquivo de texto
#'
#' Obtém uma tabela de índice a partir de um arquivo de texto. Assume que os tópicos do sumário estejam separados em linhas.
#'
#' @param arquivo_texto Um arquivo.txt bem formatado.
#' @return Um data_frame com duas colunas: topico e página.
extrair_indice <- function(arquivo_texto){

  n_linhas <- 100
  texto <- readr::read_lines(arquivo_texto, n_max = n_linhas)

  k <-  grepl('........', texto, fixed = T)

  indice <- texto[k]

  if(k[n_linhas] == T | sum(k) == 0){
    n_linhas <- n_linhas + 10

    texto <- readr::read_lines(arquivo_texto, n_max = n_linhas)

    if(sum(k) == 0 & n_linhas > length(texto)){
      stop('Não encontrei nada parecido com um índice.')
    }

    k <-  grepl('........', texto, fixed = T)

    indice <- texto[k]
  }

  dplyr::data_frame(indice) %>%
    tidyr::separate(indice, into = c('topico','pagina'), extra = 'merge', sep = '[.]') %>%
    dplyr::mutate(pagina = as.numeric(gsub('[.]','',pagina)),
                  ult_pagina = dplyr::lead(pagina)-1,
                  ult_pagina = ifelse(ult_pagina == 0|is.na(ult_pagina), pagina, ult_pagina))
  }

#' Consulta um número de processo no sistema do TJRS
#'
#' Salva as páginas baixadas a partir de um vetor de números de processos.
#'
#' @param p Vetor com números de processos.
#' @param path_file diretório onde os .html serão salvos
#' @param path_log diretório onde o log será salvo. Quando recebe NULL, seu valor padrão,
#' apenas retorna o log em um objeto do R.
#' @return Uma tabela de log do download

cpo_pg_um <- function(p, path_file, path_log = NULL){

  base <- "http://www.tjrs.jus.br/site_php/consulta"
  u <- paste0(base, "/human_check/humancheck_showcode.php")

  p_limpo <- gsub('[^0-9]','',p)
  p_limpo2 <- gsub('[^0-9]','',substr(p,4,nchar(p)))
  tabelas <- 1
  ii = 1
  cod_comarca <- substr(p, 1,3)

  param <- comarcas_tjrs %>%
    filter(codigo_themis == cod_comarca) %>%
    with(c(nome_comarca,id_comarca))

  while(tabelas != 12){

    if(ii == 11){
      return(data_frame('ERRO'))
    }

    s <- rvest::html_session(base)

    tmp <- tempfile()

    s %>%
      rvest::jump_to(u) %>%
      '$'('response') %>%
      httr::content('raw')  %>%
      writeBin(tmp)

    captcha <- tmp %>%
      captchaTJRS::predizer()

    r <- s %>%
      rvest::html_form() %>%
      dplyr::first() %>%
      rvest::set_values(
        'nome_comarca' = param[1],
        'id_comarca' = param[2],
        'versao_fonetica' = '2',
        'tipo' = '1',
        'num_processo_mask' = p,
        'num_processo' = p_limpo2,
        'id_comarca2' = '700',
        'uf_oab' = 'RS',
        'numCNJ' = 'N',
        'tipo_pesq' = 'F',
        'code' = captcha) %>%
      rvest::submit_form(s, .)

      tabelas <- r %>%
        rvest::html_table(fill = T) %>%
        length

      ii = ii + 1
  }

  finais <- r %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href')

  #converte o texto das url pra hexadecimal

  finais <- gsub(' ','%20', finais, fixed = T)
  finais <- gsub('í','%ED', finais, fixed = T)
  finais <- gsub('ª','%AA', finais, fixed = T)

  log <- plyr::ldply(c(0,seq(finais)), function(x){

    if(x == 0){

      link <- base
      arq <- sprintf('%s/%s_%s.html',path_file, p_limpo, x)

      r$response %>% httr::content('text') %>%
        cat(file = arq)
    } else {

      link <- paste0(base,'/',finais[x])
      arq <- sprintf('%s/%s_%s.html',path_file, p_limpo, x)

      rvest:::request_GET(r, link) %>%
        '$'('response') %>%
        httr::content('text') %>%
        cat(file = arq)
    }
    return(data.frame(link, tmstmp = timestamp(quiet = T), arq))
  })

  log <- cbind(n_processo = p_limpo, log, tentativas = ii-1) %>%
    dplyr::as_data_frame()

  if(!is.null(path_log)){
    saveRDS(log,sprintf('%s/%s.rds',path_log,p_limpo))
  }

  return(log)
}


#' Consulta um número de processo na ferramenta de consulta processual do TJRS
#'
#' Salva as páginas baixadas a partir de um vetor de números de processos.
#'
#' @param p Vetor com números de processos.
#' @param path_file diretório onde os .html serão salvos
#' @param path_log diretório onde o log será salvo. Quando recebe NULL, seu valor padrão,
#' apenas retorna o log em um objeto do R.
#' @return Uma tabela de log do download

cpo_pg_um <- function(p, path_file, path_log = NULL){

  base <- "http://www.tjrs.jus.br/site_php/consulta"
  u <- paste0(base, "/human_check/humancheck_showcode.php")

  p_limpo <- gsub('[^0-9]','',p)
  p_limpo2 <- gsub('[^0-9]','',substr(p,4,nchar(p)))
  tabelas <- 1
  ii = 1
  cod_comarca <- substr(p, 1,3)

  param <- comarcas_tjrs %>%
    filter(codigo_themis == cod_comarca) %>%
    with(c(nome_comarca,id_comarca))

  while(tabelas != 12){

    if(ii == 11){
      return(data_frame('ERRO'))
    }

    s <- rvest::html_session(base)

    tmp <- tempfile()

    s %>%
      rvest::jump_to(u) %>%
      '$'('response') %>%
      httr::content('raw')  %>%
      writeBin(tmp)

    captcha <- tmp %>%
      captchaTJRS::predizer()

    r <- s %>%
      rvest::html_form() %>%
      dplyr::first() %>%
      rvest::set_values(
        'nome_comarca' = param[1],
        'id_comarca' = param[2],
        'versao_fonetica' = '2',
        'tipo' = '1',
        'num_processo_mask' = p,
        'num_processo' = p_limpo2,
        'id_comarca2' = '700',
        'uf_oab' = 'RS',
        'numCNJ' = 'N',
        'tipo_pesq' = 'F',
        'code' = captcha) %>%
      rvest::submit_form(s, .)

    tabelas <- r %>%
      rvest::html_table(fill = T) %>%
      length

    ii = ii + 1
  }

  finais <- r %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href')

  #converte o texto das url pra hexadecimal

  finais <- gsub(' ','%20', finais, fixed = T)
  finais <- gsub('í','%ED', finais, fixed = T)
  finais <- gsub('ª','%AA', finais, fixed = T)

  log <- plyr::ldply(c(0,seq(finais)), function(x){

    if(x == 0){

      link <- base
      arq <- sprintf('%s/%s_%s.html',path_file, p_limpo, x)

      r$response %>% httr::content('text') %>%
        cat(file = arq)
    } else {

      link <- paste0(base,'/',finais[x])
      arq <- sprintf('%s/%s_%s.html',path_file, p_limpo, x)

      rvest:::request_GET(r, link) %>%
        '$'('response') %>%
        httr::content('text') %>%
        cat(file = arq)
    }
    return(data.frame(link, tmstmp = timestamp(quiet = T), arq))
  })

  log <- cbind(n_processo = p_limpo, log, tentativas = ii-1) %>%
    dplyr::as_data_frame()

  if(!is.null(path_log)){
    saveRDS(log,sprintf('%s/%s.rds',path_log,p_limpo))
  }

  return(log)
}
