#' Transforma os arquivos associados a um processo em um conjunto de dados retangular
#'
#' @param path_file diretório onde os .html estão salvos
#' @param path_log diretório onde os logs estão salvos.
#' @param p Vetor com números de processos. Por padrão esse parâmetro não é utilizado.
#' @return Uma tabela de log do parse

parse_cpopg <- function(path_file, path_log, p = NULL){

  if(!is.null(p)){
    logs <- sprintf('%s/%s.rds', path_log, p)

    existe <- file.exists(logs)

    if(prod(existe) == 0){
      warning(sprintf('Some files are missing. Could not find %s',
        sample(logs[!existe], 1)))
    }
  } else {
    logs <- list.files(path_log, full.names = T)
  }

  d <- empilha_RDS(logs) %>%
    mutate(final = stringr::str_extract(arq,'\\_[0-9]{1,2}\\.html'))

  d_partes <- d %>%
    filter(final == '_4.html') %>%
    group_by(n_processo) %>%
    do(partes = {
      xml2::read_html(.$arq, encoding = 'utf-8') %>%
        rvest::html_table(fill = T) %>%
        dplyr::nth(3)
    })

  d_infos <- d %>%
    filter(final == '_0.html') %>%
    group_by(n_processo) %>%
    do(comarca_situacao_distribuicao = {
        xml2::read_html(.$arq, encoding = 'utf-8') %>%
        rvest::html_table(fill = T) %>%
        dplyr::nth(5) %>%
        select(X2,X3) %>%
        rename(key = X2, value = X3)},
      pedido_natureza = {
        xml2::read_html(.$arq, encoding = 'utf-8') %>%
        rvest::html_table(fill = T) %>%
        dplyr::nth(4) %>%
        select(X2,X3) %>%
        rename(key = X2, value = X3)
      }
    )

  d_notas_expediente <- d %>%
    filter(final == '_6.html') %>%
    group_by(n_processo) %>%
    do(notas = {
      xml2::read_html(.$arq, encoding = 'utf-8') %>%
        rvest::html_table(fill = T) %>%
        dplyr::nth(1)
    })

  d_movs <- d %>%
    filter(final == '_5.html') %>%
    group_by(n_processo) %>%
    do(movs = {
      xml2::read_html(.$arq, encoding = 'utf-8') %>%
        rvest::html_node("div[id='conteudo']") %>%
        rvest::html_nodes('table') %>%
        dplyr::nth(2) %>%
        rvest::html_table() %>%
        dplyr::select(X2,X3) %>%
        dplyr::rename(data_mov = X2, titulo = X3)
    })

  d_infos_adicionais <- d %>%
    filter(final == '_11.html') %>%
    group_by(n_processo) %>%
    do(infos2 = {
      xml2::read_html(.$arq, encoding = 'utf-8') %>%
          rvest::html_nodes('table') %>%
          dplyr::nth(3) %>%
          rvest::html_text()
      })

  d <- d_partes %>%
    left_join(d_infos, 'n_processo') %>%
    left_join(d_notas_expediente, 'n_processo') %>%
    left_join(d_movs, 'n_processo') %>%
    left_join(d_infos_adicionais, 'n_processo')

  saveRDS(d, 'd_tjrs.rds')
}
