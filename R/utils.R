#' PDF para texto
#'
#' Função para transformar um pdf em texto.
#'
#' @param a arquivo PDF que será transformado
#' @param first_pg (NA) primeira página do arquivo
#' @param last_pg (NA) última página do arquivo
#' @param raw (FALSE)
#' @param keep_file (FALSE)
#' @param new_file nome do arquivo txt
#' @param overwrite (FALSE) você quer apagar o arquivo?
#'
#' @rdname pdf2text
#'
#' @export
pdf2text <- dplyr::failwith(FALSE, function(a, new_file = 'repo.txt',
                                            first_pg = NA, last_pg = NA, raw = T) {

  if(pdf2text_verify(a)){
    pdf2text_system(a, new_file, raw, first_pg, last_pg)
    return(T)
  } else {
    return(F)
  }

})

#' Só faz wrap do system
#'
#' Função auxiliar para simplificar o código.
#'
#' @rdname pdf2text
#'
pdf2text_system <- function(a, new_file, raw, first_pg, last_pg){
  sprintf("pdftotext %s %s%s%s%s", a,
          ifelse(raw, "-raw ", " "),
          ifelse(!is.na(first_pg), paste("-f", first_pg, ""), " "),
          ifelse(!is.na(last_pg), paste("-l", last_pg, ""), " "),
          new_file) %>% system()
}

#' Verificar para o PDF2TEXT
#'
#' Faz as verificações necessárias para o pdf2text
#'
#' @rdname pdf2text
#'
pdf2text_verify <- function(a){
  if(file.exists(a)){
    file.size(a) > 500
  } else {
    FALSE
  }
}

#' Lê e empilha os RDS's de uma pasta
#'
#' @param path Diretório que contém os RDS
empilha_RDS <- function(path){
  plyr::llply(path, readRDS) %>%
    dplyr::bind_rows()
}

`%>%` <- magrittr::`%>%`


