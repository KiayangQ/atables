
#' @title output_docx
#'
#' @description A simple function that helps to output tables in .docx
#'
#' @param ... One or more data.frames
#'
#' @param gap Separator between each table, the default is a SPACE
#'
#' @param filename File name of the .docx
#'
#' @param captions Title for each table
#'
#' @param digits Number of digits shown in the table, the default is 2
#'
#'
#' @examples
#'
#' output_docx(cars,iris)
#'
#'
#' @importFrom cli cli_alert_success
#'
#' @importFrom purrr map map2
#'
#' @importFrom flextable set_flextable_defaults flextable body_add_flextable set_caption
#'
#' @import officer
#'
#' @import magrittr
#'
#'
#' @export



output_docx <- function(...,gap=" ",filename="output.docx",captions=NULL,digits=2){


  set_flextable_defaults(digits = digits)

  # defensive:
  if (grepl("\\.docx",filename)==FALSE){
    stop("Wrong file extension,please set file name as xxxx.docx")
  }

  dfs <- map(list(...),class) %>% unlist()

  if (all(unique(dfs)%in%c("tbl_df","tbl","data.frame","data.table"))==FALSE){
    stop("the rendered objects include non-data.frame.Please check the input.")
  }


  if (is.null(captions)){
    tables_list <- list(...) %>% map(.,flextable)
  }else if(is.null(captions)!=TRUE&length(list(...))==length(captions)){
    tables_list <- list(...) %>% map(.,flextable) %>% map2(.,captions,function(x,y)set_caption(x,caption = y))
  }else if (is.null(captions)!=TRUE&length(list(...))!=length(captions)){
    stop("The number of cations does not match the number of tables.")
  }

  if (length(tables_list)==1){
    trans_tables <- read_docx() %>% body_add_flextable(tables_list[[1]])
  }else if (length(tables_list)>1){
    trans_tables_str <- "read_docx()%>%body_add_flextable(tables_list[[1]])"
    iterions <- seq(2,length(tables_list))
    strs <- "%>% body_add_par(.,gap) %>% body_add_flextable(tables_list"
    trans_table_srt1 <- purrr::map(iterions,function(x)paste0(strs,"[[",x,"]]",")"))
    before_final <- paste0(unlist(trans_table_srt1),collapse=" ")
    final <- paste0(trans_tables_str,before_final)
    trans_tables <- eval(parse(text=final))
  }
  print(trans_tables,target=filename)
  filename <- paste0(getwd(),"/",filename)
  cli_alert_success("{length(tables_list)} tables have been rendered to {filename}")
}
