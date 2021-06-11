output_docx1 <- function(lists,gap=" ",filename="output.docx",captions=NULL,digits=2,theme="theme_booktabs",row_com=FALSE){

 # default parameters
  set_flextable_defaults(font.family = "Times New Roman",font.size=10,theme_fun=theme)

  lists <- map(lists,function(x)mutate_if(x,is.numeric, round, digits))

  if(row_com!=FALSE){
    lists <- map(lists,function(x)rownames_to_column(x,"row_names"))
  }

  # defensive:
  if (grepl("\\.docx",filename)==FALSE){
    stop("Wrong file extension,please set file name as xxxx.docx")
  }


  if (is.null(captions)){
    tables_list <- lists %>% map(.,flextable)
  }else{
    str_after <- strsplit(captions,";") %>% unlist()
    if (length(str_after)==length(lists)){
    tables_list <- lists %>% map(.,flextable) %>% map2(.,str_after,function(x,y)set_caption(x,caption = y))
    }else{
    stop("The number of cations does not match the number of tables.")
    }
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
  cli_alert_success("rendered {length(tables_list)} tables to {filename}")
}
