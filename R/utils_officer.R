
# === === === === === === === === === === 
# Some parameters used in the functions
# === === === === === === === === === === 

txt_page_width <- docx_dim(my_doc)$page[[1]] - docx_dim(my_doc)$margins[[3]] - docx_dim(my_doc)$margins[[4]]

inter_col_width <- cm_to_in
txt_col_width   <- (txt_page_width - inter_col_width)/2

# === === === === === === === === === === 
# Functions
# === === === === === === === === === === 


add_heading1 <- function(heading_text){
  body_add_par(x = my_doc, 
               style = 'heading 1', 
               value = heading_text)
}


add_heading2 <- function(heading_text){
  body_add_par(x = my_doc, 
               style = 'heading 2', 
               value = heading_text)
}


add_end_section_continuous <- function(){
  body_end_section_continuous(x = my_doc)
}


# To consider to unify the two function below by using (txt_page_width - 1 * cm_to_in)/2

add_end_section_2columns <- function(widths = rep(txt_col_width, 2)){
  body_end_section_columns(
    x = my_doc, 
    widths = widths, 
    sep = FALSE, 
    space = 1 * cm_to_in)
}


add_end_section_3columns <- function(widths = rep(txt_col_width * 2/3, 3)){
  body_end_section_columns(
    x = my_doc, 
    widths = widths, 
    sep = FALSE, 
    space = c(inter_col_width/2))
}


add_figure_map_world <- function(object_name, figure_title, width = 10 * cm_to_in, height = 6.66 * cm_to_in){
  body_add_img(
    x = my_doc, 
    style = 'Figure body', 
    src = file.path(path.local.graphs, object_name), 
    width = width, 
    height = height) %>% 
    body_add_par(
      style = 'Figure title', 
      value = figure_title)
}


add_figure_map_world_grid <- function(object_name, figure_title){
  body_add_img(
    x = my_doc, 
    style = 'Figure body', 
    src = file.path(path.local.graphs, object_name), 
    width = 7, 
    height = 3) %>% 
    body_add_par(
      style = 'Figure title', 
      value = figure_title)
}


add_figure_dot_plot <- function(object_name, figure_title, width = 6, height = 4){
  body_add_img(
    x = my_doc, 
    style = 'Figure body', 
    src = file.path(path.local.graphs, object_name), 
    width = width, 
    height = height) %>% 
    body_add_par(
      style = 'Figure title', 
      value = figure_title)
}


add_table <- function(table_title, object_name, width = 5, height = 5 * 1.414){
  body_add_par(
    x = my_doc, 
    style = 'Table title', 
    value = table_title) %>% 
    body_add_img(
      style = 'Table as Figure', 
      src = file.path(path.local.tables, object_name), 
      width = width, 
      height = height)
}


add_par_normal <- function(par_text = 'More text here...'){
  body_add_par(
    x = my_doc, 
    style = 'Normal',
    value = par_text)
}



add_bullet_normal <- function(bullet_text = 'Bullet text here...'){
  body_add_par(
    x = my_doc, 
    style = 'Normal bullet',
    value = bullet_text)
}
