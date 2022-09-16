
# source('./inst/developer/save_data.R')

rm(list = ls(all.names = TRUE))

files = list.files(path = './inst/extdata', pattern = '\\.csv$', full.names = TRUE)
nms = gsub('\\.csv$', replacement = '', x = basename(files))

pkg_doc = character()

for (i in seq_along(nms)) { # (i = 1L)
  ival = read.csv(file = files[i], header = TRUE)
  assign(nms[i], value = ival)
  tmp = strsplit(nms[i], split = '_')[[1L]]
  pkg_doc = c(
    pkg_doc, 
    paste0('#\' @title ', switch(tmp[1L], EXA =, EXR = {
      tmp[2:3] = substr(tmp[2:3], start = 2L, stop = 3L)
      paste('Data of', switch(tmp[1L], EXA = 'Example', EXR = 'Exercise'),
            paste(as.numeric(tmp[-1L]), collapse = '.'))
    }, LDS = {
      paste0('Large Data \\code{', tmp[3L], '} from Chapter ', 
             as.numeric(substr(tmp[2L], start = 2L, stop = 3L)))
    }, REV = {
      paste('Review Exercise', as.numeric(tmp[3L]), 'of Chapter', 
            as.numeric(substr(tmp[2L], start = 2L, stop = 3L)))
    }, stop())),
    paste0('#\' @description \\code{', nms[i], '}'), 
    paste('#\' @format A \\link[base]{data.frame} with', 
          .row_names_info(ival, type = 2L), 'rows and', length(ival), 'columns'),
    paste0('\'', nms[i], '\''), 
    ''
  )
}

writeLines(pkg_doc, con = './R/data_doc.R')

save(list = nms, file = './data/data.rda', compress = 'xz')

rm(list = ls(all.names = TRUE))

