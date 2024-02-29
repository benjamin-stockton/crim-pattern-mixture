setwd("~/Documents/GitHub/crim-pattern-mixture")


sens_scales <- tidyr::tibble(set_n = 1:9,
                             scales = c(0.33, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 2))

purrr::pwalk(.l = sens_scales,
             .f = function(scales, set_n){
                 cat(
                     whisker::whisker.render(
                         readLines('pattern-mixture-modeling/full-data-analysis.tmpl'),
                         data = list(
                             sens_scale = scales,
                             n_cores = 124)
                     ),
                     file = file.path('pattern-mixture-modeling', 'analysis-scripts',
                                      sprintf("full-data-analysis-scale-%s.R",
                                              set_n)
                     ),
                     sep='\n')
             })


purrr::pwalk(.l = sens_scales,
             .f = function(scales, set_n){
                 cat(
                     whisker::whisker.render(
                         readLines('pattern-mixture-modeling/analysis.sh.tmpl'),
                         data = list(
                             set_n = set_n,
                             n_cores = 124)
                     ),
                     file = file.path('pattern-mixture-modeling', 'bash',
                                      sprintf("analysis-scale-%s.sh",
                                              set_n)
                     ),
                     sep='\n')
             })


# Sub sampling

sens_scales <- tidyr::tibble(set_n = 1:9,
                             scales = c(0.33, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, 2))

purrr::pwalk(.l = sens_scales,
             .f = function(scales, set_n){
                 cat(
                     whisker::whisker.render(
                         readLines('pattern-mixture-modeling/sub-data-analysis-brms.R.tmpl'),
                         data = list(
                             sens_scale = scales,
                             n_cores = 100)
                     ),
                     file = file.path('pattern-mixture-modeling', 'analysis-scripts',
                                      sprintf("sub-data-analysis-scale-%s.R",
                                              set_n)
                     ),
                     sep='\n')
             })


purrr::pwalk(.l = sens_scales,
             .f = function(scales, set_n){
                 cat(
                     whisker::whisker.render(
                         readLines('pattern-mixture-modeling/sub-analysis.sh.tmpl'),
                         data = list(
                             set_n = set_n,
                             n_cores = 100)
                     ),
                     file = file.path('pattern-mixture-modeling', 'bash',
                                      sprintf("sub-analysis-scale-%s.sh",
                                              set_n)
                     ),
                     sep='\n')
             })
