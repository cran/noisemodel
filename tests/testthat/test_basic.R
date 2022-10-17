###############################################################
###############################################################
###############################################################

library(noisemodel)

model <- c("sym_cuni_cn", "uncs_guni_cn", "sym_cuni_an", "sym_uni_an", "symd_uni_an", 
           "sym_int_an", "sym_gau_an", "symd_gau_an", "sym_sgau_an", "sym_end_an", 
           "unc_fixw_an", "symd_rpix_an", "unc_vgau_an", "symd_gimg_an", "asy_uni_an", 
           "asy_int_an", "imp_int_an", "boud_gau_an", "sym_cuni_ln", "sym_exc_ln", 
           "sym_ddef_ln", "sym_def_ln", "sym_uni_ln", "sym_hie_ln", 
           "sym_dran_ln", "sym_adj_ln", "sym_nuni_ln", "sym_natd_ln", "sym_usim_ln", 
           "sym_opt_ln", "sym_pes_ln", "sym_dia_ln", "sym_nexc_ln", "sym_hienc_ln", 
           "glev_uni_ln", "sym_cen_ln", "sym_con_ln", "sym_nean_ln", 
           "asy_def_ln", "maj_udir_ln", "asy_uni_ln", "minp_uni_ln", 
           "mulc_udir_ln", "pai_bdir_ln", "irs_bdir_ln", 
           "fra_bdir_ln", "mind_bdir_ln", "asy_spa_ln", "opes_idu_ln", 
           "opes_idnn_ln", "exps_cuni_ln", "qua_uni_ln", "attm_uni_ln", "oned_uni_ln", 
           "smu_cuni_ln", "hubp_uni_ln", "larm_uni_ln", "sigb_uni_ln", "gau_bor_ln", 
           "gaum_bor_ln", "ugau_bor_ln", "lap_bor_ln", "ulap_bor_ln", "nei_bor_ln", 
           "nlin_bor_ln", "smam_bor_ln", "mis_pre_ln", "sco_con_ln", "clu_vot_ln", 
           "pmd_con_ln", "exp_bor_ln", "gam_bor_ln")

###############################################################
###############################################################
###############################################################

apply_def_model <- function(m, x, y, l){
  
  data(iris2D)
  tramat <- matrix(data = c(0.9, 0.03, 0.07, 0.03, 0.9, 0.07, 0.03, 0.07, 0.9), 
                   nrow = 3, ncol = 3, byrow = TRUE)
  
  set.seed(9)
  if(model[m] %in% c("asy_uni_an", "asy_int_an", "imp_int_an"))
    outdef <- do.call(model[m], list(x = x, y = y, level = c(l, l)))
  else if(model[m] %in% c("sym_hie_ln", "sym_hienc_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, group = list(c(1,2))))
  else if(model[m] %in% c("sym_nuni_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, tramat = tramat))
  else if(model[m] %in% c("asy_def_ln", "asy_uni_ln", "asy_spl_ln", "ugau_bor_ln", "ulap_bor_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = c(l, l, l)))
  else if(model[m] %in% c("mulc_udir_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, goal = c(NA, 1, 2)))
  else if(model[m] %in% c("pai_mer_ln", "pai_bdir_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, pairs = list(c(1,2))))
  else if(model[m] %in% c("asy_spa_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, levelO = l, levelE = l))
  else if(model[m] %in% c("qua_uni_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = c(l, l, l, l)))
  else if(model[m] %in% c("oned_uni_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level =l , att = 1, lower = c(1.5,2,6), upper = c(2,4,7)))
  else
    outdef <- do.call(model[m], list(x = x, y = y, level = l))
  
  return(outdef)
}

###############################################################
###############################################################
###############################################################

apply_frm_model <- function(m, d){
  
  tramat <- matrix(data = c(0.9, 0.03, 0.07, 0.03, 0.9, 0.07, 0.03, 0.07, 0.9), 
                   nrow = 3, ncol = 3, byrow = TRUE)
  
  set.seed(9)
  if(model[m] %in% c("asy_uni_an", "asy_int_an", "imp_int_an"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = c(0.1, 0.2)))
  else if(model[m] %in% c("sym_hie_ln", "sym_hienc_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = 0.1, group = list(c(1,2))))
  else if(model[m] %in% c("sym_nuni_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = 0.1, tramat = tramat))
  else if(model[m] %in% c("asy_def_ln", "asy_uni_ln", "asy_spl_ln", "ugau_bor_ln", "ulap_bor_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = c(0.1, 0.2, 0.3)))
  else if(model[m] %in% c("mulc_udir_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = 0.1, goal = c(NA, 1, 2)))
  else if(model[m] %in% c("pai_mer_ln", "pai_bdir_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = 0.1, pairs = list(c(1,2))))
  else if(model[m] %in% c("asy_spa_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, levelO = 0.1, levelE = 0.3))
  else if(model[m] %in% c("qua_uni_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = c(0.05, 0.15, 0.20, 0.4)))
  else if(model[m] %in% c("oned_uni_ln"))
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = 0.5, att = 1, lower = c(1.5,2,6), upper = c(2,4,7)))
  else
    outdef <- do.call(model[m], list(formula = Species ~ ., data = d, level = 0.1))
  
  return(outdef)
}

###############################################################
###############################################################
###############################################################

test_that('output in default method is of class ndmodel',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    out <- apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0.1)
    expect_s3_class(out, "ndmodel")
  }

})

###############################################################
###############################################################
###############################################################

test_that('output in formula method is of class ndmodel',{

  skip_on_cran()
  data(iris2D)
  
  for(m in 1:length(model)){
    out <- apply_frm_model(m = m, d = iris2D)
    expect_s3_class(out, "ndmodel")
  }

})

###############################################################
###############################################################
###############################################################

test_that('data is not a data.frame in formula method',{
  
  skip_on_cran()
  data(iris2D)
  
  for(m in 1:length(model)){
    expect_error(
      apply_frm_model(m = m, d = iris2D[,ncol(iris2D)])
    )
  }
  
})

###############################################################
###############################################################
###############################################################

test_that('plot function',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    out <- apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0.1)

    gr <- plot(out, pca = TRUE, noise = NA)
    expect_s3_class(gr, "ggplot")
    expect_s3_class(gr, "gg")

    gr <- plot(out, pca = TRUE, noise = TRUE)
    expect_s3_class(gr, "ggplot")
    expect_s3_class(gr, "gg")

    gr <- plot(out, pca = TRUE, noise = FALSE)
    expect_s3_class(gr, "ggplot")
    expect_s3_class(gr, "gg")

    gr <- plot(out, pca = FALSE, noise = NA)
    expect_s3_class(gr, "ggplot")
    expect_s3_class(gr, "gg")

    gr <- plot(out, pca = FALSE, noise = TRUE)
    expect_s3_class(gr, "ggplot")
    expect_s3_class(gr, "gg")

    gr <- plot(out, pca = FALSE, noise = FALSE)
    expect_s3_class(gr, "ggplot")
    expect_s3_class(gr, "gg")
  }

})

###############################################################
###############################################################
###############################################################

test_that('summary function',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    out <- apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0.1)

    sm <- summary(out, showid = TRUE)
    expect_s3_class(sm, "sum.ndmodel")

    expect_output(print(sm))
  }

})

###############################################################
###############################################################
###############################################################

test_that('print function',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    out <- apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0.1)
    expect_output(print(out))
  }

})

###############################################################
###############################################################
###############################################################
