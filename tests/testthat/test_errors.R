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

apply_def_model <- function(m, x, y, l, order = levels(y)){
  
  data(iris2D)
  tramat <- matrix(data = c(0.9, 0.03, 0.07, 0.03, 0.9, 0.07, 0.03, 0.07, 0.9), 
                   nrow = 3, ncol = 3, byrow = TRUE)
  
  set.seed(9)
  if(model[m] %in% c("asy_uni_an", "asy_int_an", "imp_int_an"))
    outdef <- do.call(model[m], list(x = x, y = y, level = c(l, l), order = order))
  else if(model[m] %in% c("sym_hie_ln", "sym_hienc_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, group = list(c(1,2)), order = order))
  else if(model[m] %in% c("sym_nuni_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, tramat = tramat, order = order))
  else if(model[m] %in% c("asy_def_ln", "asy_uni_ln", "asy_spl_ln", "ugau_bor_ln", "ulap_bor_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = c(l, l, l), order = order))
  else if(model[m] %in% c("mulc_udir_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, goal = c(NA, 1, 2), order = order))
  else if(model[m] %in% c("pai_mer_ln", "pai_bdir_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = l, pairs = list(c(1,2)), order = order))
  else if(model[m] %in% c("asy_spa_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, levelO = l, levelE = l, order = order))
  else if(model[m] %in% c("qua_uni_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level = c(l, l, l, l), order = order))
  else if(model[m] %in% c("oned_uni_ln"))
    outdef <- do.call(model[m], list(x = x, y = y, level =l , att = 1, lower = c(1.5,2,6), upper = c(2,4,7), order = order))
  else
    outdef <- do.call(model[m], list(x = x, y = y, level = l, order = order))
  
  return(outdef)
}

###############################################################
###############################################################
###############################################################

test_that('x is not a data.frame',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    expect_error(
      apply_def_model(m = m, x = iris2D[,ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0.1)
    )
  }

})

###############################################################
###############################################################
###############################################################

test_that('y is not a factor vector',{

  skip_on_cran()
  data(iris2D)
  y <- runif(nrow(iris2D))

  for(m in 1:length(model)){
    expect_error(
      apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = y, l = 0.1)
    )
  }

})

###############################################################
###############################################################
###############################################################

test_that('y has less than two levels',{

  skip_on_cran()
  data(iris2D)
  iris2D$Species <- as.factor(replicate(nrow(iris2D), "singleclass"))

  for(m in 1:length(model)){
    expect_error(
      apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0.1)
    )
  }

})

###############################################################
###############################################################
###############################################################

test_that('x and y have a different length',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    expect_error(
      apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[-1,ncol(iris2D)], l = 0.1)
    )
  }

})

###############################################################
###############################################################
###############################################################

test_that('level out of limits',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    if(!model[m] %in% c("unc_rpix_an", "unc_gimg_an", "pai_mer_ln", "mis_pre_ln", "clu_vot_ln")){
      expect_error(
        apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 10)
      )
    }
  }

})

###############################################################
###############################################################
###############################################################

test_that('level equal to 0',{

  skip_on_cran()
  data(iris2D)

  for(m in 1:length(model)){
    if(!model[m] %in% c("hyb_guni_cn", "sym_nuni_ln", "unc_rpix_an", "unc_gimg_an", "pai_mer_ln", "mis_pre_ln", "clu_vot_ln")){
      out <- apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0)
      expect_identical(out$xnoise, iris2D[,-ncol(iris2D)])
      expect_identical(out$ynoise, iris2D[,ncol(iris2D)])
    }
  }

})

###############################################################
###############################################################
###############################################################

test_that('argument order with wrong values',{
  
  skip_on_cran()
  data(iris2D)
  
  for(m in 1:length(model)){
    if(model[m] %in% c("sym_ddef_ln", "sym_def_ln", "sym_hie_ln", "sym_adj_ln", "sym_opt_ln", 
                       "sym_pes_ln", "sym_dia_ln", "sym_nexc_ln", "sym_hienc_ln", "asy_def_ln", 
                       "asy_uni_ln", "mulc_udir_ln", "asy_spl_ln", "pai_mer_ln", "pai_bdir_ln", 
                       "asy_spa_ln", "opes_idu_ln", "opes_idc_ln", "opes_idnn_ln")){
      expect_error(
        apply_def_model(m = m, x = iris2D[,-ncol(iris2D)], y = iris2D[,ncol(iris2D)], l = 0.1, order = c("virginica", "setosa"))
      )
    }
  }
  
})

###############################################################
###############################################################
###############################################################
