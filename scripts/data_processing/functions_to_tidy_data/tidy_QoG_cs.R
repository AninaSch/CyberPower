
# Aim of this function:
#   read the PENN data, select the variables of interest, add new computed variables if needed, and,
#   save the output as PENN_tidy.rds.

# note: this function is called from tidy_datasets.R

# setup
library(rio) # to import and export data
library(tidyverse) # data wrangling etc.

tidy_QGS_ts <- function(path_loadoriginal, path_savetidy){
  
  #       1. read Polity Data
  print("importing Penn data... ")
  QoG <- rio::import(path_loadoriginal)
  QoG <- rio::import("../../../data/original_data/QoG_QualityOfGovernment/qog_std_cs_jan20.csv") # for debugging

  print("importing done")
  
  QoG_select <- QoG %>%
    select(
      country= cname,
      voh_gti,
      wbgi_cce, wbgi_gee, wbgi_pve, wbgi_rle, wbgi_vae,
      vdem_corr,vdem_delibdem, vdem_edcomp_thick, vdem_egal, vdem_egaldem, vdem_elvotbuy,
      vdem_exbribe,vdem_excrptps,vdem_execorr,vdem_exembez,vdem_exthftps, vdem_gcrrpt, vdem_gender,
      vdem_jucorrdc,vdem_libdem,vdem_liberal,vdem_mecorrpt, vdem_partip, vdem_partipdem,vdem_polyarchy,
      vdem_pubcorr,vdem_dl_delib,vdem_edcomp_thick,
      ideavt_legcv,ideavt_legvt,ideavt_prescv,ideavt_presvt,
      cspf_sfi,
      fh_aor, fh_cl, fh_ep, fh_feb,fh_fog,
      fh_ipolity2, fh_pair, fh_polity2, fh_ppp
      # fh_pr, fh_rol, fh_status,
      # ffp_dp, ffp_eco, ffp_ext, ffp_fe, ffp_fsi,
      # ffp_gg, ffp_hf, ffp_hr, ffp_ps, ffp_ref,
      # ffp_sec, ffp_sl, ffp_ued,
      # hf_business, hf_efiscore, hf_financ,
      # hf_govint,hf_govt, hf_invest,
      # hf_labor,hf_monetary,hf_prights,
      # hf_taxbur, hf_trade,
      # ipi_ab,ipi_e, ipi_ipi,ipi_tradeopen,
      # wvs_confpar, wvs_confpol,
      # wvs_confpr, wvs_confun,
      # wvs_imppol,wvs_imprel,wvs_jabribe,
      # wvs_jacgb, wvs_jacot, wvs_polint,
      # wvs_psarmy,wvs_psdem, wvs_psexp,
      # wvs_pssl, wvs_satlif,wvs_trust, wvs_confjs,
      # wvs_confaf, wvs_confch,wvs_confcs,
      # ciri_assn,
      # ciri_dommov,
      # ciri_formov,
      # ciri_injud,
      # ciri_physint,
      # ciri_polpris,
      # ciri_speech,
      # ciri_tort,
      # cpds_tg, 
      # cpds_vt,
      # ti_cpi,
      # bci_bci, bci_bcistd, icrg_qog,
      # qs_closed, qs_impar, qs_proff,
      # ess_trparl, ess_trpart, ess_trpolit 
      )
 
QoG_cs_tidy <- QoG_select %>%
    mutate(
      country = as.factor(country)
      ) %>%
    arrange(country)
  # glimpse(penn_tidy)
    
  print("tidying done")
  
  # saveRDS(QoG_cs_tidy, file = path_savetidy)
  saveRDS(QoG_cs_tidy, file = "../../../data/processed_data/QoG_cs_tidy.rds")
  
  print("processed QoG_ts data saved")
  
}

