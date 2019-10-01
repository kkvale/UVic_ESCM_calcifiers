!====================== include file "npzd.h" =========================

!   variables for npzd model

!   ntnpzd   = number of npzd tracers
!   nbio     = number of npzd timesteps per ocean timestep
!   trcmin   = minimum tracer for flux calculations
!   alpha_P    = initial slope P-I curve [(W/m^2)^(-1)/day]
!   kw       = light attenuation due to water [1/m]
!   kc       = light attenuation by phytoplankton [1/(m*mmol m-3)]
!   ki       = light attenuation through sea ice & snow
!   abio     = maximum growth rate parameter [1/day]
!   bbio     = b
!   cbio     = [1/deg_C]
!   k1n      = half saturation constant for N uptake [mmol m-3]
!   nup      = specific mortality rate (Phytoplankton) [day-1]
!   gamma1   = assimilation efficiency (zpk)
!   gbio     = maximum grazing rate at 0 deg C [day-1]
!   nuz      = quadratic mortality (zpk)
!   nud0     = remineralization rate [day-1]
!   LFe      = Iron limitation
!   wd       = sinking speed of detritus [m day-1]
!   ztt      = depth to top of grid cell [cm]
!   rkwz     = reciprical of light attenuation times grid depth
!   par      = fraction of photosythetically active radiation
!   dtnpzd   = time step of biology
!   capr     = carbonate to carbon production ratio
!   dcaco3   = remineralisation depth of calcite [cm]
!   rcak     = array used in calculating calcite remineralization
!   rcab     = array used in calculating bottom calcite remineralization
!   nupt0    = specific mortality rate (Phytoplankton) [1/day]
!   wd0      = sinking speed of detritus at surface [m/day]
!   k1p_P      = half saturation constant for P uptake
!   jdiar    = factor reducing the growth rate of diazotrophs
!   redctn   = C/N Redfield ratio (includes mol to mmol conversion)
!   redctp   = C/P Redfield ratio (includes mol to mmol conversion)
!   redptn   = P/N Redfield ratio
!   redntp   = N/P Redfield ratio
!   redotn   = O/N Redfield ratio (includes mol to mmol conversion)
!   redotp   = O/P Redfield ratio (includes mol to mmol conversion)
!   rnbio    = reciprical of nbio
!   rdtts    = reciprical of dtts [s-1]
!   dtbio    = npzd time step [s]
!   rnpp     = rate of net primary production [nmol cm-3 s-1]
!   rgraz    = rate of grazing [nmol cm-3 s-1]
!   rmorp    = rate of mortality of phytoplankton [nmol cm-3 s-1]
!   rmorz    = rate of mortality of zooplankton [nmol cm-3 s-1]
!   rremi    = rate of remineralization [nmol cm-3 s-1]
!   rexcr    = rate of excretion [nmol cm-3 s-1]
!   rexpo    = rate of export through the bottom [nmol cm-3 s-1]
!   rnpp_D   = npp for diazotraphs [nmol cm-3 s-1]
!   rgraz_D  = rgraz for diazotraphs [nmol cm-3 s-1]
!   rmorp_D  = rmorp for diazotraphs [nmol cm-3 s-1]
!   rnfix    = rate of nitrogen fixation [nmol cm-3 s-1]
!   rdeni    = rate of denitrification [nmol cm-3 s-1]
!   kzoo     = half saturation constant for Z grazing
!   zprefP   = Z preference for grazing on P
!   zprefDiaz   = Z preference for grazing on Diaz
!   zprefZ   = Z preference for grazing on other Z
!   zprefDet = Z preference for grazing on Detritus
!   rgraz_Det = rate of grazing on Detritus [nmol cm-3 s-1]
!   rgraz_Z   = rate of grazing on other Zooplankton [nmol cm-3 s-1]
!   geZ      = growth efficiency of zooplankton
!   alpha_C  = initial slope P-I curve coccs[(W/m^2)^(-1)/day]
!   abioc     = maximum growth rate parameter coccolithophores [1/day]
!   k1n_C    = half saturation constant for N uptake coccolithophores [mmol m-3]
!   nuc      = specific mortality rate (coccolithophores) [day-1]
!   gbio_C   = maximum grazing rate (coccolithophores) [da-1y]
!   bapr     = fixed calcite detritus production ratio
!   kcal     = half saturation constant for PIC dissolution
!   dissk0    = PIC dissolution rate parameter [day -1]
!   dissk0op    = opal dissolution rate parameter [day -1]
!   nuct0    = specific mortality rate (coccolithophores) [1/day]
!   wc0      = sinking speed of calcite at surface [m/day]
!   k1p_C      = half saturation constant for P uptake (coccolithophores)
!   rnpp_C     = rate of net primary production coccolithophores [nmol cm-3 s-1]
!   rgraz_C    = rate of grazing coccolithophores [nmol cm-3 s-1]
!   rmorp_C    = rate of mortality of coccolithophores [nmol cm-3 s-1]
!   rremi_B  = rate of ballasted detritus remineralization [nmol cm-3 s-1]
!   rexpo_B    = rate of export through the bottom [nmol cm-3 s-1]
!   zprefC   = Z preference for grazing on C
!   rgraz_Det_B = rate of grazing on ballasted Detritus [nmol cm-3 s-1]
!   kfe_C = Fe limitation half sat. param. for cocc.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! New diagnostic output
!   ravej      = light-dependant growth rate of P
!   ravej_D    = light-dependant growth rate of Diaz
!   rgmax      = temp-dependant growth rate of zoo
!   rno3P      = nitrate-dependant growth rate of P
!   rpo4P       = phosphate-dependant growth rate of P
!   rpo4_D     = phosphate-dependant growth rate of D
!
!   fe_dissolved = dissolved iron concentration
!   kfe = Fe limitation half saturation parameter
!   kfe_D = Fe limitation half sat. param. for diaz.
!

      integer ntnpzd, nbio
      parameter (ntnpzd = 4
#if defined O_npzd_nitrogen
     $                   +2
#endif
#if defined O_cal_kk
     $                   +1
#endif
#if defined O_kk_caco3tr
     $                   +1
#endif
#if defined O_kk_ballast
     $                   +1
#endif
     $                    )
      common /npzd_i/ nbio(km)

      real trcmin
      parameter (trcmin=5e-12)

      real alpha_P, kw, kc, ki, abio, bbio, cbio, k1n, nup, gamma1, gbio
      real epsbio, nuz, gamma2, nud0, LFe, wd, ztt, rkwz, par, dtnpzd
      real capr, dcaco3, rcak, rcab, nupt0, wd0, k1p_P, jdiar, redctn
      real redctp, redptn, redntp, redotn, redotp, rnbio, rdtts, dtbio
      real rnpp, rgraz, rmorp, rmorpt, rmorz, rremi, rexcr, rexpo
      real rnpp_D, rgraz_D, rmorp_D, rnfix, rdeni, kzoo, zprefP
      real zprefDiaz, zprefZ, zprefDet, rgraz_Det, rgraz_Z, geZ, kfe
      real ravej, ravej_D, rgmax, rno3P, rpo4P, rpo4_D, kfe_D
#if defined O_kk_ballast
      real rgraz_Det_B, rexpo_B,rremi_B,bapr
#endif
#if defined O_cal_kk
      real abioc , k1n_C, k1p_C, rnpp_C, nuc, nuct0, alpha_C
      real rgraz_C, rmorp_C, rmorpt_C
      real zprefC, kfe_C
#endif
#if defined O_kk_caco3tr
      real rcalpro, kcal, wc, wc0, dissk0, rdissl
      real rexpocaco3, rcaldiss, rcalatt, rnegdissl, rimpocaco3
      real kc_c, romca, rco3, rco3_sat, rdel_sat, rdissl_new
#endif
#if defined O_kk_ballast
      common /npzd_r/ bapr
#endif
#if defined O_kk_caco3tr
      common /npzd_r/ wc0, kcal, dissk0, wc(km)
      common /npzd_r/ kc_c
      common /npzd_r/ rcalpro(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rcaldiss(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rcalatt(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rdissl(imt,km,jsmw:jemw)
      common /npzd_r/ rdissl_new(imt,km,jsmw:jemw)
      common /npzd_r/ rexpocaco3(imt,km,jsmw:jemw)
      common /npzd_r/ rimpocaco3(imt,km,jsmw:jemw)
      common /npzd_r/ rnegdissl(imt,km,jsmw:jemw)
      common /npzd_r/ romca(imt,km,jsmw:jemw)
      common /npzd_r/ rco3(imt,km,jsmw:jemw)
      common /npzd_r/ rco3_sat(imt,km,jsmw:jemw)
      common /npzd_r/ rdel_sat(imt,km,jsmw:jemw)
#endif
      common /npzd_r/ alpha_P, kw, kc, ki, abio, bbio, cbio, k1n, nup
      common /npzd_r/ gamma1, gbio, epsbio, nuz, nud0, LFe
      common /npzd_r/ wd(km), ztt(km), rkwz(km), par, dtnpzd, capr
      common /npzd_r/ dcaco3, rcak(km), rcab(km), nupt0, wd0, k1p_P
      common /npzd_r/ jdiar, redctn, redctp, redptn, redntp, redotn
      common /npzd_r/ redotp, rnbio(km), rdtts(km), dtbio(km), geZ
      common /npzd_r/ kzoo, zprefP, zprefDiaz, zprefZ, zprefDet, kfe
      common /npzd_r/ kfe_D
#if defined O_cal_kk
      common /npzd_r/ abioc, k1n_C, k1p_C, nuc, nuct0, alpha_C
      common /npzd_r/ kfe_C
      common /npzd_r/ rnpp_C(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rgraz_C(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rmorp_C(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rmorpt_C(imt,kpzd,jsmw:jemw)
#endif
#if defined O_npzd_fe_limitation
      real fe_dissolved
      common /fe_dissolved/ fe_dissolved(imt,jmt,3,12)
#endif
#if defined O_save_npzd
      common /npzd_r/ rnpp(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rgraz(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rmorp(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rmorpt(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rmorz(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rexcr(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rremi(imt,km,jsmw:jemw)
      common /npzd_r/ rexpo(imt,km,jsmw:jemw)
      common /npzd_r/ rgraz_Det(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rgraz_Z(imt,kpzd,jsmw:jemw)
# if defined O_kk_ballast
      common /npzd_r/ rgraz_Det_B(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rremi_B(imt,km,jsmw:jemw)
      common /npzd_r/ rexpo_B(imt,km,jsmw:jemw)
# endif
# if defined O_npzd_nitrogen
      common /npzd_r/ rnpp_D(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rgraz_D(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rmorp_D(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rnfix(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rdeni(imt,km,jsmw:jemw)
#  if defined O_npzd_extra_diagnostics
      common /npzd_r/ ravej(imt,kpzd,jsmw:jemw)
      common /npzd_r/ ravej_D(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rgmax(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rno3P(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rpo4P(imt,kpzd,jsmw:jemw)
      common /npzd_r/ rpo4_D(imt,kpzd,jsmw:jemw)
#  endif
# endif
#endif

