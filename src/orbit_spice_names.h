#ifndef _ORBIT_SPICE_NAMES_H_
#define _ORBIT_SPICE_NAMES_H_

#ifdef CTOFORT_
#define c_orbit c_orbit_
#define ospice_init0 ospice_init0_
#define ospice_init1 ospice_init1_
#define ospice_init2 ospice_init2_
#define ospice_init3 ospice_init3_
#define ospice_spkez4 ospice_spkez4_
#define ospice_elts4 ospice_elts4_
#define ospice_spkezj2k ospice_spkezj2k_
#define ospice_utc2doy ospice_utc2doy_
#define ospice_et2doy ospice_et2doy_
#define ospice_et2jd ospice_et2jd_
#define ospice_et2isocal ospice_et2isocal_
#define ospice_utc2cal ospice_utc2cal_
#define ospice_scdecd ospice_scdecd_
#define ospice_scencd ospice_scencd_
#define ospice_ellint ospice_ellint_
#define ospice_tparse ospice_tparse_
#define ospice_utc2et ospice_utc2et_
#define ospice_sclkch2et ospice_sclkch2et_
#define ospice_getrefrmmtx ospice_getrefrmmtx_
#define ospice_getbodymtx ospice_getbodymtx_
#define ospice_et_ckgp ospice_et_ckgp_
#define ospice_spkluef ospice_spkluef_
#define ospice_cklupf ospice_cklupf_
#define ospice_pckluof ospice_pckluof_
#define ospice_writeck ospice_writeck_
#define ospice_mmt2av ospice_mmt2av_
#define ospice_qqt2av ospice_qqt2av_
#define ospice_ldpool ospice_ldpool_
#define ospice_clpool ospice_clpool_
#define ospice_obody ospice_obody_
#define sce2t sce2t_
#define sct2e sct2e_
#define m2eul m2eul_
#define eul2m eul2m_
#define m2q m2q_
#define q2m q2m_
#define vhat vhat_
#define vrotv vrotv_
#define ucrss ucrss_
#define unormg unormg_
#define vminus vminus_
#define vcrss vcrss_
#define surfnm surfnm_
#define conics conics_
#define bodmat bodmat_
#define failed failed_
#define reset reset_
#endif

#if defined(CTOFORT_f2c) || defined(CTOFORT_g77)
#define c_orbit c_orbit__
#define ospice_init0 ospice_init0__
#define ospice_init1 ospice_init1__
#define ospice_init2 ospice_init2__
#define ospice_init3 ospice_init3__
#define ospice_spkez4 ospice_spkez4__
#define ospice_elts4 ospice_elts4__
#define ospice_spkezj2k ospice_spkezj2k__
#define ospice_utc2doy ospice_utc2doy__
#define ospice_et2doy ospice_et2doy__
#define ospice_et2jd ospice_et2jd__
#define ospice_et2isocal ospice_et2isocal__
#define ospice_utc2cal ospice_utc2cal__
#define ospice_scdecd ospice_scdecd__
#define ospice_scencd ospice_scencd__
#define ospice_ellint ospice_ellint__
#define ospice_tparse ospice_tparse__
#define ospice_utc2et ospice_utc2et__
#define ospice_sclkch2et ospice_sclkch2et__
#define ospice_getrefrmmtx ospice_getrefrmmtx__
#define ospice_getbodymtx ospice_getbodymtx__
#define ospice_spkluef ospice_spkluef__
#define ospice_cklupf ospice_cklupf__
#define ospice_pckluof ospice_pckluof__
#define ospice_writeck ospice_writeck__
#define ospice_mmt2av ospice_mmt2av__
#define ospice_qqt2av ospice_qqt2av__
#define ospice_ldpool ospice_ldpool__
#define ospice_clpool ospice_clpool__
#define ospice_obody ospice_obody__
#ifdef CTOFORT_f2c
#define ospice_et_ckgp ospice_et_ckgp___
#else
#define ospice_et_ckgp ospice_et_ckgp__
#endif
#define sce2t sce2t_
#define sct2e sct2e_
#define m2eul m2eul_
#define eul2m eul2m_
#define m2q m2q_
#define q2m q2m_
#define vhat vhat_
#define vrotv vrotv_
#define ucrss ucrss_
#define unormg unormg_
#define vminus vminus_
#define vcrss vcrss_
#define surfnm surfnm_
#define conics conics_
#define bodmat bodmat_
#define failed failed_
#define reset reset_
#endif

#endif /* #ifndef _ORBIT_SPICE_NAMES_H_ */
