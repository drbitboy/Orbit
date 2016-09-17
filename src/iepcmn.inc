CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     iepcmn.inc
C
      include 'iepcmn.par'
C
      doubleprecision iepdbl
     &, l, s, ieplin, iepsmp, iepcla, iepclo, iepola, iepolo
     &, ieprng, iepnor, ieprpp, incang, emiang, phaang
     &, iepet, iepmet
C
      integer iepint
     &, ieppic, iepcam
C
C     spudfote function file name
C     lightcurve file name
C
      character*255
     & iepsfn, ieplcf
C
      common/IEPCMN/ iepdbl(IEPDBLCOUNT), iepint(IEPDBLCOUNT)
     &, iepsfn, ieplcf
C
      equivalence
     &  ( iepdbl(IEP_l), l)
     &, ( iepdbl(IEP_s), s)
     &, ( iepdbl(IEP_lin), ieplin)
     &, ( iepdbl(IEP_smp), iepsmp)
     &, ( iepdbl(IEP_cla), iepcla)
     &, ( iepdbl(IEP_clo), iepclo)
     &, ( iepdbl(IEP_ola), iepola)
     &, ( iepdbl(IEP_olo), iepolo)
     &, ( iepdbl(IEP_rng), ieprng)
     &, ( iepdbl(IEP_nor), iepnor)
     &, ( iepdbl(IEP_rpp), ieprpp)
     &, ( iepdbl(IEP_inc), incang)
     &, ( iepdbl(IEP_emi), emiang)
     &, ( iepdbl(IEP_pha), phaang)
     &, ( iepdbl(IEP_et), iepet)
     &, ( iepdbl(IEP_met), iepmet)
C
     &, ( iepint(IEP_pic), ieppic)
     &, ( iepint(IEP_cam), iepcam)
C
C    end of iepcmn.inc
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
