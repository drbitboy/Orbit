C      program main
C      call c_orbit
C      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      subroutine ospice_spkez4( scid, scv, scmx
     &                        , asterid, asterv, astermx
     &                        , sunid, sunv, sunmx
     &                        , earthid, earthv, earthmx
     &                        , success, besthope
     &                        , et, utclen, utc1)
      implicit none
      integer scid, earthid, sunid, asterid, targetid
      doubleprecision scv(6), earthv(6), sunv(6), asterv(6), targetv(6)
      doubleprecision scmx(3,3), earthmx(3,3), sunmx(3,3), astermx(3,3)
     &              , targetmx(3,3)
      integer scEph, asterEph, earthEph
C
C keep the following section consistent with orbit3d.h
C 
      integer  ORB_USESPK, ORB_USEAA, ORB_USESO
      parameter (ORB_USESPK=0)
      parameter (ORB_USESO=1)
      parameter (ORB_USEAA=2)
C
      doubleprecision scSo(8), asterSo(8), earthSo(8)
      doubleprecision scJ2, scRpl, scPeriod, scUPA0(3), scUTP0(3)
      doubleprecision scSofromj2k(3,3)
     &              , asterSofromj2k(3,3), earthSofromj2k(3,3)
      integer success, bitess, besthope
      doubleprecision et, etlt, etast
      integer utclen
      character*1 utc1
C
      doubleprecision lt, v1(3), v2(3), degpr, cl
      doubleprecision dpr, clight, vnorm
      character*80 utc
C
C local vectors for use with orbital Elements in ospice_elts4() entry
C
      doubleprecision asterve(6), scve(6), earthve(6), targetve(6)
C
      data degpr / 0.0 /
C
      save
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      if ( degpr .eq. 0.0) then
        degpr = dpr()
        cl = clight()
      endif
C
      bitess = 1
      success = 0
      besthope = 0
      call reset
      if ( utclen .gt. 0) then
        call cxfer( utclen, utc, utc1)
C        print*, utc(:utclen), ' = utc'
        call utc2et( utc(:utclen), et)
C        print*, et, ' = et'
      endif
      call setsuc( success, bitess, besthope)
C
C et is local to sc
C return asteroid vector wrt sc lt corrected to time light leaves asteroid
C return sc vector wrt to sun lt corr'ed to time light leaves sun
C - return asteroid matrix at same time light leaves asteroid
C return sun vector wrt asteroid lt corr'ed to time light leaves sun
C return earth vector wrt sc lt corr'ed to time light (i.e. signal) leaves sc
C
      lt = 0D0
      call spkez( asterid, et, 'j2000', 'lt', scid, asterv, lt)
      etast = et - lt
      call chkin( 'ospice_spkez4-A')
      call bodmat( asterid, etast, astermx)
      call chkout( 'ospice_spkez4-A')
      call setsuc( success, bitess, besthope)
C
      lt = 0D0
      call spkez( sunid, et, 'j2000', 'lt', scid, scv, lt)
      call vminug( scv, 6, scv)
      call setsuc( success, bitess, besthope)
C
      call vminus( asterv, v1)
      call mxv( astermx, v1, v2)
      call reclat( v2, v1(1), v1(2), v1(3) )
C      print '(1x,a/5g15.7)'
C     &    , 'sub-s/c xyz, lat,lon:  ', v2, v1(3)*degpr, v1(2)*degpr
C
      lt = 0D0
      call spkez( sunid, etast, 'j2000', 'lt', asterid, sunv, lt)
      call setsuc( success, bitess, besthope)
C
C      call spkez( earthid, etlt, 'j2000', 'lt', scid, earthv, lt)
C
      lt = 0D0
      etlt = et
      call spkez( scid, etlt, 'j2000', 'lt', earthid, earthv, lt)
      etlt = et + lt 
      call spkez( scid, etlt, 'j2000', 'lt', earthid, earthv, lt)
      etlt = et + lt 
      call spkez( scid, etlt, 'j2000', 'lt', earthid, earthv, lt)
      call vminug( earthv, 6, earthv)
      call bodmat( earthid, etlt, earthmx)
      call setsuc( success, bitess, besthope)

      return

C-----------------------------------------------------------------------
C here's the entry for using orbital elements to get ephemeris info
C
      entry ospice_elts4( scid, scv, scmx, scEph, scSo, scSofromj2k
     &          , scJ2, scRpl, scPeriod, scUPA0, scUTP0
     &          , asterid, asterv, astermx
     &              , asterEph, asterSo, asterSofromj2k
     &          , sunid, sunv, sunmx
     &          , earthid, earthv, earthmx
     &              , earthEph, earthSo, earthSofromj2k
     &          , success, besthope
     &          , et, utclen, utc1)
C
      if ( degpr .eq. 0.0) then
        degpr = dpr()
        cl = clight()
      endif
C
      bitess = 1
      success = 0
      besthope = 0
      call reset
      if ( utclen .gt. 0) then
        call cxfer( utclen, utc, utc1)
C        print*, utc(:utclen), ' = utc'
        call utc2et( utc(:utclen), et)
C        print*, et, ' = et'
      endif
      call setsuc( success, bitess, besthope)
C
C et is local to sc
C return sc vector wrt to sun lt corr'ed to time light leaves sun
C return asteroid vector wrt sc lt corrected to time light leaves asteroid
C - return asteroid matrix at same time light leaves asteroid
C return sun vector wrt asteroid lt corr'ed to time light leaves sun
C return earth vector wrt sc lt corr'ed to time light (i.e. signal) leaves sc
C
C MODIFIED AS FOLLOWS FOR CONICS:
C - assume 0 light time from asteroid to s/c
C - do earth light-times manually
C - vectors (*ve) orbital elements do not need lt correction wrt their center
C
C (1) get initial values for asterve, scve - vectors wrt sun
C     - initial earthve is at s/c et
C
      if ( asterEph .eq. ORB_USESPK) then
        call spkez( sunid, et, 'j2000', 'lt', asterid, asterve, lt)
        call vminug( asterve, 6, asterve)
      else
        call conics( asterSo, et, asterve)
        call mtxv( asterSofromJ2k, asterve, asterve)
      endif

      call chkin( 'ospice_elts4-A')
      call bodmat( asterid, et, astermx)
      call chkout( 'ospice_elts4-A')
      call setsuc( success, bitess, besthope)
C
      if ( scEph .eq. ORB_USESPK .and. asterEph .eq. ORB_USESPK) then
        call spkez( scid, et, 'j2000', 'none', asterid, scve, lt)
      elseif ( scEph .eq. ORB_USESPK) then
        call spkez( scid, et, 'j2000', 'none', sunid, scve, lt)
        call vsubg( scve, asterve, 6, scve)
      else
        call conics( scSo, et, scve)
        call ospice_j2fix( scSo, et, scve
     &                   , scJ2, scRpl, scPeriod, scUPA0, scUTP0)
        call mtxv( scSofromJ2k, scve, scve)
      endif
      call setsuc( success, bitess, besthope)
C
C (2) sun vector is sun wrt asteroid;
C (3) asteroid vector is asteroid wrt s/c
C (4) s/c vector is s/c wrt sun
C
      call vminug( asterve, 6, sunv)
      call vminug( scve, 6, asterv)
      call vaddg( asterve, scve, 6, scv)
      call setsuc( success, bitess, besthope)
C
C (5) earth vector is earth wrt s/c corrected to s/c et
C
      if ( earthEph .eq. ORB_USESPK) then
        call spkez( earthid, et, 'j2000', 'none', sunid, earthve, lt)
      else
        call conics( earthSo, et, earthve)
        call mtxv( earthSofromJ2k, earthve, earthve)
      endif
C
      call vsub( earthve, scve, earthv)
      lt = vnorm(earthv) / cl
      etlt = et - lt
C
      if ( earthEph .eq. ORB_USESPK) then
        call spkez( earthid, etlt, 'j2000', 'none', sunid, earthve, lt)
      else
        call conics( earthSo, etlt, earthve)
        call mtxv( earthSofromJ2k, earthve, earthve)
      endif
      call vsubg( earthve, scve, 6, earthv)
      call bodmat( earthid, etlt, earthmx)
      call setsuc( success, bitess, besthope)
C
      return

C-----------------------------------------------------------------------
C here's the entry for using using spkez from spicelib to get ephem info
C
      entry ospice_spkezj2k( targetid, et, scid, targetv, targetmx
     &                     , success, besthope)
C
      if ( degpr .eq. 0.0) then
        degpr = dpr()
        cl = clight()
      endif
C
      bitess = 1
      success = 0
      besthope = 0
      call reset
C
C return target vector wrt sc lt corrected to time light leaves target
C - return target matrix at same time light leaves target
C
      lt = 0D0
      call spkez( targetid, et, 'j2000', 'lt', scid, targetv, lt)
      call chkin( 'ospice_spkezJ2k-A')
      call bodmat( targetid, et-lt, targetmx)
      call chkout( 'ospice_spkezJ2k-A')
      call setsuc( success, bitess, besthope)
C
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      subroutine setsuc( success, ibit, besthope)
      implicit none
      integer success, ibit, besthope
      logical failed

      besthope = besthope + ibit
      if ( .not. failed()) then
        success = success + ibit
      else
        call reset
      endif
      ibit = ibit * 2
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C spice initialisation
C - level 0:  load files, save body & spacecraft id's
C
      subroutine ospice_init0( i1len, filnam)
      implicit none
      integer i1len
      character*1 filnam(i1len)
C
      integer scid, earthid, sunid, asterid
      doubleprecision asterad(3)
      doubleprecision aster_orbit(8)
      integer aster_utclen_out
      integer aster_reflen_out
      integer aster_utcreflen_in
      character*1 aster_utcref(aster_utcreflen_in)
      doubleprecision fov(4)
      integer ncampts
      doubleprecision campts
      doubleprecision boresight(3)
      doubleprecision panel(3)
      doubleprecision sampdir(3)
      doubleprecision updir(3)
C
      doubleprecision nisNomB0(3)
      doubleprecision nisAxis(3)
      doubleprecision nisNomUpDir(3)
      doubleprecision nisNomSampDir(3)
      doubleprecision nisfovy
      doubleprecision nisfovx
      doubleprecision nisStepAngle
      integer nnispts
      integer nisStepNom
      integer nisStepMax
C
      doubleprecision reffrm
      doubleprecision jed
      integer irefnum
C
      integer i3lenin, i3lenout
      character*1 utcout(i3lenin)
      character*1 instrname(i3lenin)
C
      character*255 c255a, c255b
      integer i, ii, ibv
      integer ilena, ilenb, iid
      integer colon
      integer lun1, handle
      doubleprecision xxx, xxx10(10)

      integer savscid, savasterid, savsunid, savearthid
      character*255 savutc
      integer savutclen

      logical failed

      integer ISPK, ICK, IPCK, ITK, IORIBPCK, ILEAP
     &, INPXLX, INPXLY, IURADY, IURADX, IFOVX, IFOVY, IUTC
     &, IEARTH, ISUN, IAST, ISC, IINCLUDE, IIDLAST
      parameter ( ISPK = 1
     &          , ICK = ISPK + 1
     &          , IPCK = ICK + 1
     &          , ITK = IPCK + 1
     &          , IORIBPCK = ITK + 1
     &          , ILEAP = IORIBPCK + 1
     &          , INPXLX = ILEAP + 1
     &          , INPXLY = INPXLX + 1
     &          , IURADX = INPXLY + 1
     &          , IURADY = IURADX + 1
     &          , IFOVX = IURADY + 1
     &          , IFOVY = IFOVX + 1
     &          , IUTC = IFOVY + 1
     &          , IEARTH = IUTC + 1
     &          , ISUN = IEARTH + 1
     &          , IAST = ISUN + 1
     &          , ISC = IAST + 1
     &          , IINCLUDE = ISC + 1
     &          , IIDLAST = IINCLUDE )
C
      doubleprecision radpd, rpd
      external rpd
C
      character*10 keys(IIDLAST), key1
      data (keys(i), i=1,IIDLAST) /
     &    'SPK:      '
     &  , 'CK:       '
     &  , 'PCK:      '
     &  , 'TK:       '
     &  , 'ORIBPCK:  '
     &  , 'LEAPSEC:  '
     &  , 'NPXLX:    '
     &  , 'NPXLY:    '
     &  , 'URADX:    '
     &  , 'URADY:    '
     &  , 'FOVX:     '
     &  , 'FOVY:     '
     &  , 'UTC:      '
     &  , 'EARTHID:  '
     &  , 'SUNID:    '
     &  , 'ASTID:    '
     &  , 'SCID:     '
     &  , 'INCLUDE:  '
     &  /
C
      integer MAXDEPTH
      parameter(MAXDEPTH=10)
      integer luns(MAXDEPTH)
      character*255 c255n(MAXDEPTH)
      integer lens(MAXDEPTH)
      integer idepth, iostat
      save
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Start Subroutine ospice_init*

      radpd = rpd()

      savutclen = 0

      call erract( 'set', 'report')

      idepth = 1

      lens(idepth) = i1len
      call cxfer( lens(idepth), c255n(idepth), filnam)

      call getlun( luns(idepth))
      open( luns(idepth), file=c255n(idepth)(:lens(idepth))
     &    , status='old')
C
C 1) read line

CCC      dowhile(.true.)
      dowhile( idepth .gt. 0)
C
        iostat = -1
        read(luns(idepth),'(a)',end=99999) c255a
        iostat = 0
C
C 1a) collapse line, get first token, check if it ends in ':'

        call flencoll( c255a, ilena)
        call nextwd( c255a, key1, c255a)
        colon = index( key1, ' ') - 1
        if ( colon .gt. 1 .and. colon .lt. (ilena-1)) then
        if ( key1(colon:colon) .eq. ':') then
C
C 1b) compare first token against keywords

          iid = -1
          i = 1
          dowhile ( i .le. IIDLAST)
            if ( key1 .eq. keys(i) ) then
              iid = i
              i = IIDLAST + 1
            else
              i = i + 1
            endif
          enddo
C
C 1c) if it matched a keyword, get next token, and act accordingly

          if ( iid .gt. 0) then
            call nextwd( c255a, c255b, c255a)
            ilenb = index( c255b, ' ') - 1
            if ( ilenb .lt. 0) ilenb=len(c255b)

            if ( iid .eq. ISPK) then
              call spklef( c255b(:ilenb), handle)

            elseif ( iid .eq. ICK) then
              call cklpf( c255b(:ilenb), handle)

            elseif ( iid .eq. IPCK) then
              call ldpool( c255b(:ilenb))

            elseif ( iid .eq. ITK) then
              call ldpool( c255b(:ilenb))

            elseif ( iid .eq. IORIBPCK) then
              call pcklof( c255b(:ilenb), handle)

            elseif ( iid .eq. ILEAP) then
              call ldpool( c255b(:ilenb))

            elseif ( iid .eq. IUTC) then
              savutc = c255b(:ilenb)
              savutclen = ilenb

            elseif ( iid .eq. IEARTH) then
              call obody( c255b(:ilenb), savearthid)

            elseif ( iid .eq. ISUN) then
              call obody( c255b(:ilenb), savsunid)

            elseif ( iid .eq. IAST) then
              call obody( c255b(:ilenb), savasterid)

            elseif ( iid .eq. ISC) then
              call obody( c255b(:ilenb), savscid)

            elseif ( iid .eq. IINCLUDE) then
C
C             include a file, filename in c255b(:ilenb)
C
              idepth = idepth + 1
C
C             if new filename does not start with '/', append after
C             last '/' in previous filename
C
              if ( c255b(1:1) .ne. '/') then
C
C               incremental filename, start with old filename 
C
                c255n(idepth) = c255n(idepth-1)
                lens(idepth) = lens(idepth-1)
C
C               look backward from end for '/' or beginning
C
                dowhile ( c255n(idepth)(lens(idepth):lens(idepth))
     &                .ne. '/' .and. lens(idepth) .gt. 1)
                  lens(idepth) = lens(idepth) - 1
                enddo
C
C               move one position up if '/' found
C
                if ( c255n(idepth)(lens(idepth):lens(idepth)) .eq. '/')
     &            lens(idepth) = lens(idepth) + 1
C
C               append new include filename
C
                c255n(idepth)(lens(idepth):) = c255b(:ilenb)
                lens(idepth) = lens(idepth) + ilenb
C
              else
C
C               absolute filename
C
                c255n(idepth) = c255b(:ilenb)
                lens(idepth) = ilenb
              endif
C
C             open include file
C
              open( luns(idepth), file=c255n(idepth)(:lens(idepth))
     &            ,status='old')

C              if iid ==. ... elseif iid .eq. ...
            endif
C
C            if iid > 0 ...
          endif
C
C          if ( key1(colon:colon) .eq. ':') then
        endif
C
C          if ( colon .gt. 1 .and. colon .lt. (ilena-1)) then
        endif
C
C       break to here if end of file
C
99999   continue
        if ( iostat .ne. 0) then
          close(luns(idepth))
          call frelun(luns(idepth))
          idepth = idepth - 1
        endif
C
C        dowhile idepth > 0 ...
      enddo
C
CCC99999 continue
CCC      close( lun1)
CCC      call frelun( lun1)
      return
C-----------------------------------------------------------------------
C spice init
C - level 1:  return id's + some data from kernel pool + size of camera
C             points array
C
C boresight is into image; updir is up in image (-lines);
C  sampdir is to the right in the image (+samples)

      entry ospice_init1( scid, asterid, earthid, sunid
     &                       , asterad, aster_orbit
     &                       , boresight, panel, updir, sampdir
     &                       , fov, ncampts
     &                       , nisNomB0, nisAxis, nisNomUpDir
     &                       , nisNomSampDir
     &                       , nisfovx, nisfovy, nisStepAngle
     &                       , nnispts, nisStepNom, nisStepMax
     &                       , aster_utclen_out, aster_reflen_out
     &                       , aster_utcreflen_in, aster_utcref)
C
C      sav*id no longer used:
C      - ospice_init0 no longer called, *id set in orbitfort.c:orbit_init
C
C      scid = savscid
C      asterid = savasterid
C      earthid = savearthid
C      sunid = savsunid
C
C     - the following lines moved here from ospice_init0
C
      radpd = rpd()
      savutclen = 0
      call erract( 'set', 'report')
C     - end lines from ospice_init0
C
C get asteroid ellipsoid radii

      ibv=3
      call bodvar( asterid, 'RADII', ibv, asterad)
C      c255a = 'RADII'
C      call bodvar( asterid, c255a(:5), 3, asterad)
      if ( failed() ) stop
C
C get nominal asteroid orbital info so user will not have to enter
C   it by hand
C
      ibv = 7
      call bodvar( asterid, 'ORBIT_IOWANEM', ibv, aster_orbit)
      ibv = 1
      call bodvar( asterid, 'ORBIT_JED_EPOCH', ibv, jed)
      ibv = 1
      call bodvar( asterid, 'ORBIT_REF_FRAME', ibv, reffrm)
C
      if ( .not. failed()) then
C
C JED string
C
        write( c255a, '(a2,f20.8)') 'JD', jed
        call flencoll( c255a, aster_utclen_out)
C
C Reference Frame string
C
        irefnum = nint( reffrm)
        call irfnam( irefnum, c255a(aster_utclen_out+1:))
        call flencoll( c255a, aster_reflen_out)
        call cxferout( aster_utcreflen_in, aster_utcref
     &               , c255a(:aster_reflen_out))
        aster_reflen_out = aster_reflen_out - aster_utclen_out
C
C if variables not found
C
      else
        call reset
        do i=1,7
          aster_orbit(i) = 0d0
        enddo
        aster_reflen_out = 0
        aster_utclen_out = 0
      endif
C
C get spacecraft MSI camera spec
      ibv = 1
      call bodvar( scid, 'MSI_NUM_CAMPTS', ibv, xxx)
      ncampts = int( xxx + 0.1)
      ibv = 4
      call bodvar( scid, 'MSI_FOV', ibv, fov)
      ibv = 3
      call bodvar( scid, 'MSI_BORESIGHT', ibv, boresight)
      ibv = 3
      call bodvar( scid, 'PANEL', ibv, panel)
      ibv = 3
      call bodvar( scid, 'MSI_SAMPDIR', ibv, sampdir)
C
C normalize boresight, find sampdirection component perpendicular to boresight,
C  normalize sampdirection, calculate updirection as cross product of
C  sampdirection and boresight

      call unorm( boresight, boresight, xxx)
      call vperp( sampdir, boresight, sampdir)
      call unorm( sampdir, sampdir, xxx)
      call vcrss( sampdir, boresight, updir)

C
C get spacecraft NIS spec - convert FOV from degrees to radians
      ibv = 1
      call bodvar( scid, 'NIS_NUM_CAMPTS', ibv, xxx)
      nnispts = int( xxx + 0.1)
      ibv = 2
      call bodvar( scid, 'NIS_FOV', ibv, xxx10)
      nisfovx = xxx10(1) * radpd
      nisfovy = xxx10(2) * radpd
      ibv = 3
      call bodvar( scid, 'NIS_BORESIGHT', ibv, nisNomB0)
      ibv = 3
      call bodvar( scid, 'NIS_AXIS', ibv, nisAxis)
      ibv = 3
      call bodvar( scid, 'NIS_SAMPDIR', ibv, nisNomSampDir)
      ibv = 3
      call bodvar( scid, 'NIS_STEPS', ibv, xxx10)
      nisStepNom = int( xxx10(1) ) + 0.1
      nisStepAngle = xxx10(2) * radpd
      nisStepMax = int( xxx10(3) ) + 0.1
C
C normalize NIS axis, boresight & sampdir vectors; determine updir for NIS

      call unorm( nisAxis, nisAxis, xxx)
      call unorm( nisNomB0, nisNomB0, xxx)
      call vperp( nisNomSampDir, nisNomB0, nisNomSampDir)
      call unorm( nisNomSampDir, nisNomSampDir, xxx)
      call vcrss( nisNomSampDir, nisNomB0, nisNomUpDir)

      return
C
C-----------------------------------------------------------------------
C spice init:
C - level 2:  get camera frame points' definitions

      entry ospice_init2( scid, ncampts, campts, i3lenin, instrname)
      call cxfer( i3lenin, c255a, instrname)
      c255b = c255a(:i3lenin) // '_CAMERA_POINTS'
      ii = i3lenin + len( '_CAMERA_POINTS')
      i = ncampts * 2
      call bodvar( scid, c255b(:ii), i, campts)
      return
C
C-----------------------------------------------------------------------
C spice init:
C - level 3:  get initial UTC

      entry ospice_init3( i3lenin, i3lenout, utcout)
      if ( savutclen .lt. 1) then
        i3lenout = 0
        return
      endif
      i3lenout = min( savutclen, i3lenin)
      call cxferout( i3lenin, utcout, savutc(:savutclen))
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C spice misc:
C - convert unknown UTC to calendar
C - entries to convert ET or SCLKDP to some kind of string
C
      subroutine ospice_utc2cal( lenin, leninmax, lenout, utc)
      implicit none
C
      doubleprecision etin, sclkin
      integer lenin, leninmax, lenout
      character*1 utc(leninmax)
      integer scid
C
      character*60 utclcl
      doubleprecision et
C
      utclcl = ' '
      call cxfer( lenin, utclcl, utc)
      call utc2et( utclcl(:lenin), et)
      call et2utc( et, 'C', 2, utclcl)
C
      lenout = len(utclcl)
      dowhile ( lenout .gt. 1 .and. utclcl(lenout:lenout) .eq. ' ')
        lenout = lenout - 1
      enddo
      if ( utclcl(lenout:lenout) .eq. ' ') then
        lenout = 0
      else
        lenout = min( lenout, leninmax)
        call cxferout( leninmax, utc, utclcl(:lenout))
      endif
      return

C-----------------------------------------------------------------------
C - convert unknown UTC to doy
C
      entry ospice_utc2doy( lenin, leninmax, lenout, utc)
      utclcl = ' '
      call cxfer( lenin, utclcl, utc)
      call utc2et( utclcl(:lenin), et)
      call et2utc( et, 'ISOD', 2, utclcl)
C
      lenout = len(utclcl)
      dowhile ( lenout .gt. 1 .and. utclcl(lenout:lenout) .eq. ' ')
        lenout = lenout - 1
      enddo
      if ( utclcl(lenout:lenout) .eq. ' ') then
        lenout = 0
      else
        lenout = min( lenout, leninmax)
        call cxferout( leninmax, utc, utclcl(:lenout))
      endif
      return

C-----------------------------------------------------------------------
C - convert et to doy
      entry ospice_et2doy( etin, leninmax, lenout, utc)
      utclcl = ' '
      call et2utc( etin, 'ISOD', 2, utclcl)
C      print*, '"' // utclcl(:32) // '", ', et
C
      lenout = len(utclcl)
      dowhile ( lenout .gt. 1 .and. utclcl(lenout:lenout) .eq. ' ')
        lenout = lenout - 1
      enddo
      if ( utclcl(lenout:lenout) .eq. ' ') then
        lenout = 0
      else
        lenout = min( lenout, leninmax)
        call cxferout( leninmax, utc, utclcl(:lenout))
      endif
      return

C-----------------------------------------------------------------------
C - convert et to ISO calendar date
      entry ospice_et2isocal( etin, leninmax, lenout, utc)
      utclcl = ' '
      call et2utc( etin, 'ISOC', 2, utclcl)
C      print*, '"' // utclcl(:32) // '", ', etin
C
      lenout = len(utclcl)
      dowhile ( lenout .gt. 1 .and. utclcl(lenout:lenout) .eq. ' ')
        lenout = lenout - 1
      enddo
      if ( utclcl(lenout:lenout) .eq. ' ') then
        lenout = 0
      else
        lenout = min( lenout, leninmax)
        call cxferout( leninmax, utc, utclcl(:lenout))
      endif
      return

C-----------------------------------------------------------------------
C - convert et to jd
      entry ospice_et2jd( etin, leninmax, lenout, utc)
      utclcl = 'xyz'
      call et2utc( etin, 'J', 8, utclcl)
C
      lenout = len(utclcl)
      dowhile ( lenout .gt. 1 .and. utclcl(lenout:lenout) .eq. ' ')
        lenout = lenout - 1
      enddo
C      print*, '"' // utclcl(:32) // '", et=', etin, ' lenout=', lenout
      if ( utclcl(lenout:lenout) .eq. ' ') then
        lenout = 0
      else
        lenout = min( lenout, leninmax)
        call cxferout( leninmax, utc, utclcl(:lenout))
      endif
      return

C-----------------------------------------------------------------------
C - convert sclk to sclk character string
      entry ospice_scdecd( scid, sclkin, leninmax, lenout, utc)
      utclcl = ' '
      call scdecd( scid, sclkin, utclcl)
C
      lenout = len(utclcl)
      dowhile ( lenout .gt. 1 .and. utclcl(lenout:lenout) .eq. ' ')
        lenout = lenout - 1
      enddo
C      print*, '"' // utclcl(:32) // '", et=', etin, ' lenout=', lenout
      if ( utclcl(lenout:lenout) .eq. ' ') then
        lenout = 0
      else
        lenout = min( lenout, leninmax)
        call cxferout( leninmax, utc, utclcl(:lenout))
      endif
      return

      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C C/FORTRAN convenience routine:
C copy character*1 array (from C string, probably) into character*(*)
C
      subroutine cxfer( ilen, cout, c1in)
      implicit none
      integer ilen
      character*(*) cout, cin
      character*1 c1in(ilen), c1out(ilen)
C
      integer outlen, i

      outlen = min( ilen, len(cout))

      do i=1,outlen
        cout(i:i) = c1in(i)
      enddo
      return
C
C-----------------------------------------------------------------------
C ... and do the reverse
C
      entry cxferout( ilen, c1out, cin)
      outlen = min(ilen,len(cin))
      do i=1,outlen
        c1out(i) = cin(i:i)
      enddo
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C convert ascii token (body name (via bodn2c) or integer) to body id
C
      subroutine obody( c, id)
      implicit none
      character*(*) c
      integer id
C
      logical found
      character*20 fmt
      character*257 lclc
      integer ilen

      call bodn2c( c, id, found)
      if ( found) return
C
      lclc = c
      lclc(256:257) = ' 0'
      dowhile ( lclc(:1) .eq. ' ')
        lclc = lclc(2:) // ' '
      enddo
      ilen = index( lclc, ' ') - 1
      write( fmt, '(a2,i10,a1)') '(i', ilen, ')'
      dowhile ( fmt(3:3) .eq. ' ')
        fmt(3:) = fmt(4:) // ' '
      enddo
      read( lclc, fmt) id
C
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C get next word and its length
C
      subroutine gettok( cin, cout, outlen)
      implicit none
      character*(*) cin, cout
      integer outlen
C
      outlen = index( cin, " ") - 1
      if ( outlen .le. 0) outlen = len(cin)
      cout = cin(:outlen)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C collapse whitespace, find net length
C
      subroutine flencoll( c, ilen)
      implicit none
      character*(*) c
      integer ilen

      integer clen

      integer ISP
      parameter ( ISP=32)
C      parameter ( ISP=ichar(' '))

      clen = len(c)

      ilen = 1

      do ilen=1,clen
        if ( ichar(c(ilen:ilen)) .lt. ISP) c(ilen:ilen)=' '
      enddo
      call cmprss( ' ', 1, c, c)
      ilen=clen
      dowhile ( ilen.gt.1 .and. c(ilen:ilen).eq.' ')
        ilen = ilen - 1
      enddo
      if ( c(ilen:ilen).eq.' ') ilen = 0
      return
      end


C***********************************************************************
C***********************************************************************
C***********************************************************************
      subroutine ospice_elllim( semis, vec, pt)
      implicit none
C
C find pt on ellipsoid that is normal to vector
C
      doubleprecision semis(3), vec(3), pt(3)
C
      doubleprecision sq, semix2, semiy2, semiz2
      logical return
C***********************************************************************
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ELLLIM' )
      END IF
C
      semix2 = semis(1)*semis(1)
      semiy2 = semis(2)*semis(2)
      semiz2 = semis(3)*semis(3)
      sq = vec(1)*vec(1)*semix2
      sq = sq + vec(2)*vec(2)*semiy2
      sq = sq + vec(3)*vec(3)*semiz2
      sq = sqrt( sq)
      pt(1) = vec(1)*semix2 / sq
      pt(2) = vec(2)*semiy2 / sq
      pt(3) = vec(3)*semiz2 / sq
C
      call chkout( 'ELLLIM')
      return
      end
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine ospice_ellint( semis, vec, dvec, l, nret)
      implicit none
C      implicit undefined (a-z)
C
C intersection of tri-axial ellipsoid with line
C - Ellipsoid: 1 = x/(semis(1)^2) + y/(semis(2)^2) + z/(semis(3)^2)
C - Line: vec> + l * dvec>
C   - vec> and dvec> are vectors;
C   - l is a scalar, representing distance from vec if dvec> is a unit vector
C   - ***N.B. returns solution closest to vec> first (i.e. abs(l(1))<abs(l(2)))
C
C inputs:
C
      doubleprecision semis(3), vec(3), dvec(3)
C
C outputs:
C
C - l(2)   scalar l in (vec> + l * dvec>) - up to 2 solutions possible
C - nret   number of solutions returned: -1=>bad inputs; else 0, 1 or 2
C
      doubleprecision l(2)
      integer nret
C
C local variables
C
C - a,b,c    quadratic coefficients 
C - det      determinant
C - m1, m2   misc dp variables
C - i        loop index
C
      doubleprecision a, b, c, det, m1, m2
      integer i
      logical return
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C START
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ELLINT' )
      END IF
C
C assume bad inputs until proven otherwise
C
      nret = -1
C
C convert inputs to quadratic coefficients
C
      a = 0D0
      b = 0D0
      c = - 1D0
      do i=1,3
        if ( semis(i) .eq. 0D0) goto 99999
        m1 = dvec(i) / semis(i)
        m2 = vec(i) / semis(i)
        a = a + m1 * m1
        b = b + 2D0 * m1 * m2
        c = c + m2 * m2
      enddo
C
C check for bad inputs (a=0 => dvec> is null)
C
      if ( a .eq. 0D0) goto 99999
C
C check for no solution; return closest solution in l1 if no solution
C
      det = b*b - 4*a*c
      if ( det .lt. 0D0) then
        nret = 0
        l(1) = - b / (2D0 * a)
        goto 99999
      endif
C
C test for single solution (line is tangent to ellipsoid); 
C   put single solution in l(2) also, just for fun
C
      if ( det .eq. 0D0) then
        nret = 1
        l(1) = - b / (2D0 * a)
        l(2) = l(1)
        goto 99999
      endif
C
C two solutions
C
      nret = 2
      det = sqrt(det)
      l(1) = (det - b) / (2D0 * a)
      l(2) = (-det - b) / (2D0 * a)
C
C return if l(1) is closer to vec>, or if b is zero (positive l(1) returned)
C else swap solutions
C
      if ( abs(l(1)) .le. abs(l(2)) ) goto 99999
      m1 = l(1)
      l(1) = l(2)
      l(2) = m1
C
99999 continue
      call chkout( 'ELLINT')
      return
      end
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine ospice_et_ckgp( et, scid, cmat, etout, found)
      implicit none
C interface to ckgp
C
      doubleprecision et, cmat(9), etout
      integer scid, found
C
      doubleprecision sclkdp, sclkdpout, toldp
      integer instr
      logical lfound
      logical failed
      character*10 noSub1FromInstr
C***********************************************************************
      instr = ( scid * 1000)
      noSub1FromInstr = ' '
      call getenv( 'ORBIT_NO_SUB1_FROM_INSTR', noSub1FromInstr)
      if ( noSub1FromInstr(1:1) .eq. ' ') then
        instr = instr - 1
        if ( scid .eq. -93) instr = -93000
        if ( scid .eq. -140) instr = -140000
      endif
      call sce2t( scid, et, sclkdp)
      call sctiks( scid, '1', toldp)
      call ckgp( instr,sclkdp,toldp,'J2000', cmat, sclkdpout, lfound)
      dowhile ( .not. failed() .and. .not. lfound .and. toldp .lt. 1d6)
        print*, 'ckgp failed, sclkdp,toldp,instr=',sclkdp,toldp,instr
        toldp = max( nint(toldp+1d0), nint(toldp * 1.414d0))
        call ckgp( instr,sclkdp,toldp,'J2000', cmat, sclkdpout, lfound)
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc72
      enddo
      if ( failed()) call reset
      if ( lfound) then
        found = 1
        call sct2e( scid, sclkdpout, etout)
      else if ( failed()) then
        call reset
      else
        found = 0
      endif
      return
      end
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine ospice_ellnorm( semis, vec, norm)
      implicit none
C      implicit undefined (a-z)
C
C find normal to tri-axial ellipsoid at a point
C
      doubleprecision semis(3), vec(3), norm(3)
C
      doubleprecision semix2, semiy2, semiz2, sum3
      logical return
C***********************************************************************
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ELLNORM' )
      END IF
C
C scale vec to semis() (if necessary)
C
      semix2 = semis(1)*semis(1)
      semiy2 = semis(2)*semis(2)
      semiz2 = semis(3)*semis(3)
C
      sum3 = vec(1)*vec(1)/semix2 + 
     &       vec(2)*vec(2)/semiy2 +
     &       vec(3)*vec(3)/semiz2
C
      if ( sum3 .ne. 1D0) then
C
C scale and calculate normal
C
        sum3 = sqrt( 1D0 / sum3)
        norm(1) = semiy2 * semiz2 * sum3 * vec(1)
        norm(2) = semix2 * semiz2 * sum3 * vec(2)
        norm(3) = semix2 * semiy2 * sum3 * vec(3)
      else
C
C calculate normal
C
        norm(1) = semiy2 * semiz2 * vec(1)
        norm(2) = semix2 * semiz2 * vec(2)
        norm(3) = semix2 * semiy2 * vec(3)
      endif
      call chkout( 'ELLNORM')
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C routines to convert strings to ephemeris times
C
      subroutine ospice_tparse( et, lentdt, tdt)
      implicit none
      doubleprecision et, sclkdp
      integer lentdt, lenutc, lensclkch, scid
      character*(1) tdt(lentdt), utc(lenutc), sclkch(lensclkch)
      integer bodyid, lennam
      character*(1) nam(lennam)
C
      character*255 c255
      character*320 error
C
      call chkin( 'ospice_tparse')
      call cxfer( lentdt, c255, tdt)
      call tparse( c255(:lentdt), et, error)
      call chkout( 'ospice_tparse')
      return
C
C-----------------------------------------------------------------------
C
      entry ospice_obody( bodyid, lennam, nam)
C
      call chkin( 'ospice_obody')
      call cxfer( lennam, c255, nam)
      call obody( c255(:lennam), bodyid)
      call chkout( 'ospice_obody')
      return
C
C-----------------------------------------------------------------------
C
      entry ospice_utc2et( et, lenutc, utc)
C
      call chkin( 'ospice_utc2et')
      call cxfer( lenutc, c255, utc)
      call utc2et( c255(:lenutc), et)
      call chkout( 'ospice_utc2et')
      return
C
C-----------------------------------------------------------------------
C
      entry ospice_sclkch2et( scid, et, lensclkch, sclkch)
C
      call chkin( 'ospice_sclkch2et')
      call cxfer( lensclkch, c255, sclkch)
      call scs2e( scid, c255(:lensclkch), et)
      call chkout( 'ospice_sclkch2et')
      return
C
C-----------------------------------------------------------------------
C
      entry ospice_scencd( scid, sclkdp, lensclkch, sclkch)
C
      call chkin( 'ospice_scencd')
      call cxfer( lensclkch, c255, sclkch)
      call scencd( scid, c255(:lensclkch), sclkdp)
      call chkout( 'ospice_scencd')
      return
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C get rotation to J2000 from either another reference frame
C                       or a body's matrix at a given time
C
      subroutine ospice_getrefrmmtx( fromj2k, lenrefrm, refrm)
      implicit none
      integer bodyid
      doubleprecision et, fromj2k(3,3)
      integer lenrefrm
      character*(1) refrm(lenrefrm)
C
      character*255 c255
      integer ij2k, irefrm
C
      call chkin( 'ospice_getrefrmmtx')
      call cxfer( lenrefrm, c255, refrm)
      call irfnum( c255(:lenrefrm), irefrm)
      call irfnum( 'J2000', ij2k)
      call irfrot( ij2k, irefrm, fromj2k)
      call chkout( 'ospice_getrefrmmtx')
      return
C
C-----------------------------------------------------------------------
C
      entry ospice_getbodymtx( bodyid, et, fromj2k)
C
      call chkin( 'ospice_getbodymtx')
      call bodmat( bodyid, et, fromj2k)
      call chkout( 'ospice_getbodymtx')
      return
      end
C
C-----------------------------------------------------------------------
C
      subroutine ospice_j2fix( scSo, et, scve
     &                       , scJ2, scRpl, scPeriod, scUPA0, scUTP0)
      implicit none
      doubleprecision scSo(8), et, scve(6)
     &              , scJ2, scRpl, scPeriod, scUPA0(3), scUTP0(3)
C
      doubleprecision tupi, onepi, angle, dot
      double precision cosinc, pv(3)
      doubleprecision epoch, ecc, p, gm, dt, oneme2, dmdt, manom
     &              , theta, z, dnode, dperi, ta
      integer k2pi
C
      doubleprecision twopi, vdot, vsep, dpr
      logical vzero
      data tupi / 0d0 /
      save
C
      if ( scJ2 .eq. 0d0) return
C
      call chkin( 'ospice_j2fix')
C
      if ( tupi .eq. 0d0) then
        tupi = TWOPI()
        onepi = tupi / 2d0
        pv(1) = 0d0
        pv(2) = 0d0
        pv(3) = 1d0
      endif
C
C        The line of node precesses. Over one orbit average rate of
C        precession,  DNode/dNu,  is given by
C
C                                3 J2
C              dNode/dNu =  -  -----------------  DCOS( inc )
C                                2 (P/RPL)**2
C
C        (Since this is always less than zero for oblate spheroids, this
C           should be called regression of nodes.)
C
C        The line of apsides precesses. The average rate of precession
C        DPeri/dNu is given by
C                                   3 J2
C              dPeri/dNu =     ----------------- ( 5*DCOS ( inc ) - 1 )
C                                2 (P/RPL)**2
C
C
C     Fetch the various entities from the inputs
C
      EPOCH  = scSo(7)
      ECC    = scSo(2)
      P      = scSo(1) * (1d0 + ECC)
      GM     = scSo(8)
 
C
C     Check all the inputs here for obvious failures.  Yes, perhaps
C     this is overkill.  However, there is a lot more computation
C     going on in this routine so that the small amount of overhead
C     here should not be significant.
C
 
 
      IF ( P .LE. 0 ) THEN
 
         CALL SETMSG ( 'The semi-latus rectum supplied to the'
     .   //            'ospice_j2fix evaluator was non-positive.  This '
     .   //            'value must be positive. The value supplied was '
     .   //            '#.'                    )
         CALL ERRDP  ( '#', P                  )
         CALL SIGERR ( 'SPICE(BADLATUSRECTUM)' )
         CALL CHKOUT ( 'ospice_j2fix'                )
         RETURN
 
      ELSE IF ( ECC .LT. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The eccentricity supplied to the ospice_j2fix '
     .   //            'routine is < 0.  It must be non-negative. '
     .   //            'The value supplied was #. ' )
         CALL ERRDP  ( '#',   ECC                )
         CALL SIGERR ( 'SPICE(BADECCENTRICITY)'  )
         CALL CHKOUT ( 'ospice_j2fix'                  )
         RETURN
 
      ELSE IF ( GM  .LE. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The GM supplied for the central body '
     .   //            'was non-positive. '
     .   //            'Masses must be positive.  The value '
     .   //            'supplied was #. '        )
         CALL ERRDP  ( '#', GM                   )
         CALL SIGERR ( 'SPICE(NONPOSITIVEMASS)'  )
         CALL CHKOUT ( 'ospice_j2fix'                  )
         RETURN
 
      ELSE IF ( VZERO(scUTP0) ) THEN
 
         CALL SETMSG ( 'The trajectory pole vector supplied to '
     .   //            'ospice_j2fix had length zero. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'ospice_j2fix'             )
         RETURN
 
      ELSE IF ( VZERO(scUPA0) ) THEN
 
         CALL SETMSG ( 'The periapse vector supplied to ospice_j2fix '
     .   //            'had length zero. ' )
         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
         CALL CHKOUT ( 'ospice_j2fix'             )
         RETURN
 
C      ELSE IF ( VZERO(PV) ) THEN
C 
C         CALL SETMSG ( 'The central pole vector supplied to '
C     .   //            'ospice_j2fix had length zero. The most likely '
C     .   //            'cause of this problem is a corrupted SPK '
C     .   //            '(ephemeris) file. ' )
C         CALL SIGERR ( 'SPICE(BADVECTOR)'   )
C         CALL CHKOUT ( 'ospice_j2fix'             )
C         RETURN
 
 
      ELSE IF ( scRpl .LT. 0.0D0 ) THEN
 
         CALL SETMSG ( 'The central body radius was negative. '
     .   //            'It must be zero or positive.  The value '
     .   //            'supplied was #. '  )
         CALL ERRDP  ( '#', scRpl            )
         CALL SIGERR ( 'SPICE(BADRADIUS)'  )
         CALL CHKOUT ( 'ospice_j2fix'            )
         RETURN
 
      END IF
C
C     One final check.  Make sure the pole and periapse vectors are
C     orthogonal. (We will use a very crude check but this should
C     rule out any obvious errors.)
C
      DOT = VDOT ( scUPA0, scUTP0 )
 
      IF ( ABS(DOT) .GT. 1.0D-5 ) THEN
 
         ANGLE = VSEP ( scUPA0, scUTP0 ) * DPR()
 
         CALL SETMSG ( 'The periapsis and trajectory pole '
     .   //            'vectors are not orthogonal. The angle'
     .   //            'between them is # degrees. '         )
         CALL ERRDP  ( '#',     ANGLE        )
         CALL SIGERR ( 'SPICE(BADINITSTATE)' )
         CALL CHKOUT ( 'ospice_j2fix'              )
         RETURN
 
      END IF
 
      DT = ET - EPOCH
 
C
C     First compute the change in mean anomaly since periapsis.
C
      ONEME2 = 1.0D0 - ECC**2
      DMDT   = (ONEME2/P)*DSQRT(GM*ONEME2/P)
      MANOM  = DMDT*DT
C
C     Next compute the angle THETA such that THETA is between
C     -pi and pi and such than MANOM = THETA + K*2*pi for
C     some integer K.
C
      THETA  = MOD( MANOM, tupi )
 
      IF ( ABS(THETA).GT. onepi ) THEN
        THETA = THETA - SIGN(tupi,THETA)
      END IF
 
      K2PI   = MANOM - THETA
C
C     We can get the accumulated true anomaly from the propagated
C     state theta and the accumulated mean anomaly prior to this
C     orbit.
C
      TA     = VSEP ( scUPA0, scve )
      TA     = SIGN ( TA, THETA )
      TA     = TA  +  K2PI
 
C
C     Determine how far the line of nodes and periapsis have moved.
C
      COSINC = dcos( scSo(3))
 
      Z      = TA * 1.5D0 * scJ2 *(scRpl/P)**2
      DNODE  = -Z * COSINC
      DPERI  =  Z * ( 2.5D0*COSINC**2 - 0.5D0 )
 
C
C     Precess the periapsis by rotating the state vector about the
C     trajectory pole
C
      CALL VROTV ( scve(1), scUTP0, DPERI, scve(1) )
      CALL VROTV ( scve(4), scUTP0, DPERI, scve(4) )
 
C
C     Regress the line of nodes by rotating the state
C     about the the pole of the central body.

      CALL VROTV ( scve,    PV, DNODE, scve    )
      CALL VROTV ( scve(4), PV, DNODE, scve(4) )
 
C
C     We could perform the rotations above in the other order,
C     but we would also have to rotate the pole before precessing
C     the line of apsides.
C
      CALL CHKOUT('ospice_j2fix')
      RETURN
      END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C load & unload SP-, C- & PC- Kernel files
C - SPK:  ospice_spkluef    (subroutine)
C -  CK:  ospice_cklupf     (entry)
C - PCK:  ospice_pckluof    (entry)

      subroutine ospice_spkluef( doload, handle, namlen, filnam)
      integer doload, handle, namlen
      character*(1) filnam
C
      character*255 c255
C
      call chkin( 'ospice_spkluef')
      call cxfer( namlen, c255, filnam)
      if ( doload .ne. 0) then
        call spklef( c255(:namlen), handle)
      else
        call spkuef( handle)
      endif
      call chkout( 'ospice_spkluef')
      return
C------------------------
      entry ospice_cklupf( doload, handle, namlen, filnam)
      call chkin( 'ospice_cklupf')
      call cxfer( namlen, c255, filnam)
      if ( doload .ne. 0) then
        call cklpf( c255(:namlen), handle)
      else
        call ckupf( handle)
      endif
      call chkout( 'ospice_cklupf')
      return
C------------------------
      entry ospice_pckluof( doload, handle, namlen, filnam)
      call chkin( 'ospice_pckluof')
      call cxfer( namlen, c255, filnam)
      if ( doload .ne. 0) then
        call pcklof( c255(:namlen), handle)
      else
        call pckuof( handle)
      endif
      return
      call chkout( 'ospice_pckluof')
      end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C write a type 3 C Kernel segment; angular velocities are optional
C
      subroutine ospice_writeck( inew, instr, writav
     &                         , nrec, sclkdp, quat, av
     &                         , numint, intstarts, fnlen, filnam)
      implicit none
C
C     set inew = 0 to append to existing CK file, else create new CK file
C     instr = instrument ID (e.g. -93000)
C     set writav = 0 for no angular velocities in av
C     nrec = number of CK records to write into this segment
C
      integer inew, instr, writav, nrec
C
C     sclkdp = ticks
C     quat = quaternions
C     av = angular velocities IF writav .ne. 0
C
      doubleprecision sclkdp(nrec), quat(4,nrec), av(3,nrec)
C
C     numint = # of intervals in segment
C     intstarts = ticks of starts of intervals
C     fnlen = length of file name of CK file to write/append
C     filnam = file name of CK DAF to write/append
C
      integer numint
      doubleprecision intstarts(numint)
      integer fnlen
      character*(1) filnam
C
C     local variables
C
      character*255 fn255
      character*40 segid
      character*60 ifname
      character*5 ref
      character*2 ck
      logical avflag
      integer nd, ni, handle
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     convert character*1 to character*(fnlen)
C
      call cxfer( fnlen, fn255, filnam)
C
C     set up to write CK segment
C     - avflag is true if av contains angular velocities
C
C              1234567890123456789012345678901234567890
      segid = 'Written by ORBIT routine OSPICE_WRITECK'
      ifname = 'ORBIT:  ' // fn255(:fnlen)
      ref = 'J2000'
      ck = 'CK'
      avflag = writav .ne. 0
      nd = 2
      ni = 6
C
C     open CK DAF
C     - if inew .ne. 0 open new CK DAF, else open existing CK DAF
C
      handle = -1
      if ( inew .ne. 0) then
        call dafonw( fn255(:fnlen), ck, nd, ni, ifname, 0, handle)
C        print*, 'onw handle = ', handle
      else
        call dafopw( fn255(:fnlen), handle)
C        print*, 'opw handle = ', handle
      endif
C
C     write the segment & close the DAF
C
      call ckw03( handle, sclkdp(1), sclkdp(nrec), instr, ref, avflag
     &          , segid, nrec, sclkdp, quat, av, numint, intstarts)
      call dafcls( handle)
C
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C find angular velocity vector (radians/s) between two quaternions
C
      SUBROUTINE ospice_qqt2av( THISQ, LASTQ, THIS, LAST, AV)
      implicit none
C
C     INPUT:  Rotations
C
      DOUBLE PRECISION      THISQ ( 4)
      DOUBLE PRECISION      LASTQ ( 4)
C
C     INPUT:  Epochs (note these are NOT ticks they are seconds.)
C
      DOUBLE PRECISION      THIS
      DOUBLE PRECISION      LAST
C
C     OUTPUT:  angular velocity
C
      DOUBLE PRECISION      AV
C
C     LOCAL:  rotation matrices
C
      doubleprecision thisr(3,3), lastr(3,3)
C
      CALL q2m( thisq, thisr)
      call q2m( lastq, lastr)
      call ospice_mmt2av( thisr, lastr, this, last, av)
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C find angular velocity vector (radians/s) between two rotation matrices
C
      SUBROUTINE ospice_mmt2av( THISR, LASTR, THIS, LAST, AV)
      implicit none
C
C     INPUT:  Rotations
C
      DOUBLE PRECISION      THISR ( 3, 3 )
      DOUBLE PRECISION      LASTR ( 3, 3 )
C
C     INPUT:  Epochs (note these are NOT ticks they are seconds.)
C
      DOUBLE PRECISION      THIS
      DOUBLE PRECISION      LAST
C
C     OUTPUT:  Angular velocities
C
      DOUBLE PRECISION      AV    ( 3 )
C
C     LOCAL:  delta rotation from LASTR to THISR
C     
      DOUBLE PRECISION      DELROT( 3, 3 )
C
C     LOCAL:  Vectors
C
      DOUBLE PRECISION      AXIS  ( 3 )
C
C     LOCAL:  Angle, angle rate, and delta time between THIS & LAST
C
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      ANGRAT
      DOUBLE PRECISION      DELTA
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      CALL CHKIN( 'MMT2AV')
C
C     We have two rotations THISR and LASTR corresponding to the
C     ephemeris epochs THIS and LAST given in seconds past J2000.  We
C     construct an angular velocity that would carry orientation LASTR
C     to THISR over the time interval from LAST to THIS
C
C     Compute the delta time between the two epochs ...
C
      DELTA = THIS - LAST
      IF ( DELTA .EQ. 0D0) THEN
        AV(1) = 0D0
        AV(2) = 0D0
        AV(3) = 0D0
        CALL CHKOUT( 'MMT2AV')
        RETURN
      ENDIF

C
C     Get the delta rotation from LASTR to THISR.
C
      CALL MXMT   ( THISR,  LASTR,  DELROT )

C
C     Compute the axis and angle associated with DELROT
C
      CALL RAXISA ( DELROT, AXIS,   ANGLE  )

C
C     We need to divide the ANGLE by DELTA to get the angular rate.
C
      ANGRAT = ANGLE / DELTA

C
C     Finally scale the AXIS by the angular rate to get, AV, the
C     angular velocity.
C
      CALL VSCL ( ANGRAT, AXIS, AV     )
C
      CALL CHKOUT( 'MMT2AV')
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Kernel pool routines - C interfaces
C
      subroutine ospice_ldpool( lenFn, Fn)
      implicit none
C
C     LDPOOL - arguments
C
      integer lenFn
      character*(1) Fn(lenFn)
C
C     LDPOOL - local variables
C
      character*255 c255fn
C
C     DTPOOL - arguments
C
      integer lenName
      integer found
      character*(1) name(lenName)
      character*(1) type(1)
C
C     DTPOOL - local vars
C
      character*255 c255name
      character*1 c1type
      logical lfound
C
C     GDPOOL - arguments
C
      integer start, room, n
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     start LDPOOL code
C
      call cxfer( lenFn, c255fn, Fn)
      call ldpool( c255fn(:lenFn))
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      entry ospice_clpool
      call clpool
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     ospice_dtpool( fortint lenName, fortint found, fortint n
C                  , char type[1], char *name)
C
      entry ospice_dtpool( lenName, found, n, type, name)
C
      call cxfer( lenName, c255name, name)
C
      call dtpool( c255name(:lenName), lfound, n, c1type)
C
      if ( lfound) then
        found = 1
      else
        found = 0
      endif
      call cxferout( 1, type, c1type)
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     ospice_gdpool( fortint lenName, fortint start, fortint room
C                  , fortint n, fortint found, char *name)
C
      entry ospice_gdpool( lenName, start, room, n, found, name)
C
      call cxfer( lenName, c255name, name)
C
      call gdpool( c255name(:lenName), start, room, n, lfound)
C
      if ( lfound) then
        found = 1
      else
        found = 0
      endif
      return
      end
