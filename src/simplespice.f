C$Procedure   SUBPT ( Compute a sub-observer point )
C
      PROGRAM SUBPT
      implicit none
C
C$ Abstract
C
C     This 'cookbook' program demonstrates the use of the SPICELIB
C     toolkit by computing the apparent sub-observer point on a target
C     body. It uses light time corrections in order to do this.
C
C$ Copyright
C
C     Copyright (1995), California Institute of Technology.
C     U.S. Government sponsorship acknowledged.
C
C$ Input
C
C     The user is prompted for the following:
C
C        - A UTC epoc at which to begin.
C        - A UTC epoch at which to end.
C        - The number of sub-observer points to be calculated in the
C          given UTC time interval.
C
C$ Output
C
C     The program calculates the planetocentric latitude and longitude
C     of the nearest point on the target body to the observing body
C     for a given number of UTC epochs (see Input above). For each
C     time, the following information is displayed on the terminal
C     screen:
C
C        - The UTC epoch.
C        - The radius of the target body.
C        - The planetocentric longitude of the nearest point on
C          the target body to the observing body.
C        - The planetocentric latitude of the nearest point on
C          the target body to the observing body.
C
C$ Particulars
C
C     The SPK file must contain data for both the observing body and
C     the target body during the specified time interval.
C
C     The `sub-observer point' is defined to be the point on the target
C     body that is closest to the observer.  The state of the target
C     body at time t-Tau is the state of the target seen by the
C     observer at time t, where Tau is the light time from the target
C     body to the observer.  The `apparent sub-observer point' is the
C     point on the target body at time t-Tau that is closest to the
C     observer at time t.
C
C     Planetocentric coordinates are defined by a distance from a
C     central reference point, an angle from a reference meridian,
C     and an angle above the equator of a sphere centered at the
C     central reference point.  These are the radius, longitude,
C     and latitude, respectively.
C
C     For the sake of brevity, this program does NO error checking
C     on its inputs. Mistakes will cause the program to crash. But,
C     since this is an example program, that's OK.
C
C$ References
C
C        KERNEL        The SPICELIB Kernel Pool
C        ROTATIONS     Rotations
C        SPK           S- and P- Kernel (SPK) Specification
C        TIME          Time routines in SPICELIB
C
C     For questions about a particular subroutine, refer to its
C     header.
C
C$ Version
C
C-    SPICELIB Version 2.1.0, 05-AUG-1994 (HAN)
C
C        Cleaned up the code.
C
C-    SPICELIB Version 2.0.0, 01-APR-1992 (KRG)
C
C        Cleaned up the comments.
C
C        Fixed some INTEGER/DOUBLE PRECISION mixed mode arithmetic
C        by using the intrinsic function DBLE to convert the INTEGER
C        to DOUBLE PRECISION.
C
C        Removed the 'real number' logical test in the DO WHILE loop,
C        replacing it with an INTEGER equivalent. Added the variable
C        NPTS to enable this.
C
C-    Beta Version 1.0.0, 25-SEP-1990 (JEM)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
 
C
C     Parameters
C
      CHARACTER*(*)         TBLFMT
      PARAMETER           ( TBLFMT = '(2X,A20,2X,F10.5,2X,F10.5)' )
 
      INTEGER               DIM
      PARAMETER           ( DIM =     3 )
 
      INTEGER               LDIM
      PARAMETER           ( LDIM =    6 )
 
      INTEGER               LONG
      PARAMETER           ( LONG  = 128 )
 
      INTEGER               MED
      PARAMETER           ( MED   =  32 )
 
      INTEGER               SHORT
      PARAMETER           ( SHORT =   8 )
 
C
C     Variables
C
      CHARACTER*(80)        LINE
      CHARACTER*(SHORT)     ABCORR
      CHARACTER*(SHORT)     CONTIN
      CHARACTER*(SHORT)     FRAME
      CHARACTER*(LONG)      LEAPSC
      CHARACTER*(LONG)      PCONST
      CHARACTER*(LONG)      SPKFIL
      CHARACTER*(MED)       UTCBEG
      CHARACTER*(MED)       UTCEND
      CHARACTER*(MED)       UTCOUT

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      ETBEG
      DOUBLE PRECISION      ETEND
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NPOINT   ( DIM      )
      DOUBLE PRECISION      POS      ( DIM      )
      DOUBLE PRECISION      RADII    ( DIM      )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      STATE    ( LDIM     )
      DOUBLE PRECISION      TIPM     ( DIM, DIM )
 
      INTEGER               HANDLE
      INTEGER               MAXPTS
      INTEGER               NPTS
      INTEGER               NRADII
      INTEGER               OBSRVR
      INTEGER               TARGET, i1, i2, ncampts

      doubleprecision fov(4), boresight(3)
      doubleprecision panel(3), updir(3), sampdir(3)

      character*1 filnam(99)
      character*9 filnam9
      data filnam9 / 'spicespec' /

C
C     Introduction.
C
      WRITE (*,*)
      WRITE (*,*) '             Welcome to SUBPT'
      WRITE (*,*)
      WRITE (*,*) 'This program demonstrates the use of SPICELIB in'
      WRITE (*,*) 'computing the apparent sub-observer point on a'
      WRITE (*,*) 'target body. The computations make use of light'
      WRITE (*,*) 'time corrections.'
      WRITE (*,*)

      do i1=1,len(filnam9)
	filnam(i1) = filnam9(i1:i1)
      enddo
      i1 = len(filnam9)
      call ospice_init0( i1, filnam)

      call ospice_init1( obsrvr, target, i1, i2
     &                       , radii
     &                       , boresight, panel, updir, sampdir
     &                       , fov, ncampts)

C
C     Begin the continuation loop.
C
      CONTIN = 'Y'
 
      DO WHILE ( CONTIN .EQ. 'Y' )
 
         WRITE (*,*)
         WRITE (*,*) 'Enter the beginning UTC time:'
         READ  (*,FMT='(A)') UTCBEG
 
         WRITE (*,*)
         WRITE (*,*) 'Enter the ending UTC time:'
         READ  (*,FMT='(A)') UTCEND
 
         WRITE (*,*)
         WRITE (*,*) 'Enter the number of points to be calculated:'
         READ  (*,*) MAXPTS
         WRITE (*,*)
 
C
C        Convert the UTC time interval to ET. ET stands for Ephemeris
C        Time and is in units of ephemeris seconds past Julian year
C        2000. ET is the time system that is used internally in SPK
C        ephemeris files and reader subroutines.
C
         CALL UTC2ET ( UTCBEG, ETBEG )
         CALL UTC2ET ( UTCEND, ETEND )
 
C
C        Extract from the kernel pool the radii (A, B, and C) of the
C        ellipsoid model of the target body.
C
         CALL BODVAR ( TARGET, 'RADII', NRADII, RADII )
 
         A = RADII (1)
         B = RADII (2)
         C = RADII (3)
 
C
C        Set the frame of reference and abberation flag. We will use
C        'J2000' and 'LT' (for Light-Time correction), respectively.
C
         FRAME  = 'J2000'
         ABCORR = 'LT'
 
C
C        DELTA is the increment between consecutive times.
C
C        Make sure that the number of sub-observer point calculations
C        is >= 1, to avoid a division by zero error.
C
         MAXPTS = MAX(1,MAXPTS)
         DELTA = ( ETEND - ETBEG ) / DBLE( MAXPTS - 1 )
 
C
C        Write the headings for the table of values.
C
         WRITE (*,*) 'Planetocentric coordinates for the nearest point'
         WRITE (*,*) 'on the target body to the observing body (deg).'
 
         LINE = 'Target body: #          Observing body: #'
         CALL REPMI ( LINE, '#', TARGET, LINE )
         CALL REPMI ( LINE, '#', OBSRVR, LINE )
         WRITE (*,*) LINE
 
         WRITE (*,*)
         WRITE (*,*) '       UTC Time            Lat         Lon'
         WRITE (*,*) '----------------------------------------------'
 
C
C        Now, everything is set up.
C
         EPOCH  = ETBEG
         NPTS   = 1
 
         DO WHILE ( NPTS .LE. MAXPTS )
 
C
C           Determine the position of the observer in target
C           body-fixed coordinates.
C
C           Call SPKEZ to compute the position (POS) of the target
C           body as seen from the observing body and the light time
C           (LT) between them.  The coordinates of POS are relative
C           to the J2000 inertial reference frame and are corrected
C           for light time.  That is, POS is the vector from the
C           observer at EPOCH to the target at EPOCH - LT.
C
            CALL SPKEZ  ( TARGET, EPOCH, FRAME, ABCORR, OBSRVR,
     .                    STATE, LT )
 
            POS (1) = STATE (1)
            POS (2) = STATE (2)
            POS (3) = STATE (3)
 
C
C           Call BODMAT to compute the rotation matrix (TIPM) that
C           transforms inertial coordinates to target body equator
C           and prime meridian (body-fixed) coordinates at EPOCH - LT.
C
            CALL BODMAT ( TARGET, EPOCH-LT, TIPM )
 
C
C           Call VMINUS to reverse the direction of the vector (POS)
C           so it will be the position of the observer at EPOCH as
C           seen from the target body at EPOCH - LT in inertial
C           coordinates.
C
C           Note that this result is not the same as the result of
C           calling SPKEZ with the target and observer switched.  We
C           computed the vector FROM the observer TO the target in
C           order to get the proper light time correction.  Now we
C           need the vector FROM the target TO the observer in order
C           to compute the desired latitude and longitude.
C
            CALL VMINUS ( POS, POS )
 
C
C           Call MXV to transform the vector, POS, to body-fixed
C           coordinates by multiplying it by the matrix TIPM.
C
            CALL MXV    ( TIPM, POS, POS )
 
C
C           Locate the sub-observer point.
C
C           Call NEARPT to locate the point on the surface of the
C           target body that is nearest to the observer.  This is
C           the sub-observer point in target body-fixed rectangular
C           coordinates. (The target body is defined by its radii
C           whose values are extracted from the kernel pool with
C           BODVAR above.)
C
            CALL NEARPT ( POS, A, B, C, NPOINT, DIST )
 
C
C           Convert the coordinates of the sub-observer point to
C           planetocentric and write out the values.
C
C           Call RECLAT to transform the rectangular coordinates of
C           the sub-observer point to planetocentric.
C
            CALL RECLAT ( NPOINT, RADIUS, LON, LAT  )
 
C
C           Multiply LAT and LON by the number of degrees per radian.
C
            LON = LON * DPR ()
            LAT = LAT * DPR ()
 
C
C           Convert the current EPOCH to UTC time for display.
C
            CALL ET2UTC ( EPOCH, 'J', 7, UTCOUT )
 
C
C           Display results:
C
            WRITE (*,FMT=TBLFMT) UTCOUT,LAT,LON
 
            EPOCH = EPOCH + DELTA
            NPTS = NPTS + 1
 
         END DO
 
C
C        Continue?
C
         WRITE (*,*)
         WRITE (*,*) 'Continue? (Enter Y or N)'
         READ  (*,FMT='(A)') CONTIN
 
         IF ( CONTIN .EQ. 'y ' ) THEN
            CONTIN = 'Y'
         ELSE IF ( CONTIN .EQ. ' ' ) THEN
            CONTIN = 'Y'
         END IF
 
      END DO
 
      END
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
      doubleprecision fov(4)
      integer ncampts
      doubleprecision campts
      doubleprecision boresight(3)
      doubleprecision panel(3)
      doubleprecision sampdir(3)
      doubleprecision updir(3)
C
      character*255 c255a, c255b
      integer i
      integer ilena, ilenb, iid
      integer colon
      integer lun1, handle
      doubleprecision xxx

      integer savscid, savasterid, savsunid, savearthid
      character*255 savutc
      integer savutclen

      integer ISPK, ICK, IPCK, IORIBPCK, ILEAP
     &, INPXLX, INPXLY, IURADY, IURADX, IFOVX, IFOVY, IUTC
     &, IEARTH, ISUN, IAST, ISC, IIDLAST
      parameter ( ISPK = 1
     &          , ICK = ISPK + 1
     &          , IPCK = ICK + 1
     &          , IORIBPCK = IPCK + 1
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
     &          , IIDLAST = ISC )

      character*10 keys(IIDLAST), key1
      data (keys(i), i=1,IIDLAST) /
     &    'SPK:      '
     &  , 'CK:       '
     &  , 'PCK:      '
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
     &  /

      save

      savutclen = 0

      call cxfer( i1len, c255a, filnam)

      call erract( 'set', 'report')
      call getlun( lun1)
      open( lun1, file=c255a(:i1len), status='old')
C
C 1) read line

      dowhile(.true.)
        read(lun1,'(a)',end=99999) c255a
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

            elseif ( iid .eq. IORIBPCK) then
      print*, 'loading orientation bpck file:  ', c255b(:ilenb)
              call pcklof( c255b(:ilenb), handle)

            elseif ( iid .eq. ILEAP) then
              call ldpool( c255b(:ilenb))

            elseif ( iid .eq. IUTC) then
              savutc = c255b(:ilenb)
              savutclen = ilenb

            elseif ( iid .eq. IEARTH) then
              call obody( c255b, savearthid)

            elseif ( iid .eq. ISUN) then
              call obody( c255b, savsunid)

            elseif ( iid .eq. IAST) then
              call obody( c255b, savasterid)

            elseif ( iid .eq. ISC) then
              call obody( c255b, savscid)

            endif
          endif
        endif
        endif
      enddo
99999 continue
      close( lun1)
      call frelun( lun1)
      return
C-----------------------------------------------------------------------
C spice init
C - level 1:  return id's + some data from kernel pool + size of camera
C             points array
C
C boresight is into image; updir is up in image (-lines);
C  sampdir is to the right in the image (+samples)

      entry ospice_init1( scid, asterid, earthid, sunid
     &                       , asterad
     &                       , boresight, panel, updir, sampdir
     &                       , fov, ncampts)

      scid = savscid
      asterid = savasterid
      earthid = savearthid
      sunid = savsunid
C
C get asteroid ellipsoid radii

      call bodvar( asterid, 'RADII', 3, asterad)
C
C get spacecraft camera spec
      call bodvar( scid, 'NUM_CAMPTS', 1, xxx)
      ncampts = int( xxx + 0.1)
      call bodvar( scid, 'FOV', 4, fov)
      call bodvar( scid, 'BORESIGHT', 3, boresight)
      call bodvar( scid, 'PANEL', 3, panel)
      call bodvar( scid, 'SAMPDIR', 3, sampdir)
C
C normalize boresight, find sampdirection component perpendicular to boresight,
C  normalize sampdirection, calculate updirection as cross product of
C  sampdirection and boresight

      call unorm( boresight, boresight, xxx)
      call vperp( sampdir, boresight, sampdir)
      call unorm( sampdir, sampdir, xxx)
      call vcrss( sampdir, boresight, updir)

      return
C
C-----------------------------------------------------------------------
C spice init:
C - level 2:  get camera frame points' definitions

      entry ospice_init2( scid, ncampts, campts)
      i = ncampts * 2
      call bodvar( scid, 'CAMERA_POINTS', i, campts)
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

      call bodn2c( c, id, found)
      if ( .not. found) read( c, '(i)') id
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
