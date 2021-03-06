C$Procedure   SUBPT ( Compute a sub-observer point )
C
      PROGRAM SUBPT
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
C        - The name of a leapseconds kernel file.
C        - The name of a Planetary constants kernel file.
C        - The name of a NAIF SPK Ephemeris file.
C        - NAIF ID for the observing body.
C        - NAIF ID for the target body.
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
      DOUBLE PRECISION      DPR, twopi, halfpi
 
C
C     Parameters
C
      CHARACTER*(*)         TBLFMT
      PARAMETER           ( TBLFMT = '(2f7.2,4g11.3,2x,a20)' )
 
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
      CHARACTER*(LONG)      PCKFIL
      CHARACTER*(MED)       UTCBEG
      CHARACTER*(MED)       UTCEND
      CHARACTER*(MED)       UTCOUT,utcoutc
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      ETBEG
      DOUBLE PRECISION      ETEND
      DOUBLE PRECISION      LAT, dec
      DOUBLE PRECISION      LON, ra
      DOUBLE PRECISION      LT, lt2
      DOUBLE PRECISION      NPOINT   ( DIM      )
      DOUBLE PRECISION      POS      ( DIM      )
      DOUBLE PRECISION      RADII    ( DIM      )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      STATE    ( LDIM     )
      DOUBLE PRECISION      astate    ( LDIM     )
      DOUBLE PRECISION      TIPM     ( DIM, DIM )
 
      INTEGER               HANDLE
      INTEGER               MAXPTS
      INTEGER               NPTS
      INTEGER               NRADII
      INTEGER               OBSRVR
      INTEGER               TARGET, outype
C
      doubleprecision sumpos(dim)
      doubleprecision vnorm
C
      integer lastch
C
      WRITE (*,*)
      WRITE (*,'(a)') 'Suggested leapsecond/PCK''s:'
      WRITE (*,'(a)') 'NEARTOP/cases/leapseconds'
      WRITE (*,'(a)') 'eros.tpc'
      WRITE (*,'(a)') '?'
      dowhile ( .true.)
      WRITE (*,*)
      WRITE (*,*) 'Enter a leapsecond or PCK file(^D/? to end):'
      READ  (*,FMT='(A)',end=99989) leapsc
      IF( leapsc(1:1) .eq. '?' .or. leapsc(2:2) .eq. '?') goto 99989
      CALL ldpool ( leapsc )
      enddo
99989 continue
 
C
C     Load the ephemeris file(s). Note that this is NOT done through
C     the kernel pool, but rather with the SPK load routine SPKLEF.
C
      WRITE (*,*)
      WRITE (*,'(a)') 'Suggested SPK''s:'
      WRITE (*,'(a)') 'de202.bsp'
      WRITE (*,'(a)') 'KERNELTOP/case1/sceph.bsp'
      WRITE (*,'(a)') 'KERNELTOP/case1/asteph.bsp'
      WRITE (*,'(a)') '?'
      dowhile ( .true.)
      WRITE (*,*)
      WRITE (*,*) 'Enter the name of a binary SPK file(^D/? to end):'
      READ  (*,FMT='(A)',end=99990) SPKFIL
      IF( SPKFIL(1:1) .eq. '?' .or. SPKFIL(2:2) .eq. '?') goto 99990
      CALL SPKLEF ( SPKFIL, HANDLE )
      enddo
99990 continue
 
C
C     Load the orientation file(s).
C
      WRITE (*,*)
      WRITE (*,'(a)') 'Suggested binary PCK:'
      WRITE (*,'(a)') 'KERNELTOP/case1/astatt.bpc'
      WRITE (*,'(a)') '?'
      dowhile ( .true.)
      WRITE (*,*)
      WRITE (*,*) 'Enter the name of a binary PCK file(^D/? to end):'
      READ  (*,FMT='(A)',end=99991) PCKFIL
      IF( PCKFIL(1:1) .eq. '?' .or. PCKFIL(2:2) .eq. '?') goto 99991
      CALL PCKLOF ( PCKFIL, HANDLE )
      enddo
99991 continue
 
C
C     Begin the continuation loop.
C
      CONTIN = 'Y'
 
      DO WHILE ( CONTIN .EQ. 'Y' )

         WRITE (*,*)
         WRITE (*,*) 'Suggested observer, target, output type:'
         WRITE (*,*) '-93,2000433,0'
         WRITE (*,*)
         WRITE (*,*) 'Enter the NAIF ID for the observing body'
     &            // ', for the target body, & the output type:'
         READ  (*,*) OBSRVR,target,outype
 
         WRITE (*,*)
         WRITE (*,*) 'Enter the beginning UTC time: 1999-31//'
         READ  (*,FMT='(A)') UTCBEG
 
         WRITE (*,*)
         WRITE (*,*) 'Enter the ending UTC time: 1999-40//'
         READ  (*,FMT='(A)') UTCEND
 
         WRITE (*,*)
         WRITE (*,*) 'Enter the number of points to be calculated: 10'
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
         LINE = 'Target body: #          Observing body: #'
         CALL REPMI ( LINE, '#', TARGET, LINE )
         CALL REPMI ( LINE, '#', OBSRVR, LINE )
         WRITE (*,*) LINE
 
C
C        Now, everything is set up.
C
         EPOCH  = ETBEG
         NPTS   = 1
 
         sumpos(1) = 0d0
         sumpos(2) = 0d0
         sumpos(3) = 0d0

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
 
            CALL SPKEZ  ( 10, EPOCH-lt, FRAME, ABCORR, TARGET,
     .                    ASTATE, LT2 )
 
C
C           Call BODMAT to compute the rotation matrix (TIPM) that
C           transforms inertial coordinates to target body equator
C           and prime meridian (body-fixed) coordinates at EPOCH - LT.
C
            CALL BODMAT ( TARGET, EPOCH-LT, TIPM )
 
            call M2EUL (  tipm,  3, 1, 3, c, b, a)
            a = a - halfpi()
            b = halfpi() - b
            if ( a.lt.0d0) a=a+twopi()
            if ( c.lt.0d0) c=c+twopi()
 
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
            CALL VMINUS ( state, POS )
            CALL VMINUS ( state, state )
            CALL VMINUS ( state(4), state(4) )

            CALL VMINUS ( astate, astate )
            CALL VMINUS ( astate(4), astate(4) )
 
C
C           Call MXV to transform the vector, POS, to body-fixed
C           coordinates by multiplying it by the matrix TIPM.
C
            CALL MXV    ( TIPM, POS, POS )
 
            CALL RECLAT ( state, RADIUS, ra, dec )
            CALL RECLAT ( POS, RADIUS, LON, LAT  )

            sumpos(1) = sumpos(1) + pos(1)
            sumpos(2) = sumpos(2) + pos(2)
            sumpos(3) = sumpos(3) + pos(3)
 
C
C           Multiply LAT and LON by the number of degrees per radian.
C
            LAT = LAT * DPR ()
            LON = LON * DPR ()
            if ( lon .lt. 0d0) lon = lon + 360d0
C
C           Convert the current EPOCH to UTC time for display.
C
            CALL ET2UTC ( EPOCH, 'ISOD', 3, UTCOUT )
            CALL ET2UTC ( EPOCH, 'ISOC', 3, UTCOUTc )
C
C           Display results:
C
            if ( outype .eq. 1) then
            WRITE (*,*)
            WRITE (*,*) '=========================================='
            WRITE (*,fmt='(1x,a,2f15.2,a20)') 
     &      'ET(s past J2k),LT=', epoch, lt, utcout

            WRITE(*,*)
            WRITE(*,fmt='(1x,a)') 
     &      'Spacecraft Position (Target centered):'
            WRITE (*,FMT='(1x,a,4g12.4,2f7.2)')
     &      'J2K XYZ/R/RA/DEC', state(1),state(2),state(3)
     &                        , radius,ra,dec
            WRITE (*,FMT='(1x,a,3g12.4)')
     &      'J2K d(XYZ)/dt   ', state(4),state(5),state(6)

            WRITE(*,*)
            WRITE(*,fmt='(1x,a)') 'Target Ephemeris (Solar centered):'
            WRITE (*,FMT='(1x,a,3g12.4)')
     &      'J2K XYZ/R/RA/DEC', state(1),state(2),state(3)
            WRITE (*,FMT='(1x,a,3g12.4)')
     &      'J2K d(XYZ)/dt   ', state(4),state(5),state(6)

            WRITE(*,*)
            WRITE(*,fmt='(1x,a)') 'Asteroid attitude:'
            WRITE(*,fmt='(1x,a,3g12.4)')
     &      'J2K ALPHA/DELT/W', a*DPR(), b*DPR(), c*DPR()
            WRITE(*,fmt='(1x,a,3g12.4)')
     &      'MTX (1,1)-(3,1):', tipm(1,1), tipm(2,1), tipm(3,1) 
            WRITE(*,fmt='(1x,a,3g12.4)')
     &      '    (1,2)-(3,2):', tipm(1,2), tipm(2,2), tipm(3,2) 
            WRITE(*,fmt='(1x,a,3g12.4)')
     &      '    (1,3)-(3,3):', tipm(1,3), tipm(2,3), tipm(3,3) 


            WRITE(*,*)
            WRITE(*,fmt='(1x,a)') 
     &      'Spacecraft Position (Target centered, Target body-fixed):'
            WRITE (*,FMT='(1x,a,4g12.4,2f7.2)')
     &      'BODY XYZ/R/LA/LO', pos(1),pos(2),pos(3)
     &                        , radius,lat,lon

            else if ( outype .eq. 0) then

            WRITE (*,FMT='(1x,4g12.4,2f7.2,f15.2,a)')
     &      pos(1),pos(2),pos(3), radius,lat,lon,epoch
     &      , ' '//utcout(:lastch(utcout)) // utcoutc(5:11)

            endif

            EPOCH = EPOCH + DELTA
            NPTS = NPTS + 1
 
         END DO

         if ( npts.gt.1) then
           sumpos(1) = sumpos(1) / (npts-1) 
           sumpos(2) = sumpos(2) / (npts-1) 
           sumpos(3) = sumpos(3) / (npts-1) 
         endif
         CALL RECLAT ( sumPOS, RADIUS, LON, LAT  )
         LAT = LAT * DPR ()
         LON = LON * DPR ()
         if ( lon .lt. 0d0) lon = lon + 360d0
         WRITE(*,*)
         WRITE(*,FMT='(1x,4g12.4,2f7.2,a)') 
     &     sumpos,radius,lat,lon
     &     , ' =AVERAGE'

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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      integer function lastch( str)
      character*(*) str
      integer i
C
      i = len(str)
      dowhile ( i.gt.1 .and. str(i:i) .eq. ' ')
        i = i - 1
      enddo
      lastch = i
      return
      end
