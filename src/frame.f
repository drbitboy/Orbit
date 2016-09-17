      PROGRAM FRAME
C
      DOUBLE PRECISION      DPR
      LOGICAL               RETURN
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM
C
      DOUBLE PRECISION      PCLON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      range
      DOUBLE PRECISION      RJ2000      ( 3,3 )
      DOUBLE PRECISION      RB1950      ( 3,3 )
      DOUBLE PRECISION      Rto      ( 3,3 )
      DOUBLE PRECISION      V           ( 6   )
      DOUBLE PRECISION      VPRIME      ( 6   )
      DOUBLE PRECISION      degpr, tupi, one
      INTEGER               ifrom, ito, J2000, B1950
      CHARACTER*32          OUTCHR
      CHARACTER*16          lclfrm, tofrm
      CHARACTER*80          lineout
      integer i
c
      DATA DEGPR /0D0/
      SAVE      
C           
      CALL CHKIN ( 'FRAME' )

      DEGPR = DPR()
      TUPI = TWOPI()
      one = 1d0
c
      dowhile ( .true.)
c
        ifrom = 0
        ito = 0
        dowhile ( ifrom .eq. 0 .and. ito .eq. 0)
          ifrom = 1
          call irfnam( ifrom, lclfrm)
          lineout = lclfrm
          i = 1
          write( *, '(/a)') ' AVAILABLE REFERENCE FRAMES:'
          dowhile ( lclfrm(1:1) .ne. ' ')
            if ( i .gt. 80) then
              write( *, *) lineout
              i = 1
            endif
            lineout(i:) = lclfrm
            i = i + 20
            ifrom = ifrom + 1
            call irfnam( ifrom, lclfrm)
          enddo
          if ( i .gt. 1) write( *, *) lineout
c
          WRITE(*,'(/a)') 
     &        ' Enter name of system of known vector (e.g. ECLIPB1950)'
     &      // ' (^Z or ^D to exit):'
          READ(*,FMT='(a)',end=99999) lclfrm
          CALL IRFNUM(lclfrm, ifrom)
c
          WRITE(*,'(/a)') 
     &        ' Enter name of system of unknown vector (e.g. J2000)'
     &      // ' (^Z or ^D to exit):'
          READ(*,FMT='(a)',end=99999) tofrm
          CALL IRFNUM(tofrm, ito)
        enddo
c
        CALL IRFNUM('J2000', J2000)
        CALL IRFNUM('B1950', B1950)
c
        CALL IRFROT( ifrom, J2000, RJ2000)
        CALL IRFROT( ifrom, B1950, RB1950)
        CALL IRFROT( ifrom, ito, Rto)
c
        dowhile ( .true.)
          WRITE(*,'(/a)') 
     &      ' Enter RA, DEC (deg) in '//lclfrm // ' (^Z/^D to exit):  '
          READ(*,FMT='(2f30.0)',END=99998,ERR=99998) pclon, lat
          lat = lat / degpr
          pclon = pclon / degpr
          range = 1d0
          CALL RADREC ( one, PCLON, LAT, V)
C
          CALL MXV( rto, v, VPRIME)
          CALL RECRAD ( VPRIME, RANGE, PCLON, LAT )
          CALL TOASTR ( PCLON, LAT, OUTCHR)
          WRITE (*,FMT='(1X,A,2F10.4,2X, ''('', A32, '')'')') 
     &        ' - RA, DEC,'//tofrm(1:8)//'='
     &        , PCLON*DEGPR, LAT*DEGPR, OUTCHR
C
          CALL MXV( rj2000, v, VPRIME)
          CALL RECRAD ( VPRIME, RANGE, PCLON, LAT )
          CALL TOASTR ( PCLON, LAT, OUTCHR)
          WRITE (*,FMT='(1X,A,2F10.4,2X, ''('', A32, '')'')') 
     &        ' - RA, DEC, J2000 = '
     &        , PCLON*DEGPR, LAT*DEGPR, OUTCHR
c
          CALL MXV( rb1950, v, VPRIME)
          CALL RECRAD ( VPRIME, RANGE, PCLON, LAT )
          CALL TOASTR ( PCLON, LAT, OUTCHR)
          WRITE (*,FMT='(1X,A,2F10.4,2X, ''('', A32, '')'')') 
     &        ' - RA, DEC, B1950 = '
     &        , PCLON*DEGPR, LAT*DEGPR, OUTCHR
        enddo
99998   continue
      enddo
99999 continue
      CALL CHKOUT( 'FRAME' )
      END
C***********************************************************************
      SUBROUTINE TOASTR( RA, DEC, OC)
      DOUBLE PRECISION RA, DEC
      CHARACTER*32 OC
C
      INTEGER               HANG, MANG
      DOUBLE PRECISION      SANG
      INTEGER               ARCDEG, ARCMIN
      DOUBLE PRECISION      ARCSEC
C
C                        1         2         3
C               12345678901234567890123456789012
C     RA DEC:   HHh MMm SS.SSSs  sDD MM' SS.SSS"
C
      CALL CONVRT( RA, 'RADIANS', 'HOURANGLE', SANG)
      HANG = INT( SANG)
      SANG = SANG - HANG
      CALL CONVRT( SANG, 'HOURANGLE', 'MINUTEANGLE', SANG)
      MANG = INT( SANG)
      SANG = SANG - MANG
      CALL CONVRT( SANG, 'MINUTEANGLE', 'SECONDANGLE', SANG)
C
      CALL CONVRT( ABS(DEC), 'RADIANS', 'DEGREES', ARCSEC)
      ARCDEG = INT( ARCSEC)
      ARCSEC = ARCSEC - ARCDEG
      IF ( DEC .LT. 0D0) ARCDEG = -ARCDEG
      CALL CONVRT( ARCSEC, 'DEGREES', 'ARCMINUTES', ARCSEC)
      ARCMIN = INT( ARCSEC)
      ARCSEC = ARCSEC - ARCMIN
      CALL CONVRT( ARCSEC, 'ARCMINUTES', 'ARCSECONDS', ARCSEC)
C
      WRITE (OC,FMT='(I2.2,''h '',I2.2,''m '',F6.3,''s''
     &              ,2X,I3,1X,I2.2,'''''' '',F6.3,''"'')') 
     &              HANG, MANG, SANG, ARCDEG, ARCMIN, ARCSEC
      RETURN
      END
