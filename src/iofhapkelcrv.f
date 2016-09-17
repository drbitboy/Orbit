c***********************************************************************
      doubleprecision function ioffn( pass, dninp)
c***********************************************************************
c IOFFN - I/F function (or any other function to operate on each pixel in
c         an image - see below) - to be used in conjuction with SPUDFOTE 
c         program which calls this function once for every visible model pixel,
c         plus once before, and once after.  To build, see end of this file.
c
c  arguments:
c    pass (input, int)         - pass #, 1=>init; 2=>do-calc; 3=>final-cleanup
c    dninp (input, dblp)       - pass=2:  current pixel value
c                                pass=1,3:  not used
c    
c  pseudo arguments in common block:
c    nitem (input, int)        - if =3 => only i,e,phi are loaded into common
c                              - if =5 => i,e,phi,lat,lon are loaded into common
c    l,s  (input, int)         - pass=2:  line,sample of current pixel
c                                pass=1:  size (#lines, #samps) of input image
c                                pass=3:  not used
c    incang (input, dblp)      - pass=2:  current pixel incidence angle, radians
c                                pass=1,3:  not used
c    emiang (input, dblp)      - pass=2:  current pixel emmision angle, radians
c                                pass=1,3:  not used
c    phaang (input, dblp)      - pass=2:  current pixel phase angle, radians
c                                pass=1,3:  not used
c    phaang (input, dblp)      - pass=2:  current pixel phase angle, radians
c                                pass=1,3:  not used
c    lat,lon (input, dblp)     - pass=2:  current pixel lat,lon, degrees
c                                pass=1,3:  not used
c
c  Return Value:               - pass=1,3:  not meaningful
c                                pass=2:  dninp corrected to i=e=phi=0
c                              
c***********************************************************************
C
      implicit none
C
      integer pass
      doubleprecision dninp
c
      include 'iepcmn.inc'
C      include 'spud$dir:iepcmn.inc'
c
      real PTERMS(10),w,h,B0,tbar,RESIDL
      real angi,ange,angg
      real reflct
C
C      doubleprecision PTERMS(10),w,h,B0,tbar,RESIDL
C      doubleprecision reflct
C      doubleprecision angi,ange,angg
C
      external reflct
      integer LSTFUN(20)
      INTEGER IRET,J
c
      integer testnan
      doubleprecision badArea
C
      doubleprecision SUMIOFxAREA, rmag
      doubleprecision sumArea, avgIof
      real iof0, iof1
      doubleprecision diof0
C      doubleprecision iof0, iof1
      doubleprecision dpr
      doubleprecision dnscal, phscal, phnom, phase0
      character*14 phnoma
      doubleprecision sunmag, oneau
      doubleprecision dfdnscal
      logical blkout
      INTEGER IOFIL1, FILOPT, STRTLN, TKDFLN, TAKDEF
C
C NUMBER OF LINES IN "FUNCTION IDENTIFICATION" AND "FREE PARAMETERS" HEADERS
C - SET IN THEIR SECTIONS BELOW
C
      INTEGER NLINFI, NLINFP
      DATA NLINFI /0/
      DATA NLINFP /0/
      integer lename, iodat, iostat
      character*255 chname
C
c default constants
      data dfdnscal / 1. /
c
c Apparent V-filter magnitude of the sun as seen from Earth
c 1 AU in km  (too many digits but who cares?)
c   ***N.B. assumes ieprng is in km and that shape model size is 
c           insignificant wrt ieprng
c
      data sunmag / -26.77 /
      data oneau / 1.495985E8 /
c
      save
C
c
c-----------------------------------------------------------------------
c find out which pass we're on
c
      TAKDEF=0
      FILOPT=0
      ioffn = 0
      goto ( 10000, 20000, 30000) pass
c
c
C SPECIAL CASE - IF PASS = 99, THEN WE ARE TO ACT LIKE PASS = 1 AND
C                               SAVE THE INPUTS IN FILE whose name is iepsfn
c
      IF ( PASS .EQ. 99) THEN
        CALL GETLUN( IOFIL1)
        OPEN( IOFIL1, FILE=iepsfn, STATUS='UNKNOWN'
     &    )
Cunixport     &    , CARRIAGECONTROL='LIST')
        REWIND(IOFIL1)
        FILOPT = 1
        GOTO 10000
C
C SPECIAL CASE - if PASS = 98 or 97, then act like PASS=1 but ask no questions,
C                set FILOPT=2 & use the inputs in file whose name is iepsfn
C                - if PASS=98, init & put out any headers
C                - if PASS=97, init only, don't put out any headers
C
      ELSE IF ( PASS .EQ. 98 .OR. PASS .EQ. 97) THEN
        CALL GETLUN( IOFIL1)
        OPEN( IOFIL1, FILE=iepsfn, STATUS='OLD')
        FILOPT = 2
        GOTO 10000
      ELSE
        PRINT*, 'Bad pass (< 1 or > 3) to ioffn'
      ENDIF
      RETURN
c
c-----------------------------------------------------------------------
c pass 1 - initialisation - set iof0 = i/f(i=0,e=0,phi=0) = .5 for lunar-like fn
c            & return dummy value
c
10000 continue
C
C----- GET HAPKE PHOTOMETRIC PARAMETERS FROM USER -----------------------
C
C
      dpr = 45d0 / datan(1d0)
      ioffn = 1.
      dnscal = dfdnscal
      SUMIOFxAREA=0d0
      sumArea = 0d0
      badArea = 0d0
C
C START ALTERNATE FILE OPTION
C   FILOPT = 0 - GET DATA FROM *
C   FILOPT = 1 - GET DATA FROM *, COPY TO FILE SF_FN
C   FILOPT = 2 - GET DATA FROM FILE SF_FN
C
C   START WITH OPTION 0
C     - IF FIRST CHARACTER TO FIRST PROMPT IS >, THEN SWITCH TO OPTION 1
C     - IF FIRST CHARACTER TO FIRST PROMPT IS <, THEN SWITCH TO OPTION 2
C
C   THE INPUT FILE WILL LOOK LIKE THIS:
C   LINE   COL1
C      V      V
C      1      HAPKE.DAT INPUT FILENAME
C      2      LCRVDAT (OR OTHER FILE NAME)
C      3      DN SCALING
C      4      NOMINAL PHASE FOR DN SCALING
C      5      SF_FN_DUMMY_LINE0
C    ...      HEADER LINES OR HEADER COMMANDS (~*), ENDING WITH LONE ","
C    N-1      ,
C      N      SF_FN_DUMMY_LINE1
C
C   THE SF_FN_DUMMY_LINE0 AND _LINE1 LINES ARE MEANT TO BE EDITED OUT,
C   BUT THEY WILL BE IGNORED IN ANY CASE.
C
c get HAPKE.DAT INPUT FILE NAME
c
60000 continue
      if ( PASS.NE.98 .AND.PASS.NE.97) write(*,80002)
80002 format( 
     &/' This SPUDFOTE(tm) function renders a synthetic view of a shape'
     &/' model using the Hapke photometric function i.e. '/
     &/'      I/F = (see Paul Helfenstein)'/
     &/' and sums the I/F''s for the view and writes the result into a'
     &/' file as SUM(I/F) and as Reduced Magnitude'/
     &/' You will be prompted for the following items:'
     &/'    (1)  HAPKE.DAT INPUT FILENAME'
     &/'    (2)  DN scaling factor for rendered output image'
     &/'    (3)  Phase scaling factor'
     &/'    (4)  optional header comments to precede results'
     &/)
c
      IF ( FILOPT .EQ. 0) write(*,80003)
80003 FORMAT(
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc72
     & ' Additional options AT THE FIRST PROMPT: (1) enter "<" to get'
     &/' input for this function from file SF_FN; (2) enter ">" to'
     &/' save fixed inputs into file SF_FN for later use with the "<"'
     &/' option'
     &/)
70002 format( a)
C
      IF ( FILOPT .EQ. 0) then
        write( *, 70002) '$Enter HAPKE.DAT filename: (or < or >):  '
70001   format( a, f10.4, '):  ')
      ELSEIF ( FILOPT .eq. 1) then
        write( *, 70002) '$Enter HAPKE.DAT filename: '
      ENDIF
C
C RECYCLE FOR "<"
C
60001 CONTINUE
C
C FILOPT = 0 OR 1 - READ FROM *
C
      IF ( FILOPT .EQ. 0 .OR. FILOPT .EQ. 1) THEN
        call qrd(  5, lename, chname, iostat)
C
C FILOPT = 0 - TEST FOR > (SWITCH TO FILOPT 1 TO CREATE SF_FN FILE)
C            - TEST FOR < (SWITCH TO FILOPT 2 TO READ FROM SF_FN FILE)
C
        IF ( FILOPT .EQ. 0) THEN
          IF ( lename .EQ. 1 .AND. chname(1:1) .EQ. '>') THEN
            CALL GETLUN( IOFIL1)
            OPEN( IOFIL1, FILE=iepsfn, STATUS='UNKNOWN'
     &        )
Cunixport     &        , CARRIAGECONTROL='LIST')
            FILOPT = 1
          ELSE IF ( lename .EQ. 1 .AND. chname(1:1) .EQ. '<') THEN
            CALL GETLUN( IOFIL1)
            OPEN( IOFIL1, FILE=iepsfn, STATUS='OLD'
     &        )
Cunixport     &        , CARRIAGECONTROL='LIST', READONLY)
            FILOPT = 2
          ENDIF
C
C TEST IF WE CHANGED FILOPT:
C
          IF ( FILOPT .EQ. 1) GOTO 60000
          IF ( FILOPT .EQ. 2) GOTO 60001
        ENDIF
C
C FILOPT = 2 - READ FROM SF_FN
C
      ELSE
        call qrd(  IOFIL1, lename,chname, iostat)
      ENDIF
C
C FILOPT = 1 - WRITE TO SF_FN
C
      IF ( FILOPT .EQ. 1) THEN
        IF ( lename .GT. 0) WRITE( IOFIL1, FMT='(A)') chname(1:lename)
        IF ( lename .EQ. 0) WRITE( IOFIL1, 70003)
70003   format()
      ELSEIF (FILOPT .EQ.2 .AND. PASS.NE.98 .AND.PASS.NE.97) THEN
        IF (lename.GT.0) WRITE( *, 70002) ' '//chname(1:lename)
        IF ( lename .EQ. 0) WRITE( *, 70003)
      ENDIF
C
C CONVERT chname TO HAPKE PARAMETERS
C
      CALL INPLIS(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL
     &                ,chname(1:lename),IRET)
      if ( PASS.NE.98 .AND.PASS.NE.97) then
        CALL OUTLIS(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL
     &                ,'SCREEN',IRET)
      endif
C
      ANGI=0.0
      ANGE=0.0
      ANGG=0.0
      iof0 = REFLCT(ANGI,ANGE,ANGG,LSTFUN,W,H,B0,TBAR,PTERMS)
      if ( PASS.NE.98 .AND.PASS.NE.97) then
        PRINT 1
   1    FORMAT(1X,/)
        PRINT *,'>>>----- MODEL NORMAL ALBEDO ---->',iof0
        PRINT 1
      endif
C
C LIGHTCURVE OUTPUT FILE NAME
C  - TAKE THE DEFAULT FOR PASS = 99 ( => FILOPT = 1)
C
      IF ( PASS .NE. 99) CALL GETLUN( IODAT)
60010 continue
      IF ( PASS .EQ. 99) THEN
        lename = 0
      ELSE
       if ( PASS.NE.98 .AND.PASS.NE.97) WRITE( *, 70002) 
     &    ' '
     &   ,'$Enter lightcurve file name (deflt=LCRVDAT):  '
C
        IF ( TAKDEF .NE. 0) THEN
          lename = 0
        ELSEIF ( FILOPT .EQ. 0 .OR. FILOPT .EQ. 1) THEN
          call qrd(  5, lename, chname, iostat)
          IF ( IOSTAT .EQ. -1) lename = 0
        ELSE
          call qrd(  IOFIL1, lename, chname, iostat)
        ENDIF
C
        IF ( lename .EQ. 0) THEN
          call iepcpynam( ieplcf, chname, lename)
        ENDIF
c
c open file old/append first, then new if it fails
c
        open( iodat, file=chname(1:lename),status='OLD',iostat=iostat
     &        , access='APPEND')
Cunixport     &        , carriagecontrol='LIST', access='APPEND')
c
        if ( iostat .ne. 0) then
          open( iodat, file=chname(1:lename), status='NEW'
     &        , iostat=iostat)
Cunixport     &        , carriagecontrol='LIST', iostat=iostat)
        endif
c
        if ( iostat .ne. 0) then
          write( *, *) 
     &      'Error opening file ' // chname(1:lename) // '; try again'
          goto 60010
        endif
      ENDIF
C
      IF ( FILOPT .EQ. 1) THEN
        IF ( lename .GT. 0) WRITE( IOFIL1, 70002) chname(1:lename)
        IF ( lename .EQ. 0) WRITE( IOFIL1, 70003)
      ELSEIF (FILOPT .EQ.2 .AND. PASS.NE.98 .AND.PASS.NE.97) THEN
        IF ( lename .GT. 0) WRITE( *, 70002) ' '//chname(1:lename)
        IF ( lename .EQ. 0) WRITE( *, 70003)
      ENDIF
c
c dn scaling for rendered output image
c
60020 continue
      if ( PASS.NE.98 .AND.PASS.NE.97) write( *, 70002) 
     &  ' '
     & ,' You may be using this function to generate rendered images.'
     & ,' Because of low albedo, any rendered image may be dark and'
     & ,' not use much dynamic range, so you may wish to'
     & ,' artificially brighten the rendered image ONLY WITHOUT'
     & ,' affecting the reduced magnitude and SUM(I/F) results.'
     & ,' The I/F values calculated by this function will be scaled'
     & ,' by the factor you enter now to determine output DN.'
     & ,' If you are not generating rendered images, just hit Return.'
     & ,' '
     & ,' Enter scaling factor to be used to determine output DN'
      if ( PASS.NE.98 .AND.PASS.NE.97)write( *, 70001)
     &  '$(deflt=', dnscal
C
      IF ( TAKDEF .NE. 0) THEN
        lename = 0
      ELSEIF ( FILOPT .EQ. 0 .OR. FILOPT .EQ. 1) THEN
        call qrd(  5, lename, chname, iostat)
        IF ( IOSTAT .EQ. -1) lename = 0
      ELSE
        call qrd(  IOFIL1, lename, chname, iostat)
      ENDIF
C
      if ( lename .gt. 0) then
        read( chname( 1:lename), fmt='(f20.0)', err=60020) dnscal
      endif
C
      IF ( FILOPT .EQ. 1) THEN
        IF ( lename .GT. 0) WRITE( IOFIL1, 70002) chname(1:lename)
        IF ( lename .EQ. 0) WRITE( IOFIL1, 70003)
      ELSEIF (FILOPT .EQ.2 .AND. PASS.NE.98 .AND.PASS.NE.97) THEN
        IF (lename.GT.0) WRITE( *, 70002) ' '//chname(1:lename)
        IF ( lename .EQ. 0) WRITE( *, 70003)
      ENDIF
C
C PHASE SCALING
C
60025 continue
      if ( PASS.NE.98 .AND.PASS.NE.97) write( *, 70002) 
     &  ' '
     & ,' You may be using this function to generate multiple rendered'
     & ,' images at varying phase angles.  If that is the case, you'
     & ,' may want to vary the above scale factor for rendering'
     & ,' publishable images so the high phase angle images are not'
     & ,' too dark compared to the low phase angle images'
     & ,' This ONLY affects the rendered images and does NOT affect'
     & ,' the Reduced Magnitude or SUM(I/F) results.'
     & ,' Enter the phase angle in degrees assumed for the above'
     & ,'$scaling factor (hit return for default=no phase effect):  '
C
      IF ( TAKDEF .NE. 0) THEN
        lename = 0
      ELSEIF ( FILOPT .EQ. 0 .OR. FILOPT .EQ. 1) THEN
        call qrd(  5, lename, chname, iostat)
        IF ( IOSTAT .EQ. -1) lename = 0
      ELSE
        call qrd(  IOFIL1, lename, chname, iostat)
      ENDIF
C
      IF ( lename .GT. 0) THEN
        READ( chname( 1:lename), FMT='(F15.0)', ERR=60025) PHNOM
        IF ( PASS .EQ. 1) THEN
C
C Get current phase angle wrt body center - a^2 + b^2 - 2abcos(phase) = c^2
C  - use unit vectors toward observer and sun so a=b=1, c^2 is distance^2 
C      between ends of unit vectors, so cos(phase0) = 1 - (c^2)/2
C
          PHASE0 = (COS(IEPCLA/DPR)*COS(IEPCLO/DPR)
     &              - COS(IEPOLA/DPR)*COS(IEPOLO/DPR))**2
     &           + (COS(IEPCLA/DPR)*SIN(IEPCLO/DPR)
     &              - COS(IEPOLA/DPR)*SIN(IEPOLO/DPR))**2
     &           + (SIN(IEPCLA/DPR)
     &              - SIN(IEPOLA/DPR))**2
          PHASE0 = MIN(1.,MAX(-1., 1.-PHASE0/2))
          PHASE0 = ACOS( PHASE0) * DPR
C
C Calculate brightness scaling factor from current to nominal phase
C  - use specular point for albedo
C
          ANGI=PHNOM/2
          ANGE=ANGI
          ANGG=PHNOM
          IOF1 = REFLCT(ANGI,ANGE,ANGG,LSTFUN,W,H,B0,TBAR,PTERMS)
          ANGI=PHASE0/2
          ANGE=ANGI
          ANGG=PHASE0
          PHSCAL= IOF1/REFLCT(ANGI,ANGE,ANGG,LSTFUN,W,H,B0,TBAR,PTERMS)
          PHNOMA = chname(1:lename) // ' DEG'
        ENDIF
      ELSE
        PHNOMA = ' (none)       '
        PHSCAL = 1.0
      ENDIF
C
      IF ( FILOPT .EQ. 1) THEN
        IF ( lename .GT. 0) WRITE( IOFIL1, 70002) chname(1:lename)
        IF ( lename .EQ. 0) WRITE( IOFIL1, 70003)
      ELSEIF (FILOPT .EQ.2 .AND. PASS.NE.98 .AND.PASS.NE.97) THEN
        IF (lename.GT.0) WRITE( *, 70002) ' '//chname(1:lename)
        IF ( lename .EQ. 0) WRITE( *, 70003)
      ENDIF
C
c optional header lines
c
      if ( PASS.NE.98 .AND.PASS.NE.97) write( *, 70002) 
     &  ' '
     & ,' Enter any header lines you want to precede this run''s '
     & ,' results in the lightcurve file.  Type them in just as you'
     & ,' want them to look in the file.  Special header options:'
c         1234567890123456789012345678901234567890
     & ,' press ^Z or enter "," for no header lines;'
     & //' enter ~H or ~h for column headers;'
     & ,' enter ~I or ~i for this function''s identification line(s);'
     & ,' enter ~F or ~f for the free parameters;'
     & //' enter ~P or ~p for the pointing info;'
     & ,' enter ~D or ~d for a standard set of header lines:'
C
C FILOPT = 0 - LOOP READ *; GOTO NEXT SECTION FOR ^Z (EOF)
C FILOPT = 1 - INSERT "SF_FN_DUMMY_LINE0";
C              LOOP READ *; INSERT "," FOR ^Z (EOF);
C              INSERT "SF_FN_DUMMY_LINE1";
C FILOPT = 2 - LOOP READ IOFIL1, GOTO NEXT SECTION FOR ","
C
      IF ( FILOPT .EQ. 1) WRITE( IOFIL1, 70002) 'SF_FN_DUMMY_LINE0'
C
C PREPARE TO COUNT HEADER LINES FOR INFORMATIONAL MESSAGE LATER
C
      STRTLN = 1
      TKDFLN = STRTLN - 1
C
C     skip headers for PASS=97
C
      if ( PASS.EQ.97) goto 60016   
C
C LOOP FROM BELOW
C
60015 continue
C
C INCREMENT 1ST DATA LINE NUMBER
C
      STRTLN = STRTLN + 1
C
      IF ( TAKDEF .NE. 0) THEN
C
C STANDARD HEADER:
C
C - FUNCTION IDENTIFICATION FOLLOWED BY BLANK LINE
C
        IF ( (STRTLN-TKDFLN) .LT. 3) THEN
          chname = '~I'
          lename = 2
C
C - FUNCTION FREE PARAMETERS FOLLOWED BY BLANK LINE
C
        ELSEIF ( STRTLN-TKDFLN .EQ. (3+NLINFI)) THEN
          chname = '~F'
          lename = 2
C
C - POINTING FOLLOWED BY BLANK LINE
C
        ELSEIF ( STRTLN-TKDFLN .EQ. (4+NLINFI+NLINFP)) THEN
          chname = '~P'
          lename = 2
C
C - COLUMN HEADERS FOLLOWED BY BLANK LINE
C
        ELSEIF ( STRTLN-TKDFLN .EQ. (6+NLINFI+NLINFP)) THEN
          chname = '~H'
          lename = 2
C
C BLANK LINE AFTER EVERY HEADER TYPE
C
        ELSEIF ( STRTLN-TKDFLN .LT. (8+NLINFI+NLINFP)) THEN
          lename = 0
C
C END OF LINES
C
        ELSE
          chname = ','
          lename = 1
        ENDIF
      ELSEIF ( FILOPT .EQ. 0) THEN
        call qrd(  5, lename, chname, iostat)
      ELSE IF ( FILOPT .EQ. 0 .OR. FILOPT .EQ. 1) THEN
        call qrd(  5, lename, chname, iostat)
C
C CONVERT ^Z TO "," IN SF_FN FILE
C  - NOT REALLY NECESSARY SINCE THERE ARE NO FURTHER INPUTS IN THIS PASS
C
        IF ( IOSTAT .EQ. -1) THEN
          lename = 1
          chname(1:1) = ','
        ENDIF
C
      ELSE
        call qrd(  IOFIL1, lename, chname, iostat)
      ENDIF
C
      if ( lename .gt. 0) then
C
C TAKE SPECIAL STEPS IF WE READ LINES SF_FN_DUMMY_LINE0 OR _LINE1
C
        if ( lename .eq. 17 .and. chname(1:17) .eq. 'SF_FN_DUMMY_LINE0')
     &     GOTO 60015

        IF ( TAKDEF .EQ. 0 .AND. lename.EQ.2 .AND. 
     &       ( chname(1:2).EQ. '~D' .OR.
     &         chname(1:2).EQ. '~d')) THEN
          STRTLN = STRTLN - 1
          TKDFLN = STRTLN - 1
          TAKDEF = 1
          GOTO 60015
        ENDIF
C
        if ( lename .eq. 17 .and. chname(1:17) .eq. 'SF_FN_DUMMY_LINE1')
     &     GOTO 60016
C
C TEST FOR END-OF-HEADERS
C
        if ( lename .eq. 1 .and. chname(1:1) .eq. ',') THEN
          IF ( FILOPT .EQ. 1) THEN
            WRITE( IOFIL1, 70002) chname(1:lename)
          ELSEIF (FILOPT .EQ.2 .AND. PASS.NE.98 .AND.PASS.NE.97) THEN
            WRITE( *, 70002) ' '//chname(1:lename)
          ENDIF
          goto 60016
        ENDIF
C
C COLUMN HEADERS
C
        if ( lename .eq. 2 .and. 
     &       (chname(1:2) .eq. '~h' .or. chname(1:2) .eq. '~H')) then
          IF ( PASS .NE. 99) WRITE( IODAT, 90001) 
C
C POINTING INFO
C
        elseif ( lename .eq. 2 .and. 
     &       (chname(1:2) .eq. '~p' .or. chname(1:2) .eq. '~P')) then
          IF ( PASS .NE. 99) WRITE( IODAT, 01431) 
     &         IEPPIC, IEPCLA, IEPCLO, IEPOLA, IEPOLO
     &       , IEPRNG, IEPNOR, IEPSMP, IEPLIN, IEPCAM
01431     format(i9,4f7.2,f10.0,f7.2,2f8.1,i3)
C
C Function-specific identification.
C   - DON'T FORGET TO SET NLINFI TO THE NUMBER OF LINES IN THIS HEADER
C
        elseif ( lename .eq. 2 .and. 
     &       (chname(1:2) .eq. '~i' .or. chname(1:2) .eq. '~I')) then
          NLINFI = 1
          IF ( PASS .NE. 99) WRITE( IODAT, 90002) 
90002     format( 'IOFHAPKELCRV:  I/F = (SEE PAUL HELFENSTEIN)')
          STRTLN = STRTLN + (NLINFI-1)
C
C Function-specific parameters.
C   - DON'T FORGET TO SET NLINFP TO THE NUMBER OF LINES
C
        elseif ( lename .eq. 2 .and. 
     &       (chname(1:2) .eq. '~f' .or. chname(1:2) .eq. '~F')) then
          NLINFP = 2
          IF ( PASS .NE. 99) WRITE( IODAT, 90003) IOF0, DNSCAL, PHNOM
90003     format( 'HAPKE PARAMETERS:   MODEL NORMAL ALBEDO=', f9.4
     &        , ';'
     &        / '      DN scaling =', 1pg12.5
     &        , '; Nominal Phase for scaling =', a
     &)
C
          NLINFP = NLINFP + 2
          IF ( PASS .NE. 99) WRITE(IODAT,90004)(LSTFUN(J),J=1,20)
90004     FORMAT(2X,'LSTFUN index: ',20(I2),/)
C
          NLINFP = NLINFP + 2
          IF ( PASS .NE. 99) WRITE(IODAT,90005)W,TBAR
90005     FORMAT(2X,'Single-Scattering Albedo (w):              '
     &          ,F12.8,/,2X
     &          ,'Average Macroscopic Roughness (theta-bar): ',F12.8)
C
          NLINFP = NLINFP + 1
          IF ( PASS .NE. 99) WRITE(IODAT,90006)H
90006     FORMAT(2X,'Regolith Compaction Parameter (h):         '
     &          , F8.4)
C
          IF(LSTFUN(3).EQ.1) THEN
            NLINFP = NLINFP + 1
            IF ( PASS .NE. 99) WRITE(IODAT,90007)B0
90007       FORMAT(2X,'Opposition Surge Amplitude Parameter (B0): '
     &            ,F8.4)
          ENDIF
C
          IF ( LSTFUN(4) .EQ. 1) THEN
            NLINFP = NLINFP + 1 + LSTFUN(5)
            IF ( PASS .NE. 99) WRITE(IODAT,90008)
90008       FORMAT(2X,'Particle Phase Function Terms:  Legendre '
     &            ,'Polynomial')
            IF ( PASS .NE. 99) 
     &      WRITE(IODAT,90009)(J,PTERMS(J),J=1,LSTFUN(5))
90009       FORMAT(24X,'P(',I2,') = ',F8.4)
C
          ELSEIF ( LSTFUN(4) .EQ. 2) THEN
            NLINFP = NLINFP + 1
            IF ( PASS .NE. 99) WRITE(IODAT,90010)PTERMS(1)
90010       FORMAT(2X,'Particle Phase Function:  '
     &               ,'Heyney-Greenstein g = ', f8.4)
C
          ELSEIF ( LSTFUN(4) .EQ. 3) THEN
            NLINFP = NLINFP + 1
            IF ( PASS .NE. 99) WRITE(IODAT,90011)(PTERMS(J),J=1,3)
90011       FORMAT(2X,'Particle Phase Function:  Heyney-Greenstein ',
     &           /,5X,'g1 = ',F8.4,4X,'g2 = ',F8.4,4X
     &            ,'Partition Coefficient:',F8.4)
C
          ELSEIF ( LSTFUN(4) .EQ. 4) THEN
            NLINFP = NLINFP + 1
            IF ( PASS .NE. 99) WRITE(IODAT,90012)PTERMS(1)
90012       FORMAT(2X,'Value of Particle Phase Function P(g): ',6X,
     &               F8.4)
          ENDIF
          STRTLN = STRTLN + (NLINFP-1)
C
        else
          IF ( PASS .NE. 99) WRITE( IODAT, 70002) chname(1:lename)
        ENDIF
C
        IF ( FILOPT .EQ. 1) THEN
          WRITE( IOFIL1, 70002) chname(1:lename)
        ELSEIF (FILOPT .EQ.2 .AND. PASS.NE.98 .AND.PASS.NE.97) THEN
          WRITE(*, 70002) ' '//chname(1:lename)
        ENDIF
c
c blank line for lename=0?  Well, the customer is always right!
c
      else
        IF ( PASS .NE. 99) WRITE( IODAT, 70003)
C
        IF ( FILOPT .EQ. 1) THEN
          WRITE( IOFIL1, 70003)
        ELSEIF (FILOPT .EQ.2 .AND. PASS.NE.98 .AND.PASS.NE.97) THEN
          WRITE( *, 70003)
        ENDIF
      endif
C
      if ( PASS .NE. 98 .AND. PASS .NE.97) write( *, 70002) 
     &  ' Type in next header line or one of the special options:'
     & ,' press ^Z or enter "," for no more header lines;'
     & ,' enter ~H or ~h for column headers;'
     & ,' enter ~I or ~i for this function''s identification line(s);'
     & ,' enter ~F or ~f for the free parameters;'
     & //' enter ~P or ~p for the pointing info;'
     & ,' enter ~D or ~d for a standard set of header lines:'
      goto 60015
C
C LOOP EXIT TARGET AFTER ^Z OR "," OR SF_FN_DUMMY_LINE1
C
60016 continue
C
C REDUCE 1ST DATA LINE NUMBER BY ONE FOR END-OF-HEADERS PASS THROUGH LOOP
C
      STRTLN = STRTLN - 1
C
C THE PURPOSE OF THE "SF_FN_DUMMY_LINEn" LINES IS TO SIMPLIFY
C   AUTOMATIC EDITING OF THE SF_FN FILE.  THIS NEXT LINE, IF LEFT IN,
C   SHOULD BE IGNORED.
C
      IF ( FILOPT .NE. 0) THEN
        IF ( FILOPT .EQ. 1) THEN
          WRITE( IOFIL1, 70002) 'SF_FN_DUMMY_LINE1'
          WRITE( *, 70004) STRTLN-1, STRTLN
70004     FORMAT( 1X, 
     &        ' You specified ', I3, ' header lines.  In Mongo use'
     &      / '   * LINES ', i3, ' 20000'
     &      / ' to skip the header in the output data file')
        ENDIF
C
        CLOSE( IOFIL1)
        CALL FRELUN( IOFIL1)
      endif
C
      return
c
c-----------------------------------------------------------------------
c pass 2 - active code
c
20000 continue
c
      ANGI = dpr*incang
      ANGE = dpr*emiang
      ANGG = dpr*phaang
C
C      IOF1=REFLCT(ANGI,ANGE,ANGG,LSTFUN,W,H,B0,TBAR,PTERMS)
C
       Iof1 = w * cos(incang) / 2
c
c sum area and (i/f * Area)
c
      if ( testnan(ieprpp).ne.0 .and. testnan(iof1*ieprpp).ne.0) then
        sumArea = sumArea + ieprpp
        SUMIOFxAREA = SUMIOFxAREA + IOF1 * ieprpp
      else
        print'(1x, ''ieprpp ok? '', L1, ''(iof1*ieprpp) ok? '', L1)'
     &  ,    testnan(ieprpp).ne.0 , testnan(iof1*ieprpp).ne.0
        badArea = badArea + ieprpp
      endif
C
      IOFFN = DNSCAL * IOF1 * DNINP
      IF ( PHSCAL .NE. 1.0) IOFFN = PHSCAL * IOFFN
C
      return
c-----------------------------------------------------------------------
c pass 3 - closing - return dummy value
c
30000 continue
      ioffn = 1.
c
c calculate Vred, reduced magnitude
c
C
C Old equation, assumes constant pixel size:
C
C      IOFFN = ((IEPRPP * IEPRNG / ONEAU)**2) * SUMIOF   * DPR/180
C
C                radians  range    1 AU         sum(I/F)    PI
C                -------   km       km
C                 pixel
C
C putting (IEPRPP*IEPRNG) inside sum:
C
C      IOFFN = sumOverPixels( iofOfEachPixel * (radian/pixel * range)^2)
C                / (PI * oneAU^2)
C
C restated for plates, varying plate size (plates are analogs of pixels):
C
C      IOFFN  = sumOverPlates( iofOfEachPlate * projectedAreaOfEachPlat)
C                 / (PI * oneAU^2)
C
C     SUMIOFxAREA is sum of product (projected plate area) x (plate I/F)
C
      IOFFN = SUMIOFxAREA * (DPR/180) / (ONEAU**2)
C
      IF ( IOFFN .GT. 0.) THEN
        RMAG = SUNMAG - 2.5 * LOG10(IOFFN)
      ELSE
        RMAG = -99.0
        WRITE(*,70002) 
     &    '***REDUCED I/F <= 0:  MAGNITUDE NOT CALCULATED, SET TO -99'
      ENDIF
      IOFFN = 1.0
c
c write out picno, reduced magnitude, sums, average
c
      if ( sumArea .gt. 0d0) then
        avgIof = SUMIOFxAREA/sumArea
      else
        avgIof = 0d0
      endif
C
      write( iodat, 90020) 
     &             ieppic,    rmag, SUMIOFxAREA,  SUMAREA
     &      ,      avgIof,         iepet,    iepmet
C
90020 format(         i10,    f9.3,       g13.5,     f11.4
     &      ,       f10.5,      f12.0,         f15.0)
C
90001 format( ' lon|picno   Vred__  S(I/F*Area)    Area___'
     &      , 'AverageI/F  sPastJ2000        SCLK___')
C
C     &      ,      avgIof,         iepet,  badArea
C     &      ,        g10.5,       f14.1,        f7.3)
C     &      , '  Average_I/F   sPastJ2000    Bad_Area_')
C
      close( iodat)
      call frelun( iodat)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc72
C perform equivalent of VMS FORTRAN
C     READ( IOUNIT, '(Q,A)', IOSTAT=IOSTAT) LENSTR, STR
C
      subroutine qrd( iounit, lenstr, str, iostat)
      implicit none
      integer iounit, lenstr, iostat
      character*(*) str
C
C      lenstr = 1
C      dowhile ( lenstr .le. len(str))
C        str(lenstr:) = '                                        '
C        lenstr = lenstr + 40
C      enddo
C
      read( iounit, '(a)', iostat=iostat) str
C
      lenstr = len(str)
      dowhile ( .true.)
        if (str(lenstr:lenstr) .gt. ' ') goto 99999
        lenstr = lenstr - 1
        if ( lenstr .lt. 1) goto 99999
      enddo
99999 continue
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC72
C copy non-spaces from input to output string, count them
C
      subroutine iepcpynam( inpstr, outstr, count)
      implicit none
      character*(*) inpstr, outstr
      integer count
C
      count = 0
      dowhile ( count .lt. len(inpstr) .and. count .lt. len(outstr))
        if ( inpstr(count+1:count+1) .le. ' ') return
        count = count + 1
        outstr(count:count) = inpstr(count:count)
      enddo
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC72
C copy double & int arrays, & strings into IEPCMN common block,
C
      subroutine iepcmnsetall( dblArr, intArr, lensfn, lentot, c12)
      implicit none
      include 'iepcmn.inc'
C
      doubleprecision dblArr(IEPDBLCOUNT)
      integer intArr(IEPINTCOUNT)
      integer lensfn, lentot
      character*1 c12(lentot)
C
      integer i,j
C-----
C     copy doubles & integers
C
      do i=1,IEPDBLCOUNT
        iepdbl(i) = dblArr(i)
      enddo
      do i=1,IEPINTCOUNT
        iepint(i) = intArr(i)
      enddo
C
C     copy two strings from c12
C     - spudfote input file name
C
      i = 0
      iepsfn = ' '
      ieplcf = ' '
      dowhile ( i .lt. lensfn 
     &    .and. i .lt. lentot
     &    .and. i .lt. len(iepsfn))
        i = i + 1
        iepsfn(i:i) = c12(i)
      enddo
C
C     - lightcurve output file name
C
      i = lensfn
      j = 0
      dowhile ( i .lt. lentot
     &    .and. j .lt. len(ieplcf))
        i = i + 1
        j = j + 1
        ieplcf(j:j) = c12(i)
      enddo
C
      return
      end
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:: SHUFLE -  LOADS AND UNLOADS PHOTOMETRIC PARAMETERS INTO AN INTER- ::
C:: ROUTINE TRANSFER ARRAY, XPAR.  PHOTOMETRIC PARAMETERS ARE W,H,TBAR::
C:: B0,PTERMS.  LOAD IS A FLAG WHICH INDICATES WHETHER PARAMETERS ARE ::
C:: TO BE LOADED INTO XPAR (LOAD=1) OR UNLOADED FROM XPAR (LOAD=2)    ::
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        SUBROUTINE SHUFLE(LOAD,XPAR,W,H,TBAR,B0,PTERMS)
C
        DIMENSION XPAR(14),PTERMS(10)
C
C-------- IF LOAD=1, DUMP PARAMETERS INTO XPAR
C            LOAD=2, DUMP XPAR=INTO PARAMETERS
C
       IF(LOAD.NE.1)GO TO 200
          XPAR(1) = W
          XPAR(2) = H
          XPAR(3) = TBAR
          XPAR(4) = B0
          DO 100 J=1,10
             XPAR(J+4) = PTERMS(J)
  100     CONTINUE
          RETURN
C
  200     W = XPAR(1)
          H = XPAR(2)
          TBAR = XPAR(3)
          B0 = XPAR(4)
          DO 300 J=1,10
             PTERMS(J) = XPAR(J+4)
  300     CONTINUE
          RETURN
          END
C----------------------------------------------------------------------------
C-- BGPGINVS - INPUT: HAPKE PARAMETERS (W,H,OPMAG,TBAR,PTERMS)           ----
C--                   REFLECTANCE I/F (BRITE)                            ----
C--                   HAPKE EQUATION VERSION FLAGS (LSTFUN)              ----
C--                                                                      ----
C--  OUTPUT:  INVERSE VALUE OF B(G)           (BGVAL)                    ----
C--           INVERSE VALUE OF P(G)           (PGVAL)                    ----
C--           INVERSE VALUE OF (1+B(G))*P(G)  (BGPGVAL)                  ----
C----------------------------------------------------------------------------
C
        SUBROUTINE BGPGINVS(I,E,G,BRITE,LSTFUN,W,H,OPMAG,TBAR,PTERMS,
     *                      BGVAL,PGVAL,BGPGVAL)
C
	REAL MU0,MU01,MU02,MU1,MU11,MU12,I,I0
        DIMENSION PTERMS(10),LSTFUN(20)
C
	COMMON /HAPKE/BGFNCT,PGFNCT,HFNCTI,HFNCTE,HAPCOF,SFNCT,
     *  MU0,MU01,MU02,MU1,MU11,MU12,COSI,COSE,COTI,COTE,COTT,TANT,
     *  BETA,PSI,COSPSI,SINPS2,F,Y,EE1,EE2,EI1,EI2,XI1,XI2,XI3
C
C
        RTEST = REFLCT(I,E,G,LSTFUN,W,H,OPMAG,TBAR,PTERMS)
C
         A1 = BRITE/(HAPCOF*SFNCT)
         A2 = 1.0-HFNCTI*HFNCTE
         A3 = PGFNCT
         A4 = 1.0+BGFNCT
C
        BGPGVAL = A1+A2
        BGVAL = BGPGVAL/A3 - 1.0
        PGVAL = BGPGVAL/A4
C
	RETURN
	END
C-------------------------------------------------------------------
C-- LUMATINV - DETERMINES THE INVERSE OF MATRIX R BY LU MATRIX   ---
C--            DECOMPOSITON AND GAUSSIAN ELIMINATION             ---
C-------------------------------------------------------------------
C
      SUBROUTINE LUMATINV(NORDER,R)
      DIMENSION R(14,14),AMAT(14,14),BMAT(14,14),AINV(14,14)
     *         ,BINV(14,14),KPIVOT(14)
      DOUBLE PRECISION R,AMAT,BMAT,AINV,BINV,AMAX,TEMP,AMULT,
     *                 SUMB,SUMA
C
C
C-------------------------------------------------------------------
C-------- COPY ORIGINAL MATRIX INTO UPPER-TRIANGULAR BUFFER  -------
C-------------------------------------------------------------------
C
        DO 100 IROW=1,NORDER
        DO 100 ICOL=1,NORDER
           AMAT(IROW,ICOL) = R(IROW,ICOL)
           BMAT(IROW,ICOL)=0.0
  100   CONTINUE
C
C-------------------------------------------------------------------
C--- PERFORM GAUSSIAN ELIMINATION ON UPPER-TRIANGULAR MATRIX AND ---
C--- SAVE MULTIPLIERS IN LOWER TRIANGULAR MATRIX                 ---
C-------------------------------------------------------------------
C
       DO 300 I = 1,NORDER-1
C
C-------------------------- FIND PIVOT POINT AND SWAP ROWS
C
       AMAX = DABS(AMAT(I,I))
       KPIVOT(I) = I
       DO 130 J = I+1,NORDER
          IF(AMAX.GT.DABS(AMAT(J,I)))GO TO 130
             KPIVOT(I) = J
             AMAX = DABS(AMAT(J,I))
  130  CONTINUE
C
       DO 140 K=1,NORDER
C
          TEMP = AMAT(I,K)
          AMAT(I,K) = AMAT(KPIVOT(I),K)
          AMAT(KPIVOT(I),K) = TEMP
C
          TEMP = BMAT(I+1,K)
          BMAT(I,K) = BMAT(KPIVOT(I),K)
          BMAT(KPIVOT(I),K) = TEMP
C
  140  CONTINUE
C
C---------------------------------------- CHECK FOR ZERO DIAGONAL
C
       IF(AMAT(I,I).NE.0.0)GO TO 201
       print*,'ZERO IN DIAGONAL TERM DURING ELIMINATION'
               DO 202 IRW=1,NORDER
               WRITE(5,203)(R(IRW,ICL),ICL=1,NORDER)
               DO 204 ICL=1,NORDER
  204          R(IRW,ICL)=0.0
  203  FORMAT(1X,7(F10.5,1X))
Cunixport  203  FORMAT(1X,<NORDER>(F10.5,1X))
  202  CONTINUE
       RETURN
C
C----------------------------------------- CONTINUE WITH ELIMINATION
C
  201  DO 200 J = I+1,NORDER
C
       AMULT = AMAT(J,I)/AMAT(I,I)
       BMAT(J,I) = AMULT
C
       DO 150 K = I,NORDER
C
         AMAT(J,K) = AMAT(J,K) - AMULT*AMAT(I,K)
C
  150  CONTINUE
  200  CONTINUE
  300  CONTINUE
C
       DO 310 K=1,NORDER
  310  BMAT(K,K) = 1.0
C
C------------------------------------------------------------------------
C----- COMPUTE INVERSE OF L AND U MATRICES  -----------------------------
C------------------------------------------------------------------------
C
C----------------------------- L MATRIX
C
      DO 800 J=1,NORDER
      DO 750 I=J,NORDER
C
            SUMB = 0.0
         DO 700 K = J,I-1
            SUMB = SUMB + BMAT(I,K)*BINV(K,J)
  700    CONTINUE
C
         BINV(I,J) = (CRNDEL(I,J) - SUMB)/BMAT(I,I)
C
  750 CONTINUE
  800 CONTINUE
C
C----------------------------- U MATRIX
C         
      DO 900 J = 1,NORDER
      DO 850 I = J,1,-1
C
         SUMA = 0.0
         DO 825 K=I+1,J
         SUMA = SUMA + AMAT(I,K)*AINV(K,J)
  825    CONTINUE
C
         AINV(I,J) = (CRNDEL(I,J)-SUMA)/AMAT(I,I)
C
  850 CONTINUE
  900 CONTINUE
C
C----------------------------------------------------------------
C---- MULTIPLY OUT [INVERSE R] = [INVERSE U]*[INVERSE L]      ---
C----------------------------------------------------------------
C
       DO 1000 IROW = 1,NORDER
       DO 990 ICOL = 1,NORDER
C
          R(IROW,ICOL) = 0.0
C
       DO 970 K=1,NORDER
C
       R(IROW,ICOL) = R(IROW,ICOL) + 
     *                               AINV(IROW,K)*BINV(K,ICOL)
C
  970  CONTINUE
  990  CONTINUE
 1000  CONTINUE
C
C-----------------------------------------------------------------------
C---- RESTORE ORDERING BY BACK-PIVOTING COLUMNS OF INVERSE MATRIX   ----
C-----------------------------------------------------------------------
C            
       DO 2000 K=NORDER-1,1,-1
C
       DO 1500 IROW=1,NORDER
C
          TEMP = R(IROW,K)
          R(IROW,K) = R(IROW,KPIVOT(K))
          R(IROW,KPIVOT(K)) = TEMP
C
 1500  CONTINUE
 2000  CONTINUE
       RETURN
       END
       FUNCTION CRNDEL(I,J)
       DOUBLE PRECISION CRNDEL
       CRNDEL=0.0
       IF(I.EQ.J)CRNDEL=1.0
       RETURN
       END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  PIVOT(NORDER,JORDER,IDIR,AMAT) -- PERFORMS COLUMN PIVOTING AND REVERSEC
CC                                    PIVOTING FOR LUMATINV WHEN DIAGONAL C
CC                                    TERMS GO TO ZERO.                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE PIVOT(NORDER,JORDER,IDIR,AMAT)
C
       DIMENSION AMAT(14,14),KPIVOT(14,2)
       DOUBLE PRECISION AMAT,TEMP
C
C      IDIR:  1 = PIVOT ZERO TERMS TO EDGE OF MATRIX
C             2 = RESTORE ORDERING OF ORIGINAL MATRIX
C
C==========================================================================
C
      IF(IDIR.EQ.2)GO TO 500
C---------------------------------------- PUSH ZEROES TO MATRIX EDGE AND
C                                         REDUCE MATRIX ORDER BY 1
         JORDER = NORDER
         NPIV = 0
         ICOL = 0
  100 ICOL = ICOL + 1
  110 IF(ICOL.GT.JORDER)RETURN
      IF(AMAT(ICOL,ICOL).NE.(0.0))GO TO 100
            NPIV = NPIV+1
            KPIVOT(NPIV,1) = ICOL
            KPIVOT(NPIV,2) = JORDER
C
C------------------------------------------ PERFORM PIVOT
C
      DO 200 J = 1,JORDER
C
         IF(J.EQ.ICOL)GO TO 220
            TEMP = AMAT(J,ICOL)
            AMAT(J,ICOL) = AMAT(J,JORDER)
            AMAT(J,JORDER) = 0.0
            GO TO 200
  220       TEMP = AMAT(ICOL,ICOL)
            AMAT(ICOL,ICOL) = AMAT(JORDER,JORDER)
            AMAT(JORDER,JORDER) = 0.0
  200    CONTINUE
C
       DO 240 J=1,JORDER
       IF(J.EQ.ICOL)GO TO 240
          TEMP = AMAT(ICOL,J)
          AMAT(ICOL,J) = AMAT(JORDER,J)
          AMAT(JORDER,J) = 0.0
  240  CONTINUE
C
               JORDER = JORDER - 1
               GO TO 110
C
C------------------------------------- RESTORE ORDERING OF ORIGINAL MATRIX
C
  500  IF(NPIV.EQ.0)RETURN
C
       DO 1000 K = NPIV,1,-1
C
       DO 600 J = 1,KPIVOT(K,2)-1
       IF(J.EQ.KPIVOT(K,1))GO TO 540
          TEMP = AMAT(J,KPIVOT(K,1))
          AMAT(J,KPIVOT(K,1)) = AMAT(J,KPIVOT(K,2))
          AMAT(J,KPIVOT(K,2)) = TEMP
          GO TO 600
  540     TEMP = AMAT(KPIVOT(K,1),KPIVOT(K,1))
          AMAT(KPIVOT(K,1),KPIVOT(K,1)) = AMAT(KPIVOT(K,2),KPIVOT(K,2))
          AMAT(KPIVOT(K,2),KPIVOT(K,2)) = TEMP
  600  CONTINUE
C
       DO 650 J=1,KPIVOT(K,2)-1
       IF(J.EQ.KPIVOT(K,1))GO TO 650
          TEMP = AMAT(KPIVOT(K,1),J)
          AMAT(KPIVOT(K,1),J) = AMAT(KPIVOT(K,2),J)
          AMAT(KPIVOT(K,2),J) = TEMP
  650  CONTINUE
C
 1000  CONTINUE
       RETURN
       END
CunixC::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
CunixC:: PRINTC -  SUBROUTINE TO PRINT CHARACTER  STRINGS WITHOUT USING AN  ::
CunixC::           EXPLICIT FORTRAN WRITE STATEMENT                         ::
CunixC::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
CunixC
CunixC	TAKES A CHAR STRING AS ITS SINGLE PARM (I.E. LOG*1 ARRAY)
CunixC
Cunix	SUBROUTINE PRINTC(STRING)
Cunix	LOGICAL*1 STRING(1)
CunixC
CunixC
Cunix	DO 10 I=1,255		! LOOP FOR MAX STRING LENGTH
Cunix		IF (STRING(I) .EQ. "000) GOTO 15
Cunix10	CONTINUE
Cunix15	N = I
Cunix	WRITE(6,20) (STRING(I),I=1,N)
Cunix20	FORMAT(1X,<N>A1)
Cunix	END
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:: XPAR:      (1) W  (2) H  (3) THETA-BAR  (4) B0          ::::
C::  IF NPHASE=1: (5) THROUGH (14) LEGENDRE COEFFICIENTS    ::::
C::     NPHASE=2: (5) HEYNEY-GREENSTEIN G                   ::::
C::     NPHASE=3: (5) HEYNEY-GREENSTEIN G1 - BACKSCATTER    ::::
C::               (6) HEYNEY-GREENSTEIN G2 - FORESCATTER    ::::
C::               (7) PARTITIONING COEFFICIENT FOR LINEAR   ::::
C::                   COMBINATION OF G1 AND G2              ::::
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C
      SUBROUTINE PARCHK(LSTFUN,XPAR)
C
      DIMENSION PARMIN(14),PARMAX(14),XPAR(14),PGSUM(181),
     * PTERMS(10),Z(10),PHASE(10,181),PCOMP(181),LSTFUN(20)
C
      REAL MU0,MU01,MU02,MU1,MU11,MU12,I,I0
C
	COMMON /HAPKE/BGFNCT,PGFNCT,HFNCTI,HFNCTE,HAPCOF,SFNCT,
     *  MU0,MU01,MU02,MU1,MU11,MU12,COSI,COSE,COTI,COTE,COTT,TANT,
     *  BETA,PSI,COSPSI,SINPS2,F,Y,EE1,EE2,EI1,EI2,XI1,XI2,XI3
C
      COMMON/PARTRM/PARMIN,PARMAX
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:: DEFAULT VALUES OF PARAMETER LIMITS --- ASSUMED HAPKE P(G) :
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
      DATA PARMIN/0.0001,0.0001,1.0E-09,0.0001,-1.732,-1.0,
     *            0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
      DATA PARMAX/1.00000,99.0,85.0,99.0,1.732,2.0,1.0,1.0,
     *            1.0,1.0,1.0,1.0,1.0,1.0/
C
      PI = 3.141592654
           NEWBG=LSTFUN(3)
           NPHASE=LSTFUN(4)
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:: SET-UP ADMISSIBLE PARAMETER RANGES ON FIRST CALL       ::::
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
        IF(NOPASS.EQ.1)GO TO 100
           NOPASS=1
C
C---------------------------------------- DEPENDS ON P(G) FORM
C
      IF(LSTFUN(4).EQ.1)GO TO 100
      IF(LSTFUN(4).EQ.3)GO TO 10
      IF(LSTFUN(4).EQ.4)GO TO 12
c                                          one term henyey-greenstein
           PARMIN(5)=-0.9
           PARMAX(5)=+0.98
           GO TO 100
C
c                                          two-term henyey-greenstein
   10      PARMIN(5)=-0.98
           PARMAX(5)=+0.98
C
           PARMIN(6)=-0.0001
           PARMAX(6)=+0.98
C           
           PARMIN(7)=0.00001
           PARMAX(7)=1.0
           GO TO 100
C
C                                         CONSTANT VALUE
   12      PARMIN(5)=0.0
           PARMAX(5)=10.0
C
C=================================================================
C=               CHECK FOR INADMISSIBLE PARAMETERS               = 
C=================================================================
C
  100 DO 200 IPAR=1,14
C
      IF(IPAR.EQ.4)GO TO 200
      IF(IPAR.GT.5.AND.LSTFUN(4).EQ.1)GO TO 200
C
      IF(XPAR(IPAR).GE.PARMAX(IPAR))XPAR(IPAR)=PARMAX(IPAR)-0.00001
      IF(XPAR(IPAR).LE.PARMIN(IPAR))XPAR(IPAR)=PARMIN(IPAR)+0.00001
C
  200 CONTINUE
C
C==================================== CHECK LEGENDRE COEFFICIENTS
C
  300 DO 310 INDEX=1,10
  310 PTERMS(INDEX)=XPAR(INDEX+4)
      IF(LSTFUN(4).NE.1)GO TO 400
C
      PARMAX(6)=1.0+SQRT(36.0-12.0*PTERMS(1)*PTERMS(1))/6.0
      PARMIN(6)= ABS(PTERMS(1)) - 1.0
C
      IF(PTERMS(2).GT.PARMAX(6))PTERMS(2)=PARMAX(6)-0.00001
      IF(PTERMS(2).LT.PARMIN(6))PTERMS(2)=PARMIN(6)+0.00001
C
C
C================================== CHECK OPPOSITION AMPLITUDE TERM
C
  400  DO 410 J=1,10
  410  XPAR(J+4) = PTERMS(J)
        LOAD=2
        CALL SHUFLE(LOAD,XPAR,W,H,TBAR,B0,PTERMS)
        IF(LSTFUN(3).NE.1)GO TO 1000
C
          IF(XPAR(4).GT.PARMAX(4))XPAR(4)=PARMAX(4)
          IF(XPAR(4).LT.PARMIN(4))XPAR(4)=PARMIN(4)+0.00001
 1000     RETURN
          END
C-------------------------------------------------------------------------
C-- FUNCTION BSF:  COMPUTES THE REGOLITH BACKSCATTER FUNCTION B(G)     ---
C--                                                                    ---
C-- INPUTS:  NEWBG:   1 = NEW HAPKE B(G)    2=OLD HAPKE B(G)           ---
C--          NPHASE:  TYPE OF PHASE FUNCTION (SEE SPPF DOCUMENTATION)  ---
C--          W:       SINGLE-SCATTERING ALBEDO                         ---
C--          H:       PARTICLE COMPACTION PARAMETER                    ---
C--          OPMAG:   B0 PARAMETER OF HAPKE'S NEW B(G)                 ---
C--          PTERMS:  PARTICLE PHASE FUNCTION PARAMTERS (SEE SPPF)     ---
C--          ANGG:    PHASE ANGLE (IN DEGREES)                         ---
C-------------------------------------------------------------------------
C--  REV: 1   02/23/88   USE B0 INSTEAD OF S(0) IN NEW HAPKE EQUATION    -
C--  REV: 2   04/08/88   GET RID OF UNUSED ARGUMENTS (NPHASE,PTERMS)     -  
C-------------------------------------------------------------------------
C
        FUNCTION BSF(NEWBG,W,H,OPMAG,ANGG)
C
                        PI = 3.141592654
                        G0 = PI*ANGG/180.0
                        TANG = TAN(ABS(G0))
C
        IF(NEWBG.EQ.2)GO TO 200
C
C----------------- NEW HAPKE B(G) ---------------------------------------
C
        B0 = OPMAG
        IF(H.EQ.0.0)GO TO 50
C
	 TANG2 = TAN(ABS(0.5*G0))
C
	IF(ABS(G0).GT.(0.01))GO TO 40  
C
	     BSF = B0*(H/(H+ABS(0.5*G0)))
	     RETURN
C
   40  	  BSF = B0*H/(H + TANG2)
          RETURN
C
   50     BSF = 0.0
          RETURN
C
C------------------ OLD HAPKE B(G) -----------------------------------
C
  200 B0 = EXP(-0.5*W*W)
      IF(ABS(G0).GT.(0.48*PI))GO TO 250
      IF(ABS(G0).LT.(0.01))GO TO 240
      
C
          EXPTRM = EXP(-H/TANG)
          BSF = B0*(1.0-0.5*TANG*(3.0-EXPTRM)*(1.0-EXPTRM)/H)
          RETURN
C
  240     BSF = B0*(1.0 - 1.5*ABS(G0)/H)
          RETURN
C
  250     BSF = 0.0
          RETURN
          END
C---------------------------------------------------------------------
C--- SPPF - This routine computes the average single-particle phase  -
c---        function for one of several models selected with the     -
c---        variable NTYPE, where:                                   -
c--- NTYPE:  1=Legendre Polynomial   2=Heyney-Greenstein             -
c---------------------------------------------------------------------
c--- OTHER INPUT PARAMETERS:                                         -
c---  PTERMS:  a) array of Legendre Coefficients if NTYPE=1          -
c---           b) terms of the Heyney-Greenstein Function if NTYPE=2 -
c---              PTERM(i)  1) g1   2) g2  3) alpha                  -
c---  PHANGL   phase angle in degrees
c---------------------------------------------------------------------
c
      FUNCTION SPPF(NTYPE,PHANGL,PTERMS)
C
      DIMENSION Z(10),PTERMS(10)
C
              PI = 3.141592654
C
C--------- LEGENDRE POLYNOMIAL (NTYPE=1)  ---------------------------
C
      IF(NTYPE.NE.1)GO TO 100
         X = COS(PI*PHANGL/180.0)
         NORDER=10
         CALL COMLEG(X,NORDER,Z0,Z)
         SPPF = 1.0
         DO 20 J=1,NORDER
            SPPF = SPPF + PTERMS(J)*Z(J)
   20 CONTINUE
      RETURN
C
C----------- HEYNEY-GREENSTEIN FUNCTION ----------------------------
C
C   ONE-TERM HG (NTYPE=2)      TWO-TERM HG (NTYPE=3)
C
c   Note:  If FRAC=0, equation reduces to one-term HG
c
  100 IF(NTYPE.GT.3)GO TO 200
         G1 = PTERMS(1)
         G2 = PTERMS(2)
         FRAC = PTERMS(3)
C
             X = COS(PI*(180.0-PHANGL)/180.0)
                 BOTTOM = 1.5*ALOG(1.0+G1*G1-2.0*G1*X)
                 BOTTOM = EXP(BOTTOM)
                 PART1 = (1.0-G1*G1)/BOTTOM
C
                 BOTTOM = 1.5*ALOG(1.0+G2*G2-2.0*G2*X)
                 BOTTOM = EXP(BOTTOM)
                 PART2 = (1.0-G2*G2)/BOTTOM
C
  150 SPPF = (1.0-FRAC)*PART1 + FRAC*PART2
      RETURN
C
C------------------------------------------- P(G) = CONSTANT
C
  200 SPPF = PTERMS(1)
      RETURN
      END
C -------------------------------------------------------------------------
C --- COMPUTES LEGENDRE POLYNOMIALS UP TO 11TH ORDER (NDR=10).            -
C ---                                                                     -
C --- INPUTS:  NDR: ORDER OF DESIRED POLYNOMIAL                           -
C ---          X:   COS(ANGLE)                                            -
C ----                                                                    -
C --- OUTPUTS: Z0: ZEROTH-ORDER TERM (Z0=1.0)                             -
C ---           Z: ARRAY OF HIGHER ORDER TERMS                            -
C--------------------------------------------------------------------------
C
C
        SUBROUTINE COMLEG(X,NDR,Z0,Z)
        DIMENSION Z(10)
C
C-------- FIRST THREE ORDERS
C
        Z0 = (1.0)
        Z(1) = X
        Z(2) = 1.5*X*X - 0.5
C
C-------- USE RECURSION FORMULA FOR HIGHER ORDER TERMS
C
        DO 100 J=3,NDR
                                      Q = (FLOAT(J))
C
        Z(J)= ((2.*Q-1.)*X*Z(J-1) - (Q-1.)*Z(J-2))/Q
C
  100   CONTINUE
C
        RETURN
        END
C****************************************************************************
C********************        FUNCTION HFNCTN.FOR    *************************
C****************************************************************************
C**                                                                        **
C**  Anne J. Verbiscer   02-JUN-87   rev: 0  program design and testing    **
C**  Paul Helfenstein    02-JUN-87   rev: 1  reorder data statements,      **
c**                                          add option for Hapke's        **
C**                                          approximation, skip to table  **
C**                                          if W hasn't changed since     **
C**                                          last call, replace X with     **
C**                                          INDEX in last four lines      **
C**                                                                        **
C****************************************************************************
C**  COMPUTES CHANDRASEKHAR'S H-FUNCTIONS BY NUMERICAL INTEGRATION WITH A ***
C**                       TEN-POINT GAUSSIAN QUADRATURE                   ***
C****************************************************************************
C
C       USAGE:  HFNCTN(M,W,NEWH)
C
C              where   M  = cos(i) or cos(e):          i = incidence angle
C                                                      e = emission angle
C                      W  = single-scattering albedo
C
C                    NEWH = 0 for Hapke's approximation to the H-function
C                           1 for the Gaussian Quadrature
C
C***************************************************************************
C
	FUNCTION HFNCTN(M,W,NEWH)
	DIMENSION X(5),F(5)
	REAL H(1001),MU(1001),SS,A,M
	DATA X/.1488743389,.4333953941,.6794095682,.8650633666,.9739065285/
	DATA F/.2955242247,.2692667193,.2190863625,.1494513491,.0666713443/
        DATA IPASS/0/
C
C------------------------ IF NEWH = 1, USE HAPKE'S APPROXIMATION
C
      IF(NEWH.NE.1)GO TO 100
         IF(W.GE.1.0)W=0.99999998
         HFNCTN = (1.0+2.0*M)/(1.0+2.0*M*SQRT(1.0-W))
         RETURN
C
C----------------------- IF NEWH = 2, USE NUMERICAL INTEGRATION
C
100   IF(W.EQ.WOLD.AND.IPASS.EQ.1)GO TO 550
         IPASS = 1
         IF(W.GE.1.0)W=1.0
         WOLD = W
C
	A=SQRT(1.-W)
C
C	Calculate first set of H-functions with H = 1 + 3mu
C
	DO 200 J=1,1001
		L=J-1
		MU(J)=FLOAT(L)/1000.
		H(J)=(SQRT(3.)*MU(J))+1.
200	CONTINUE
C
C	Perform iterations converging to solutions for H-functions
C
	DO 500 N=1,20
	DO 300 J=2,1001
		FUNCP=((0.5+(0.5*X(1)))/(MU(J)+(0.5+(0.5*X(1)))))*H(575)
		FUNCM=((0.5-(0.5*X(1)))/(MU(J)+(0.5-(0.5*X(1)))))*H(427)
		SS=F(1)*(FUNCP+FUNCM)
		FUNCP=((0.5+(0.5*X(2)))/(MU(J)+(0.5+(0.5*X(2)))))*H(718)
		FUNCM=((0.5-(0.5*X(2)))/(MU(J)+(0.5-(0.5*X(2)))))*H(284)
		SS=SS+(F(2)*(FUNCP+FUNCM))
		FUNCP=((0.5+(0.5*X(3)))/(MU(J)+(0.5+(0.5*X(3)))))*H(841)
		FUNCM=((0.5-(0.5*X(3)))/(MU(J)+(0.5-(0.5*X(3)))))*H(161)
		SS=SS+(F(3)*(FUNCP+FUNCM))
		FUNCP=((0.5+(0.5*X(4)))/(MU(J)+(0.5+(0.5*X(4)))))*H(934)
		FUNCM=((0.5-(0.5*X(4)))/(MU(J)+(0.5-(0.5*X(4)))))*H(68)
		SS=SS+(F(4)*(FUNCP+FUNCM))
		FUNCP=((0.5+(0.5*X(5)))/(MU(J)+(0.5+(0.5*X(5)))))*H(988)
		FUNCM=((0.5-(0.5*X(5)))/(MU(J)+(0.5-(0.5*X(5)))))*H(14)
		SS=SS+(F(5)*(FUNCP+FUNCM))
		H(J)=1./(((W/4.)*SS)+A)
300	CONTINUE
500	CONTINUE
c
550     CONTINUE
	INDEX=1+NINT(M*1000.)
	HFNCTN=H(INDEX)
	RETURN
	END		
C-------------------------------------------------------------------------
C--  PARWGT -  Allows user to control which parameters are being searched 
C-------------------------------------------------------------------------
C
        SUBROUTINE PARWGT(LSTFUN,XPAR,LISPAR)
C
        CHARACTER*4 ACTIVE,ON,OFF
        DIMENSION XPAR(14),DELPAR(14),LISPAR(14),DELMAX(14)
     *     ,PHAWGT(14),PTERMS(10),TOGGLE(14),PARDEL(14),LSTFUN(20)
        COMMON /SWITCH/PHAWGT
C
        DATA TOGGLE/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     *              1.0,1.0,1.0,1.0/
        DATA ON,OFF/'ON  ','OFF '/
        DATA PARDEL/1.0,0.5,90.0,1.5,4.0,3.0,1.0,1.0,1.0,1.0,
     *              1.0,1.0,1.0,1.0/
C
           NPARS = LSTFUN(1)
           NEWH = LSTFUN(2)
           NEWBG = LSTFUN(3)
           NPHASE = LSTFUN(4)
           NTERMS = LSTFUN(5)
C
C---------------- FIND MODE:  1 = MANUALLY SELECT ACTIVE PARAMETERS
C                             0 = ALL PARAMETERS ACTIVE
       IF(IPASS.EQ.1)GO TO 10
          DO 600 J=1,14
  600     PHAWGT(J)=1.0
          IPASS = 1
       print*,'------ SELECTION OF ACTIVE PARAMETERS --------- '
       print*,'     '
Cunix
      PRINT*,'-----> 0 = ALL PARAMETERS ACTIVE'
      PRINT*,'       1 = MANUALLY SELECT ACTIVE PARAMETERS'
Cunix
Cunix      CALL PRINTC('-----> 0 = ALL PARAMETERS ACTIVE')
Cunix      CALL PRINTC('       1 = MANUALLY SELECT ACTIVE PARAMETERS')
      print2
    2 FORMAT(23X,'-------- ENTER OPTION ------> ',$)
      read(*,*),MODE
      IF(MODE.NE.1)GO TO 10
C
  100 print*,'              '
      print*,'  XPAR                          STATUS'
      DO 300 J=1,LSTFUN(1)
         PHAWGT(J)=1.0
         IF(LISPAR(J).EQ.0)GO TO 300
            ACTIVE = ON
         IF(TOGGLE(LISPAR(J)).NE.1.0)ACTIVE=OFF
         GO TO (110,120,130,140,150,150,150,150,150,150,150,150,
     *          150,150),LISPAR(J)
  110 print111,ACTIVE
  111 FORMAT(5X,'1. SINGLE-SCATTERING ALBEDO:  ',A4)
      GO TO 300
  120 print121,ACTIVE
  121 FORMAT(5X,'2. PARTICLE COMPACTION:       ',A4)
      GO TO 300
  130 print131,ACTIVE
  131 FORMAT(5X,'3. MACROSCOPIC ROUGNNESS:     ',A4)
      GO TO 300
  140 print141,ACTIVE
  141 FORMAT(5X,'4. OPPOSITION AMPLITUDE:      ',A4)
      GO TO 300
  150 GO TO (160,170,180,188),NPHASE
  160 JTERM=LISPAR(J)-4
      print161,J,JTERM,ACTIVE
  161 FORMAT(4X,I2,'. PARTICLE PHASE P(',I2,'):      ',A4)
      GO TO 300
  170 print171,ACTIVE
  171 FORMAT(5X,'5. HEYNEY-GREENSTEIN g:       ',A4)
      GO TO 300
  180 JTERM = LISPAR(J)-4
      IF(JTERM.EQ.3)GO TO 185
  182 print183,J,JTERM,ACTIVE
  183 FORMAT(5X,I1,'. HEYNEY-GREENSTEIN g',I1,':      ',A4)
      GO TO 300
  185 print186,J,ACTIVE
  186 FORMAT(5X,I1,'. PARTITION COEFFICIENT:     ',A4)
      GO TO 300
  188 print189,J,ACTIVE
  189 FORMAT(5X,I1,'. VALUE OF P(G):             ',A4)
C
  300 CONTINUE
C
      print*,' '
      print*,' '
      print320
  320 FORMAT(1X,'ENTER XPAR NUMBER TO CHANGE OR 0 TO EXIT: ',$)
      read(*,*),LPAR
      IF(LPAR.EQ.0)GO TO 500
      IF(TOGGLE(LPAR).EQ.0.0)GO TO 330
         TOGGLE(LPAR)=0.0
         GO TO 100
  330    TOGGLE(LPAR)=1.0
         GO TO 100
C
  500 DO 515 J=1,14
  515    PHAWGT(J) = TOGGLE(J)*PHAWGT(J)
C
   10 RETURN
      END
C---------------------------------------------------------------------
C------- PARINDEX: SETS UP ORDER FOR PARAMETER SEARCH INDEX LISPAR.  -
C-------           THE INTEGER VALUE OF EACH ELEMENT OF LISPAR IS    -
C-------           THE INDEX OF THE PARAMETER IN PARAMETER TRANSFER  -
C-------           ARRAY, XPAR.  SEE SUBROUTINE SHUFLE.              -
C-------                                                             -
C------- LISPAR(1) = 1   SINGLE SCATTERING ALBEDO, W => XPAR(1)      -
C------- LISPAR(2) = 2   OPPOSITION WIDTH PARAMETER, h => XPAR(2)    -
C------- LISPAR(3) = 3   MACROSCOPIC ROUGHNESS, THETA-BAR => XPAR(3) -
C------- LISPAR(4) = 4   IF NEW B(G) IS SPECIFIED, VALUE INDEXES     -
C-------                 OPPOSITION AMPLITUDE, B0 => XPAR(4)         -
C-------                 IS SPECIFIED)                               -
C-------           = 5   IF OLD B(G) IS SPECIFIED, VALUE INDEXES     -
C-------                 FIRST TERM OF PARTICLE PHASE FUNCTION       -
C-------                 P(G): PTERMS(1) => XPAR(5)                  -
C------- LISPAR(5) TO    REMAINING TERMS OF THE PARTICLE PHASE       -
C------- LISPAR(14)      FUNCTION, PTERMS(1 TO 10) => XPAR(5-14)     -
C---------------------------------------------------------------------
C
       SUBROUTINE PARINDEX(LSTFUN,LISPAR)
C
       DIMENSION LSTFUN(20),LISPAR(14)
C
C      ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C      ::::::::::::::::::::::::::::::::::: SET-UP PARAMETER INDEX :::::
C      ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
        LISPAR(1)=1
        LISPAR(2)=2
        LISPAR(3)=3
        INTERM = 3
C
C------------------------------------------------ DEPENDS ON B(G) FORM
C
      IF(LSTFUN(3).NE.1)GO TO 2
           INTERM = INTERM + 1
           LISPAR(INTERM)=4
C
C------------------------------------------------ DEPENDS ON P(G) FORM
C
    2 IF(LSTFUN(4).NE.1)GO TO 3
C                                                 LEGENDRE POLYNOMIAL
       DO 12 J=1,LSTFUN(5)
           INTERM = INTERM + 1
   12      LISPAR(INTERM)=4+J
           GO TO 9 
C                                                 HEYNEY-GREENSTEIN
    3 IF(LSTFUN(4).GT.4.OR.LSTFUN(4).LT.2)GO TO 9
           LOOPS = LSTFUN(4) - 1
           DO 4 J=1,LOOPS
           INTERM = INTERM + 1
           LISPAR(INTERM)=J+4
    4      CONTINUE
           IF(LSTFUN(4).NE.3)GO TO 9
           INTERM=INTERM+1
           LISPAR(INTERM)=7
C
    9 RETURN
      END
C-----------------------------------------------------------------------
C-- HAPGET:  USER-INTERFACE WHICH GETS DESIRED VALUES OF PHOTOMETRIC   -
C--          FUNCTION PARAMETERS FROM THE USER.                        -
C-----------------------------------------------------------------------
C-- PARAMETER LIST:                                                    -
C-   NEWBG:   SPECIFIES NEW/OLD HAPKE BACKSCATTER FUNCTION 1=NEW, 2=OLD-
C-   NPHASE:  SPECIFIES TYPE OF PARTICLE PHASE FUNCTION                -
C-            1 = LEGENDRE POLYNOMIAL                                  -
C-            2 = ONE-TERM HEYNEY-GREENSTEIN                           -
C-            3 = TWO-TERM HEYNEY-GREENSTEIN                           -
C-            4 = CONSTANT P(G)
C-   W:       SINGLE-SCATTERING ALBEDO                                 -
C-   H:       PARTICLE COMPACTION PARAMETER FOR BACKSCATTER FUNCTION   -
C-   B0:      OPPOSITION SURGE AMPLITUDE TERM FOR HAPKE'S NEW B(G)     -
C-   TBAR:    AVERAGE SUB-PIXEL SCALE MACROSCOPIC ROUGHNESS (THETA-BAR)-
C-   PTERMS:  ARRAY (10 MAXIMUM ELEMENTS) OF CONSTANTS FOR THE PARTICLE-
C-            PHASE FUNCTION.                                          -
C-                NPHASE                      PTERMS                   -
C-                  1      LEGENDRE POLYNOMIAL COEFFICIENTS P(2)-P(11) -
C-                  2      PTERMS(1) IS THE HEYNEY-GREENSTEIN PARAMETER-
C-                  3      PTERMS(1) IS G1 TERM OF HEYNEY-GREENSTEIN,  -
C-                         PTERMS(2) IS G2 TERM OF HEYNEY-GREENSTEIN   -
C-                         PTERMS(3) IS FRACTIONAL PART FOR LINEAR     -
C-                         COMBINATION OF G1 AND G2 CONTRIBUTIONS      -
C-                  4      P(G) = PTERMS(1) = CONSTANT                 -
C-----------------------------------------------------------------------
C
      SUBROUTINE HAPGET(LSTFUN,W,H,B0,TBAR,PTERMS)
      DIMENSION PTERMS(10),LSTFUN(20)
C
                NPARS=0
                NTERMS=0
                IPASS=0
C
      print1
      print*,' INPUT ONE OF THE FOLLOWING OPTIONS: '
      print1
      print*,' 1.  ENTER HAPKE PARAMETERS MANUALLY '
      print*,' 2.  READ IN HAPKE PARAMETERS FROM HAPKE.DAT FILE'
      print1
      print1001
 1001 FORMAT(1X,'                              ENTER OPTION: ',$)
      read(*,*),INPMODE
      print1
      IF(INPMODE.EQ.1)GO TO 1010       
         CALL INPLST(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL)
              NPARS  = LSTFUN(1)
              NEWH   = LSTFUN(2)
              NEWBG  = LSTFUN(3)
              NPHASE = LSTFUN(4)
              NTERMS = LSTFUN(5)
 1002    print1
         CALL OUTLST(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL)
              IPASS=1
      print1
      print*,' OPTIONS FOR MODIFYING INPUT VALUES: '
      print1
      print*,' (0)  KEEP PARAMETERS AT PRESENT VALUES'
      print*,' (1)  CHANGE SINGLE SCATTERING ALBEDO W'
      print*,' (2)  CHANGE OPPOSITION FUNCTION B(G)'
      print*,' (3)  CHANGE PARTICLE PHASE FUNCTION P(G)'
      print*,' (4)  CHANGE MACROSCOPIC ROUGHNESS THETA-BAR'
      print1
      print1001
      read(*,*),MODOPT
      print1
      IF(MODOPT.EQ.0)GO TO 20000
      GO TO (1005,1006,1007,1008),MODOPT
 1005        NPARS=NPARS-1
             GO TO 1010
 1006        IF(NEWBG.EQ.1)NPARS=NPARS-2
             IF(NEWBG.EQ.2)NPARS=NPARS-1
             GO TO 1020
 1007        NPARS = NPARS - NTERMS
             NTERMS=0
             GO TO 1030
 1008        NPARS=NPARS-1
             GO TO 1040
C
C----------------------------------------------------------------------
C--------------------------------------------- SINGLE-SCATTERING ALBEDO
C----------------------------------------------------------------------
C
 1010  print1
       print120
       print30
   30  FORMAT(1X,45('-'),' SINGLE-SCATTERING ALBEDO')
       print120
       print1
       print32
   32  FORMAT(1X,' ENTER SINGLE-SCATTERING ALBEDO: ',$)
       read(*,*),W
       NPARS=NPARS+1
       print1
       print34
   34  FORMAT(1X,' COMPUTATION OF CHANDRASEKHAR H-FUNCTIONS ',/,
     *        1X,'    1 = HAPKE APPROXIMATION ',/,   
     *        1X,'    2 = NUMERICAL EVALUATION OF EXACT SOLUTION',/,
     *        1X,'                                  ENTER OPTION: ',$)
       read(*,*),NEWH
       IF(MODOPT.NE.0)GO TO 10000
C
C-----------------------------------------------------------------------
C-------------------------------------------- BACKSCATTER FUNCTION TERMS
C-----------------------------------------------------------------------
C 
 1020 print1
    1 FORMAT(1X,'                                                     ')
      print120
  120 FORMAT(1X,72('-'))
      print122
  122 FORMAT(1X,41('-'),' BACKSCATTER FUNCTION TERMS')
      print120
      print1
      print130
  130 FORMAT(' ENTER BACKSCATTER FUNCTION TYPE: 1 = NEW HAPKE B(G)',/,
     *       '                                  2 = OLD HAPKE B(G)',/,
     *'                                                    OPTION:',$)
      read(*,*),NEWBG
      print1
      print132
  132 FORMAT(' ENTER VALUE OF COMPACTION PARAMETER H: ',$)
      read(*,*),H
               NPARS=NPARS+1
      IF(NEWBG.NE.1)GO TO 138
      print134
  134 FORMAT(' ENTER VALUE OF OPPOSITION AMPLITUDE TERM B0: ',$)
      read(*,*),B0
               NPARS=NPARS+1
      IF(MODOPT.NE.0)GO TO 10000
C
  138 print1
 1030 print120
      print140
  140 FORMAT(1X,41('-'),' PARTICLE PHASE FUNCTION TERMS')
      print120
      print1
C
C-----------------------------------------------------------------------
C----------------------------------------- PARTICLE PHASE FUNCTION TERMS
C-----------------------------------------------------------------------
      print2
    2 FORMAT(' ENTER PHASE FUNCTION TYPE: 1=LEGENDRE POLYNOMIAL',/,
     *       '                            2=ONE-TERM HEYNEY-GREENSTEIN',
     *  /,   '                            3=TWO-TERM HEYNEY-GREENSTEIN',
     *  /,   '                            4=CONSTANT VALUE',
     *  /,   '                                       OPTION: ',$)
      read(*,*),NPHASE
      IF(NPHASE.NE.1)GO TO 12
         print4
    4 FORMAT(1X,/,' HOW MANY TERMS IN THE LEGENDRE POLYNOMIAL? :',$)
      read(*,*),NTERMS
C
       DO 6 J=1,NTERMS
       WRITE(6,5)J
    5  FORMAT(' ENTER TERM ',I2,': ',$)
       read(*,*),PJ
          PTERMS(J) = PJ
          NPARS = NPARS+1
    6  CONTINUE
C
    9    DO 7 J=NTERMS+1,10
         PTERMS(J)=0.0
    7    CONTINUE
         GO TO 50
C
C                                         HENYEY-GREENSTEIN 
   12 IF(NPHASE.GT.3)GO TO 20
      print14
   14 FORMAT(' ENTER G1 TERM OF HENYEY-GREENSTEIN FUNCTION: ',$)
      read(*,*),PJ
               NPARS=NPARS+1
         PTERMS(1)=PJ
      IF(NPHASE.NE.2)GO TO 15
         NTERMS=1
         GO TO 9
C
   15 print16
   16 FORMAT(' ENTER G2 TERM OF HEYNEY-GREENSTEIN FUNCTION: ',$)
      read(*,*),PJ
         PTERMS(2)=PJ
         NPARS = NPARS+1
      print18
   18 FORMAT(' ENTER PARTITIONING COEFFICIENT: ',$)
      read(*,*),PJ
         PTERMS(3)=PJ
         NTERMS = 3
         NPARS=NPARS+1
         GO TO 9
C
C                                                P(G) = CONSTANT
   20 print22
   22 FORMAT(' ENTER VALUE OF P(G): ',$)
      read(*,*),PJ
      PTERMS(1)=PJ
      NTERMS=1
      NPARS=NPARS+1
      GO TO 9
   50 print1
       IF(MODOPT.NE.0)GO TO 10000
C      
C------------------------------------------------------------------------
C-------------------------------------------------  MACROSCOPIC ROUGHNESS
C------------------------------------------------------------------------
C    
 1040 print120
      print310
  310 FORMAT(1X,49('-'),' MACROSCOPIC ROUGHNESS',$)
      print120
      print1
      print320
  320 FORMAT(' THETA-BAR IN DEGREES: ',$)
      read(*,*),TBAR
      NPARS=NPARS+1
C
C------------------------------------------------------------------------
C-------------------------------------------- LOAD UP FLAG TRANSFER ARRAY
C------------------------------------------------------------------------
C
10000 LSTFUN(1)=NPARS
      LSTFUN(2)=NEWH
      LSTFUN(3)=NEWBG
      LSTFUN(4)=NPHASE
      LSTFUN(5)=NTERMS
      GO TO 1002
20000 RETURN
      END
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C::                                                                  :::
C:: INPLST:   INPUT HAPKE PHOTOMETRIC PARAMETERS FROM A PREVIOUS     :::
C::           HAPKE.DAT OUTPUT FILE AND OPTION FOR CHANGING SOME     :::
C::           OF THE PARAMETERS THAT HAVE BEEN READ IN.              :::
C::                                                                  :::
C::  Paul Helfenstein   15-SEPT-1987  rev 0:  CANNIBALIZED OUTLST    :::
C::                                                                  :::
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
      SUBROUTINE INPLST(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL)
      DIMENSION PTERMS(10),LSTFUN(20)
      CHARACTER*50 INPFILE
      character*(*) argfil
C
C------------------------------------------ INPUT FROM HAPKE.DAT FILE
C
1000   WRITE(6,1001)
1001   FORMAT(1X,' o ENTER FILE NAME FOR INPUT HAPKE PARAMETERS: ',
     *        $)
       READ(5,2000)INPFILE
2000   FORMAT(A)
C
       OPEN(UNIT=82,NAME=INPFILE,TYPE='OLD',ERR=2100)
Cunix       OPEN(UNIT=2,NAME=INPFILE,TYPE='OLD',SHARED,ERR=2100)
            GO TO 2500
2100    print*,'  '
        print*,'******** FILE DOES NOT EXIST *********'
            GO TO 1000
C
       entry inplis(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL,argfil,iret)
       iret=0
       OPEN(UNIT=82,NAME=argfil,TYPE='OLD',ERR=99101)
Cunix       OPEN(UNIT=2,NAME=argfil,TYPE='OLD',SHARED,ERR=99101)
            GO TO 99102
99101  continue
       iret = -1
       return
99102  continue
C
 2500   READ(82,3000)(LSTFUN(J),J=1,20)
 3000   FORMAT(16X,20(I2),/)
C
	READ(82,4000)W,TBAR
 4000   FORMAT(45X,F12.8,/,45X,F8.4)
C
        READ(82,4015)H
 4015   FORMAT(45X,F8.4)
        IF(LSTFUN(3).NE.1)GO TO 4019
        READ(82,4015)B0
C
 4019   GOTO(4020,4040,4060,4072),LSTFUN(4)
Cunix
 4020   continue
        read(2,'(1x)')
        READ(82,4030)(PTERMS(J),J=1,LSTFUN(5))
 4030   FORMAT(32X,F8.4)
Cunix
Cunix 4020   READ(2,4030)(PTERMS(J),J=1,LSTFUN(5))
Cunix 4030   FORMAT(1X,<LSTFUN(5)>(/,32X,F8.4))
Cunix
               GO TO 4080
 4040   READ(82,4050)PTERMS(1)
 4050   FORMAT(50X,f8.4)
               GO TO 4080
 4060   READ(82,4070)(PTERMS(J),J=1,3)
 4070   FORMAT(2X,/,10X,F8.4,9X,F8.4,26X,F8.4)
               GO TO 4080
 4072   READ(82,4074)PTERMS(1)
 4074   FORMAT(47X,F8.4)
C
 4080   READ(82,4090)RESIDL
 4090   FORMAT(1X,//,15X,E13.7)
 	CLOSE(UNIT=82)
        RETURN 
        END
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C::                                                                  :::
C:: OUTLST:   OUTPUTS HAPKE PHOTOMETRIC PARAMETERS AND RESIDUAL TO   :::
C::           MONITOR AND HAPKE.DAT OUTPUT FILE.                     :::
C::                                                                  :::
C::  Paul Helfenstein   15-SEPT-1987  rev 1:  Output LSTFUN array    :::
C::                                                                  :::
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C

      SUBROUTINE OUTLST(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL)
      DIMENSION PTERMS(10),LSTFUN(20),IUNIT(2)
      CHARACTER*50 OUTFILE
      CHARACTER*6 SCREEN(2)
      character*(*) argfil
C
      DATA SCREEN/'SCREEN','screen'/
      DATA IUNIT/6,82/
Cunix      DATA IUNIT/6,2/
C
      IF(IPASS.NE.0)GO TO 100
         IPASS=1
         WRITE(6,10)
  10  FORMAT(2X,' o ENTER FILE NAME FOR OUTPUT HAPKE PARAMETERS',/,
     *       2X,'   OR "SCREEN" FOR LISTING TO SCREEN ONLY: ',$)
      READ(5,15)OUTFILE
  15  FORMAT(A)
      print*,' '
C
       goto 99016
c
       entry outlis(LSTFUN,W,H,TBAR,B0,PTERMS,RESIDL,argfil,iret)
       outfile = argfil
       IF(OUTFILE(1:6).EQ.SCREEN(1).OR.OUTFILE(1:6).EQ.SCREEN(2))
     & goto 99016
       iret=0
       OPEN(UNIT=82,NAME=OUTFILE,TYPE='OLD',ERR=99002)
Cunix       OPEN(UNIT=2,NAME=OUTFILE,TYPE='OLD',SHARED,ERR=99002)
       close(82)
       goto 99016
99002  OPEN(UNIT=82,NAME=OUTFILE,TYPE='NEW',ERR=99003)
Cunix 99002  OPEN(UNIT=2,NAME=OUTFILE,TYPE='NEW',SHARED,ERR=99003)
       close(82)
       goto 99016
99003  continue
       iret = -1
       return
C
99016  continue
C
         IPASS=2
      IF(OUTFILE(1:6).EQ.SCREEN(1).OR.OUTFILE(1:6).EQ.SCREEN(2))
     *   IPASS=1
C
      IF(IPASS.EQ.1)GO TO 7
C
 100  OPEN(UNIT=82,ERR=2,NAME=OUTFILE,TYPE='OLD')
Cunix 100  OPEN(UNIT=2,ERR=2,NAME=OUTFILE,TYPE='OLD',SHARED)
           GO TO 5
    2 OPEN(UNIT=82,NAME=OUTFILE,TYPE='NEW')
Cunix    2 OPEN(UNIT=2,NAME=OUTFILE,TYPE='NEW',SHARED)
C
    5   WRITE(82,3000)(LSTFUN(J),J=1,20)
 3000   FORMAT(2X,'LSTFUN index: ',20(I2),/)
C
    7   DO 5000 K=1,IPASS
        WRITE(IUNIT(K),4000)W,TBAR
 4000   FORMAT(2X,'Single-Scattering Albedo (w):              ',F12.8,/,
     *         2X,'Average Macroscopic Roughness (theta-bar): ',F12.8)
C
        WRITE(IUNIT(K),4015)H
 4015   FORMAT(2X,'Regolith Compaction Parameter (h):         ',F8.4)
        IF(LSTFUN(3).NE.1)GO TO 4019
        WRITE(IUNIT(K),4016)B0
 4016   FORMAT(2X,'Opposition Surge Amplitude Parameter (B0): ',F8.4)
C
 4019   GOTO(4020,4040,4060,4072),LSTFUN(4)
 4020   continue
        WRITE(IUNIT(K),4029)
 4029   FORMAT(2X,'Particle Phase Function Terms:  Legendre Polynomial')
        WRITE(IUNIT(K),4030)(J,PTERMS(J),J=1,LSTFUN(5))
 4030   FORMAT(24X,'P(',I2,') = ',F8.4)
Cunix
Cunix 4020   WRITE(IUNIT(K),4030)(J,PTERMS(J),J=1,LSTFUN(5))
Cunix 4030   FORMAT(2X,'Particle Phase Function Terms:  Legendre Polynomial',
Cunix     *  <LSTFUN(5)>(/,24X,'P(',I2,') = ',F8.4))
Cunix
               GO TO 4080
 4040   WRITE(IUNIT(K),4050)PTERMS(1)
 4050   FORMAT(2X,'Particle Phase Function:  Heyney-Greenstein g = ',
     *         f8.4)
               GO TO 4080
 4060   WRITE(IUNIT(K),4070)(PTERMS(J),J=1,3)
 4070   FORMAT(2X,'Particle Phase Function:  Heyney-Greenstein ',
     *  /,5X,'g1 = ',F8.4,4X,'g2 = ',F8.4,4X,'Partition Coefficient:',
     *        F8.4)
              GO TO 4080
 4072   WRITE(IUNIT(K),4074)PTERMS(1)
 4074   FORMAT(2X,'Value of Particle Phase Function P(g): ',6X,
     *         F8.4)
C
 4080   WRITE(IUNIT(K),4090)RESIDL
 4090   FORMAT(1X,//,2X,'RESIDUAL:   ',E13.7,///)
 5000   CONTINUE
 	IF(IPASS.EQ.2)CLOSE(UNIT=82)
        RETURN 
        END
C***********************************************************************
C** REFLCT - EVALUATES BIDIRECTIONAL REFLECTANCE (I/F) FROM HAPKE'S    *
C**          PHOTOMETRIC EQUATION GIVEN PHOTOMETRIC ANGLES AND VALUES  *
C**          OF HAPKE'S PHOTOMETRIC PARAMETERS.  VARIABLE NAMES ARE    *
C**          SIMILAR TO THOSE USED BY HAPKE.  THIS VERSION HAS BEEN    *
C**          ALTERED FOR THE NEW OPPOSITION EFFECT EQUATION IN HAPKE'S *
C**          PAPER #4 SUBMITTED TO ICARUS AND INCLUDES AN OPTION FOR   *
C**          FOR NUMERICAL EVALUATION OF THE MULTIPLE SCATTERING       *
C**          FUNCTIONS (H-FUNCTIONS) COURTESY OF A. VERBISCER.         *
C** ------------------------------------------------------------------ *
C** INPUTS:                                                            *
C**      I = INCIDENCE ANGLE IN DEGREES                                *
C**      E = EMERGENCE ANGLE IN DEGREES                                *
C**      G = PHASE ANGLE IN DEGREES                                    *
C**      W = SINGLE SCATTERING COEFFICIENT                             *
C**      H = BACKSCATTER PARAMETER RELATED TO SOIL POROSITY            *
C**  OPMAG = OPPOSTION AMPLITUDE COEFFICIENT, B0                       *
C**   TBAR = AVERAGE MACAROSCOPIC SLOPE ANGLE (IN DEGREES)             *
C** PTERMS = ARRAY OF CONSTANTS FOR THE SINGLE-PARTICLE PHASE FUNCTION *
C** LSTFNC = ARRAY OF FLAGS WHICH SPECIFY OPTIONS FOR ASSORTED         *
C**          VERSIONS OF HAPKE'S EQUATION AS FOLLOWS:                  *
C**                                                                    *
C** LSTFNC(1):  NUMBER OF HAPKE PARAMETERS BEING USED                  *
C**                                                                    *
C** LSTFNC(2):  TYPE OF H-FUNCTION USED                                *
C**             1=HAPKE'S APPROXIMATION TO CHANDRASEKHAR'S H-FUNCTION  *
C**             2=NUMERICAL EVALUATION OF CHANDRASEKHAR'S H-FUNCTION   *
C**                                                                    *
C** LSTFNC(3):  TYPE OF BACKSCATTER FUNCTION => FORMERLY NEWBG         *
C**             1=NEW HAPKE B(G)  [must specify h and B0]              *
C**             2=OLD HAPKE B(G)  [must specify h; B0 = EXP(-0.5*W*W)] *
C**                                                                    *
C** LSTFNC(4):  TYPE OF PARTICLE PHASE FUNCTION => FORMERLY NPHASE     *
C**             1=LEGENDRE POLYNOMIAL (SEE PTERMS)                     *
C**             2=ONE-TERM HEYNEY-GREENSTEIN                           *
C**             3=TWO-TERM HEYNEY-GREENSTEIN                           *
C**             4=CONSTANT P(G)
C**                                                                    *
C** LSTFNC(5):  NUMBER OF TERMS IN THE PARTICLE PHASE FUNCTION         *
C**                                                                    *
C** LSTFNC(6)-(20): PRESENTLY UNUSED                                   *
C**                                                                    *
C***********************************************************************
C* PAUL HELFENSTEIN  REV:0 3/26/83  BROWN UNIVERSITY IMAGE PROCESSSING *
C*                   REV:1 3/20/84  NEW VERSION OF HAPKE'S PAPER       *
C*                   REV:3 3/25/84  DECLARE COMPONENTS IN COMMON       *
C*                   REV:4 7/20/84  HAPKE'S NEW B(G)                   *
C*                   REV:5 2/03/86  B0 TERM OF HAPKE'S NEW B(G)        *
C*                   REV:6 4/24/86  GENERALIZED P(G)                   *
C*                   REV:7 6/16/86  ENABLE OLD B(G) AND NEW B(G)       *
C*                   REV:8 7/22/87  ENABLE ANALYTIC H-FUNCTIONS AND    *
C*                                  CREATE FLAG ARRAY TO REPLACE NEWBG,*
C*                                  NPHASE,NTERMS ... ETC.             *
C*                   REV:9 2/23/88  USE B0 INSTEAD OF S(0) IN BSF      *
C*                  REV:10 4/08/88  ROUGHNESS CORRECTION AS A REAL*8   *
C*                                  SUBROUTINE (ROUGH)                 *
C***********************************************************************
C
      FUNCTION REFLCT(I,E,G,LSTFUN,W,H,OPMAG,TBAR,PTERMS)
C
	REAL MU0,MU01,MU02,MU1,MU11,MU12,I,I0
        DIMENSION PTERMS(10),LSTFUN(20)
C
	COMMON /HAPKE/BGFNCT,PGFNCT,HFNCTI,HFNCTE,HAPCOF,SFNCT,
     *  MU0,MU01,MU02,MU1,MU11,MU12,COSI,COSE,COTI,COTE,COTT,TANT,
     *  BETA,PSI,COSPSI,SINPS2,F,Y,EE1,EE2,EI1,EI2,XI1,XI2,XI3
C
C
C------- DETERMINE PRINCIPAL VARIABLE VALUES -------------------------
C
	PI=3.141592654
C
        NPARS  = LSTFUN(1)
        NHFN   = LSTFUN(2)
        NEWBG  = LSTFUN(3)
        NPHASE = LSTFUN(4)
        NTERMS = LSTFUN(5)
C
C-------- DETERMINE THE BACKSCATTER FUNCTION B(G) ---------------------
C
      BGFNCT = BSF(NEWBG,W,H,OPMAG,G)
C
C
C---- DETERMINE P(G): AVERAGE SINGLE PARTICLE SCATTERING FUNCTION ----
C
      PGFNCT = SPPF(NPHASE,G,PTERMS)
C
C---- DETERMINE THE CORRECTION FOR ROUGHNESS FROM THETA-BAR
C
      CALL ROUGH(I,E,G,TBAR,SFNCT,MU01,MU11)
C
C---- DETERMINE MULTIPLE SCATTERING (H) FUNCTIONS
C
       HFNCTI = HFNCTN(MU01,W,NHFN)
       HFNCTE = HFNCTN(MU11,W,NHFN)
C
C--------- NOW, CALCULATE THE COMPLETE PHOTOMETRIC REFLECTANCE ------
C
  500  HAPCOF = (0.25*W)*(MU01/(MU01+MU11))
C
       REFLCT = HAPCOF*((1.0+BGFNCT)*PGFNCT-1.0+HFNCTI*HFNCTE)*SFNCT
C
C
	RETURN
	END
C***********************************************************************
C** ROUGH  - EVALUATES THE CORRECTION FOR MACROSCOPIC ROUGHNESS FOR    *
C**          HAPKE'S BIDIRECTIONAL REFLECTANCE EQUATION AT ANY GIVEN   *
C**          INCIDENCE, EMISSION, AND PHASE ANGLE AND FOR ANY GIVEN    *
C**          VALUE OF THE MACROSCOPIC ROUGHNESS PARAMETER, THETA-BAR.  *
C** ------------------------------------------------------------------ *
C** INPUTS:                                                            *
C**      I = INCIDENCE ANGLE IN DEGREES                                *
C**      E = EMERGENCE ANGLE IN DEGREES                                *
C**      G = PHASE ANGLE IN DEGREES                                    *
C**   TBAR = AVERAGE MACAROSCOPIC SLOPE ANGLE (IN DEGREES)             *
C**                                                                    *
C***********************************************************************
C* PAUL HELFENSTEIN  REV:0 4/8/88  CANNIBALIZED FROM ORIGINAL REFLCT   *
C***********************************************************************
C
      SUBROUTINE ROUGH(I,E,G,TBAR,SFNCT,SMU0,SMU)
C
        REAL*4 I
	REAL*8 MU0,MU01,MU02,MU1,MU11,MU12,PI,I0,E0,G0,THEBAR
        DOUBLE PRECISION COSI,SINI,COSE,SINE,COTI,COTE,COTT,TANT,
     *  BETA,PSI,COSPSI,SINPS2,F,Y,EE1,EE2,EI1,EI2,XI1,XI2,XI3
C
C------- DETERMINE PRINCIPAL VARIABLE VALUES -------------------------
C
        PI=3.1415926536D+00

	I0 = DBLE(I)*PI/1.8D+02
	E0 = DBLE(E)*PI/1.8D+02
	G0 = DBLE(G)*PI/1.8D+02
	THEBAR = DBLE(TBAR)*PI/1.8D+02
C
	COSI = DCOS(I0)
	COSE = DCOS(E0)
	COSG = DCOS(G0)
C
	SINI = DSIN(I0)
	SINE = DSIN(E0)
	SING = DSIN(G0)
C
C
C------------- IF THETA-BAR = 0, THEN ROUGH = 1.0
C
      IF(TBAR.NE.0.0)GO TO 1
         SFNCT = 1.0
         SMU0 = SNGL(COSI)
         SMU  = SNGL(COSE)
         RETURN
C
C-------------- IF THETA-BAR NOT 0, FIND EFFECTIVE MU'S AND MU0'S
C
    1   IF(I0.NE.(0.0).AND.E0.NE.(0.0))GO TO 5
C
		COSPSI=1.0D+00
		GO TO 8
C
    5	COSPSI = (COSG-COSI*COSE)/(SINI*SINE)
C
    8	IF(COSPSI.GE.1.0D+00)COSPSI=1.0D+00
	IF(COSPSI.LE.-1.0D+00)COSPSI=-1.0D+00
C
C---- VALUES FOR DELTA VARIABLES: FIRST NON-CASE DEPENDANT TERMS---
C
	PSI = DACOS(COSPSI)
	SINPS2 = DSIN(0.5D+00*PSI)
	SINPS2 = SINPS2*SINPS2
C
	IF(DABS(PSI).LT.(3.10D+00))GO TO 102
C
	   F = 0.0D+00
	   GO TO 105
C
  102  F = DTAN(0.5*PSI)
       F = DEXP(-2.0D+00*F)
C
  105 IF(THEBAR.GT.1.4D+00)THEBAR = 1.4D+00
      TANT = DTAN(THEBAR)
      COTT = 1.0D+00/TANT
      BETA = DSQRT(1.0D+00 + PI*TANT*TANT)
C
C------------------------------------------------------------------
C---- NOW, DETERMINE WHICH CASE IS APPROPRIATE FOR THE MODEL ------
C----      CASE #1: I<=E      CASE #2: I>=E                  ------
C------------------------------------------------------------------
C
C------- EXPONENTIAL TERMS FIRST --------------------------
C
      IF(DABS(SINE).LT.(0.1D-03))GO TO 110
	 COTE = COSE/SINE
	    EE2 = -2.0D+00*COTT*COTE/PI
	    EE1 = -0.25D+00*PI*EE2*EE2
	 IF(EE1.GT.(88.0))EE1=88.0
	 IF(EE2.GT.(88.0))EE2=88.0
	    EE1 = DEXP(EE1)
	    EE2 = DEXP(EE2)
	    IF(EE1.GT.(1.0D+20))EE1=1.0D+20
            IF(EE2.GT.(1.0D+20))EE2=1.0D+20
	    GO TO 120
C
  110	 EE1 = 0.0
	 EE2 = 0.0
C
  120 IF(DABS(SINI).LT.(0.1D-03))GO TO 130
	 COTI = COSI/SINI
	    EI2 = -2.0D+00*COTT*COTI/PI
	    EI1 = -0.25D+00*PI*EI2*EI2
	 IF(EI1.GT.(88.0))EI1 = 88.0
	 IF(EI2.GT.(88.0))EI2 = 88.0
	    EI1 = DEXP(EI1)
	    EI2 = DEXP(EI2)
	    IF(EI1.GT.(1.0D+20))EI1=1.0E+20
            IF(EI2.GT.(1.0D+20))EI2=1.0E+20
	    GO TO 140
C
  130	EI1 = 0.0
	EI2 = 0.0
	GO TO 140
C
  140 MU0 = COSI
      MU1 = COSE
      MU02 = (COSI + SINI*TANT*(EI1/(2.0D+00-EI2)))/BETA
      MU12 = (COSE + SINE*TANT*(EE1/(2.0D+00-EE2)))/BETA
C
      IF(I.GT.E)GO TO 400
       XI2 = 2.0D+00 - EE2 - (PSI/PI)*EI2
       XI1 = (COSPSI*EE1 + SINPS2*EI1)
         MU01 = (COSI + SINI*TANT*(XI1/XI2))/BETA
       XI3 = (EE1 - SINPS2*EI1)
         MU11 = (COSE + SINE*TANT*(XI3/XI2))/BETA
C
	Y = BETA - F*BETA + F*(MU0/MU02)
		GO TO 500
C
C>>>>>>>>>> CASE #2:
C
  400 XI2 = 2.0D+00 - EI2 - (PSI/PI)*EE2
       XI1 = (EI1 - SINPS2*EE1)
        MU01 = (COSI + SINI*TANT*(XI1/XI2))/BETA
       XI3 = (COSPSI*EI1 + SINPS2*EE1)
        MU11 = (COSE + SINE*TANT*(XI3/XI2))/BETA
C
C
	Y = BETA - F*BETA + F*(MU1/MU12)
C
C--------- NOW, CALCULATE THE COMPLETE PHOTOMETRIC REFLECTANCE ------
C
  500  SFNCT = SNGL((MU0/MU02)*(MU11/MU12)/Y)
       SMU0 =  SNGL(MU01)
       SMU  =  SNGL(MU11)

       if ( smu0.lt.-.999 .or. smu.lt.-.999
     &  .or. smu0.gt..999 .or. smu.gt..999) then
         smu0 = max( -.999, min( .999, smu0))
         smu = max( -.999, min( .999, smu))
       endif
C
	RETURN
	END
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:: THIS PROGRAM COMPUTES AN INTEGRAL DISK BRIGHTNESS AND MAGNITUDE FROM A:
C::      SET OF HAPKE PHOTOMETRIC PARAMETERS (W,H,B,C,THETA-BAR)          :
C::                                                                       :
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:: PROGRAMMER: DAMON P. SIMONELLI       (ORIGINALLY FOR SPIF COMPUTER)   :
C:: REVISED BY: PAUL HELFENSTEIN          FOR USE ON GALILEO VAX          :
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C:: NOTES:  ALPHA  = PHASE ANGLE IN DEGREES                               :
C::         MAG    = BRIGHTNESS (ABSOLUTE MAGNITUDE)                      : 
C::         RATIO  = BRIGHTNESS RELATIVE TO ZERO DEGREES PHASE ANGLE      :
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
      SUBROUTINE DISKNT(ALPHA,LSTFUN,XPAR,HAPINT,MAG)
	REAL GAUORD(8),GAUWT(8),CHEORD(4),CHEWT(4)
	REAL ALPHA,MAG,RATIO,HAPINT
	REAL MUN,MU,IFATPT,IFDI,IFSMOO,K,INTEG1,INTEG2
        DIMENSION XPAR(14),PTERMS(10),LSTFUN(20)
C
C
	DATA GAUORD/-.9602899,-.7966665,-.5255324,-.1834346,
     1  	     .1834346,.5255324,.7966665,.9602899/
	DATA GAUWT/.1012285,.2223810,.3137066,.3626838,
     1               .3626838,.3137066,.2223810,.1012285/
C
	DATA CHEORD/.9396926,.7660444,.5000000,.1736482/
	DATA CHEWT/.04083295,.1442256,.2617994,.3385402/
C
	PI=3.141592654
	CF=PI/180.
C
C----------------------------- UNLOAD XPAR ARRAY INTO HAPKE PARAMETERS
C
       LOAD=2
       CALL SHUFLE(LOAD,XPAR,W,H,TBAR,B0,PTERMS)
C
C----------------------------- LOAD ARRAYS WITH VALUES OF PHASE ANGLE AND
C                              LOG OF MAGNITUDE
C
	SUM=0.
	DO 483 LONG=1,8
C
		RHO=GAUORD(LONG)
		Q=(COS(ALPHA*CF)+1.)/2.
		R=(COS(ALPHA*CF)-1.)/(COS(ALPHA*CF)+1.)
		PHI=ASIN(Q*(RHO-R))/CF
		IF(PHI.LT.-90.) PHI=-180.-PHI
		IF(PHI.GT.90.) PHI=180.-PHI
C
		DO 482 LAT=1,4
C
			TAU=CHEORD(LAT)
			THETA=ASIN(TAU)/CF
			IF(THETA.LT.-90.) THETA=-180.-THETA
			IF(THETA.GT.90.) THETA=180.-THETA
C
	     MUN=COS((PHI-ALPHA)*CF)*COS(THETA*CF)
	     MU=COS(PHI*CF)*COS(THETA*CF)
                   ANGI = ACOS(MUN)/CF
                   ANGE = ACOS(MU)/CF
                   ANGG = ALPHA
C
        IFATPT = REFLCT(ANGI,ANGE,ANGG,LSTFUN,W,H,B0,TBAR,PTERMS)
C
	     SUM=SUM+(GAUWT(LONG)*CHEWT(LAT)*IFATPT)
C
482			CONTINUE
C
483		CONTINUE
C
	HAPINT = ((COS(ALPHA*CF)+1.)/PI)*SUM
        IF(HAPINT.LE.0.0)GO TO 1484
	MAG = -2.5*ALOG10(HAPINT)
        RETURN
1484    MAG=50.0
C
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	RETURN
	END
Cunix        SUBROUTINE CHIME(J)
Cunix        LOGICAL*1 BELL
Cunix        BELL=007
Cunix        DO 20 IBEEP=1,J
Cunix        WRITE(6,10)BELL
Cunix10      FORMAT('+',A1)
Cunix20      CONTINUE
Cunix        RETURN
Cunix        END
C-------------------------------------------------------------------------
C--  SCANLIM -  USED FOR GRID SEARCH ON PARAMETERS.  USER SETS
C---            o which parameters are being searched
C---            o lower and upper limits for each parameter (scan range)
C---            o scan increment for each parameter 
C-------------------------------------------------------------------------
C---   4-MAR-89   REV: 0    CANNIBALIZED FROM SUBROUTINE PARWGT
C-------------------------------------------------------------------------
C
        SUBROUTINE SCANLIM(LSTFUN,LISPAR,SCNFLG,XLOW,XHIGH,XINC)
C
C
        CHARACTER*4 ACTIVE,ON,OFF
        DIMENSION SCNFLG(14),LISPAR(14),XLOW(14),XHIGH(14),
     *   XINC(14),PTERMS(10),LSTFUN(20)
C
        DATA ON,OFF/'ON  ','OFF '/
C
           NPARS = LSTFUN(1)
           NEWH = LSTFUN(2)
           NEWBG = LSTFUN(3)
           NPHASE = LSTFUN(4)
           NTERMS = LSTFUN(5)
C
       IF(IPASS.EQ.1)GO TO 10
          DO 600 J=1,14
  600     SCNFLG(J)=0.0
          IPASS = 1
       print*,' '
       print*,'--------------------------------------------------------'
       print*,'-------- GRID-SEARCH: SELECTION OF ACTIVE --------------'
       print*,'-------- GRID-SEACH PARAMETERS AND THEIR  --------------'
       print*,'-------- SCAN RANGES AND INCREMENTS       --------------' 
       print*,'--------------------------------------------------------'
       print*,'     '
C
  100 print*,'              '
      print101
  101 FORMAT(1X,'XPAR',28X,'STATUS',4X,'LOWER',4X,'UPPER',4X,
     *          'PARAMETER',/,1X,'INDEX',37X,'LIMIT',4X,'LIMIT',
     *           4X,'INCREMENT',/)
      DO 300 J=1,LSTFUN(1)
      IF(LISPAR(J).EQ.0)GO TO 300
            ACTIVE = OFF
         IF(SCNFLG(J).EQ.1.0)ACTIVE=ON
         GO TO (110,120,130,140,150,150,150,150,150,150,150,150,
     *          150,150),LISPAR(J)
  110 IF(ACTIVE.EQ.OFF)print111,ACTIVE
      IF(ACTIVE.EQ.ON)print112,ACTIVE,XLOW(J),XHIGH(J)
     *                               ,XINC(J)
  111 FORMAT(3X,'1.   SINGLE-SCATTERING ALBEDO:  ',A4)
  112 FORMAT(3X,'1.   SINGLE-SCATTERING ALBEDO:  ',A4,
     *       3X,F6.4,3X,F6.4,5X,F7.5)
      GO TO 300
  120 IF(ACTIVE.EQ.OFF)print121,ACTIVE
      IF(ACTIVE.EQ.ON)print122,ACTIVE,XLOW(J),XHIGH(J),
     *                         XINC(J)
  121 FORMAT(3X,'2.   PARTICLE COMPACTION:       ',A4)
  122 FORMAT(3X,'2.   PARTICLE COMPACTION:       ',A4,
     *        3X,F6.4,3X,F6.4,5X,F7.5)
      GO TO 300
  130 IF(ACTIVE.EQ.OFF)print131,ACTIVE
      IF(ACTIVE.EQ.ON)print132,ACTIVE,XLOW(J),XHIGH(J),XINC(J)
  131 FORMAT(3X,'3.   MACROSCOPIC ROUGNNESS:     ',A4)
  132 FORMAT(3X,'3.   MACROSCOPIC ROUGNNESS:     ',A4,
     *       4X,F5.2,4X,F5.2,7X,F5.2)
      GO TO 300
  140 IF(ACTIVE.EQ.OFF)print141,ACTIVE
      IF(ACTIVE.EQ.ON)print142,ACTIVE,XLOW(J),XHIGH(J),XINC(J)
  141 FORMAT(3X,'4.   OPPOSITION AMPLITUDE:      ',A4)
  142 FORMAT(3X,'4.   OPPOSITION AMPLITUDE:      ',A4,
     *       3X,F6.4,3X,F6.4,5X,F7.5)
      GO TO 300
  150 GO TO (160,170,180,188),NPHASE
  160 JTERM=LISPAR(J)-4
      IF(ACTIVE.EQ.OFF)print161,J,JTERM,ACTIVE
      IF(ACTIVE.EQ.ON)print162,J,JTERM,ACTIVE,XLOW(J),XHIGH(J),XINC(J)
  161 FORMAT(2X,I2,'.   PARTICLE PHASE P(',I2,'):      ',A4)
  162 FORMAT(2X,I2,'.   PARTICLE PHASE P(',I2,'):      ',A4,
     *       2X,F7.4,2X,F7.4,4X,F8.5)
      GO TO 300
  170 IF(ACTIVE.EQ.OFF)print171,ACTIVE
      IF(ACTIVE.EQ.ON)print172,ACTIVE,XLOW(J),XHIGH(J),XINC(J)
  171 FORMAT(3X,'5.   HENYEY-GREENSTEIN g:       ',A4)
  172 FORMAT(3X,'5.   HENYEY-GREENSTEIN g:       ',A4,
     *      2X,F7.4,2X,F7.4,5X,F7.4)
      GO TO 300
  180 JTERM = LISPAR(J)-4
      IF(JTERM.EQ.3)GO TO 185
  182 IF(ACTIVE.EQ.OFF)print183,J,JTERM,ACTIVE
      IF(ACTIVE.EQ.ON)print184,J,JTERM,ACTIVE,XLOW(J),XHIGH(J),XINC(J)
  183 FORMAT(3X,I1,'.   HEYNEY-GREENSTEIN g',I1,':      ',A4)
  184 FORMAT(3X,I1,'.   HEYNEY-GREENSTEIN g',I1,':      ',A4,
     *       4X,F7.4,4X,F7.4,4X,F7.4)
      GO TO 300
  185 IF(ACTIVE.EQ.OFF)print186,J,ACTIVE
      IF(ACTIVE.EQ.ON)print187,J,ACTIVE,XLOW(J),XHIGH(J),XINC(J)
  186 FORMAT(3X,I1,'.   PARTITION COEFFICIENT:     ',A4)
  187 FORMAT(3X,I1,'.   PARTITION COEFFICIENT:     ',A4,
     *       4X,F7.5,4X,F7.5,4X,F7.5)
      GO TO 300
  188 IF(ACTIVE.EQ.OFF)print189,J,ACTIVE
      IF(ACTIVE.EQ.ON)print191,J,ACTIVE,XLOW(J),XHIGH(J),XINC(J)
  189 FORMAT(3X,I1,'.   VALUE OF P(G):             ',A4)
  191 FORMAT(3X,I1,'.   VALUE OF P(G):             ',A4,
     *       4X,F7.4,4X,F7.4,4X,F7.4)
C
  300 CONTINUE
C
      print*,' '
      print*,' '
      print320
  320 FORMAT(1X,/,
     *     1X,'o To toggle bewteen ON and OFF, enter XPAR index',
     *   /,1x,'o To reset limits, toggle off then toggle on again',
     *   /,1x,'o To EXIT, enter 0',
     *   //,25x,'OPTION: ',$)
      read(*,*),LPAR
      IF(LPAR.EQ.0)GO TO 10
      IF(SCNFLG(LPAR).EQ.0.0)GO TO 330
         SCNFLG(LPAR)=0.0
         XLOW(LPAR)=0.0
         XHIGH(LPAR)=0.0
         XINC(LPAR)=0.0
         GO TO 100
  330    SCNFLG(LPAR)=1.0
         print325
  325  FORMAT(1X,///,2X,
     *        'o SET PARAMETER LIMITS AND PARAMETER INCREMENT',
     *      /,2X,'  (enter minimum, maximum, increment): ',$)
       read(*,*),XLOW(LPAR),XHIGH(LPAR),XINC(LPAR)
         GO TO 100
C
   10 RETURN
      END
