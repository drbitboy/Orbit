C$Procedure   getckav ( get pointing & angular velocities )
C
      PROGRAM getckav
      implicit none
C
C$ Abstract
C
C     Print out body or spacecraft attitude info
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
C        - The name(s) of text kernel files for the kernel pool
C        - The name of a NAIF CK file
C        - NAIF ID for the spacecraft instrument or body
C        - A SCLK string at which to begin
C        - A delta sclk (may be negative)
C        - The number of sclk's at which to output information
C
C$ Output
C
C     The program outputs the J2000 inertial coordinates of
C     the major axes of the requested reference frame in RA & DEC, and 
C     also the RA, DEC, & magnitude of the angular velocity vector
C
C$ Particulars
C
C     For the sake of brevity, this program does NO error checking
C     on its inputs. Mistakes will cause the program to crash. But,
C     since this is an example program, that's OK.
C
C$ References
C
C        KERNEL        The SPICELIB Kernel Pool
C        ROTATIONS     Rotations
C        CK            C- Kernel (CK) Specification
C        TIME          Time routines in SPICELIB
C
C     For questions about a particular subroutine, refer to its
C     header.
C
C$ Version
C
C-    Beta Version 1.0.0, 23-DEC-1998 (BTCarcich, Cornell University)
C
C-&
 
C
C     SPICELIB functions
C
      doubleprecision DPR, twopi, halfpi
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
      CHARACTER*(LONG) CKFIL, leapsc, outfil, tmpstr
      CHARACTER*(LONG) inpline
      CHARACTER*(MED)  sclkch, utc
      CHARACTER*(MED)  frame
      character*1 contin
 
      DOUBLEPRECISION sclkdp0, sclkdp, sclkdpout, delsclk
      DOUBLE PRECISION      radec(2,4), range
      DOUBLE PRECISION      cmat     ( DIM, DIM )
      DOUBLEPRECISION av(DIM), et
 
      INTEGER ptok(6), pmin
      INTEGER numsclk, nsclk
      INTEGER scid, instr, i, handle, itok, iolun, inext, ilen, toklen
C
      logical found
C
      external lastch, parsit
      integer lastch, parsit
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Load text kernel files into kernel pool
C
      WRITE (*,*)
      WRITE (*,'(a)') 'Suggested TK''s:'
      WRITE (*,'(a)') '../data/leapseconds'
      WRITE (*,'(a)') '../data/sclk'
      WRITE (*,'(a)') '../data/nis_frame/msi13.ti'
      WRITE (*,'(a)') '?'
      call clpool
      dowhile ( .true.)
      WRITE (*,*)
      WRITE (*,*) 'Enter a leapsecond, sclk, & instr TK file'
     &         // '(^D/? to end):'
      READ  (*,FMT='(A)',end=99989) leapsc
      IF( leapsc(1:1) .eq. '?' .or. leapsc(2:2) .eq. '?') goto 99989
      CALL ldpool ( leapsc )
      enddo
99989 continue
C
C     Load the CK file(s).
C
      WRITE (*,*)
      WRITE (*,'(a)') 'Suggested CK''s:'
      WRITE (*,'(a)') 'near_1998354_v01.bc'
      WRITE (*,'(a)') 'near_1998356_v01.bc'
      WRITE (*,'(a)') 'near_1998357_v01.bc'
      WRITE (*,'(a)') '?'
      dowhile ( .true.)
      WRITE (*,*)
      WRITE (*,*) 'Enter the name of a binary CK file(^D/? to end):'
      READ  (*,FMT='(A)',end=99990) CKFIL
      IF( CKFIL(1:1) .eq. '?' .or. CKFIL(2:2) .eq. '?') goto 99990
      CALL CKLPF ( CKFIL, HANDLE )
      enddo
99990 continue
C
C     Begin the continuation loop.
C
      CONTIN = 'Y'
      DO WHILE ( CONTIN .EQ. 'Y' )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc72
         WRITE (*,*)
         WRITE (*,*) 'Sample instrument, start, delta, # SCLKs, file'
         WRITE (*,*) '-93000,6/0089601151161,-1,10,-'
         WRITE (*,*)
         WRITE (*,*) 'Enter the NAIF ID for the instrument'
     &            // ', the start, delta # SCLKs, output file '
     &            // '("-" => stdout)'
         inpline = ' '
         READ  (*,'(a128)') inpline
C
C        Parse input
C
         ilen = lastch( inpline)
         inext = 0
         toklen = parsit( inpline(:ilen), '-93000', tmpstr, inext)
         read( tmpstr(:toklen), *) instr
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC72
         toklen = parsit(inpline(:ilen),'6/0089601151161',sclkch,inext)
         toklen = parsit( inpline(:ilen), '-10000', tmpstr, inext)
         read( tmpstr(:toklen), *) delsclk
         toklen = parsit( inpline(:ilen), '100', tmpstr, inext)
         read( tmpstr(:toklen), *) numsclk
         toklen = parsit( inpline(:ilen), '-', outfil, inext)
         if ( toklen .ne. 1 .or. outfil(:1) .ne. '-') then
           call getlun( iolun)
           open( iolun, file=outfil(:toklen), status='unknown')
         else
           iolun = 6
         endif
C
         call ckmeta( instr, 'SCLK', scid)
         call scencd( scid, sclkch, sclkdp0)
C
         FRAME  = 'J2000'
C
C        DELTA is the increment between consecutive times.
C
C        Make sure that the number of sub-observer point calculations
C        is >= 1, to avoid a division by zero error.
C
         numsclk = MAX(1,numsclk)
C
C        Now, everything is set up.
C
         sclkdp  = sclkdp0
         nsclk   = 1
 
         DO WHILE ( nsclk .LE. numsclk )
            call ckgpav( instr, sclkdp, delsclk, frame, cmat, av
     &                 , sclkdpout, found)
C
            if ( found) then
              call sct2e( scid, sclkdpout, et)
              call et2utc( et, 'ISOD', 3, utc)
C
C             invert so cmat & av are Instrument-to-inertial
C               i.e. so cmat(1:3,i) are i'th major axis in inertial coordinates
C             and convert to RA, DEC in degrees
C
              call xpose( cmat, cmat)
              call vminus( av)
C
              do i=1,3
                call recrad( cmat(1,i), range, radec(1,i), radec(2,i))
              enddo
              call recrad( av, range, radec(1,4), radec(2,4))
C
              do i=1,4
                radec(1,i) = radec(1,i) * DPR()
                radec(2,i) = radec(2,i) * DPR()
              enddo
C
C             Display results:
C
              write(iolun, '(2f15.0, 8f8.2, g17.10, f17.2, 1x, a)') 
     &          sclkdp, sclkdpout, radec, range, et, utc

            endif
            sclkdp = sclkdp + delsclk
            nsclk = nsclk + 1
         END DO
         if ( iolun .ne. 6) then
           close(iolun)
           call frelun( iolun)
         endif
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C get next character string between whitespace &/or commas - return length
C
      integer function parsit( chin, tokdflt, tokout, inext)
      implicit none
C
C     input string, default output token, output token
C     starting position (input), next string's starting position (output)
C
      character*(*) chin, tokdflt, tokout
      integer inext
C
      integer ibeg
      character*128 chlcl
C
C     TCS => delimiters (Tab, Comma, or Space)
      character*3 TCS
C      parameter (TCS = '	, ')
      integer ilen, commasofar
      integer ISTCS,I
      ISTCS(I) = index(TCS,chin(I:I))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC72
      TCS = char(9) // ', '
      chlcl = chin
      tokout = tokdflt
      parsit = min( len(tokout), len( tokdflt))
C
C     special case - inext = 0 => first parse => set commasofar so
C                    parsit will parse initial comma as a null first token
C
      if ( inext .eq. 0) then
        commasofar = 1
        inext = 1
      else
        commasofar = 0
      endif
C
      ibeg = inext
      ilen = len(chin)
      if ( ibeg .gt. ilen) return
C
C     find 1st non-delimiter, but only 1 comma
C
C      dowhile ( ibeg .lt. ilen .and. index(TCS,chin(ibeg:ibeg)) .ne. 0)
      dowhile ( ibeg .lt. ilen .and. ISTCS(ibeg) .ne. 0)
        if ( chin(ibeg:ibeg) .eq. ',') then
          if ( commasofar .eq. 0) then
            commasofar = 1
          else
            inext = ibeg
            return
          endif
        endif
        ibeg = ibeg + 1
      enddo
C
      inext = ibeg + 1
C
C     token starts at end of input string
C
      if ( ibeg .eq. ilen) then
C        if ( index(TCS,chin(ibeg:ibeg)) .ne. 0) return
        if ( ISTCS(ibeg) .ne. 0) return
        tokout = chin(ibeg:ibeg)
        parsit = 1
        return
      endif
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC72
C      dowhile ( inext.lt.ilen .and. index(TCS,chin(inext:inext)) .eq. 0)
      dowhile ( inext.lt.ilen .and. ISTCS(inext) .eq. 0)
        inext = inext + 1
      enddo
C
C     special case:  inext .eq. ilen and chin(inext:inext) is not delimiter
C
C      if ( index(TCS,chin(inext:inext)) .eq. 0) inext = inext + 1
      if ( ISTCS(inext) .eq. 0) inext = inext + 1
C
      parsit = min( len(tokout), inext - ibeg)
      tokout = chin(ibeg:inext-1)
      return
      end
