C kmet2cal:  convert kMET to Calendar DOY format for NEAR
C
C Standard input:
C   Leapseconds file (blank/hit Enter defaults to /near2/SpiceOps/.../near.tls)
C   SCLK file (blank/hit Enter defaults to /near2/SpiceOps/.../near.tls)
C     More kernel pool files; loop until Hit Enter or blank 1st char entered
C   kMETSTART1   kMETEND1
C   kMETSTART2   kMETEND2
C   End of file or ^D/^Z
C
C Standard output (Excluding '^ *Enter ...' prompts):
C
C   kMETSTART1   kMETEND1    UTCinDOYformat1  UTCinDOYformat2
C   kMETSTART2   kMETEND2    UTCinDOYformat2  UTCinDOYformat2
C   ...
C
C
      program kmet2cal
      implicit none
      character*32 intim, outtim
      character*255 intims, outtims
      character*255 pfx 
      character*255 fil 
      character*255 opts(3)
      integer iopt
      character*255 prompt(3)
      integer pprompt, popt
      integer pfxins
      integer metins
      integer scid
      double precision et
      integer instart, inend
      integer outstart, outend
      integer s, e
      save
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      pfx = '/near2/SpiceOps/ops/current/ '
      scid = -93
      pfxins = index( pfx, ' ')

      prompt(1) = 'Enter leapseconds file ['
      opts(1) = 'near.tls'

      prompt(2) = 'Enter SCLK file ['
      opts(2) = 'near.tsc'

      prompt(3) = 'Enter any other kernel pool file ['
      opts(3) = ' Hit enter to start entering kMETs '
C
      iopt = 1
      fil(:1) = '.'
      dowhile ( fil(:1) .ne. ' ')
      
        pprompt = index(prompt(iopt), '[')
        popt = index( opts(iopt), ' ')

        print*, prompt(iopt)(:pprompt) // pfx(:pfxins-1) 
     &       // opts(iopt)(:popt) // ']:'
        read(*,'(a)') fil

        if ( fil(:1) .eq. ' ' .and. opts(iopt)(:1) .ne. ' ') then
          fil = pfx
          fil(pfxins:) = opts(iopt)
        endif
      
        if ( fil(:1) .ne. ' ') call ldpool( fil)

        if ( opts(iopt)(:1) .ne. ' ') iopt = iopt + 1
      enddo
C
00001 continue
      print*, 'Enter time (^D/^Z to exit):'
      read(*,'(a)', end=99999) intims
C
C                123
      outtims = '   '
      outstart = 4
C
      instart = 1
      call nexttok( intims, instart, inend)
      e = 1
C
      dowhile (instart .le. inend)
        intim = intims(instart:inend) // '000'
C
C       convert from kMET to MET (append 000) & convert to DOY UTC
C
        call scs2e( scid, intim, et)
        call et2utc( et, 'ISOD', 0, outtim)
C
C       get DOY UTC start and end characters
C
        s = 1
        call nexttok( outtim, s, e)
C
C       append DOY UTC and 3 spaces to outtims
C
C                                            123
        outtims(outstart:) = outtim(s:e) // '   '
C
C       find next outtims insertion point
C
        call nexttok( outtims, outstart, e)
        outstart = e + 4
        if ( outstart .gt. len(outtims)) outstart = len(outtims)
C
C       save inend, look for next intims token
C
        e = inend
        instart = inend + 1
        call nexttok( intims, instart, inend)
      enddo
C
      if ( outstart .gt. 1) outstart = outstart - 1
C
C     ouput intims (through last valid inend), append outtims
C
      print*, intims(:e), outtims(:outstart)
      goto 00001
C
99999 continue
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine nexttok( c, s, e)
      implicit none
      character*(*) c
      integer s, e
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      if ( s .gt. len(c)) then
        s = len(c)
        e = s - 1
        return
      endif
C
      e = s - 1
C
      dowhile ( s .lt. len(c) .and. c(s:s) .eq. ' ')
        s = s + 1
      enddo
C
      if ( c(s:s) .eq. ' ') return
C
      e = s
      dowhile ( e .lt. len(c) .and. c(e:e) .ne. ' ')
        e = e + 1
      enddo
C
      if ( c(e:e) .eq. ' ') e = e - 1
C
      return
      end
