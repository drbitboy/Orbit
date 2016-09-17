      program cktimes
      implicit none
      character*32 sclk, utc
      integer sc,hand01,icd(6), nrec, i, len1, len2, iplus
      double precision et, descr(5), dcd(2), record(8)
      logical found
      integer ioout
      character*255 inline
C
      integer lastch
      external lastch
c
c set error response action
c
c      call erract('set','ignore')
c      write(6,*)' error response action set' 
c
c load SCLK, leapseconds & C kernels
c
      call doinp( 'Input SCLK filename', inline, 'sclk', i)
      call ldpool(inline(1:i))
C
      call doinp( 'Input LEAPSECONDS kernel filename'
     &          , inline, 'leapseconds', i)
      call ldpool(inline(1:i))
C
      call doinp( 'Input C-kernel filename', inline, 'ck', i)
      call cklpf( inline(1:i), hand01)     
C
      call doinp( 'Input SC name or code e.g. "near" or -77'
     &          , inline, 'near', i)
      call bodn2c( inline(1:i), sc, found)
      if ( .not. found) read( inline(1:i), '(i)') sc
C
      call doinp( 'Output file name; "-" for stdout'
     &          , inline, '-', i)
      if ( inline(1:i) .ne. '-') then
        call getlun( ioout)
        open( ioout, file=inline(1:i), status='unknown')
        rewind( ioout)
      else
        ioout = 6
      endif
C
C begin fwd search, get & unpack seg descr, check for # of records
C
      call dafbfs(hand01)
      call daffna( found)
      iplus = 1
      dowhile ( found)
        call dafgs( descr)
        call dafus( descr, 2, 6, dcd, icd)
        if ( icd(3) .gt. 3 .or. icd(3) .lt. 1) stop '~123'
        if ( icd(3) .eq. 1) call cknr01( hand01, descr, nrec)
        if ( icd(3) .eq. 2) call cknr02( hand01, descr, nrec)
        if ( icd(3) .eq. 3) call cknr03( hand01, descr, nrec)
        print*, 'Instrument ', icd(1), '; CK Type ', icd(3)
        do i=1,nrec
          if ( icd(3) .eq. 1) call ckgr01(hand01, descr, i, record)
          if ( icd(3) .eq. 2) call ckgr02(hand01, descr, i, record)
          if ( icd(3) .eq. 3) call ckgr03(hand01, descr, i, record)
          call scdecd( sc, record(1), sclk)
          call sct2e( sc, record(1), et)
          call et2utc( et, 'ISOC', 3, utc)
C
          write( inline, '(1x, i5)') iplus
          len1 = lastch(inline) + 1
          inline(len1:) = ' '
          len1 = len1 + 1
C
          write( inline(len1:), '(1x,f14.0)') record(1)
          len1 = lastch(inline) + 1
          inline(len1:) = ' '
          len1 = len1 + 1
C
          write( inline(len1:), '(1x,f14.2)') et
          len1 = lastch(inline) + 1
          inline(len1:) = ' '
          len1 = len1 + 1
C
          len2 = lastch(sclk)
          inline(len1:) = sclk(:len2) // '  '
          len1 = lastch(inline) + 2
          len2 = lastch(utc)
          inline(len1:) = utc(:len2) // '  '
          len1 = lastch(inline)
C
          write(ioout, '(a)') inline(:len1)
          iplus = iplus + 1
        enddo
        call daffna( found)
      enddo 
      if ( ioout .ne. 6) then
        close( ioout)
        call frelun( ioout)
      endif
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      integer function lastch( c)
      implicit none
C
      character*1 sp, tab, nul
C
      character*(*) c
      integer i
      logical isblank
      logical first
      data first / .true. /
      save
C
      if ( first) then
        first = .false.
        sp=char(32)
        tab=char(9)
        nul=char(0)
      endif
C
      i = len( c)
      isblank = c(i:i).eq.sp .or. c(i:i).eq.tab .or. c(i:i).eq.nul
      dowhile ( i .gt. 0 .and. isblank)
        i = i - 1
        if ( i .gt. 0) then
          isblank = c(i:i).eq.sp .or. c(i:i).eq.tab .or. c(i:i).eq.nul
        endif
      enddo
      lastch = i
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine doinp( prom, inline, dflt, ilen)
      implicit none
C
      character*(*) prom
      character*(*) inline
      character*(*) dflt
      integer ilen
C
      integer lastch
      external lastch
C
      inline = ' '
      print'(1x,a,a,a,a)', prom, ' (dflt=', dflt, ')'
      read(*, '(a)') inline
      ilen = lastch(inline)
      if ( ilen .eq. 0) then
        inline = dflt
        ilen = lastch(inline)
      endif
      return
      end
