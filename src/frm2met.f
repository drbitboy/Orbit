      program frm2met
C
C     f77 -o frm2met frm2met.f {spicelibdir}/spicelib.a -lm
C
      implicit none
C
      character*255 filnam, inpline
      character*40 utc
      integer inlun, outlun, i, j, scid
      doubleprecision et, met
C
      scid = -93
C
      print*, ' Enter name of SCLK file: '
      read( 5, '(a)') filnam
      call ldpool( filnam)
C
      print*, ' Enter name of LEAPSECONDS file: '
      read( 5, '(a)') filnam
      call ldpool( filnam)
C
      print*, ' Enter name of frames file:'
      read( 5, '(a)') filnam
      if ( filnam(:2) .eq. '- ') then
        inlun = 5
      else
        call getlun( inlun)
        open( inlun, file=filnam, status='old')
      endif
C
      print*, 'Enter name of output file:'
      read( 5, '(a)') filnam
      if ( filnam(:2) .eq. '- ') then
        outlun = 6
      else
        call getlun( outlun)
        open( outlun, file=filnam, status='unknown')
      endif
C
      dowhile ( .true.) 
        inpline = ' '
        read( inlun, '(a)', end=99999) inpline
        if ( inlun .ne. 5) then
          i = index( inpline, ',') - 3
          if ( i .gt. 1) then
          if ( inpline(i:i+3) .eq. ':et,') then
            read( inpline(:12), '(f12.0)') et
            call sce2t( -93, et, met)
            write( outlun, '(f18.3)') met/1000.0
          endif
          endif
        else
          if ( inpline(:2) .eq. 'et' .or. inpline(:2) .eq. 'ET') then
            read( inpline(3:), '(f12.0)') et
          else 
            if ( inpline(:3).eq.'utc' .or. inpline(:3).eq.'UTC') then
              call utc2et( inpline(4:), et)
            else
              print*
     &        , 'BAD FORMAT, e.g. "utc1999-001 //" or "et -3.14159e7"'
              et = 0.0
            endif
          endif
          call et2utc( et, 'ISOC', 3, utc)
          print*, utc
          call et2utc( et, 'ISOD', 3, utc)
          print*, utc
          call sce2t( -93, et, met)
          write( outlun, '(f18.3)') met/1000.0
          if ( outlun .ne. 6) print'(f18.3)', met/1000.0
        endif
      enddo
99999 continue
      if ( inlun .ne. 5) then
        close( inlun)
        call frelun( inlun)
      endif
      if ( outlun .ne. 6) then
        close( outlun)
        call frelun( outlun)
      endif
      end
