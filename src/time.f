      character*32 intim, outtim
      double precision et
C
      print*, 'Enter leapseconds file:'
      read(*,'(a)') intim
      call ldpool( intim)
C
00001 continue
      print*, 'Enter time (^D/^Z to exit):'
      read(*,'(a)', end=99999) intim
      if ( intim(:1) .ne. '-') then
        call utc2et( intim, et)
        print*, et
      else
        read(intim,*) et
      endif
      call et2utc( et, 'ISOC', 3, outtim)
      print*, outtim
      call et2utc( et, 'ISOD', 3, outtim)
      print*, outtim
      call et2utc( et, 'J', 7, outtim)
      print*, outtim
      goto 00001
C
99999 continue
      end
