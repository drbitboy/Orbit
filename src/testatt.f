      program astatt
      implicit none

      doubleprecision tipm(3,3)
      doubleprecision et

      integer eros
      integer ihandl

      character*40 utc

      call clpool()
      print*, 'loading NEARTOP/cases/leapseconds '
     &// ' & KERNELTOP/case1/astatt.bpc ...'
      call ldpool( 'NEARTOP/cases/leapseconds')
      call pcklof( 'KERNELTOP/case1/astatt.bpc', ihandl)

      print*, 'load text pck eros.tpc?'
      read(*, '(a)') utc
      if ( utc(:1) .eq. 'y') then
        print*, 'loading eros.tpc...'
        call ldpool( 'eros.tpc')
      endif

      print*, 'load spk sceph.bsp?'
      read(*, '(a)') utc
      if ( utc(:1) .eq. 'y') then
        print*, 'loading spk sceph.bsp...'
        call spklef( 'KERNELTOP/case1/sceph.bsp', ihandl)
      endif

      eros = 2000433
      dowhile ( .true.)
        print*, 'enter utc:'
        read( *, '(a)', end=99999) utc
        call utc2et( utc, et)
        call et2utc( et, 'ISOC', 2, utc)
        print*, 'utc:       ', utc
        print*, 'et:        ', et
        call bodmat( eros, et, tipm)
        print*, 'TIPM:'
        print80000, tipm
80000   format( 1x, 3g18.10)
      enddo
99999 continue
      end
