      program testatt_lpp
      implicit none

      doubleprecision tipm(3,3)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      doubleprecision et0, etZero, jdZero
     &, vec0pm(3,2), vpole(3), vpm(3)
     &, llr(3,2), dllr
     &, delet, deletp, deletm, et
     &, vdot, dpr, twopi, spd, unitim
     &, delangbest, elonr2wlond
      external vdot, dpr, twopi, spd, unitim
     &, delangbest, elonr2wlond
      integer iter
C
C     Nominal rotation period = 5.27011 h
C
      doubleprecision NOMHPROT, NOMSPROT, NOMSPDEG, MINET
      parameter (NOMHPROT=5.27011D0 
     &        ,  NOMSPROT=NOMHPROT*36D2 
     &        ,  NOMSPDEG=NOMSPROT/36D1 
     &        ,  MINET=-2d100
     &        )

      doubleprecision calcspdeg(2)
      data calcspdeg / NOMSPDEG, NOMSPDEG /
      doubleprecision numrot, modet, calcsprot
C
      integer ihandl, iostat
C
      character*255 lin, tok1, tok2
      integer len0, len1, len2
      character*255 utc, utc1, utc2
      integer utclen, ul1, ul2
      character*255 spaces
      integer bodyids(2), nbody
C
      integer i, j
C
      doubleprecision id(3,3), mtx0(3,3), pm(3), perp(3)
      doubleprecision rng(2), ra(2), dec(2)
C
      doubleprecision d(9)
CCCCCC
      do i=1,255
        spaces(i:i) = ' '
      enddo
      nbody = 0
      et0 = MINET
      call ident( id)

      len0 = 0
      iostat = -1

      call clpool()

      etZero = -1e301

      dowhile ( .true.)
      lin = spaces
      read( *, '(a255)', end=99999) lin
      call token2( lin, tok1, tok2, len0, len1, len2)
      if ( len2 .gt. 0) then
        if ( tok1(:len1) .eq. 'PCK:') call ldpool( tok2(:len2))
        if ( tok1(:len1) .eq. 'LEAPSEC:') call ldpool( tok2(:len2))

        if ( tok1(:len1) .eq. 'ORIBPCK:') then
          call pcklof( tok2(:len2), ihandl)
        endif

        if ( tok1(:len1) .eq. 'ASTID:' .and. nbody .lt. 2) then
          nbody = nbody+1
          call obody( tok2(:len2), bodyids(nbody)) 
        endif

        if ( tok1(:len1) .eq. 'UTC:' .and. nbody .eq. 2) then
          utc = tok2
          utclen = len2
          call utc2et( utc(:len2), d(1))
          if ( etZero .le. -1e300) etZero = d(1)
          do j=1,36
            do i=1,nbody
              call bodmat( bodyids(i), d(1), tipm)
C
C             Convert PM & pole to J2000 RA,DEC
C             INPUT:  id(1,3) is the north pole, id(1,1) is the PM
C             OUTPUT: d(2/6) are the pole J2000 RA values
C             OUTPUT: d(3/7) are the pole J2000 DEC values
C             OUTPUT: d(4/8) are the PM J2000 RA values
C             OUTPUT: d(5/9) are the PM J2000 DEC values
C
              call mybodmat2radec( tipm, id(1,3), d(i*4-2), d(i*4-1))
              call mybodmat2radec( tipm, id(1,1), d(i*4), d(i*4+1))
C
C             Convert 90 deg east lon to J2000 vectors
C             id(1,2) is the body-fixed vector (0,1,0)
C
              if ( et0 .lt. (MINET/2)) then
                call mtxv( tipm, id(1,1), vec0pm(1,i))
              endif
            enddo
C                \do i=1,nbody
C
            if ( et0 .lt. (MINET/2)) et0 = d(1)
C
            call et2utc( d(1), 'ISOD', 1, utc)
            call token2( utc, utc1, utc2, utclen, ul1, ul2)
            print'(1x, f11.0, 8f9.4, 1x, a)', d, utc(:utclen)
C
C           add enough time for 10 degrees of rotation
C
            d(1) = d(1) + 1d1 * calcspdeg(1)
          enddo
C              \do j=1,36
C
C         find time to rotate pm to perp-to-pole nearest vec0pm
C
          do i=1,nbody
            calcsprot = 36d1 * calcspdeg(i)
C           add another rotation, get delta et
            et = (d(1) - et0) + calcsprot
            et = et - dmod( et, calcsprot)
            delet = delangbest( bodyids(i), et0+et, id, vec0pm(1,i))
     &              * calcspdeg(i)
C
            iter = 0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            dowhile ( abs(delet) .gt. abs(et*1d-10) .and. iter .lt. 50)
              iter = iter + 1
              deletp = delangbest( bodyids(i), et0+et+delet, id
     &                 , vec0pm(1,i)) * calcspdeg(i)
              deletm = delangbest( bodyids(i), et0+et-delet, id
     &                 , vec0pm(1,i)) * calcspdeg(i)
              if ( abs(deletp) .le. abs(deletm)) then
                et = et + delet
                delet = deletp
              else
                et = et - delet
                delet = deletm
              endif
            enddo
C                \dowhile (abs(delet) .gt. ...
C
C           get number of rotations
C
            modet = mod( et, calcsprot)
            numrot = (et-modet) / calcsprot
            if ( modet .ge. (calcsprot/2)) numrot = numrot + 1d0
            calcspdeg(i) = et / (numrot * 36d1)
          enddo
C              \do i=1,nbody
C
C         get body 1 Pole & PM in body 2 at time of body 2 pm near vec0pm
C
          call bodmat( bodyids(1), et0+et, tipm)
          call mtxv( tipm, id(1,3), vpole)
          call mtxv( tipm, id(1,1), vpm)
          call bodmat( bodyids(2), et0+et, tipm)
          call mxv( tipm, vpole, vpole)
          call mxv( tipm, vpm, vpm)
          call reclat( vpole, vpole(1), vpole(2), vpole(3))
          call reclat( vpm, vpm(1), vpm(2), vpm(3))
C
C         output:  #1 et, body 1 pole lat,lon in body 2, body 1 pm lat,lon ...
C
          call et2utc( et+et0, 'ISOD', 1, utc)
          call token2( utc, utc1, utc2, utclen, ul1, ul2)
          print'(a2, f11.0, 4f9.4, 1x, a)', '#1'
     &         , et+et0
     &         , vpole(3) * dpr(), elonr2wlond( vpole(2))
     &         , vpm(3) * dpr(), elonr2wlond( vpm(2))
     &         , utc(:utclen)
C
C         output:  #2 body 1 rotation period (h), rate (deg/d), error (deg/y)
C                    body 2 rotation period (h), rate (deg/d), fractional error
C
          print '(a2,2(f16.12,f11.4,g12.5))', '#2'
     &        , calcspdeg(1)/1d1
     &        , spd()/calcspdeg(1)
     &        , (1d0/calcspdeg(1)-1d0/NOMSPDEG) * (spd() * 365d0)
     &        , calcspdeg(2)/1d1
     &        , spd()/calcspdeg(2)
     &        , (1d0/calcspdeg(2)-1d0/NOMSPDEG) * (spd() * 365d0)
        endif
C            \if ( tok1 .eq. 'UTC:' ...
      endif
C          \if ( len2 .gt. 0) ...
      enddo
C          \dowhile(.true.) ...
99999 continue
C
C     output:   #3 Body 1/2 RA, DEC, W0, Wdot
C
      do j=1,2
C       jdZero = (etZero/spd()) + j2000()
        jdZero = unitim( etZero, 'ET', 'JDTDB')
        print'(a,f20.9)', '#3  JED Epoch:  ', jdZero
        if ( nbody .eq. 2) then
          do i=1,nbody
            if ( j .eq. 1) then
              call bodmat( bodyids(i), etZero, tipm)
              call mtxv( tipm, id(1,1), pm)
              call mtxv( tipm, id(1,3), mtx0(1,3))
              call recrad( mtx0(1,3), rng(i), ra(i), dec(i))
              call vcrss( id(1,3), mtx0(1,3), mtx0(1,1))
              call vhat( mtx0(1,1), mtx0(1,1))
              call vcrss( mtx0(1,3), mtx0(1,1), mtx0(1,2))
              call mtxv( mtx0, pm, pm)
              call recrad( pm, llr(1,i), llr(2,i), llr(3,i))
            endif
C                \if j .eq 1
            print '(a2,i9,4f14.7)', '#3', bodyids(i)
     &        , dpr() * ra(i)
     &        , dpr() * dec(i)
     &        , dpr() * llr(2,i)
     &        , spd()/calcspdeg(i)
C
            dllr = dmod( abs(etZero / calcspdeg(i)), 36d1) / dpr()
            if ( etZero .lt. 0d0) dllr = twopi() - dllr
            llr(2,i) = dmod( (llr(2,i) - dllr) + twopi(), twopi())
C
          enddo
C              \do i=1,nbody
        endif
C            \if nbody .eq. 2
        etZero = 0d0
      enddo
C          \do j=1,2
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C get two tokens from a string
C
      subroutine token2( lin, tok1, tok2, len0, len1, len2)
      implicit none
      character*(*) lin, tok1, tok2
      integer len0, len1, len2
C
      integer i, j, k, l
C
      len1 = 0
      len2 = 0

      len0 = len(lin)
      do while (len0 .gt. 1 .and. lin(len0:len0) .le. ' ')
        len0 = len0 - 1
      enddo
      if ( lin(len0:len0) .le. ' ') then
        len0 = 0
        return
      endif
C
C     get 1st token at beginning of string
C
      i = 1
      do while ( i .lt. len0 .and. lin(i:i) .gt. ' ')
        i = i + 1
      enddo
      if ( i .eq. 1 .and. lin(i:i) .le. ' ') return
      len1 = i - 1
      if ( lin(i:i) .gt. ' ') len1 = len1 + 1
      tok1 = lin(1:len1)
C
C     step past whitespace
C
      do while ( i .lt. len0 .and. lin(i:i) .le. ' ')
        i = i + 1
      enddo
      if ( i .eq. len0) return
C
C     get 2nd token
C
      j = i
      do while ( i .lt. len0 .and. lin(i:i) .gt. ' ')
        i = i + 1
      enddo
      len2 = i - j
      if ( lin(i:i) .gt. ' ') len2 = len2 + 1
      tok2 = lin(j:j+len2-1)
C
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
      character*15 cc
      logical found

      call bodn2c( c, id, found)
      if ( .not. found) then
        cc = '               '
        cc(16-len(c):15) = c
        read( cc, '(i15)') id
      endif
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C convert body-centered vector to RA & DEC in degrees
C
      subroutine mybodmat2radec( tipm, vec, ra, dec)
      implicit none
      doubleprecision tipm(3,3), vec(3), ra, dec
      doubleprecision vtmp(3), r
C
      doubleprecision dpr
      external dpr, twopi
CCCCCC
      call mtxv( tipm, vec, vtmp)
      call recrad( vtmp, r, ra, dec)
      ra = ra * dpr()
      dec = dec * dpr()
      if ( ra .lt. 0d0) ra = ra + 360d0
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C return number of degrees to turn to get body pm closest to vec0pm
C
      doubleprecision function delangbest( bodyid, et, id, vec0pm)
      implicit none
      integer bodyid
      doubleprecision et, id(3,3), vec0pm(3)
C
      doubleprecision vec0(3), vec0pole(3), vec0best(3), tipm(3,3)
      doubleprecision c
      doubleprecision dpr, vdot
      external dpr, vdot
CCCCCC
      call bodmat( bodyid, et, tipm)
      call mtxv( tipm, id(1,1), vec0)
      call mtxv( tipm, id(1,3), vec0pole)
      call vperp( vec0pm, vec0pole, vec0best)
      call vhat( vec0best, vec0best)
      c = vdot( vec0, vec0best)
      if ( c .gt. 1d0 ) then
        delangbest = 0d0
      else 
        if ( c .lt. -1d0 ) then
          delangbest = 180d0
        else
          delangbest = dpr() * acos(c)
        endif
      endif
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C convert east long (radians) to west long (degrees, 0-360)
C
      doubleprecision function elonr2wlond( elonr)
      implicit none
      doubleprecision elonr
C
      doubleprecision wlond, dpr
      external dpr
C
      wlond = -elonr * dpr()
      if ( wlond .lt. 0d0) wlond = wlond + 36d1
      elonr2wlond = wlond
      return
      end
