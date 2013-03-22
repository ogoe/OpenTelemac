!                    *******************
                     SUBROUTINE LAYERS_P
!                    *******************
!
     &(PATH_PRE,JG)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief   .CSV-file output of a LAYER Profile in Point J
!
!history  UWE MERKEL
!+        2011-07-20
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| JG             |<--| GLOBAL POINT NUMBER
!| PATH_PRE       |<--| Where to save
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE BIEF
      USE BIEF_DEF
      !
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: JG
      character(*),        INTENT(IN )    :: PATH_PRE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      character*100, debugfile
      integer  I,K,J
      doubleprecision depth, AT, myfra, bsum
!
!----------------------------------------------------------------
!
      AT = DT*LT/PERCOU
!
!     global NUMBERS TO global NUMBERS
      J = JG
!     NOTE JMH : KNOGL will be suppressed in future
      if(NCSIZE>1) J = mesh%knogl%I(JG)
!
      write(unit=debugfile, fmt='(A,I8,A,G15.8,A)')
     &      PATH_PRE,JG,'_T_',AT,'.LAY.CSV'
      do I=1,38
        if(debugfile(i:i)==' ') debugfile(i:i)='_'
      end do
!
      if(J > 0) THEN !0 if node is not on this partition
      open(80, file=debugfile , status='UNKNOWN')
        rewind 80
        write(80,*)"J K FD50(I) AT Z AVAIL(J,K,I) X Y D50 TAU H"
!
        depth = ZF%R(J)
!
        !LAYER TOP
        do K=1,NLAYER%I(J)
!
          Bsum = 0.D0
          do I=1,NSICLA
            bsum = FDM(I)*AVAIL(J,K,I) + bsum
          enddo
!
          do I=1,NSICLA
            write (80,'(I8,1X,I4,1X,7(G15.8,1X))')
     &      JG,(NLAYER%I(J)-K+1),FDM(I),AT,depth,
     &      AVAIL(J,K,I),X(J),Y(J), bsum, TOB%R(J), Z%R(J)
          enddo
            depth = depth - ES(J,K)

        enddo
!
!     RIGID BED
!
      do I=1,NSICLA
        bsum = FDM(I)*AVAIL(J,NLAYER%I(J),I) + bsum
      enddo
!
      do I=1,NSICLA
            myfra = 0.D0
            if (I==1) myfra = 1.D0
            write (80,'(I8,1X,I4,1X,7(G15.8,1X))')
     &      JG,0,FDM(I),AT,depth,myfra,X(J),Y(J),BSUM
      enddo
!
      close(80)
      endif
C
C----------------------------------------------------------------
C
      RETURN
      END SUBROUTINE LAYERS_P
