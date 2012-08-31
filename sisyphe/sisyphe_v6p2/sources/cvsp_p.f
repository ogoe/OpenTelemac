!                    *********************
                     SUBROUTINE CVSP_P (PATH_PRE,File_PRE,JG)
!                    *********************
!

!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    .CSV-file output of a Vertical Sorting Profile in Point J
!
!history  UWE MERKEL
!+        2011-07-20
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| JG             |<--| GLOBAL POINT NUMBER
!| PATH_PRE       |<--| Where to save
!| FILE_PRE       |<--| FileNameTrunk
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
      USE BIEF
      USE BIEF_DEF
      USE CVSP_OUTPUTFILES


!
      IMPLICIT NONE

      INTEGER,             INTENT(IN)     :: JG
      character(*),        INTENT(IN )    :: PATH_PRE
      character(*),        INTENT(IN )    :: FILE_PRE

      character*100, debugfile
      character*5, ocstr
      integer  I, K, J, J2
      doubleprecision AT, bsum, levelbelow

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      !return ! DEBUG

            AT = DT*LT/PERCOU
            J = JG
            outputcounter = outputcounter + 1

            !GLOBAL NUMBERS TO global NUMBERS
            if (NCSIZE > 1) then
                J = mesh%knogl%I(JG)
            endif


            write(unit=ocstr, fmt='(I5)')
     &      outputcounter
                do I=1,5
                  if(ocstr(i:i)==' ') ocstr(i:i)='0'
                end do

            write(unit=debugfile, fmt='(A,A,A,A,I8,A,G15.8,A)')
     &      PATH_PRE,ocstr,'_',File_PRE,
     &      JG,'_T_',AT,'.VSP.CSV'
                do I=1,LEN_TRIM(debugfile)
                  if(debugfile(i:i)==' ') debugfile(i:i)='_'
                end do

	  !print *, debugfile

      if(J > 0) THEN
      open(81, file=debugfile, STATUS='UNKNOWN' )

        rewind 81
!RK
      write(81,*)"J K FD50(I) AT PRO_D(K_I) PRO_F(K_I) X Y D50 ALT t H"

        do K=1,PRO_MAX(J)

            Bsum = 0.D0
            do I=1,NSICLA
                bsum = FDM(I)*PRO_F(J,PRO_MAX(J)+1-K,I) + bsum
            enddo

            do I=1,NSICLA

            if (k.eq.1) then
        ! Full output with coordinates etc. on surface
        write (81,'(I8,1X,I4,1X,10(G20.12,1X))')
     &              JG,PRO_MAX(J)+1-K,FDM(I),AT,
     &              PRO_D(J,PRO_MAX(J)+1-K,I),
     &              PRO_F(J,PRO_MAX(J)+1-K,I),X(J),Y(J),
     &              bsum,ES(J,1),TOB%R(J), Z%R(J) !UHM

            else




        ! Following sections
        write (81,'(I8,1X,I4,1X,5(G20.12,1X))')
     &              JG,PRO_MAX(J)+1-K,FDM(I),AT,
     &              PRO_D(J,PRO_MAX(J)+1-K,I),
     &              PRO_F(J,PRO_MAX(J)+1-K,I)
            endif

            enddo


        enddo

            Bsum = 0.D0
            do I=1,NSICLA
                bsum = FDM(I)*PRO_F(J,1,I) + bsum
            enddo



      close(81)


      !print *, debugfile

      endif

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        RETURN
      END SUBROUTINE CVSP_P
