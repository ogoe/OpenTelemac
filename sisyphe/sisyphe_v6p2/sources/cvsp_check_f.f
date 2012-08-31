!                    ************************
                     LOGICAL FUNCTION CVSP_CHECK_F
!                    ************************
!
     &(J,K, SOMETEXT)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Checks if Sum of Fractions = 1 for
!+        a section in the Vertical Sorting Profile
!
!history  UWE MERKEL
!+        2011-08-19
!+        V6P2
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!| K              |<--| INDEX of a Section in VERTICAL SORTING PROFILE
!| SOMETEXT       |<--| DEBUGING TEXT FOR LOG-OUTPUT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY: IPID, NCSIZE
      USE BIEF
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: K
      character*10,     INTENT(IN)    :: SOMETEXT

      doUBLE PRECISION TEMP, AT
      integer I, JG
! PAT
!      logical isnan
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             AT = DT*LT/PERCOU
             JG = J
             if (ncsize > 1) JG = mesh%knolg%I(J)

             CVSP_CHECK_F = .TRUE.
             TEMP = 0.D0


           !SUM UP and slight correction
           do I=1,NSICLA

!            !ISNAN
!            if (isnan(PRO_F(J,K,I))) then
!               print *, "CF: PRO_F(J,K,I) IS NaN",SOMETEXT,
!     &  			J,K,I, PRO_F(J,K,I)
!                call plante(1)
!            endif

            ! <0
            if (PRO_F(J,K,I).LT.0.D0) then
                if (PRO_F(J,K,I).GE.-1.D-8) then
!        print *,"CF: , PRO_F(J,K,I)<0: OK ",SOMETEXT,JG,K,I,PRO_F(J,K,I)
                PRO_F(J,K,I) = 0.D0
                else
            print *,"CF:,PRO_F <0: BAD !!",SOMETEXT,JG,K,I,PRO_F(J,K,I)
                ! I'm not willing to correct anything here anymore, better it breaks down ...
                endif
            endif

            TEMP = TEMP + PRO_F(J,K,I)

           enddo


           ! Correct Deviations
           if (ABS(TEMP-1.D0).GT.0.D0) then
               if (ABS(TEMP-1.D0).GT.0.000001D0) then
                !Strong differnces are corrected by normalizing all fractions
                    CVSP_CHECK_F = .FALSE.
                    do I=1,NSICLA
                    if (PRO_F(J,K,I).GT.0.D0) THEN
                     PRO_F(J,K,I) = PRO_F(J,K,I) / TEMP
!                     print*," CF:Normal!, J;K;I;AT;ErrTEMP; "
!     &                      ,SOMETEXT,JG,K,I,AT,TEMP
                     exit
                    endif
                    enddo

               else
               !Slight differnces to 0 are corrected by changing only the first fraction big enough
                    do I=1,NSICLA
                    if (PRO_F(J,K,I).GT.ZERO) THEN
                     PRO_F(J,K,I) = 1.D0-(TEMP-PRO_F(J,K,I))
                     exit
                    endif
                    enddo

               endif
           endif
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

        RETURN
      END FUNCTION CVSP_CHECK_F
