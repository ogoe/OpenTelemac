!                    ****************************
                     SUBROUTINE CVSP_ADD_FRACTION
!                    ****************************
!
     &(J, I, dZFCL, EVL)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    Adds a Fraction to the topmost Vertical Sorting Profile Section
!
!history  UWE MERKEL
!+        2011
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!| I              |<--| INDEX of a FRACTION
!| dZFCL          |<--| EVOLUTION of FRACTION I [m]
!| EVL            |<--| EVOLUTION of all FRACTIONS [m]
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: I
      doUBLE PRECISION, INTENT(IN)    :: dZFCL
      doUBLE PRECISION, INTENT(IN)    :: EVL

      double precision STR_OLD, STR_NEW, temp1, temp2, AT
      integer II
      logical ret, CVSP_CHECK_F
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        AT = DT*LT/PERCOU
!
    ! Makes sure that there is no influence on the profile points below
    ! By inserting a section with 0 strength if it doesn't exist already
!
       !Checks for breakpoint (= 0 strength)
          if (PRO_MAX(J).gt.2) then
          if (PRO_D(J,PRO_MAX(J)-1,1).gt.PRO_D(J,PRO_MAX(J)-2,1)) Then
            !insert
              PRO_MAX(j) = PRO_MAX(j) + 1
            !shifts breakpoint
            do II=1,NSICLA
              PRO_F(J,PRO_MAX(J),II) = PRO_F(J,PRO_MAX(J)-1,II)
              PRO_F(J,PRO_MAX(J)-1,II) = PRO_F(J,PRO_MAX(J)-2,II)
              PRO_D(J,PRO_MAX(J),II) = PRO_D(J,PRO_MAX(J)-1,II)
              PRO_D(J,PRO_MAX(J)-1,II) = PRO_D(J,PRO_MAX(J)-2,II)
            enddo

          endif
          endif


       ! Adds material

       !Strength of Fraction
       STR_OLD = (PRO_D(J,PRO_MAX(J),I)-PRO_D(J,PRO_MAX(J)-1,I))
       STR_NEW = dZFCL + STR_OLD

       !New Fractions
       !top
       PRO_F(J,PRO_MAX(J),I) =
     &   (dZFCL + PRO_F(J,PRO_MAX(J),I) * STR_OLD) / (STR_NEW)
       !bottom
       PRO_F(J,PRO_MAX(J)-1,I) =
     &   (dZFCL + PRO_F(J,PRO_MAX(J)-1,I) * STR_OLD) / (STR_NEW)

       !New Depth=Z of Fraction
       PRO_D(J,PRO_MAX(J),I) = dZFCL + PRO_D(J,PRO_MAX(J),I)

       !Shifting Percentage for the other Fractions
            do II=1,NSICLA
              if (I /= II) then
                ! SUM OF FRACTIONS AFTER SEDIMENTATION /= I
                temp1 =
     &             PRO_F(J,PRO_MAX(J),II) * STR_OLD / STR_NEW
                temp2 =
     &             PRO_F(J,PRO_MAX(J)-1,II) * STR_OLD / STR_NEW
                ! Assign New Thickness & Corrected Fractions
                PRO_F(J,PRO_MAX(J),II) = temp1
                PRO_D(J,PRO_MAX(J),II) = dZFCL + PRO_D(J,PRO_MAX(J),II)
                PRO_F(J,PRO_MAX(J)-1,II) = temp2
              endif
            enddo


        ! Removes Floating Point Trucations
        ret =  CVSP_CHECK_F(J,PRO_MAX(J),'ADF: MAX  ')
        ret =  CVSP_CHECK_F(J,PRO_MAX(J)-1,'ADF: MAX+1')
        if (PRO_MAX(J).gt.2) then
            ret =  CVSP_CHECK_F(J,PRO_MAX(J)-2,'ADF: MAX+2')
        endif
        if (PRO_MAX(J).gt.3) then
            ret =  CVSP_CHECK_F(J,PRO_MAX(J)-3,'ADF: MAX+3')
        endif
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      RETURN
      END SUBROUTINE CVSP_ADD_FRACTION


