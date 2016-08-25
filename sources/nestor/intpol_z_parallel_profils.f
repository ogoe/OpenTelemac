      SUBROUTINE  Intpol_Z_parallel_Profils      !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( F, xA,yA,zA,  xB,yB,zB,  xC,yC,zC,  xD,yD,zD,  ip, P1km,P2km )

      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ipid

      IMPLICIT NONE
      TYPE(t_Field) ,INTENT(INOUT) :: F

      REAL (KIND=8),INTENT(IN) :: xA,yA,zA,xB,yB,zB,xC,yC,zC,xD,yD,zD ! points:  A,B,C,D
      REAL (KIND=8),INTENT(IN) :: P1km, P2km  ! measure of river length: profile1, profile2
      INTEGER,INTENT(IN)       :: ip          ! index of profile1

      !------- local variables ---------------
      INTEGER         :: i
      REAL (KIND=8)   :: distNP, distPP                ! distance: profile-1 - point N, profile-1 - profile-2
      REAL (KIND=8)   :: xN,yN, xSa,ySa, xSb,ySb       ! point N, Sa, Sb
      REAL (KIND=8)   :: xAB,yAB, xAC,yAC, xAN,yAN     ! vetkors: AB, AC, AN
      REAL (KIND=8)   :: xBD,yBD, xBN,yBN              ! vetkors: BD, BN
      REAL (KIND=8)   :: xCD,yCD ,xCN,yCN              ! vetkors: CD, CN
      REAL (KIND=8)   :: absAB, faktor1, faktor2
      REAL (KIND=8)   :: distSaN, distSaSb
      REAL (KIND=8)   :: z1, z2                        ! interpolated z-values between: zA zB , zC zD
      REAL (KIND=8)   :: DetABAN, DetCDCN, DetACAN, DetBDBN
      TYPE(t_String_Length) :: SRname ! name of current Subroutine

      !
      !
      !
      !                 D         z2                            C
      !    profile2     *=========^=============================*--------------------
      !                  \                                     /:               :
      !                   \                                   / :               :
      !                    \ Sb      N                    Sa /  :              distPP
      !                     +-------*-----------------------+   :  ---          :
      !                     :\      :                      /:   :   :           :
      !                     : \     :                     / :   :  distNP       :
      !                     :  \    : z1                 /  :   :   :           :
      !    profile1      ---:---*=====^=================*---:---:---------------------
      !                     :   B   :                   A   :
      !                     :       |--------distSaN--------|
      !                     |------------distSaSb-----------|
      !
      !
      !
      WRITE(6,*)'?>-------  SR Intpol_Z_parallel_Profils ----'
      SRname%s = "Intpol_Z_parallel_Profils"                     ! subroutine name
      SRname%i =  25                                             ! length of name string

      xAB = xB - xA             !> vector AB: x component (Profile1)
      yAB = yB - yA             !>     "      y component      "
      xCD = xD - xC             !> vector CD: x component (Profile2)
      yCD = yD - yC             !>     "      y component      "

      xAC = xC - xA             !> vector AC: x component
      yAC = yC - yA             !>     "      y component

      xBD = xD - xB             !> vector BD: x component
      yBD = yD - yB             !>     "      y component

      absAB  = SQRT( xAB**2 + yAB**2 )
      distPP = abs( xAC*yAB - yAC*xAB ) / absAB   !> Distance profile1 - profile2
                                                  !  keywords: point,line distance, 2D
      IF(distPP < 1.e-8_8 ) Call ErrMsgAndStop2(
     &  " ",1,"reason: Profiles are super narrow !",35
     & ," ",1,"        Fit profile  ",21, ip,  SRname, ipid )


      DO i = 1 , F%nNodes
        xN = F%X(i)    !> get the coordinate of node which receives the "refz value"
        yN = F%Y(i)

        xAN     = xN - xA            !> vector AN: x component
        yAN     = yN - yA            !>     "      y component
        xCN     = xN - xC            !> vector CN: x component
        yCN     = yN - yC            !>     "      y component
        DetABAN = xAB*yAN - yAB*xAN  !> determinate of vectors AB and AN
        DetCDCN = xCD*yCN - yCD*xCN  !> determinate of vectors CD and CN

        !______________________________________________________________
        !> Before we do further calculations we                        |
        !  test if node N is inside the quadrangle which is given      |
        !  by the two parallel Profiles                                |
        !                  keywords: triangle, area, determinant, sign |
        !  step 1) if the sign of DetABAN and DetCDCN is the same      |
        !          Point N is not between the line-AB and line-CD      |
        !               ==> next loop                                  |
        !  step 2) if the sign of DetACAN and DetBDBN is the same      |
        !          Point N is not between the line-AB and line-CD      |
        !               ==> next loop                  ________________|
        IF(     (DetABAN < 0.0_8 .AND. DetCDCN < 0.0_8)            ! test step 1
     &      .OR.(DetABAN > 0.0_8 .AND. DetCDCN > 0.0_8)  )  CYCLE  ! node is not between the lines

        xBN     = xN - xB              !> vector BN: x component
        yBN     = yN - yB              !>     "      y component
        DetACAN = xAC*yAN - yAC*xAN    !> determinate of vectors AC and AN
        DetBDBN = xBD*yBN - yBD*xBN    !> determinate of vectors BD and BN

        IF(     (DetACAN < 0.0_8 .AND. DetBDBN < 0.0_8)            ! test step 2
     &      .OR.(DetACAN > 0.0_8 .AND. DetBDBN > 0.0_8)  )  CYCLE  !> node is not between the lines

        !> node is inside the quadrangle

        distNP = abs( xAN*yAB - yAN*xAB ) / absAB   !> Distance node - profile1
                                                    !  keywords: point, line, distance, 2D
        faktor1 = distNP / distPP
        xSa    = xA  +   faktor1 * xAC
        ySa    = yA  +   faktor1 * yAC
        xSb    = xB  +   faktor1 * xBD
        ySb    = yB  +   yBD

        distSaN  =  (xN  - xSa)**2 + (yN  - ySa)**2   !> We do the square root 2 lines below
        distSaSb =  (xSb - xSa)**2 + (ySb - ySa)**2   !> We do the square root 1 line below
        faktor2  = SQRT(  distSaN  / distSaSb   )     !> Here we do it

        z1     = zA  +   faktor2 * (zB-zA)   !> interpolate z1 between zA and zB
        z2     = zC  +   faktor2 * (zD-zC)   !> interpolate z2 between zC and zD

        F%refZ(i) = z1 + faktor1 * (z2-z1)   !> interpolate zN between z1 and z2

        F%km(i)   = P1km + faktor1 * (P2km - P1km)  !> interpolate measure of river length

      ENDDO ! (i = 1 , F%nNodes)

      WRITE(6,*)'?>-------  SR Intpol_Z_parallel_Profils END-'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE Intpol_Z_parallel_Profils   !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************


!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
