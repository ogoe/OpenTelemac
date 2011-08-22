C
c   Lock exchange test case  in Non Hydrostatic
C   In here Only :
C   Condim          
C
C
!                       *****************
                        SUBROUTINE CONDIM 
!                       *****************
!
!
!***********************************************************************
! TELEMAC-3D   V5.1                       J-M HERVOUET(LNH) 30 87 80 18
! fortran95 version         march 1999        Jacek A. Jankowski pinxit
!***********************************************************************
!
!      FONCTION:
!      =========
!
!    INITIALISATION DES TABLEAUX DES GRANDEURS PHYSIQUES
!
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I, ipoin2, ipoin3     
!
!***********************************************************************
! TIME ORIGIN
!
      AT  = 0.D0
!
! INITIALISATION DE H , LA HAUTEUR D'EAU.
!
      CALL OS( 'X=C     ' , H   , H , H , 0.D0)
      CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
!
      DO I=1,NPOIN2
       H%R(I)=MAX(H%R(I),HMIN)
      ENDDO
!
      call OS ('X=Y     ', hn, h, h, 0.d0)
!
!-----------------------------------------------------------------------
!
      IF (NPLINT.GE.2) THEN
        CALL OV( 'X=C     ' ,Z((NPLINT-1)*NPOIN2+1:NPLINT*NPOIN2)
     *                      ,Z,Z,COTINT,NPOIN2)
      ENDIF
!
!
      IF (NPLINT.GE.2) THEN
        DO IPLAN = 1,NPLINT-1
          ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLINT-1)
        END DO
      ENDIF
!
      DO IPLAN = NPLINT,NPLAN
        ZSTAR%R(IPLAN) = DBLE(IPLAN-NPLINT)/DBLE(NPLAN-NPLINT)
      END DO
!
!***********************************************************************
!
      CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
!     INITIALISATION DES VITESSES
!
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
      CALL OS( 'X=C     ' , W , W , W , 0.D0 )
!
!-----------------------------------------------------------------------
!
!     INITIALISATION DES TRACEURS ACTIFS
!
      IF (NTRAC.NE.0) THEN
         CALL OS( 'X=C     ' , TA , TA ,TA , 0.D0 )
              
         DO 21 IPLAN=1,NPLAN
            DO 20 IPOIN2=1,NPOIN2
              IPOIN3 = IPOIN2 + (IPLAN-1)*NPOIN2
              IF(X(IPOIN3).LE.15.D0) TA%ADR(1)%P%R(IPOIN3) = 1.D0
20          CONTINUE
21       CONTINUE
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALIZE THE PRESSURE FIELDS TO 0.0
! 
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
