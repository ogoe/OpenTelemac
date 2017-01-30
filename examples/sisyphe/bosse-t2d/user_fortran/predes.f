!                       *****************
                        SUBROUTINE PREDES
!                       *****************
!
     &(LLT,AAT)
!
!***********************************************************************
! SISYPHE VERSION 6.0                             E. PELTIER    11/09/95
!                                                 C. LENORMANT
!                                                 J.-M. HERVOUET
!
!
! JMH 07/12/2009: KS SET TO 0 IF LLT=0
!
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
!***********************************************************************
!
!     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
!                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |      LLT       |--> | LOCAL LT (MAY BE LT-1+PERCOU)
! |      AAT       |--> | CURRENT TIME (FOR BUILDING SOLUTIONS)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!     - PROGRAMME APPELANT : SISYPH
!     - SOUS-PROGRAMMES APPELES : OVD,OV
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LLT
      DOUBLE PRECISION, INTENT(IN) :: AAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION BID
      INTEGER LTT,IN
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME THAN IN DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
!
!     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
!
!=======================================================================
! ECRITURE DES VARIABLES CALCULEES
!=======================================================================
!
!     VITESSE U:    U=QU/H
!
      IF ((LEO.AND.SORLEO(1)).OR.(IMP.AND.SORIMP(1))) THEN
        CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
!
!     VITESSE V:    V=QV/H
!
      IF ((LEO.AND.SORLEO(2)).OR.(IMP.AND.SORIMP(2))) THEN
        CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
!
!     CELERITE C:   (GRAV*H)**0.5
!
      IF ((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(HN,T3)
        DO IN=1, NPOIN
          T3%R(IN)= SQRT (GRAV*HN%R(IN))
        ENDDO
      ENDIF
!
!     SURFACE LIBRE Z: H+ZF
!
      IF ((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS('X=Y+Z   ',Z,HN,ZF, BID )
      ENDIF
!
!     FROUDE F: ((QU**2+QV**2)/(GRAV*H**3))**0.5
!
      IF ((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
        CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN )
        CALL CPSTVC(QU,T4)
        DO IN=1,NPOIN
           T4%R(IN)= T1%R(IN)**2+T2%R(IN)**2
        ENDDO
        CALL OS( 'X=Y/Z   ' , T4 , T4 , HN , 0.D0 , 2 , 0.D0 , HMIN )
        DO IN=1,NPOIN
          T4%R(IN)=SQRT(T4%R(IN)/GRAV)
        ENDDO
      ENDIF
!
!=======================================================================
!
!     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
!
      IF(LLT.EQ.0) THEN
!       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
!
!=======================================================================
!
!  SOLUTION ANALYTIQUE POUR LE FOND (PREMIER TABLEAU PRIVE)
!
!  CV    IF((LEO.AND.SORLEO(27+(NOMBLAY+4)*NSICLA+NOMBLAY)).OR.
!  CV  *   (IMP.AND.SORIMP(27+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
      IF(LEO.OR.IMP) THEN
            CALL CARACTERISTIQUE(MESH%X%R,MESH%Y%R,NPOIN,
     &                                        PRIVE%ADR(1)%P%R,AAT)
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
!
      RETURN
      END

