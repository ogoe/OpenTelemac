!                    ****************************
                     DOUBLE PRECISION FUNCTION SL
!                    ****************************
!
     &( I , N )
!
!***********************************************************************
! TELEMAC2D   V6P2                                   08/11/2011
!***********************************************************************
!
!brief    PRESCRIBES THE FREE SURFACE ELEVATION FOR LEVEL IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        17/08/1994
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification size FCT due to modification of TRACER numbering
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF LIQUID BOUNDARY
!| N              |-->| GLOBAL NUMBER OF POINT
!|                |   | IN PARALLEL NUMBER IN THE ORIGINAL MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_SL => SL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I,N
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
      DOUBLE PRECISION PI,OMEGA,PERIODE,A
      DATA PI/3.141592653589D0/
!
!-----------------------------------------------------------------------
!
!     IF THE LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OKSL REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKSL IS SET  TO .FALSE.
!
      IF(OKSL(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
        FCT='SL(      '
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:5)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:6)=')'
        ELSE
          WRITE(LU,*) 'SL NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL READ_FIC_FRLIQ(SL,FCT,AT,T2D_FILES(T2DIMP)%LU,
     &                      ENTET,OKSL(I))
!
      ENDIF
!
      IF(.NOT.OKSL(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     SL IS READ FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        IF(NCOTE.GE.I) THEN
          SL = COTE(I)
!         DESCENTE DE LA COTE 0 A -0.55 EN 300 S
          PERIODE=600.D0
          OMEGA=2*PI/PERIODE
          A=0.55D0/2.D0
          SL=A*(COS(OMEGA*AT)-1.D0)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,100) I
100       FORMAT(1X,/,1X,'SL : COTES IMPOSEES EN NOMBRE INSUFFISANT'
     &             ,/,1X,'     DANS LE FICHIER DES PARAMETRES'
     &             ,/,1X,'     IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,101) I
101       FORMAT(1X,/,1X,'SL: MORE PRESCRIBED ELEVATIONS ARE REQUIRED'
     &             ,/,1X,'     IN THE PARAMETER FILE'
     &             ,/,1X,'     AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    ***************************
                     DOUBLE PRECISION FUNCTION Q
!                    ***************************
!
     &(I)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   08/11/2011
!***********************************************************************
!
!brief    PRESCRIBES THE DISCHARGE FOR FLOW IMPOSED
!+                LIQUID BOUNDARIES.
!
!history  J-M HERVOUET (LNHE)
!+        09/01/2004
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification of FCT size due to modification of TRACER numbering
!
!history  J-M HERVOUET (LNHE)
!+        21/05/2012
!+        V6P2
!+   Discharge taken at mid distance between AT-DT AND AT, except
!+   for finite volumes (DT unknown, and AT time of beginning of time
!+   step in this case)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_Q => Q
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
      DOUBLE PRECISION Q1,Q2
!
!-----------------------------------------------------------------------
!
!     IF LIQUID BOUNDARY FILE EXISTS, ATTEMPTS TO FIND
!     THE VALUE IN IT. IF YES, OKQ REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKQ IS SET  TO .FALSE.
!
      IF(OKQ(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT='Q(       '
        IF(I.LT.10) THEN
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:4)=')'
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:5)=')'
        ELSE
          WRITE(LU,*) 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
          CALL READ_FIC_FRLIQ(Q1,FCT,AT-DT,
     &                        T2D_FILES(T2DIMP)%LU,ENTET,OKQ(I))
          CALL READ_FIC_FRLIQ(Q2,FCT,AT   ,
     &                        T2D_FILES(T2DIMP)%LU,ENTET,OKQ(I))
          Q=(Q1+Q2)*0.5D0
        ELSE
          CALL READ_FIC_FRLIQ(Q,FCT,AT   ,
     &                        T2D_FILES(T2DIMP)%LU,ENTET,OKQ(I))
        ENDIF
!
      ENDIF
!
      IF(.NOT.OKQ(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     Q IS TAKEN FROM THE STEERING FILE, BUT MAY BE CHANGED
!
        IF(NDEBIT.GE.I) THEN
!         Q = DEBIT(I)
          Q = -5.D0*HN%R(236)/0.6D0
        ELSE
          IF(LNG.EQ.1) WRITE(LU,400) I
400       FORMAT(1X,/,1X,'Q : DEBITS IMPOSES',/,
     &                1X,'    EN NOMBRE INSUFFISANT',/,
     &                1X,'    DANS LE FICHIER DES PARAMETRES',/,
     &                1X,'    IL EN FAUT AU MOINS : ',1I6)
          IF(LNG.EQ.2) WRITE(LU,401) I
401       FORMAT(1X,/,1X,'Q : MORE PRESCRIBED FLOWRATES',/,
     &                1X,'    ARE REQUIRED IN THE PARAMETER FILE',/,
     &                1X,'    AT LEAST ',1I6,' MUST BE GIVEN')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER IM,JM,I,J,POS_LOC
      DOUBLE PRECISION EIKON
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
      IM = 47
      JM = 10
!
!  VARIANTE FOND EN PENTE RECTILIGNE + CUVETTE
!
      DO I=1,IM
      DO J=1,JM
!       PENTE RECTILIGNE
        POS_LOC = GLOBAL_TO_LOCAL_POINT(I+(J-1)*IM,MESH)
!
!       NOTE JMH: THIS IS VERY HEAVY, THERE SHOULD BE A
!                 FORMULA FUNCTION OF X.
!
        IF(POS_LOC.GT.0) THEN
          ZF%R(POS_LOC)=-0.6D0+0.46D0*FLOAT(I-1)/FLOAT(IM-1)
!         BOSSE GAUSSIENNE
          IF(I.GT.9.AND.I.LT.29) THEN
            EIKON = -(I-19)**2/20.D0
            ZF%R(POS_LOC) = ZF%R(POS_LOC) + 0.1D0*EXP(EIKON)
          ENDIF
        ENDIF
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END


