!                    *****************
                     SUBROUTINE LITENR
!                    *****************
!
     &(VARSOR,CLAND,
     & NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,TEXTLU,
     & NVAR,VARCLA,NVARCL,TROUVE,ALIRE,W,LISTIN,MAXVAR,
     & NPOIN_PREV,NPLAN_PREV,WD)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    READS A TIME RECORD FROM A SELAFIN FILE.
!+
!+            THE FILE IS ASSUMED OPEN AND THE TIMESTEP READY TO READ.
!+
!+            INTERPOLATES IN 3D IF THE NUMBER OF PLANES IS DIFFERENT.
!
!history  J-M HERVOUET (LNHE)
!+        09/12/08
!+        V5P9
!+   STD IS NOW A STRING OF ANY SIZE 
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALIRE          |-->| VARIABLES QU'IL FAUT LIRE (POUR LES AUTRES ON
!|                |   | SAUTE L'ENREGISTREMENT CORRESPONDANT)
!|                |   | LES VARIABLES CLANDESTINES SONT LUES
!|                |   | SYSTEMATIQUEMENT.
!| AT             |-->| TEMPS
!| CLAND          |<--| VARIABLES CLANDESTI-NES
!| HIST           |-->| TABLEAU DE VALEURS MISES DANS L'ENREGISTREMENT
!|                |   | DU TEMPS.
!| LISTIN         |-->| SI OUI, IMPRESSION D'INFORMATIONS SUR LISTING
!| MAXVAR         |-->| NOMBRE MAXIMUM DE VARIABLES
!| NHIST          |-->| NOMBRE DE VALEURS DANS LE TABLEAU HIST.
!| NPLAN_PREV     |---| 
!| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
!| NPOIN_PREV     |---| 
!| NPRE           |-->| NUMERO DE CANAL DU FICHIER
!| NVAR           |-->| NOMBRE DE VARIABLES NORMALES
!| NVARCL         |-->| NOMBRE DE VARIABLES CLANDESTINES.
!|                |   | NVAR + NVARCL FERA LE NOMBRE TOTAL.
!| STD            |-->| FORMAT DU FICHIER
!| TEXTLU         |-->| NOMS ET UNITES DES VARIABLES LUES DANS LE
!|                |   | FICHIER ET VARIABLES CLANDEST.
!| TEXTPR         |-->| NOMS ET UNITES DES VARIABLES DANS LE LOGICIEL
!| TROUVE         |<--| INDIQUE (TROUVE(K)=1) LES VARIABLES TROUVEES
!|                |   | DANS LE FICHIER.
!|                |   | DE K =  1 A MAXVAR VARIABLES NORMALES
!|                |   | DE K = MAXVAR+1 A MAXVAR+10 VARIABLES CLANDESTINES.
!| VARCLA         |-->| TABLEAU OU L'ON RANGE LES VARIABLES
!|                |   | CLANDESTINES.
!| VARSOR         |<--| BLOC DES TABLEAUX CONTENANT LES VARIABLES
!| W              |-->| TABLEAU DE TRAVAIL REEL, DE TAILLE NPOIN.
!| WD             |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: VARSOR,CLAND
      INTEGER, INTENT(IN)             :: NPRE,NHIST,NPOIN,MAXVAR,NVARCL
      INTEGER, INTENT(IN)             :: NVAR,ALIRE(MAXVAR)
      INTEGER, INTENT(OUT)            :: TROUVE(MAXVAR)
      INTEGER, INTENT(IN), OPTIONAL   :: NPOIN_PREV,NPLAN_PREV
      CHARACTER(LEN=*)                :: STD
      CHARACTER(LEN=32)               :: TEXTPR(MAXVAR),TEXTLU(MAXVAR)
      CHARACTER(LEN=32)               :: VARCLA(NVARCL)
      DOUBLE PRECISION, INTENT(INOUT) :: HIST(*)
      DOUBLE PRECISION, INTENT(OUT)   :: AT
      REAL                            :: W(NPOIN)
      LOGICAL, INTENT(IN)             :: LISTIN
!
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: WD(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ISTAT,L,IBID(1),NVAL,K,IHIST,NPLAN,NPOIN2,IPLAN,IP1,IP2,I
      DOUBLE PRECISION AAT(20),TETA,ARG
      CHARACTER(LEN=1) CBID
      LOGICAL OK,INTERP
!
!-----------------------------------------------------------------------
!
!     INTERPOLATES ?
!
      INTERP=.FALSE.
      IF(PRESENT(NPOIN_PREV).AND.PRESENT(NPLAN_PREV)) THEN
        IF(NPOIN_PREV.NE.NPOIN) THEN
          INTERP=.TRUE.
          NPOIN2=NPOIN_PREV/NPLAN_PREV
          NPLAN=NPOIN/NPOIN2
          IF(.NOT.PRESENT(WD)) THEN
            CALL PLANTE(1)
            STOP 'WD NOT PRESENT IN LITENR'
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO L=1,MAXVAR
        TROUVE(L) = 0
      ENDDO
!
!-----------------------------------------------------------------------
!
!  READS THE TIME
!
      NVAL = 1 + NHIST
      CALL LIT(AAT ,W,IBID,CBID,NVAL,'R4',NPRE,STD,ISTAT)
      AT = AAT(1)
!
!  GETS THE VALUES WRITTEN IN THE TIME RECORD
!
      IF(NHIST.NE.0) THEN
        DO 139 IHIST = 1, NHIST
          HIST(IHIST) = AAT(1+IHIST)
139     CONTINUE
      ENDIF
!
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,140) AT
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,141) AT
140   FORMAT(//,1X,'TEMPS DE L''ENREGISTREMENT : ',G16.7,' S')
141   FORMAT(//,1X,'TIME OF RECORD: ',G16.7,' S')
!
!-----------------------------------------------------------------------
!
!  READS THE REQUIRED VARIABLES (IF THEY ARE FOUND)
!
!     CORRECTION JMH 16/12/03: NVARCL IS TAKEN INTO ACCOUNT HERE IN NVAR
!                              AS AN OUPUT OF SKIPGEO
!     DO 80 K=1,NVAR+NVARCL
      DO 80 K=1,NVAR
!
        OK = .FALSE.
!
      DO 81 L=1,MAXVAR
!
      IF (TEXTLU(K)(1:32).EQ.TEXTPR(L)(1:32) ) THEN
!
        OK = .TRUE.
!
        IF(ALIRE(L).EQ.1) THEN
!
          IF(.NOT.INTERP) THEN
!
            CALL LIT(VARSOR%ADR(L)%P%R,
     &               W,IBID,CBID,NPOIN,'R4',NPRE,STD,ISTAT)
!
          ELSE
            CALL LIT(WD,W,IBID,CBID,NPOIN_PREV,'R4',NPRE,STD,ISTAT)
!           COPIES BOTTOM AND FREE SURFACE
            CALL OV('X=Y     ',VARSOR%ADR(L)%P%R,WD,WD,0.D0,NPOIN2)
            CALL OV('X=Y     ',VARSOR%ADR(L)%P%R(NPOIN-NPOIN2+1:NPOIN),
     &                         WD(NPOIN_PREV-NPOIN2+1:NPOIN_PREV),
     &                         WD,0.D0,NPOIN2)
!           INTERPOLATES OTHER PLANES
            IF(NPLAN.GT.2) THEN
              DO IPLAN=2,NPLAN-1
                ARG=(NPLAN_PREV-1)*FLOAT(IPLAN-1)/FLOAT(NPLAN-1)
                TETA=ARG-INT(ARG)
!               IP1 : LOWER PLANE NUMBER - 1
                IP1=INT(ARG)
!               IP2 : UPPER PLANE NUMBER - 1
                IP2=IP1+1
                DO I=1,NPOIN2
                  VARSOR%ADR(L)%P%R(I+NPOIN2*(IPLAN-1))=
     &            TETA *WD(I+NPOIN2*IP2)+(1.D0-TETA)*WD(I+NPOIN2*IP1)
                ENDDO
              ENDDO
            ENDIF
          ENDIF
!
        ELSE
!
          IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,75) TEXTLU(K)
          IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,76) TEXTLU(K)
75        FORMAT(/,1X,'LA VARIABLE : ',A32,/,1X,
     &                'EST DANS LE FICHIER MAIS ELLE N''EST PAS LUE')
76        FORMAT(/,1X,'VARIABLE : ',A32,/,1X,
     &                'IS IN THE FILE BUT WILL NOT BE READ')
          CALL LIT(AAT,W,IBID,CBID ,2,'R4',NPRE,STD,ISTAT)
!
        ENDIF
!
        TROUVE(L)=1
!
      ENDIF
!
81    CONTINUE
!
!     THIS SHOULD NEVER HAPPEN NOW ?????
!
      IF(NVARCL.NE.0) THEN
!
      DO 82 L=1,NVARCL
!
      IF(TEXTLU(K)(1:32).EQ.VARCLA(L)(1:32) ) THEN
        OK = .TRUE.
        CALL LIT(CLAND%ADR(L)%P%R,
     &           W,IBID,CBID,NPOIN,'R4',NPRE,STD,ISTAT)
        TROUVE(MAXVAR+L)=1
      ENDIF
!
82    CONTINUE
      ENDIF
!
        IF(.NOT.OK) THEN
          IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,77) TEXTLU(K)
          IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,78) TEXTLU(K)
77        FORMAT(/,1X,'LA VARIABLE : ',A32,/,1X,
     &                'EST INCONNUE, ELLE NE SERA PAS CONSERVEE')
78        FORMAT(/,1X,'VARIABLE : ',A32,/,1X,
     &                'UNKNOWN, IT WILL NOT BE KEPT')
          CALL LIT(AAT,W,IBID,CBID ,2,'R4',NPRE,STD,ISTAT)
        ENDIF
!
80    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END