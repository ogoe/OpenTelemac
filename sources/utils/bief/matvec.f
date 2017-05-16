!                    *****************
                     SUBROUTINE MATVEC
!                    *****************
!
     &( OP , X , A , Y , C , MESH , LEGO )
!
!***********************************************************************
! BIEF   V7P2
!***********************************************************************
!
!brief    MATRIX VECTOR OPERATIONS.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND MATRIX M.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   THESE OPERATIONS ARE DIFFERENTS DEPENDING ON THE DIAGONAL TYPE
!+   AND THE OFF-DIAGONAL TERMS TYPE.
!+
!+   IMPLEMENTED OPERATIONS :
!+
!+      OP = 'X=AY    '  : X = AY
!+      OP = 'X=-AY   '  : X = -AY
!+      OP = 'X=X+AY  '  : X = X + AY
!+      OP = 'X=X-AY  '  : X = X - AY
!+      OP = 'X=X+CAY '  : X = X + C AY
!+      OP = 'X=CAY   '  : X = C AY
!+      OP = 'X=TAY   '  : X = TA Y (TA: TRANSPOSE OF A)
!+      OP = 'X=-TAY  '  : X = - TA Y (TA: TRANSPOSE OF A)
!+      OP = 'X=X+TAY '  : X = X + TA Y
!+      OP = 'X=X-TAY '  : X = X - TA Y
!+      OP = 'X=X+CTAY'  : X = X + C TA Y
!+      OP = 'X=CTAY  '  : X = C TA Y
!
!note     IMPORTANT :
!+
!+     1) X, Y AND A CAN BE STRUCTURES OF BLOCKS
!+
!+        IF X IS A SIMPLE VECTOR, CALLS MATVEC
!+
!+        IF X IS A BLOCK OF 2 VECTORS, CALLS MA4VEC
!+
!+        IF X IS A BLOCK OF 3 VECTORS, CALLS MA9VEC
!+
!+
!+
!+     2) X AND Y CAN BE THE SAME AT THE TIME OF THE CALL; IN THIS
!+
!+        CASE, USES AN INTERMEDIATE WORKING ARRAY: MESH%T
!
!history  J-M HERVOUET (LNHE)
!+        28/12/05
!+        V5P6
!+   First version.
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
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+        ADD MODASS=3
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        22/03/2016
!+        V7P2
!+   Adding argument STOX to MATVCT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| MATRIX
!| C              |-->| A GIVEN CONSTANT
!| LEGO           |-->| IF PRESENT AND FALSE, NO ASSEMBLY
!| MESH           |-->| MESH STRUCTURE
!| OP             |-->| OPERATION TO BE DONE
!| X              |<--| RESULTING VECTOR
!| Y              |-->| A GIVEN VECTOR USED IN OPERATION OP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MATVEC => MATVEC
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
      USE DECLARATIONS_TELEMAC, ONLY : W_IS_FULL
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8)     , INTENT(IN)           :: OP
      TYPE(BIEF_OBJ)  , INTENT(INOUT)        :: X
      TYPE(BIEF_OBJ)  , INTENT(IN)           :: A,Y
      DOUBLE PRECISION, INTENT(IN)           :: C
      TYPE(BIEF_MESH) , INTENT(INOUT)        :: MESH
      LOGICAL         , INTENT(IN), OPTIONAL :: LEGO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELM1,IELM2,IELMX,IELMY,NELEM,NELMAX,SIZXA
      INTEGER NPT,NPT1,NPT2,NPOIN,NPMAX,DIMIKM,NPTFR
!
      INTEGER, DIMENSION(:), POINTER :: IKLE
!
      LOGICAL TRANS,LEGO2
!
!-----------------------------------------------------------------------
!
      IF(PRESENT(LEGO)) THEN
        LEGO2 = LEGO
      ELSE
        LEGO2 = .TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(Y%TYPE.NE.2.OR.A%TYPE.NE.3) THEN
        IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
        IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
        IF (LNG.EQ.1) WRITE(LU,52) A%NAME,A%TYPE
        IF (LNG.EQ.1) WRITE(LU,53)
        IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
        IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
        IF (LNG.EQ.2) WRITE(LU,62) A%NAME,A%TYPE
        IF (LNG.EQ.2) WRITE(LU,63)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  OPERATION WITH THE MATRIX TRANSPOSE ?
!
        TRANS =.FALSE.
        IF(OP(3:3).EQ.'T'.OR.OP(4:4).EQ.'T'.OR.OP(5:5).EQ.'T'.OR.
     &     OP(6:6).EQ.'T') TRANS = .TRUE.
!
!       EXTRACTS THE CHARACTERISTICS OF MATRIX M
!
        IELM1 = A%ELMLIN
        IELM2 = A%ELMCOL
        NPT1  = BIEF_NBPTS(IELM1,MESH)
        NPT2  = BIEF_NBPTS(IELM2,MESH)
        NPT   = MIN(NPT1,NPT2)
!
        IF(TRANS) THEN
          MESH%T%ELM = IELM2
          MESH%T%DIM1 = BIEF_NBPTS(IELM2,MESH)
          IELMX=IELM2
        ELSE
          MESH%T%ELM = IELM1
          MESH%T%DIM1 = BIEF_NBPTS(IELM1,MESH)
          IELMX=IELM1
        ENDIF
!       TRIAL
        CALL CPSTVC(MESH%T,X)
!       END TRIAL
!
        IELMY = Y%ELM
!
        IF(.NOT.TRANS.AND.IELM2.NE.IELMY) THEN
          IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
          IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
          IF (LNG.EQ.1) WRITE(LU,52) A%NAME,A%TYPE
          IF (LNG.EQ.1) WRITE(LU,54) IELM1,IELM2,IELMY
          IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
          IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
          IF (LNG.EQ.2) WRITE(LU,62) A%NAME,A%TYPE
          IF (LNG.EQ.2) WRITE(LU,64) IELM1,IELM2,IELMY
50        FORMAT(1X,'MATVEC (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
51        FORMAT(1X,'                NOM DE Y : ',A6,'  TYPE : ',1I6)
52        FORMAT(1X,'                NOM DE A : ',A6,'  TYPE : ',1I6)
53        FORMAT(1X,'                CAS NON PREVU')
54        FORMAT(1X,'A ET Y INCOMPATIBLES  : ',1I6,2X,1I6,' ET ',1I6)
60        FORMAT(1X,'MATVEC (BIEF) : NAME OF X : ',A6,'  TYPE : ',1I6)
61        FORMAT(1X,'                NAME OF Y : ',A6,'  TYPE : ',1I6)
62        FORMAT(1X,'                NAME OF A : ',A6,'  TYPE : ',1I6)
63        FORMAT(1X,'                NOT IMPLEMENTED')
64        FORMAT(1X,'A AND Y INCOMPATIBLE  : ',1I6,2X,1I6,' AND ',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(TRANS.AND.IELM1.NE.IELMY) THEN
          IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
          IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
          IF (LNG.EQ.1) WRITE(LU,52) A%NAME,A%TYPE
          IF (LNG.EQ.1) WRITE(LU,154) IELM1,IELM2,IELMY
          IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
          IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
          IF (LNG.EQ.2) WRITE(LU,62) A%NAME,A%TYPE
          IF (LNG.EQ.2) WRITE(LU,164) IELM1,IELM2,IELMY
154       FORMAT(1X,'A ET Y INCOMPATIBLES : ',1I6,2X,1I6,' ET ',1I6,/,
     &           1X,'POUR UNE OPERATION SUR LA TRANSPOSEE DE A')
164       FORMAT(1X,'A AND Y INCOMPATIBLE  : ',1I6,2X,1I6,' AND ',1I6,/,
     &           1X,'BECAUSE THE TRANSPOSED OF A IS USED')
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(.NOT.TRANS.AND.IELM1.NE.IELMX) THEN
          IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
          IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
          IF (LNG.EQ.1) WRITE(LU,52) A%NAME,A%TYPE
          IF (LNG.EQ.1) WRITE(LU,55) IELM1,IELMX
          IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
          IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
          IF (LNG.EQ.2) WRITE(LU,62) A%NAME,A%TYPE
          IF (LNG.EQ.2) WRITE(LU,65) IELM1,IELMX
55        FORMAT(1X,'A ET X INCOMPATIBLES : ',1I6,2X,1I6,' ET ',1I6)
65        FORMAT(1X,'A AND X INCOMPATIBLE  : ',1I6,2X,1I6,' AND ',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(TRANS.AND.IELM2.NE.IELMX) THEN
          IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
          IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
          IF (LNG.EQ.1) WRITE(LU,52) A%NAME,A%TYPE
          IF (LNG.EQ.1) WRITE(LU,155) IELM1,IELMX
          IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
          IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
          IF (LNG.EQ.2) WRITE(LU,62) A%NAME,A%TYPE
          IF (LNG.EQ.2) WRITE(LU,165) IELM1,IELMX
155       FORMAT(1X,'A ET X INCOMPATIBLES : ',1I6,2X,1I6,' ET ',1I6,/,
     &           1X,'POUR UNE OPERATION SUR LA TRANSPOSEE DE A')
165       FORMAT(1X,'A AND X INCOMPATIBLE: ',1I6,2X,1I6,' AND ',/,1X,
     &           1X,'BECAUSE THE TRANSPOSED OF A IS USED')
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(DIMENS(IELM1).EQ.MESH%DIM1) THEN
!
!         NORMAL MATRIX
!
          NELEM = MESH%NELEM
          NELMAX= MESH%NELMAX
          IKLE=>MESH%IKLE%I
          IF(A%STO.EQ.1) THEN
            SIZXA=NELMAX
          ELSEIF(A%STO.EQ.3) THEN
            SIZXA=A%X%DIM1
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'STOCKAGE INCONNU DANS MATVEC : ',A%STO
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'UNKNOWN STORAGE IN MATVEC : ',A%STO
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!
        ELSE
!
!         BOUNDARY MATRIX (NEVER WITH EDGE-BASED STORAGE)
!
          NELEM = MESH%NPTFR
          NELMAX= MESH%NPTFRX
          IKLE=>MESH%NBOR%I
          SIZXA=NELMAX
!
        ENDIF
!
        NPTFR= MESH%NPTFR
        NPOIN= MESH%NPOIN
        NPMAX= MESH%NPMAX
        DIMIKM=MESH%IKLEM1%DIM1
!
!-----------------------------------------------------------------------
!       CALLS MATVCT
!-----------------------------------------------------------------------
!
      IF(W_IS_FULL.AND.OP(3:3).NE.'X') THEN
!
        IF (LNG.EQ.1) WRITE(LU,500)
        IF (LNG.EQ.2) WRITE(LU,501)
500     FORMAT(1X,'MATVEC (BIEF) : UN APPEL AVEC LEGO = .FALSE.',/,
     &         1X,'                DOIT ETRE SUIVI D''UN APPEL AVEC',/,
     &         1X,'                OP=''X=X+....''')
501     FORMAT(1X,'MATVEC (BIEF) : A CALL WITH LEGO = .FALSE.',/,
     &         1X,'                MUST BE FOLLOWED BY A CALL WITH',/,
     &         1X,'                OP=''X=X+....''')
        CALL PLANTE(1)
        STOP
!
      ELSEIF(W_IS_FULL.OR.(X%NAME.NE.Y%NAME.AND.A%STO.EQ.3)) THEN
!
        IF (MODASS .EQ.1) THEN
          CALL MATVCT( OP,X%R,A%D%R,A%TYPDIA,A%X%R,A%TYPEXT,Y%R,
     &               C,IKLE,NPT,NELEM,NELMAX,MESH%W%R,
     &               LEGO2,IELM1,IELM2,IELMX,MESH%LV,A%STO,A%PRO,
     &               MESH%IKLEM1%I,DIMIKM,MESH%LIMVOI%I,MESH%MXPTVS,
     &               NPMAX,NPOIN,NPTFR,
     &               MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,SIZXA,
     &               BIEF_NBPEL(IELMX,MESH),MESH,A%STOX)
        ELSEIF (MODASS .EQ.3) THEN
          CALL MATVCT( OP,X%R,A%D%R,A%TYPDIA,A%X%R,A%TYPEXT,Y%R,
     &               C,IKLE,NPT,NELEM,NELMAX,MESH%W%R,
     &               LEGO2,IELM1,IELM2,IELMX,MESH%LV,A%STO,A%PRO,
     &               MESH%IKLEM1%I,DIMIKM,MESH%LIMVOI%I,MESH%MXPTVS,
     &               NPMAX,NPOIN,NPTFR,
     &               MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,SIZXA,
     &               BIEF_NBPEL(IELMX,MESH),MESH,A%STOX,
     &             X_ERR=X%E, Y_ERR=Y%E , DA_ERR=A%D%E,
     &               XA_ERR=A%X%E)
        ENDIF
!
      ELSE
!
        IF(TRANS) THEN
!
          CALL MATVCT( 'X=TAY   ',MESH%T%R,A%D%R,A%TYPDIA,A%X%R,
     &                 A%TYPEXT,Y%R,C,IKLE,NPT,NELEM,NELMAX,MESH%W%R,
     &                 LEGO2,IELM1,IELM2,IELMX,MESH%LV,A%STO,A%PRO,
     &                 MESH%IKLEM1%I,DIMIKM,MESH%LIMVOI%I,MESH%MXPTVS,
     &                 NPMAX,NPOIN,NPTFR,
     &                 MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,SIZXA,
     &                 BIEF_NBPEL(IELMX,MESH),MESH,A%STOX)
!
          IF(OP(3:8).EQ.'TAY   ') THEN
            CALL OS( 'X=Y     ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'-TAY  ') THEN
            CALL OS( 'X=-Y    ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'X+TAY ') THEN
            CALL OS( 'X=X+Y   ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'X-TAY ') THEN
            CALL OS( 'X=X-Y   ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'X+CTAY') THEN
            CALL OS( 'X=X+CY  ' , X , MESH%T , X , C )
!         ELSEIF(OP(3:8).EQ.'CTAY  ') THEN
!           CALL OS( 'X=CY    ' , X , MESH%T , X , C )
          ELSE
           IF (LNG.EQ.1) WRITE(LU,3000) OP
           IF (LNG.EQ.2) WRITE(LU,3001) OP
3000       FORMAT(1X,'MATVEC (BIEF) : OPERATION INCONNUE : ',A8)
3001       FORMAT(1X,'MATVEC (BIEF) : UNKNOWN OPERATION : ',A8)
           CALL PLANTE(1)
           STOP
          ENDIF
!
        ELSE
!
          IF (MODASS .EQ.1) THEN
            CALL MATVCT( 'X=AY    ',MESH%T%R,A%D%R,A%TYPDIA,A%X%R,
     &                 A%TYPEXT,Y%R,C,IKLE,NPT,NELEM,NELMAX,MESH%W%R,
     &                 LEGO2,IELM1,IELM2,IELMX,MESH%LV,A%STO,A%PRO,
     &                 MESH%IKLEM1%I,DIMIKM,MESH%LIMVOI%I,MESH%MXPTVS,
     &                 NPMAX,NPOIN,NPTFR,
     &                 MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,SIZXA,
     &                 BIEF_NBPEL(IELMX,MESH),MESH,A%STOX)
          ELSEIF (MODASS .EQ.3) THEN
            MESH%T%E=0.D0
            CALL MATVCT( 'X=AY    ',MESH%T%R,A%D%R,A%TYPDIA,A%X%R,
     &                 A%TYPEXT,Y%R,C,IKLE,NPT,NELEM,NELMAX,MESH%W%R,
     &                 LEGO2,IELM1,IELM2,IELMX,MESH%LV,A%STO,A%PRO,
     &                 MESH%IKLEM1%I,DIMIKM,MESH%LIMVOI%I,MESH%MXPTVS,
     &                 NPMAX,NPOIN,NPTFR,
     &                 MESH%GLOSEG%I,MESH%GLOSEG%MAXDIM1,SIZXA,
     &                 BIEF_NBPEL(IELMX,MESH),MESH,A%STOX,
     &               X_ERR=MESH%T%E, Y_ERR=Y%E , DA_ERR=A%D%E)
          ENDIF
!
          IF(OP(3:8).EQ.'AY    ') THEN
            CALL OS( 'X=Y     ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'X+AY  ') THEN
            CALL OS( 'X=X+Y   ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'-AY   ') THEN
            CALL OS( 'X=-Y    ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'X-AY  ') THEN
            CALL OS( 'X=X-Y   ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'X+CAY ') THEN
            CALL OS( 'X=X+CY  ' , X , MESH%T , X , C )
          ELSEIF(OP(3:8).EQ.'CAY   ') THEN
            CALL OS( 'X=CY    ' , X , MESH%T , X , C )
          ELSE
           IF (LNG.EQ.1) WRITE(LU,3000) OP
           IF (LNG.EQ.2) WRITE(LU,3001) OP
           CALL PLANTE(1)
           STOP
          ENDIF
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IF LEGO WAS FALSE, MATVEC WILL HAVE TO TAKE IT INTO ACCOUNT,
!     BECAUSE A NON-ASSEMBLED VECTOR WILL BE IN MESH%W
!
      IF(.NOT.LEGO2) THEN
        W_IS_FULL = .TRUE.
      ELSE
        W_IS_FULL = .FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
