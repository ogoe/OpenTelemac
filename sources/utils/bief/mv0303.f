!                    *****************
                     SUBROUTINE MV0303
!                    *****************
!
     &(OP, X , DA,TYPDIA,XA12,XA13,XA21,XA23,XA31,XA32,
     & TYPEXT, Y,C,IKLE1,IKLE2,IKLE3,NPOIN,NELEM,W1,W2,W3
     & ,X_ERR,Y_ERR,DA_ERR)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    MATRIX VECTOR OPERATIONS FOR P1 TRIANGLES.
!code
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON VECTORS X,Y AND MATRIX M.
!+
!+   THE RESULT IS VECTOR X.
!+
!+   THESE OPERATIONS ARE DIFFERENT DEPENDING ON THE DIAGONAL TYPE
!+   AND THE TYPE OF EXTRADIAGONAL TERMS.
!+
!+   IMPLEMENTED OPERATIONS:
!+
!+      OP = 'X=AY    '  : X = AY
!+      OP = 'X=CAY   '  : X = CAY
!+      OP = 'X=-AY   '  : X = -AY
!+      OP = 'X=X+AY  '  : X = X + AY
!+      OP = 'X=X-AY  '  : X = X - AY
!+      OP = 'X=X+CAY '  : X = X + C AY
!+      OP = 'X=TAY   '  : X = TA Y (TRANSPOSE OF A)
!+      OP = 'X=-TAY  '  : X = - TA Y (- TRANSPOSE OF A)
!+      OP = 'X=X+TAY '  : X = X + TA Y
!+      OP = 'X=X-TAY '  : X = X - TA Y
!+      OP = 'X=X+CTAY'  : X = X + C TA Y
!
!history  J-M HERVOUET (LNH)    ; F LEPEINTRE (LNH)
!+        05/02/91
!+        V5P1
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
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+        ADD MODASS=3
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT
!| DA             |-->| MATRIX DIAGONAL
!| IKLE1          |-->| FIRST POINTS OF TRIANGLES
!| IKLE2          |-->| SECOND POINTS OF TRIANGLES
!| IKLE3          |-->| THIRD POINTS OF TRIANGLES
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| TYPDIA         |-->| TYPE OF DIAGONAL:
!|                |   | TYPDIA = 'Q' : ANY VALUE
!|                |   | TYPDIA = 'I' : IDENTITY
!|                |   | TYPDIA = '0' : ZERO
!| TYPEXT         |-->| TYPE OF OFF-DIAGONAL TERMS
!|                |   | TYPEXT = 'Q' : ANY VALUE
!|                |   | TYPEXT = 'S' : SYMMETRIC
!|                |   | TYPEXT = '0' : ZERO
!| W1             |<->| RESULT IN NON ASSEMBLED FORM
!| W2             |<->| RESULT IN NON ASSEMBLED FORM
!| W3             |<->| RESULT IN NON ASSEMBLED FORM
!| X              |<->| RESULT IN ASSEMBLED FORM
!| XA13           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA21           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA23           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA31           |-->| OFF-DIAGONAL TERM OF MATRIX
!| XA32           |-->| OFF-DIAGONAL TERM OF MATRIX
!| Y              |-->| VECTOR USED IN THE OPERATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_MV0303=> MV0303
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NPOIN
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: XA12(*),XA13(*),XA23(*)
      DOUBLE PRECISION, INTENT(IN) :: XA21(*),XA31(*),XA32(*)
      DOUBLE PRECISION, INTENT(IN) ::C
!
      CHARACTER(LEN=8), INTENT(IN) :: OP
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
      DOUBLE PRECISION, OPTIONAL, INTENT(INOUT) :: X_ERR(*)
      DOUBLE PRECISION, OPTIONAL, INTENT(IN) :: Y_ERR(*),DA_ERR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'X=AY    ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
     &                    + XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =     XA23(IELEM) * Y(IKLE3(IELEM))
     &                    + XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) =     XA31(IELEM) * Y(IKLE1(IELEM))
     &                    + XA32(IELEM) * Y(IKLE2(IELEM))
          END DO
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=YZ    ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =  C * (   XA12(IELEM) * Y(IKLE2(IELEM))
     &                         + XA13(IELEM) * Y(IKLE3(IELEM))  )
            W2(IELEM) =  C * (   XA23(IELEM) * Y(IKLE3(IELEM))
     &                         + XA21(IELEM) * Y(IKLE1(IELEM))  )
            W3(IELEM) =  C * (   XA31(IELEM) * Y(IKLE1(IELEM))
     &                         + XA32(IELEM) * Y(IKLE2(IELEM))  )
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=CYZ   ', X , Y , DA , C  , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=CYZ   ', X , Y , DA , C  , NPOIN,
     &         X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=CY    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-AY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS:
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
     &                    - XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =   - XA23(IELEM) * Y(IKLE3(IELEM))
     &                    - XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) =   - XA31(IELEM) * Y(IKLE1(IELEM))
     &                    - XA32(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=-YZ   ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM)   + XA12(IELEM) * Y(IKLE2(IELEM))
     &                              + XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM)   + XA23(IELEM) * Y(IKLE3(IELEM))
     &                              + XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) = W3(IELEM)   + XA31(IELEM) * Y(IKLE1(IELEM))
     &                              + XA32(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM)   - XA12(IELEM) * Y(IKLE2(IELEM))
     &                              - XA13(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM)   - XA23(IELEM) * Y(IKLE3(IELEM))
     &                              - XA21(IELEM) * Y(IKLE1(IELEM))
            W3(IELEM) = W3(IELEM)   - XA31(IELEM) * Y(IKLE1(IELEM))
     &                              - XA32(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X-YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM)=W1(IELEM) + C * (XA12(IELEM) * Y(IKLE2(IELEM))
     &                                +XA13(IELEM) * Y(IKLE3(IELEM)))
            W2(IELEM)=W2(IELEM) + C * (XA23(IELEM) * Y(IKLE3(IELEM))
     &                                +XA21(IELEM) * Y(IKLE1(IELEM)))
            W3(IELEM)=W3(IELEM) + C * (XA31(IELEM) * Y(IKLE1(IELEM))
     &                                +XA32(IELEM) * Y(IKLE2(IELEM)))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL:
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=X+CYZ  ', X , Y , DA , C , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+CYZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+CY   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   + XA21(IELEM) * Y(IKLE2(IELEM))
     &                    + XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =   + XA12(IELEM) * Y(IKLE1(IELEM))
     &                    + XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) =   + XA13(IELEM) * Y(IKLE1(IELEM))
     &                    + XA23(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
             CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=YZ    ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=-TAY   ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) =   - XA21(IELEM) * Y(IKLE2(IELEM))
     &                    - XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
     &                    - XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) =   - XA13(IELEM) * Y(IKLE1(IELEM))
     &                    - XA23(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).EQ.'0') THEN
!
          CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
          CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
!
        ELSE
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=-YZ   ', X , Y , DA , C  , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'0') THEN
          CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) + XA21(IELEM) * Y(IKLE2(IELEM))
     &                            + XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM) + XA12(IELEM) * Y(IKLE1(IELEM))
     &                            + XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) = W3(IELEM) + XA13(IELEM) * Y(IKLE1(IELEM))
     &                            + XA23(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) - XA21(IELEM) * Y(IKLE2(IELEM))
     &                            - XA31(IELEM) * Y(IKLE3(IELEM))
            W2(IELEM) = W2(IELEM) - XA12(IELEM) * Y(IKLE1(IELEM))
     &                            - XA32(IELEM) * Y(IKLE3(IELEM))
            W3(IELEM) = W3(IELEM) - XA13(IELEM) * Y(IKLE1(IELEM))
     &                            - XA23(IELEM) * Y(IKLE2(IELEM))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X-YZ  ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
!
!   CONTRIBUTION OF EXTRADIAGONAL TERMS
!
        IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
!
          DO IELEM = 1 , NELEM
            W1(IELEM) = W1(IELEM) + C*(XA21(IELEM) * Y(IKLE2(IELEM))
     &                                +XA31(IELEM) * Y(IKLE3(IELEM)))
            W2(IELEM) = W2(IELEM) + C*(XA12(IELEM) * Y(IKLE1(IELEM))
     &                                +XA32(IELEM) * Y(IKLE3(IELEM)))
            W3(IELEM) = W3(IELEM) + C*(XA13(IELEM) * Y(IKLE1(IELEM))
     &                                +XA23(IELEM) * Y(IKLE2(IELEM)))
          ENDDO ! IELEM
!
        ELSEIF(TYPEXT(1:1).NE.'0') THEN
!
          IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
          IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
          CALL PLANTE(0)
          STOP
!
        ENDIF
!
!   CONTRIBUTION OF THE DIAGONAL
!
        IF(TYPDIA(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP ('X=X+CYZ ', X , Y , DA , C , NPOIN,
     &        X_ERR, Y_ERR , DA_ERR)
          ENDIF
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,3000) OP
        IF (LNG.EQ.2) WRITE(LU,3001) OP
        CALL PLANTE(0)
        STOP
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
!
1000  FORMAT(1X,'MV0303 (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MV0303 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MV0303 (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MV0303 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MV0303 (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MV0303 (BIEF) : UNKNOWN OPERATION : ',A8)
!
      END
