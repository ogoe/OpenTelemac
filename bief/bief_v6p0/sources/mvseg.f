C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MATRIX VECTOR PRODUCT FOR EDGE-BASED STORAGE.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND MATRIX M.<br>
!>   THE RESULT IS VECTOR X.<br>
!>   THESE OPERATIONS ARE DIFFERENT DEPENDING ON THE DIAGONAL TYPE
!>   AND THE TYPE OF EXTRADIAGONAL TERMS.<br>
!>   IMPLEMENTED OPERATIONS:<br>
!>      OP = 'X=AY    '  : X = AY
!>      OP = 'X=CAY   '  : X = CAY
!>      OP = 'X=-AY   '  : X = -AY
!>      OP = 'X=X+AY  '  : X = X + AY
!>      OP = 'X=X-AY  '  : X = X - AY
!>      OP = 'X=X+CAY '  : X = X + C AY
!>      OP = 'X=TAY   '  : X = TA Y (TRANSPOSE OF A)
!>      OP = 'X=-TAY  '  : X = - TA Y (- TRANSPOSE OF A)
!>      OP = 'X=X+TAY '  : X = X + TA Y
!>      OP = 'X=X-TAY '  : X = X - TA Y
!>      OP = 'X=X+CTAY'  : X = X + C TA Y
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, DA, GLOSEG1, GLOSEG2, IELM1, IELM2, NELEM, NPOIN, NSEG1, NSEG2, OP, TYPDIA, TYPEXT, X, XA1, XA2, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ISEG, MAXSEG, MINSEG, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MVSEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATVCT()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 05/02/91
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>DA
!></td><td>--></td><td>DIAGONALE DE LA MATRICE
!>    </td></tr>
!>          <tr><td>GLOSEG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS.
!>    </td></tr>
!>          <tr><td>NSEG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td>--></td><td>TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
!>                  TYPDIA = 'Q' : DIAGONALE QUELCONQUE
!>                  TYPDIA = 'I' : DIAGONALE IDENTITE.
!>                  TYPDIA = '0' : DIAGONALE NULLE.
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>--></td><td>TYPEXT = 'Q' : QUELCONQUES.
!>                  TYPEXT = 'S' : SYMETRIQUES.
!>                  TYPEXT = '0' : NULS.
!>    </td></tr>
!>          <tr><td>W1,
!></td><td><--</td><td>TABLEAUX DE TRAVAIL DE DIMENSION NELEM
!>                  QUI CONTIENDRONT UNE PARTIE DU RESULTAT SOUS
!>                  FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR IMAGE
!>    </td></tr>
!>          <tr><td>XA1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA12,
!></td><td>--></td><td>TERMES EXTRADIAGONAUX ELEMENTAIRES
!>    </td></tr>
!>          <tr><td>XA2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MVSEG
     &(OP, X , DA,TYPDIA,XA1,XA2,
     & TYPEXT, Y,C,NPOIN,NELEM,NSEG1,NSEG2,GLOSEG1,GLOSEG2,IELM1,IELM2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| DA             |-->| DIAGONALE DE LA MATRICE
C| GLOSEG1        |---| 
C| GLOSEG2        |---| 
C| IELM1          |---| 
C| IELM2          |---| 
C| IKLE1,         |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NPOIN          |-->| NOMBRE DE POINTS.
C| NSEG1          |---| 
C| NSEG2          |---| 
C| OP             |-->| OPERATION A EFFECTUER
C| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
C|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
C|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
C|                |   | TYPDIA = '0' : DIAGONALE NULLE.
C| TYPEXT         |-->| TYPEXT = 'Q' : QUELCONQUES.
C|                |   | TYPEXT = 'S' : SYMETRIQUES.
C|                |   | TYPEXT = '0' : NULS.
C| W1,            |<--| TABLEAUX DE TRAVAIL DE DIMENSION NELEM
C|                |   | QUI CONTIENDRONT UNE PARTIE DU RESULTAT SOUS
C|                |   | FORME NON ASSEMBLEE.
C| X             |<--| VECTEUR IMAGE
C| XA1            |---| 
C| XA12,          |-->| TERMES EXTRADIAGONAUX ELEMENTAIRES
C| XA2            |---| 
C| Y             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF,EX_MVSEG => MVSEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN
      INTEGER, INTENT(IN) :: GLOSEG1(*),GLOSEG2(*)
      INTEGER, INTENT(IN) :: NSEG1,NSEG2,NELEM,IELM1,IELM2
C
      DOUBLE PRECISION, INTENT(IN)    :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN)    :: XA1(*),XA2(*)
      DOUBLE PRECISION, INTENT(IN)    :: C
C
      CHARACTER(LEN=8),INTENT(IN) :: OP
      CHARACTER(LEN=1),INTENT(IN) :: TYPDIA,TYPEXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ISEG,I,MINSEG,MAXSEG
      DOUBLE PRECISION Z(1)
C
      INTRINSIC MIN,MAX
C
C-----------------------------------------------------------------------
C
      MINSEG = MIN(NSEG1,NSEG2)
      MAXSEG = MAX(NSEG1,NSEG2)
C
      IF(OP(1:8).EQ.'X=AY    ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
C          SQUARE PART
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))+XA1(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
C
C          THE REST OF THE RECTANGULAR MATRIX
C
           IF(NSEG1.GT.NSEG2) THEN
C            PART OF X HAS NOT BEEN INITIALISED
C            BY THE CONTRIBUTION OF THE DIAGONAL
             IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
C              OPTIMISATION FOR QUASI-BUBBLE ELEMENT
               DO I = NPOIN+1,NPOIN+NELEM
                 X(I)=0.D0
               ENDDO
             ELSE
               DO ISEG = MINSEG+1,MAXSEG
                 X(GLOSEG2(ISEG))=0.D0
               ENDDO
             ENDIF
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF(LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF(LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=CYZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=CY    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))+C*XA1(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
C              OPTIMISATION FOR QUASI-BUBBLE ELEMENT
               DO I = NPOIN+1,NPOIN+NELEM
                 X(I)=0.D0
               ENDDO
             ELSE
               DO ISEG = MINSEG+1,MAXSEG
                 X(GLOSEG2(ISEG))=0.D0
               ENDDO
             ENDIF
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=-AY   ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))-XA1(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
C              OPTIMISATION FOR QUASI-BUBBLE ELEMENT
               DO I = NPOIN+1,NPOIN+NELEM
                 X(I)=0.D0
               ENDDO
             ELSE
               DO ISEG = MINSEG+1,MAXSEG
                 X(GLOSEG2(ISEG))=0.D0
               ENDDO
             ENDIF
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))+XA1(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))-XA1(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))+C*XA1(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))+XA1(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
C              OPTIMISATION FOR QUASI-BUBBLE ELEMENT
               DO I = NPOIN+1,NPOIN+NELEM
                 X(I)=0.D0
               ENDDO
             ELSE
               DO ISEG = MINSEG+1,MAXSEG
                 X(GLOSEG2(ISEG))=0.D0
               ENDDO
             ENDIF
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=-TAY   ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))-XA1(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             IF(IELM1.EQ.12.OR.IELM2.EQ.12) THEN
C              OPTIMISATION FOR QUASI-BUBBLE ELEMENT
               DO I = NPOIN+1,NPOIN+NELEM
                 X(I)=0.D0
               ENDDO
             ELSE
               DO ISEG = MINSEG+1,MAXSEG
                 X(GLOSEG2(ISEG))=0.D0
               ENDDO
             ENDIF
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))+XA1(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))+XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))+XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))-XA1(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))-XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))-XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q'.OR.TYPEXT(1:1).EQ.'S') THEN
C
           DO ISEG = 1 , MINSEG
             X(GLOSEG1(ISEG))=
     &       X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
             X(GLOSEG2(ISEG))=
     &       X(GLOSEG2(ISEG))+C*XA1(ISEG)*Y(GLOSEG1(ISEG))
           ENDDO
           IF(NSEG1.GT.NSEG2) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG1(ISEG))=
     &         X(GLOSEG1(ISEG))+C*XA2(ISEG)*Y(GLOSEG2(ISEG))
             ENDDO
           ELSEIF(NSEG2.GT.NSEG1) THEN
             DO ISEG = MINSEG+1,MAXSEG
               X(GLOSEG2(ISEG))=
     &         X(GLOSEG2(ISEG))+C*XA2(ISEG)*Y(GLOSEG1(ISEG))
             ENDDO
           ENDIF
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,3000) OP
        IF (LNG.EQ.2) WRITE(LU,3001) OP
        CALL PLANTE(1)
        STOP
C
C-----------------------------------------------------------------------
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
C
1000  FORMAT(1X,'MVSEG (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MVSEG (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MVSEG (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MVSEG (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MVSEG (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MVSEG (BIEF) : UNKNOWN OPERATION : ',A8)
C
      END
C
C#######################################################################
C