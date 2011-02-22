C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SAME AS DOTS BUT TAKING PARALLELISM INTO ACCOUNT.
!><br>            SCALAR PRODUCT OF TWO OBJECTS, WHICH CAN BE:
!><br>            TWO VECTORS STRUCTURES, OR
!><br>            TWO VECTOR BLOCKS STRUCTURES OF IDENTICAL NUMBER AND
!>                CHARACTERISTICS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS IGNORED
!>            FOR THE TIME BEING

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH, X, Y
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IBL, NPX, TYPX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_P_DOTS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DOT(), PLANTE(), P_DOT(), P_DSUM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CGSQUA(), CGSTAB(), CHECK_DOT(), CVDFTR(), EQUNOR(), ERRMIN(), GMRES(), GRACJG(), RESCJG()

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
!> </td><td> 24/04/97
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td> AFTER REINHARD HINKELMANN (HANNOVER UNI.)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X ET Y
!></td><td>--></td><td>LES DEUX STRUCTURES DONT ON VEUT LE PRODUIT
!>                  SCALAIRE.
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION P_DOTS
     &( X , Y , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |-->| MAILLAGE.
C| X             |---| 
C| X ET Y         |-->| LES DEUX STRUCTURES DONT ON VEUT LE PRODUIT
C|                |   | SCALAIRE.
C| Y             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_P_DOTS => P_DOTS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)  :: X,Y
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER NPX,IBL,TYPX
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
C-----------------------------------------------------------------------
C
      TYPX = X%TYPE
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURES ARE BLOCKS
C
      IF(TYPX.EQ.4) THEN
C
       P_DOTS = 0.D0
C
       IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
         DO 99 IBL = 1 , X%N
           P_DOTS=P_DOTS+DOT(X%ADR(IBL)%P%DIM1,X%ADR(IBL)%P%R,
     &                                         Y%ADR(IBL)%P%R)
99       CONTINUE
       ELSE
         DO 100 IBL = 1 , X%N
           P_DOTS=P_DOTS+P_DOT(X%ADR(IBL)%P%DIM1,X%ADR(IBL)%P%R,
     &                                           Y%ADR(IBL)%P%R,
     &                                           MESH%FAC%R)
100      CONTINUE
       ENDIF
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURES ARE NOT BLOCKS
C  (ASSUMES THAT Y HAS THE SAME TYPE AS X)
C
      ELSEIF(TYPX.EQ.2) THEN
C
        NPX = X%DIM1
C
        IF(Y%DIM1.NE.NPX) THEN
          IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
          IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
          IF (LNG.EQ.1) WRITE(LU,52) X%DIM1,Y%DIM1
          IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
          IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
          IF (LNG.EQ.2) WRITE(LU,62) X%DIM1,Y%DIM1
52        FORMAT(1X,'TAILLES DIFFERENTES : ',1I6,' ET ',1I6)
62        FORMAT(1X,'DIFFERENT SIZES: ',1I6,' AND ',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
          P_DOTS=DOT(NPX,X%R,Y%R)
        ELSE
          P_DOTS=P_DOT(NPX,X%R,Y%R,MESH%FAC%R)
        ENDIF
C
C-----------------------------------------------------------------------
C
C  ERROR
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
         IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
         IF (LNG.EQ.1) WRITE(LU,53)
         IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
         IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
         IF (LNG.EQ.2) WRITE(LU,63)
50       FORMAT(1X,'P_DOTS (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
51       FORMAT(1X,'                NOM DE Y : ',A6,'  TYPE : ',1I6)
53       FORMAT(1X,'                CAS NON PREVU')
60       FORMAT(1X,'P_DOTS (BIEF) : NAME OF X : ',A6,'  TYPE : ',1I6)
61       FORMAT(1X,'                NAME OF Y : ',A6,'  TYPE : ',1I6)
63       FORMAT(1X,'                NOT IMPLEMENTED')
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C FINAL SUM ON ALL THE SUB-DOMAINS
C
      IF(NCSIZE.GT.1) P_DOTS = P_DSUM(P_DOTS)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C