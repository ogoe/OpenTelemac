C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SCALAR PRODUCT OF TWO OBJECTS, WHICH CAN BE:
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
!>    </th><td> X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IBL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DOTS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DOT(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BILANT(), BILANT1(), CVDFTR(), SUSPENSION_BILAN(), SUSPENSION_MAIN()

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
!> </td><td> 08/12/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
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
                        DOUBLE PRECISION FUNCTION DOTS
     &( X , Y )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| X             |---| 
C| X ET Y         |-->| LES DEUX STRUCTURES DONT ON VEUT LE PRODUIT
C|                |   | SCALAIRE.
C| Y             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DOTS => DOTS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN) :: X,Y
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IBL
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURES ARE BLOCKS
C
      IF(X%TYPE.EQ.4) THEN
C
       DOTS = 0.D0
       DO IBL = 1 , X%N
         DOTS=DOTS+DOT(X%ADR(IBL)%P%DIM1,X%ADR(IBL)%P%R,Y%ADR(IBL)%P%R)
       ENDDO
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURES ARE NOT BLOCKS
C  (ASSUMES THAT Y HAS THE SAME TYPE AS X)
C
      ELSEIF(X%TYPE.EQ.2) THEN
C
        DOTS = DOT(X%DIM1,X%R,Y%R)
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
50       FORMAT(1X,'DOTS (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
51       FORMAT(1X,'              NOM DE Y : ',A6,'  TYPE : ',1I6)
53       FORMAT(1X,'              CAS NON PREVU')
60       FORMAT(1X,'DOTS (BIEF) : NAME OF X : ',A6,'  TYPE : ',1I6)
61       FORMAT(1X,'              NAME OF Y : ',A6,'  TYPE : ',1I6)
63       FORMAT(1X,'              NOT IMPLEMENTED')
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C