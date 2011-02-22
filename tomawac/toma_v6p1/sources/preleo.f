C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SELECTS THE COMPUTATION NODES CLOSEST
!>                TO THE REQUESTED OUTPUT POINTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NLEO, NOLEO, NPOIN2, X, XLEO, Y, YLEO
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIST, DIST2, I, ILEO
!>   </td></tr>
!>     </table>

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F. MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NLEO
!></td><td>--></td><td>NOMBRE DE POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>NOLEO
!></td><td><--</td><td>TABLEAU DES NUMERO DES POINTS CHOISIS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>ABSCISSES DES POINTS
!>    </td></tr>
!>          <tr><td>XLEO
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>ORDONNEES DES POINTS
!>    </td></tr>
!>          <tr><td>YLEO
!></td><td>--></td><td>TABLEAU DES ORDONNEES DES POINTS DE SORTIE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRELEO
     &(XLEO,YLEO,NLEO,X,Y,NPOIN2,NOLEO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NLEO           |-->| NOMBRE DE POINTS DE SORTIE
C| NOLEO          |<--| TABLEAU DES NUMERO DES POINTS CHOISIS
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| X             |-->| ABSCISSES DES POINTS
C| XLEO           |-->| TABLEAU DES ABSCISSES DES POINTS DE SORTIE
C| Y             |-->| ORDONNEES DES POINTS
C| YLEO           |-->| TABLEAU DES ORDONNEES DES POINTS DE SORTIE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER I,ILEO,NLEO,NPOIN2
C
      DOUBLE PRECISION X(NPOIN2)  , Y(NPOIN2)
      DOUBLE PRECISION XLEO(NLEO)  , YLEO(NLEO)
      DOUBLE PRECISION DIST,DIST2
C
      INTEGER NOLEO(NLEO)
C
C-----------------------------------------------------------------------
C
      DO 10 ILEO=1,NLEO
        DIST=1.D99
        DO 20 I=1,NPOIN2
         DIST2=(XLEO(ILEO)-X(I))**2+(YLEO(ILEO)-Y(I))**2
         IF (DIST2.LT.DIST) THEN
             DIST=DIST2
             NOLEO(ILEO)=I
         ENDIF
20      CONTINUE
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C