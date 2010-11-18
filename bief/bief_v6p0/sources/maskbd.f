C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MASKS DRY OR PARTIALLY DRY ELEMENTS.
!>  @code
!>     ALGORITHM:<br>
!>    - AN ELEMENT IS MASKED IF THE BOTTOM ELEVATION ZFE IS HIGHER THAN
!>      THE FREE SURFACE ELEVATION AT ITS BARYCENTRE<br>
!>    - ANY ELEMENT WHICH BOTTOM ELEVATION ZFE IS HIGHER THAN THE
!>      ELEVATION ZFE OF A MASKED NEIGHBOUR IS IN TURN MASKED<br>
!>      WHEN TURNING AROUND A NODE, FUNCTION ZFE ONLY HAS A MIN AND
!>      A MAX (SEE TOPOGR). THE SECOND PHASE OF THE ALGORITHM THUS
!>      ENSURES THAT NO 2 PARTS OF THE DOMAIN ARE ONLY CONNECTED BY
!>      1 VERTEX. THIS TREATMENT ALSO PREVENTS INOPPORTUNE MASKING-
!>      DEMASKING, IN PARTICULAR IN TIDAL FLAT AREAS.<br><br>
!>      DISADVANTAGES:<br>
!>      THIS ALGORITHM ASSUMES THAT THE FREE SURFACE IS QUASI HORIZONTAL.
!>      IT IS WELL SUITED TO STUDY EVOLUTIONS DUE TO TIDAL EFFECTS, BUT
!>      NOT TO STUDY DAM BREAKS.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> HMIN, HN, IFABOR, IKLE, ITRA01, MASKEL, NELEM, NPOIN, ZF, ZFE
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPSILO, FLAG, I1, I2, I3, IELEM, N, ZSE
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MASK3D(), TELEMAC2D()

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
!> </td><td> 11/08/94
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS VOISINS.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE.
!>    </td></tr>
!>          <tr><td>ITRA01
!></td><td>--></td><td>TABLEAU DE TRAVAIL D'ENTIERS.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td><--</td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE NOEUDS.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND PAR NOEUD.
!>    </td></tr>
!>          <tr><td>ZFE
!></td><td>--></td><td>COTE DU FOND PAR ELEMENT.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MASKBD
     &(MASKEL,ZFE,ZF,HN,HMIN,IKLE,IFABOR,ITRA01,NELEM,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| HMIN           |---| 
C| HN             |---| 
C| IFABOR         |-->| NUMEROS DES ELEMENTS VOISINS.
C| IKLE           |-->| TABLE DE CONNECTIVITE.
C| ITRA01         |-->| TABLEAU DE TRAVAIL D'ENTIERS.
C| MASKEL         |<--| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NPOIN          |-->| NOMBRE DE NOEUDS.
C| ZF             |-->| COTE DU FOND PAR NOEUD.
C| ZFE            |-->| COTE DU FOND PAR ELEMENT.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM,NPOIN
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3),IFABOR(NELEM,3)
      INTEGER, INTENT(INOUT)          :: ITRA01(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: ZFE(NELEM),ZF(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HMIN
      DOUBLE PRECISION, INTENT(INOUT) :: MASKEL(NELEM)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I1,I2,I3,N
C
      DOUBLE PRECISION EPSILO,ZSE
C
      LOGICAL FLAG
C
      DATA EPSILO / 1.D-6 /
C
C-----------------------------------------------------------------------
C
      FLAG = .FALSE.
C
      DO 10 IELEM = 1,NELEM
         I1 = IKLE(IELEM,1)
         I2 = IKLE(IELEM,2)
         I3 = IKLE(IELEM,3)
         ZSE = (ZF(I1)+HN(I1)+ZF(I2)+HN(I2)+ZF(I3)+HN(I3))/3.D0
         IF (ZFE(IELEM)+HMIN+EPSILO.GT.ZSE) THEN
            FLAG = .TRUE.
            MASKEL(IELEM) = 0.D0
         ENDIF
10    CONTINUE
C
20    CONTINUE
C
      IF (FLAG) THEN
C
         FLAG = .FALSE.
         DO 30 IELEM = 1,NELEM
C
            ITRA01(IELEM) = 0
            IF (MASKEL(IELEM).GT.0.5D0) THEN
C
               N=IFABOR(IELEM,1)
               IF (N.GT.0) THEN
                  IF (MASKEL(N).LT.0.5D0.AND.ZFE(IELEM).GT.
     &                ZFE(N)-EPSILO) ITRA01(IELEM) = 1
               ENDIF
               N=IFABOR(IELEM,2)
               IF (N.GT.0) THEN
                  IF (MASKEL(N).LT.0.5D0.AND.ZFE(IELEM).GT.
     &                ZFE(N)-EPSILO) ITRA01(IELEM) = 1
               ENDIF
               N=IFABOR(IELEM,3)
               IF (N.GT.0) THEN
                  IF (MASKEL(N).LT.0.5D0.AND.ZFE(IELEM).GT.
     &                ZFE(N)-EPSILO) ITRA01(IELEM) = 1
               ENDIF
C
            ENDIF
C
30       CONTINUE
C
         DO 40 IELEM = 1,NELEM
            IF (ITRA01(IELEM).EQ.1) THEN
               FLAG = .TRUE.
               MASKEL(IELEM) = 0.D0
            ENDIF
40       CONTINUE
C
         GOTO 20
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