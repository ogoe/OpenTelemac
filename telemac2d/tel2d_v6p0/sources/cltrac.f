C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MANAGES THE BOUNDARY CONDITIONS FOR TRACER.
!>                FOR WEIRS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> H, LITBOR, NBOR, NPSING, NPSMAX, NTRAC, NUMDIG, NWEIRS, T, TBOR, ZDIG, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ITRAC, N, N1, N2, Z1, Z2
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 03/10/1996
!> </td><td> J.-M. HERVOUET (LNH) 01 30 87 80 18
!> </td><td> MODIFIED
!> </td></tr>
!>      <td><center>                                           </center>
!> </td><td> 19/04/1996
!> </td><td> V. GUINOT (LHF)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU.
!>    </td></tr>
!>          <tr><td>LITBOR(J)
!></td><td><--</td><td>TYPE DE LA CONDITION EN TRACEUR AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION GLOBALE DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NPSING(N)
!></td><td>--></td><td>NOMBRE DE POINTS DE CHAQUE COTE DE LA
!>                  SINGULARITE N.
!>    </td></tr>
!>          <tr><td>NPSMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE POINTS POUR UN COTE D'UNE
!>                  SINGULARITE.
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMDIG(K,N,I)
!></td><td>--></td><td>NUMERO DES POINTS DES DIGUES
!>                  DANS LA NUMEROTATION DES POINTS DE BORD
!>                  DES CONDITIONS AUX LIMITES) DU I-EME
!>                  POINT SUR LE COTE K DE L'OUVRAGE N
!>    </td></tr>
!>          <tr><td>NWEIRS
!></td><td>--></td><td>NOMBRE DE SINGULARITES LINEIQUES.
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TRACEUR.
!>    </td></tr>
!>          <tr><td>TBOR(J)
!></td><td><--</td><td>VALEUR DE LA CONDITION EN TRACEUR AU
!>                  J-EME POINT LIMITE
!>    </td></tr>
!>          <tr><td>ZDIG
!></td><td>--></td><td>COTE DES POINTS DES SEUILS.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CLTRAC
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,ZF,ZDIG,H,T,NBOR,LITBOR,TBOR,NTRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| H             |-->| HAUTEUR D'EAU.
C| LITBOR(J)      |<--| TYPE DE LA CONDITION EN TRACEUR AU
C|                |   | J-EME POINT LIMITE
C| NBOR           |-->| NUMEROTATION GLOBALE DES POINTS DE BORD.
C| NPSING(N)      |-->| NOMBRE DE POINTS DE CHAQUE COTE DE LA
C|                |   | SINGULARITE N.
C| NPSMAX         |-->| NOMBRE MAXIMUM DE POINTS POUR UN COTE D'UNE
C|                |   | SINGULARITE.
C| NTRAC          |---| 
C| NUMDIG(K,N,I)  |-->| NUMERO DES POINTS DES DIGUES
C|                |   | DANS LA NUMEROTATION DES POINTS DE BORD
C|                |   | DES CONDITIONS AUX LIMITES) DU I-EME
C|                |   | POINT SUR LE COTE K DE L'OUVRAGE N
C| NWEIRS         |-->| NOMBRE DE SINGULARITES LINEIQUES.
C| T             |-->| TRACEUR.
C| TBOR(J)        |<--| VALEUR DE LA CONDITION EN TRACEUR AU
C|                |   | J-EME POINT LIMITE
C| ZDIG           |-->| COTE DES POINTS DES SEUILS.
C| ZF             |-->| COTE DU FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NWEIRS,NPSMAX,NTRAC
      INTEGER, INTENT(IN) :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,NPSMAX)
      INTEGER, INTENT(IN) :: NBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: ZDIG(NWEIRS,NPSMAX)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(*),H(*)
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LITBOR,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: T
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,N,N1,N2,ITRAC
C
      DOUBLE PRECISION Z1,Z2
C
C-----------------------------------------------------------------------
C
      DO ITRAC=1,NTRAC
C
        DO 10 N=1,NWEIRS
        DO 20 I=1,NPSING(N)
C
          N1=NBOR(NUMDIG(1,N,I))
          N2=NBOR(NUMDIG(2,N,I))
          Z1=H(N1)+ZF(N1)
          Z2=H(N2)+ZF(N2)
          IF(Z1.GT.Z2.AND.Z1.GT.ZDIG(N,I)) THEN
            TBOR%ADR(ITRAC)%P%R(NUMDIG(1,N,I))=T%ADR(ITRAC)%P%R(N1)
            TBOR%ADR(ITRAC)%P%R(NUMDIG(2,N,I))=T%ADR(ITRAC)%P%R(N1)
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=4
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=5
          ELSEIF(Z2.GE.Z1.AND.Z2.GT.ZDIG(N,I)) THEN
            TBOR%ADR(ITRAC)%P%R(NUMDIG(1,N,I))=T%ADR(ITRAC)%P%R(N2)
            TBOR%ADR(ITRAC)%P%R(NUMDIG(2,N,I))=T%ADR(ITRAC)%P%R(N2)
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=5
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=4
          ELSE
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(1,N,I))=2
            LITBOR%ADR(ITRAC)%P%I(NUMDIG(2,N,I))=2
          ENDIF
C
20      CONTINUE
10      CONTINUE
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C