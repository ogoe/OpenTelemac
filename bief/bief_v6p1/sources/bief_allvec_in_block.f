C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES MEMORY FOR N VECTORS, WHICH WILL BE PART
!>                OF A GIVEN BLOCK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THIS MODIFICATION OF ALLVEC_IN_BLOCK ALLOWS ADDING A NUMBER
!>         OF IDENTICALLY NUMBERED VECTORS TO AN ALREADY EXISTING BLOCK
!>         WITHOUT DESTROYING THE PREVIOUS STRUCTURE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BLO, IELM, N, NAT, NDIM, NOMGEN, STATUT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CHIFFRE, I, IDEB, II, NOM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ALLVEC_IN_BLOCK
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLVEC()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>POINT_ARTEMIS(), POINT_SISYPHE(), POINT_TELEMAC2D(), POINT_TELEMAC3D()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 11/07/1995
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BLO
!></td><td><-></td><td>BLOC OU ON VA ALLOUER LES VECTEURS
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT DU VECTEUR, OU DIMENSION
!>                  (SUIVANT LE STATUT, VOIR PLUS BAS)
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NOMBRE DE VECTEURS A ALLOUER
!>    </td></tr>
!>          <tr><td>NAT
!></td><td><--</td><td>1: VECTEUR REEL   2:VECTEUR ENTIER
!>    </td></tr>
!>          <tr><td>NDIM
!></td><td>--></td><td>DEUXIEME DIMENSION DU VECTEUR
!>    </td></tr>
!>          <tr><td>NOMGEN
!></td><td>--></td><td>NOM GENERIQUE FORTRAN DES VECTEURS
!>    </td></tr>
!>          <tr><td>STATUT
!></td><td>--></td><td>STATUT DU VECTEUR :
!>                  0 : VECTEUR LIBRE, IELM EST ALORS SA DIMENSION
!>                  1 : VECTEUR DEFINI SUR LE MAILLAGE
!>                  IELM EST ALORS LE TYPE D'ELEMENT
!>                  CHANGEMENT DE DISCRETISATION INTERDIT
!>                  2 : COMME 1 MAIS CHANGEMENTS AUTORISES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BIEF_ALLVEC_IN_BLOCK
     &( BLO , N , NAT , NOMGEN , IELM , NDIM , STATUT , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BLO            |<->| BLOC OU ON VA ALLOUER LES VECTEURS
C| IELM           |-->| TYPE D'ELEMENT DU VECTEUR, OU DIMENSION
C|                |   | (SUIVANT LE STATUT, VOIR PLUS BAS)
C| N             |-->| NOMBRE DE VECTEURS A ALLOUER
C| NAT            |<--| 1: VECTEUR REEL   2:VECTEUR ENTIER
C| NDIM           |-->| DEUXIEME DIMENSION DU VECTEUR
C| NOMGEN         |-->| NOM GENERIQUE FORTRAN DES VECTEURS
C| STATUT         |-->| STATUT DU VECTEUR :
C|                |   | 0 : VECTEUR LIBRE, IELM EST ALORS SA DIMENSION
C|                |   | 1 : VECTEUR DEFINI SUR LE MAILLAGE
C|                |   | IELM EST ALORS LE TYPE D'ELEMENT
C|                |   | CHANGEMENT DE DISCRETISATION INTERDIT
C|                |   | 2 : COMME 1 MAIS CHANGEMENTS AUTORISES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_BIEF_ALLVEC_IN_BLOCK => BIEF_ALLVEC_IN_BLOCK
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: BLO
      INTEGER         , INTENT(IN)    :: IELM,NDIM,STATUT,NAT,N
      CHARACTER(LEN=6), INTENT(IN)    :: NOMGEN
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IDEB,I,II
C
      CHARACTER(LEN=6) :: NOM
      CHARACTER*1 CHIFFRE(0:9)
      DATA CHIFFRE/'0','1','2','3','4','5','6','7','8','9'/
      SAVE CHIFFRE
C
C-----------------------------------------------------------------------
C
      IDEB = 6
      DO 5 I=5,2,-1
        IF(NOMGEN(I:I).EQ.' ') IDEB = I
5     CONTINUE
C
C-----------------------------------------------------------------------
C
      IF(BLO%N+N.LE.BLO%MAXBLOCK) THEN
C
      IF(N.GT.0) THEN
C
      DO 10 I = BLO%N+1 , BLO%N+N
C
C  NAME OF THE VECTOR
C
        NOM=NOMGEN
        IF(I.LT.10) THEN
          IDEB = MIN(6,IDEB)
          NOM(IDEB:IDEB) = CHIFFRE(I)
        ELSEIF(I.LT.100) THEN
          IDEB = MIN(5,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/10)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(I-10*(I/10))
        ELSEIF(I.LT.1000) THEN
          IDEB = MIN(4,IDEB)
          NOM(IDEB  :IDEB  ) = CHIFFRE(I/100)
          II=I-100*(I/100)
          NOM(IDEB+1:IDEB+1) = CHIFFRE(II/10)
          NOM(IDEB+2:IDEB+2) = CHIFFRE(II-10*(II/10))
        ELSE
          STOP 'MORE THAN 999 VECTORS ASKED IN ALLVEC_IN_BLOCK'
        ENDIF
C
C  ALLOCATES THE VECTOR
C
        ALLOCATE(BLO%ADR(I)%P)
        CALL BIEF_ALLVEC(NAT,BLO%ADR(I)%P,NOM,IELM,NDIM,STATUT,MESH)
C
10    CONTINUE
C
      BLO%N=BLO%N+N
C
      ENDIF
C
      ELSE
C
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BIEF_ALLVEC_IN_BLOCK :'
        WRITE(LU,*) 'PLUS DE ',BLO%MAXBLOCK,' (',N,')'
        WRITE(LU,*) 'VECTEURS DEMANDES, CHANGER MAXBLOCK DANS ALLBLO.'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BIEF_ALLVEC_IN_BLOCK:'
        WRITE(LU,*) 'MORE THAN ',BLO%MAXBLOCK,'(',N,')'
        WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
        WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
      ENDIF
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
