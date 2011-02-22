C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TAKES INTO ACCOUNT POINTS OF TYPE DIRICHLET IN A SYSTEM
!>                OF LINEAR EQUATIONS WITH SYMMETRICAL MATRIX.
!><br>            IN THE EQUATIONS FOR POINTS NOT OF TYPE DIRICHLET :
!>                DIRICHLET VALUES ARE REMOVED.
!><br>            IN THE EQUATIONS FOR POINTS OF TYPE DIRICHLET :
!>                DEFINES AN EQUATION FIXING THE IMPOSED VALUE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  FOR SYSTEMS OF MATRICES BLOCKS :
!>            THE EXTRA-DIAGONAL MATRICES MUST BE NONSYMMETRICAL
!>            BECAUSE TAKING INTO ACCOUNT THE DIRICHLET POINTS
!>            MAKES THEM NONSYMMETRICAL

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FBOR, KDIR, LIMDIR, MASKPT, MESH, MSK, S, SM, WORK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIMLIM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DIRICH
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DIRI01(), DIRI04(), DIRI09(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO(), CVDFTR(), KEPSIL(), PROPAG(), PROPAG_ADJ(), WAVE_EQUATION()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 11/07/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> LIMPRO DIMENSION MODIFIED FOR QUADRATIC ELEMENTS
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F
!></td><td><-></td><td>VALEURS A L'ETAPE N+1 ET INITIALISATION
!>    </td></tr>
!>          <tr><td>FBOR
!></td><td>--></td><td>CONDITIONS AUX LIMITES DES POINTS DIRICHLET.
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>CONVENTION POUR LES CONDITIONS DE DIRICHLET
!>    </td></tr>
!>          <tr><td>LIMDIR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES .
!>                  SI LIMDIR(K) = KDIR LE KIEME POINT DE BORD
!>                  EST DE TYPE DIRICHLET.
!>                  DIMENSION LIMDIR(LIMDIM,6)
!>                  LIMDIM EST NPTFR OU 2*NPTFR (EN QUADRATIQUE)
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES POINTS
!>                  =1. : NORMAL   =0. : POINT MASQUE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>S
!></td><td><-></td><td>MATRICE DU SYSTEME
!>    </td></tr>
!>          <tr><td>SM
!></td><td>--></td><td>SECOND MEMBRE DU SYSTEME.
!>    </td></tr>
!>          <tr><td>WORK
!></td><td>--></td><td>BLOC DE TABLEAUX DE TRAVAIL.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIRICH
     &(F, S, SM , FBOR,LIMDIR,WORK,MESH,KDIR,MSK,MASKPT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F             |<->| VALEURS A L'ETAPE N+1 ET INITIALISATION
C| FBOR           |-->| CONDITIONS AUX LIMITES DES POINTS DIRICHLET.
C| KDIR           |-->| CONVENTION POUR LES CONDITIONS DE DIRICHLET
C| LIMDIR         |-->| TYPES DE CONDITIONS AUX LIMITES .
C|                |   | SI LIMDIR(K) = KDIR LE KIEME POINT DE BORD
C|                |   | EST DE TYPE DIRICHLET.
C|                |   | DIMENSION LIMDIR(LIMDIM,6)
C|                |   | LIMDIM EST NPTFR OU 2*NPTFR (EN QUADRATIQUE)
C| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS
C|                |   | =1. : NORMAL   =0. : POINT MASQUE.
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| S             |<->| MATRICE DU SYSTEME
C| SM             |-->| SECOND MEMBRE DU SYSTEME.
C| WORK           |-->| BLOC DE TABLEAUX DE TRAVAIL.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DIRICH => DIRICH
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C                                                   DIMLIM,6
      INTEGER        , INTENT(IN)    :: KDIR,LIMDIR(*)
      LOGICAL        , INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: WORK
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: F,SM,S
      TYPE(BIEF_OBJ) , INTENT(IN)    :: FBOR,MASKPT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER DIMLIM
C
C----------------------------------------------------------------------
C
C  IF S IS A MATRIX
C
      IF(S%TYPE.EQ.3) THEN
C
      CALL DIRI01(F, S, SM , FBOR,LIMDIR,WORK%ADR(1)%P,WORK%ADR(2)%P,
     &            MESH,KDIR,MSK,MASKPT)
C
C  IF S IS A BLOCK OF 4 MATRICES
C
      ELSEIF(S%TYPE.EQ.4.AND.S%N.EQ.4) THEN
C
      DIMLIM=MAX(FBOR%ADR(1)%P%DIM1,
     &           FBOR%ADR(2)%P%DIM1)
C
      CALL DIRI04(F%ADR(1)%P,F%ADR(2)%P,
     &     S%ADR(1)%P,S%ADR(2)%P,S%ADR(3)%P,S%ADR(4)%P,
     &     SM%ADR(1)%P,SM%ADR(2)%P,
     &     WORK%ADR(1)%P,WORK%ADR(2)%P,WORK%ADR(3)%P,WORK%ADR(4)%P,
     &     FBOR%ADR(1)%P,FBOR%ADR(2)%P,
     &     LIMDIR(1:DIMLIM),LIMDIR(DIMLIM+1:2*DIMLIM),
     &     MESH,KDIR,MSK,MASKPT)
C
C  IF S IS A BLOCK OF 9 MATRICES
C
      ELSEIF(S%TYPE.EQ.4.AND.S%N.EQ.9) THEN
C
      DIMLIM=MAX(FBOR%ADR(1)%P%DIM1,
     &           FBOR%ADR(2)%P%DIM1,
     &           FBOR%ADR(3)%P%DIM1)
C
      CALL DIRI09(F%ADR(1)%P,F%ADR(2)%P,F%ADR(3)%P,
     &            S%ADR(1)%P,S%ADR(2)%P,S%ADR(3)%P,
     &            S%ADR(4)%P,S%ADR(5)%P,S%ADR(6)%P,
     &            S%ADR(7)%P,S%ADR(8)%P,S%ADR(9)%P,
     &            SM%ADR(1)%P,SM%ADR(2)%P,SM%ADR(3)%P,
     &            WORK%ADR(1)%P,WORK%ADR(2)%P,WORK%ADR(3)%P,
     &            WORK%ADR(4)%P,WORK%ADR(5)%P,WORK%ADR(6)%P,
     &            FBOR%ADR(1)%P,FBOR%ADR(2)%P,FBOR%ADR(3)%P,
     &            LIMDIR(         1:  DIMLIM),
     &            LIMDIR(  DIMLIM+1:2*DIMLIM),
     &            LIMDIR(2*DIMLIM+1:3*DIMLIM),
     &            MESH,KDIR,MSK,MASKPT)
C
C  ERROR
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,1000) S%TYPE
         IF (LNG.EQ.2) WRITE(LU,1001) S%TYPE
1000     FORMAT(1X,'DIRICH (BIEF) : MAUVAIS TYPE POUR S :',1I6)
1001     FORMAT(1X,'DIRICH (BIEF): WRONG TYPE FOR S:',1I6)
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C