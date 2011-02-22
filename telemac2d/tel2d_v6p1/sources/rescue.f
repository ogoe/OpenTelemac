C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES MISSING DATA/VARIABLES (WHEN RESUMING SIMULATION).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AKEP, H, ITURB, NPOIN, NTRAC, S, T, TRAC0, TROUVE, U, V, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BID, ITRAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), OV(), PLANTE()
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
!>      <td><center> 5.8                                       </center>
!> </td><td> 31/08/2007
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AK
!></td><td><--</td><td>ENERGIE TURBULENTE.
!>    </td></tr>
!>          <tr><td>AKEP
!></td><td>--></td><td>LOGIQUE QUI INDIQUE S'IL FAUT INITIALISER K
!>                  ET EPSILON.
!>    </td></tr>
!>          <tr><td>C
!></td><td><--</td><td>CELERITE.
!>    </td></tr>
!>          <tr><td>EP
!></td><td><--</td><td>DISSIPATION.
!>    </td></tr>
!>          <tr><td>F
!></td><td><--</td><td>NOMBRE DE FROUDE.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR.
!>    </td></tr>
!>          <tr><td>H
!></td><td><--</td><td>HAUTEUR.
!>    </td></tr>
!>          <tr><td>ITURB
!></td><td>--></td><td>MODELE DE TURBULENCE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DANS LE MAILLAGE
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td><--</td><td>DEBIT SCALAIRE.
!>    </td></tr>
!>          <tr><td>S
!></td><td><--</td><td>SURFACE LIBRE.
!>    </td></tr>
!>          <tr><td>T
!></td><td><--</td><td>TRACEUR.
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>INDIQUE LA PRESENCE D'UN TRACEUR.
!>    </td></tr>
!>          <tr><td>TRAC0
!></td><td>--></td><td>VALEUR INITIALE DU TRACEUR.
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td>--></td><td>TABLEAU INDIQUANT LES VARIABLES TROUVEES DANS
!>                  LE FICHIER DES RESULTATS DU CALCUL PRECEDENT
!>    </td></tr>
!>          <tr><td>U , V
!></td><td><--</td><td>COMPOSANTES DES VECTEURS VITESSE.
!>    </td></tr>
!>          <tr><td>VISC
!></td><td><--</td><td>VISCOSITE TURBULENTE.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><--</td><td>COTE DES POINTS DU FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE RESCUE
     &(U,V,H,S,ZF,T,TRAC0,NTRAC,ITURB,NPOIN,AKEP,TROUVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |<--| ENERGIE TURBULENTE.
C| AKEP           |-->| LOGIQUE QUI INDIQUE S'IL FAUT INITIALISER K
C|                |   | ET EPSILON.
C| C             |<--| CELERITE.
C| EP             |<--| DISSIPATION.
C| F             |<--| NOMBRE DE FROUDE.
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR.
C| H             |<--| HAUTEUR.
C| ITURB          |-->| MODELE DE TURBULENCE.
C| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
C| NTRAC          |---| 
C| Q             |<--| DEBIT SCALAIRE.
C| S             |<--| SURFACE LIBRE.
C| T             |<--| TRACEUR.
C| TRAC           |-->| INDIQUE LA PRESENCE D'UN TRACEUR.
C| TRAC0          |-->| VALEUR INITIALE DU TRACEUR.
C| TROUVE         |-->| TABLEAU INDIQUANT LES VARIABLES TROUVEES DANS
C|                |   | LE FICHIER DES RESULTATS DU CALCUL PRECEDENT
C| U , V          |<--| COMPOSANTES DES VECTEURS VITESSE.
C| VISC           |<--| VISCOSITE TURBULENTE.
C| ZF             |<--| COTE DES POINTS DU FOND.
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
      INTEGER, INTENT(IN)             :: TROUVE(*),ITURB,NPOIN,NTRAC
      LOGICAL, INTENT(INOUT)          :: AKEP
      DOUBLE PRECISION, INTENT(INOUT) :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: S(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TRAC0(NTRAC)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: T
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC
      DOUBLE PRECISION BID
C
C-----------------------------------------------------------------------
C
C  VELOCITY U-COMPONENT
C
      IF(TROUVE(1).NE.1 )  THEN
          IF(LNG.EQ.1) WRITE(LU,190)
          IF(LNG.EQ.2) WRITE(LU,191)
190       FORMAT(1X,'RESCUE : FICHIER DE RESULTATS DU CALCUL PRECEDENT',
     &         /,1X,'         SANS LA VITESSE U, ON LA PREND NULLE')
191       FORMAT(1X,'RESCUE : PREVIOUS COMPUTATION RESULTS FILE',
     &         /,1X,'         WITHOUT VELOCITY U, WE FIX IT TO ZERO')
          CALL OV( 'X=C     ' , U , U , U , 0.D0 , NPOIN )
      ENDIF
C
C-----------------------------------------------------------------------
C
C  VELOCITY V-COMPONENT
C
      IF(TROUVE(2).NE.1 )  THEN
          IF(LNG.EQ.1) WRITE(LU,200)
          IF(LNG.EQ.2) WRITE(LU,201)
200       FORMAT(1X,'RESCUE : FICHIER DE RESULTATS DU CALCUL PRECEDENT',
     &         /,1X,'         SANS LA VITESSE V, ON LA PREND NULLE')
201       FORMAT(1X,'RESCUE : PREVIOUS COMPUTATION RESULTS FILE',
     &         /,1X,'         WITHOUT VELOCITY V, WE FIX IT TO ZERO')
          CALL OV( 'X=C     ' , V , V , V , 0.D0 , NPOIN )
      ENDIF
C
C-----------------------------------------------------------------------
C
C  WATER DEPTH
C
      IF(TROUVE(4).NE.1) THEN
        IF(TROUVE(5).EQ.1) THEN
          IF(LNG.EQ.1) WRITE(LU,400)
          IF(LNG.EQ.2) WRITE(LU,401)
400       FORMAT(1X,'RESCUE : HAUTEUR D''EAU CALCULEE AVEC LE FOND',
     &         /,1X,'         ET LA SURFACE LIBRE')
401       FORMAT(1X,'RESCUE : WATER DEPTH COMPUTED WITH BATHYMETRY',
     &         /,1X,'         AND SURFACE ELEVATION')
          CALL OV( 'X=Y-Z   ' , H , S , ZF , BID , NPOIN )
        ELSE
          IF(LNG.EQ.1) WRITE(LU,420)
          IF(LNG.EQ.2) WRITE(LU,421)
420       FORMAT(1X,'RESCUE : IMPOSSIBLE DE CALCULER LA HAUTEUR D''EAU')
421       FORMAT(1X,'RESCUE : WATER DEPTH CANNOT BE COMPUTED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C  TRACER
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(TROUVE(31+ITRAC).EQ.0) THEN
            IF(LNG.EQ.1) WRITE(LU,900)
            IF(LNG.EQ.2) WRITE(LU,901)
900         FORMAT(1X,'RESCUE : CALCUL PRECEDENT SANS TRACEUR,',
     &           /,1X,'         ON PREND TRAC0')
901         FORMAT(1X,'RESCUE : PREVIOUS CALCULATION WITHOUT TRACER',
     &           /,1X,'         WE FIX IT TO TRAC0')
            CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC))
          ENDIF
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C  K AND EPSILON
C
      IF(ITURB.EQ.3.AND.TROUVE(10).EQ.1.AND.TROUVE(11).EQ.1) THEN
        AKEP=.FALSE.
      ENDIF
      IF(ITURB.EQ.3.AND.(TROUVE(10).EQ.0.OR.TROUVE(11).EQ.0)) THEN
        IF(LNG.EQ.1) WRITE(LU,950)
        IF(LNG.EQ.2) WRITE(LU,951)
950     FORMAT(1X,'RESCUE : K ET EPSILON SERONT REINITIALISES')
951     FORMAT(1X,'RESCUE : K ET EPSILON WILL BE SET AGAIN')
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C