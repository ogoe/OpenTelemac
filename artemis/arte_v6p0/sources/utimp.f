C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    ALMOST ALL THE COMPUTATION VARIABLES ARE AVAILABLE
!>             HERE TO WRITE OUT SPECIFIC OUTPUT, COMPUTE ANALYTICAL
!>             SOLUTIONS...

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!>  @code
!>     EXAMPLE : U1 AND V1
!>              (HORIZONTAL VELOCITIES AT T/4)
!>               ARE TRANSFERRED TO PRIVE(I,1) AND PRIVE(I,2)
!>------------------------------------------------------------------
!>
!>      CALL VECTOR(TRA02, '=' , 'GRADF          X' , IELM ,
!>     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!>     *            MESH , MESH , MSK , MASKEL )
!>
!>      CALL VECTOR(TRA03 , '=' , 'GRADF          Y' , IELM ,
!>     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
!>     *            MESH , MESH , MSK , MASKEL )
!>     *            MESH , XMESH ,
!>
!>      CALL VECTOR(TRA01 , '=' , 'MASBAS          ' , IELM ,
!>     *            1.D0 , BID , BID , BID , BID , BID , BID ,
!>     *            MESH , MESH , MSK , MASKEL )
!>     *            MESH , XMESH ,
!>
!>      CALL OS( 'X=Y/Z   ' , TRA02 , TRA02 , TRA01 , BID )
!>      CALL OS( 'X=Y/Z   ' , TRA03 , TRA03 , TRA01 , BID )
!>
!>      DO 25 I = 1,NPOIN
!>         PRIVE%ADR(1)%P%R(1) = TRA02(I)
!>         PRIVE%ADR(1)%P%R(2) = TRA03(I)
!> 25   CONTINUE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, CG, GRAV, H, HHO, IELM, IELMB, IKLE, INCI, K, KP1BOR, NBOR, NELEM, NELMAX, NPOIN, NPTFR, OMEGA, PER, PHAS, PHII, PHIR, PRIVE, S, TRA01, TRA02, TRA03, TRA04, U0, V0, X, Y, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_UTIMP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), TELEMAC3D()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C,CG
!></td><td>--></td><td>VITESSE DE PHASE ET DE GROUPE
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU AU REPOS
!>    </td></tr>
!>          <tr><td>HHO
!></td><td>--></td><td>HAUTEUR DE LA HOULE
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IELMB
!></td><td>--></td><td>TYPE D'ELEMENT DE BORD
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLE DE CONNECTIVITE
!>    </td></tr>
!>          <tr><td>INCI
!></td><td>--></td><td>INCIDENCE DE LA HOULE
!>    </td></tr>
!>          <tr><td>K
!></td><td>--></td><td>NOMBRE D'ONDE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMERO DE BORD DU POINT SUIVANT
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>OMEGA
!></td><td>--></td><td>PULSATION DE LA HOULE
!>    </td></tr>
!>          <tr><td>PER
!></td><td>--></td><td>PERIODE DE LA HOULE
!>    </td></tr>
!>          <tr><td>PHAS
!></td><td>--></td><td>PHASE DE LA HOULE
!>    </td></tr>
!>          <tr><td>PHIR,PHII
!></td><td>--></td><td>COMPOSANTES DU POTENTIEL
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td><-></td><td>TABLEAUX RESERVE A L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>S
!></td><td>--></td><td>COTE DE LA SURFACE LIBRE
!>    </td></tr>
!>          <tr><td>TRA01,
!></td><td>---</td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA04
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U0,V0
!></td><td>--></td><td>VITESSES EN SURFACE (A T=0)
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE UTIMP
     &(PHIR,PHII,C,CG,K,X,Y,ZF,H,
     & HHO,U0,V0,PHAS,S,TRA01,TRA02,TRA03,TRA04,INCI,
     & GRAV,PER,OMEGA,IKLE,NBOR,KP1BOR,
     & NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN,PRIVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C,CG           |-->| VITESSE DE PHASE ET DE GROUPE
C| GRAV           |-->| GRAVITE
C| H             |-->| HAUTEUR D'EAU AU REPOS
C| HHO            |-->| HAUTEUR DE LA HOULE
C| IELM           |-->| TYPE D'ELEMENT
C| IELMB          |-->| TYPE D'ELEMENT DE BORD
C| IKLE           |-->| TABLE DE CONNECTIVITE
C| INCI           |-->| INCIDENCE DE LA HOULE
C| K             |-->| NOMBRE D'ONDE
C| KP1BOR         |-->| NUMERO DE BORD DU POINT SUIVANT
C| NBOR           |-->| ADRESSES DES POINTS DE BORD
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |---| 
C| NPOIN          |-->| NOMBRE DE POINTS
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE
C| OMEGA          |-->| PULSATION DE LA HOULE
C| PER            |-->| PERIODE DE LA HOULE
C| PHAS           |-->| PHASE DE LA HOULE
C| PHIR,PHII      |-->| COMPOSANTES DU POTENTIEL
C| PRIVE          |<->| TABLEAUX RESERVE A L'UTILISATEUR
C| S             |-->| COTE DE LA SURFACE LIBRE
C| TRA01,         |---| TABLEAUX DE TRAVAIL
C| TRA02          |---| 
C| TRA03          |---| 
C| TRA04          |---| 
C| U0,V0          |-->| VITESSES EN SURFACE (A T=0)
C| W1             |<->| TABLEAU DE TRAVAIL
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C| ZF             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_ARTEMIS, EX_UTIMP=> UTIMP
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN
      INTEGER IKLE(NELMAX,3),NBOR(NPTFR),KP1BOR(NPTFR)
C
      DOUBLE PRECISION PHIR(NPOIN),PHII(NPOIN)
      DOUBLE PRECISION C(NPOIN),CG(NPOIN),K(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION ZF(NPOIN),H(NPOIN),HHO(NPOIN),U0(NPOIN),V0(NPOIN)
      DOUBLE PRECISION INCI(NPOIN)
      DOUBLE PRECISION PHAS(NPOIN),S(NPOIN)
      DOUBLE PRECISION TRA01(NPOIN),TRA02(NPOIN)
      DOUBLE PRECISION TRA03(NPOIN),TRA04(NPOIN)
C
      TYPE(BIEF_OBJ) :: PRIVE
C
      DOUBLE PRECISION GRAV,PER,OMEGA
C
C------------------------------------------------------------------
C     EXAMPLE : U1 AND V1
C              (HORIZONTAL VELOCITIES AT T/4)
C               ARE TRANSFERRED TO PRIVE(I,1) AND PRIVE(I,2)
C------------------------------------------------------------------
C
C      CALL VECTOR(TRA02, '=' , 'GRADF          X' , IELM ,
C     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
C     *            MESH , MESH , MSK , MASKEL )
C
C      CALL VECTOR(TRA03 , '=' , 'GRADF          Y' , IELM ,
C     *            1.D0 , PHII , BID , BID , BID , BID , BID ,
C     *            MESH , MESH , MSK , MASKEL )
C     *            MESH , XMESH ,
C
C      CALL VECTOR(TRA01 , '=' , 'MASBAS          ' , IELM ,
C     *            1.D0 , BID , BID , BID , BID , BID , BID ,
C     *            MESH , MESH , MSK , MASKEL )
C     *            MESH , XMESH ,
C
C      CALL OS( 'X=Y/Z   ' , TRA02 , TRA02 , TRA01 , BID )
C      CALL OS( 'X=Y/Z   ' , TRA03 , TRA03 , TRA01 , BID )
C
C      DO 25 I = 1,NPOIN
C         PRIVE%ADR(1)%P%R(1) = TRA02(I)
C         PRIVE%ADR(1)%P%R(2) = TRA03(I)
C 25   CONTINUE
C
      RETURN
      END
C
C#######################################################################
C