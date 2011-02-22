C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALLS THE METHOD OF CHARACTERISTICS
!>               (SUBROUTINE CARACT).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, STREAMLINE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, FN, FTILD, IELM, IFAMAS, IKLE2, INILOC, IT1, IT2, IT3, IT4, MASKEL, MESH, MSK, NELEM2, NELMAX2, NOMB, NPLAN, NPLINT, NPOIN2, SHP, SHZ, SURDET2, TB, UCONV, VCONV, WCONV, ZSTAR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IELMU, IFA, INITLOC, J, K, NPOIN, T1, T2, T3, T4, T5, T6, T7
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CHARAC, IFA, T1, T2, T3, T4, T5, T6, T7
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CARACT(), CPSTVC(), PLANTE(), SCARACT()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECON(), SUSPENSION_COMPUTATION(), TELEMAC2D()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 12/02/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>FN
!></td><td>--></td><td>VARIABLES A L'ETAPE N .
!>    </td></tr>
!>          <tr><td>FTILD
!></td><td><--</td><td>VARIABLES APRES LA CONVECTION .
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT : 11 : TRIANGLE P1
!>                  41 : PRISME DE TEL3D
!>    </td></tr>
!>          <tr><td>IFAMAS
!></td><td>--></td><td>IFABOR MODIFIE QUAND DES ELEMENTS SONT MASQUES
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INILOC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE.
!>    </td></tr>
!>          <tr><td>MAT
!></td><td>--></td><td>MATRICE DE TRAVAIL
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES ENTIERS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMB
!></td><td>--></td><td>NOMBRE DE VARIABLES A CONVECTER.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).
!>    </td></tr>
!>          <tr><td>NPLINT
!></td><td>--></td><td>PLAN DE REFERENCE INTERMEDIAIRE (POUR TEL3D).
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).
!>    </td></tr>
!>          <tr><td>SHP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SURDET2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TB
!></td><td>--></td><td>BLOC DE TABLEAUX DE TRAVAIL (AU MOINS 8)
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>COMPOSANTES DES VITESSES DU CONVECTEUR.
!>    </td></tr>
!>          <tr><td>WCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZSTAR
!></td><td>--></td><td>COORDONNEES VERTICALES EN 3D.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHARAC
     &( FN  , FTILD  , NOMB   , UCONV  , VCONV , WCONV  , ZSTAR ,
     &  DT  , IFAMAS , IELM   , NPOIN2 , NPLAN , NPLINT ,
     &  MSK , MASKEL , SHP,SHZ , TB    , IT1,IT2,IT3,IT4,MESH ,
     &  NELEM2,NELMAX2,IKLE2,SURDET2   , INILOC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS
C| FN             |-->| VARIABLES A L'ETAPE N .
C| FTILD          |<--| VARIABLES APRES LA CONVECTION .
C| IELM           |-->| TYPE D'ELEMENT : 11 : TRIANGLE P1
C|                |   | 41 : PRISME DE TEL3D
C| IFAMAS         |-->| IFABOR MODIFIE QUAND DES ELEMENTS SONT MASQUES
C| IKLE2          |---| 
C| INILOC         |---| 
C| IT1            |---| 
C| IT2            |---| 
C| IT3            |---| 
C| IT4            |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
C| MAT            |-->| MATRICE DE TRAVAIL
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NELEM2         |---| 
C| NELMAX2        |---| 
C| NOMB           |-->| NOMBRE DE VARIABLES A CONVECTER.
C| NPLAN          |-->| NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).
C| NPLINT         |-->| PLAN DE REFERENCE INTERMEDIAIRE (POUR TEL3D).
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).
C| SHP            |---| 
C| SHZ            |---| 
C| SURDET2        |---| 
C| TB             |-->| BLOC DE TABLEAUX DE TRAVAIL (AU MOINS 8)
C| UCONV,VCONV    |-->| COMPOSANTES DES VITESSES DU CONVECTEUR.
C| WCONV          |---| 
C| ZSTAR          |-->| COORDONNEES VERTICALES EN 3D.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CHARAC => CHARAC
      USE STREAMLINE, ONLY : SCARACT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)         :: NOMB
      INTEGER         , INTENT(IN)         :: NPLAN,NPLINT,NELEM2
      INTEGER         , INTENT(IN)         :: NPOIN2,IELM,NELMAX2
      INTEGER         , INTENT(INOUT)      :: IT1(*),IT2(*)
      INTEGER         , INTENT(INOUT)      :: IT3(*),IT4(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)         :: FN,UCONV,VCONV,WCONV
      TYPE(BIEF_OBJ)  , INTENT(IN)         :: ZSTAR,MASKEL,IKLE2,SURDET2
      TYPE(BIEF_OBJ)  , INTENT(INOUT)      :: FTILD,TB,SHP,SHZ
      LOGICAL         , INTENT(IN)         :: MSK
      DOUBLE PRECISION, INTENT(IN)         :: DT
      TYPE(BIEF_MESH) , INTENT(INOUT)      :: MESH
      TYPE(BIEF_OBJ)  , INTENT(IN), TARGET :: IFAMAS
      LOGICAL, OPTIONAL, INTENT(IN)        :: INILOC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NPOIN,IELMU
      LOGICAL INITLOC
C
C-----------------------------------------------------------------------
C
      TYPE(BIEF_OBJ), POINTER :: T1,T2,T3,T4,T5,T6,T7
      INTEGER, DIMENSION(:), POINTER :: IFA
      INTEGER I,J,K
C
C-----------------------------------------------------------------------
C  WORKING ARRAYS FROM BLOCK TB
C-----------------------------------------------------------------------
C
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
      T5 =>TB%ADR( 5)%P
      T6 =>TB%ADR( 6)%P
      T7 =>TB%ADR( 7)%P
C
C-----------------------------------------------------------------------
C  INITIALISES THE LOCATION OF POINTS, OR NOT
C-----------------------------------------------------------------------
C
      IF(PRESENT(INILOC)) THEN
        INITLOC=INILOC
      ELSE
        INITLOC=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C  DEPLOYS THE MESH STRUCTURE
C-----------------------------------------------------------------------
C
      NPOIN = MESH%NPOIN
      IELMU = UCONV%ELM
C
C-----------------------------------------------------------------------
C     CHECKS SHP SIZE (ONCE A BUG...)
C-----------------------------------------------------------------------
C
      IF(3*NPOIN.GT.SHP%MAXDIM1*SHP%MAXDIM2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TAILLE DE SHP:',SHP%MAXDIM1*SHP%MAXDIM2
          WRITE(LU,*) 'TROP PETITE DANS CHARAC, ',3*NPOIN
          WRITE(LU,*) 'REQUISE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SIZE OF SHP:',SHP%MAXDIM1*SHP%MAXDIM2
          WRITE(LU,*) 'TOO SMALL IN CHARAC, ',3*NPOIN
          WRITE(LU,*) 'REQUESTED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C  CALLS CARACT
C-----------------------------------------------------------------------
C
      IF(MSK) THEN
C       CALL WITH IFAMAS
        IFA=>IFAMAS%I
      ELSE
C       CALL WITH IFABOR
        IFA=>MESH%IFABOR%I
      ENDIF
!
      CALL CPSTVC(MESH%X,T1)
      CALL CPSTVC(MESH%Y,T2)
!
      IF(NCSIZE.EQ.0) THEN
!
        CALL CARACT( FN , FTILD , UCONV%R , VCONV%R , WCONV%R ,
     &               MESH%X%R,MESH%Y%R,ZSTAR%R,
     &               T1,T2,T3%R,T4%R,T5%R,T6%R,
     &               MESH%Z%R,SHP%R,SHZ%R,
     &               SURDET2%R,DT,IKLE2%I,IFA,
     &               IT1,IT2,IT3,IT4,
     &               IELM,IELMU,NELEM2,NELMAX2,NOMB,NPOIN,NPOIN2,
     &               3,NPLAN,MESH%LV,
     &               MSK,MASKEL%R,MESH,MESH%FAC%R,T7%R,T7,INITLOC)
!
      ELSEIF(NCSIZE.GE.1) THEN
!
        CALL SCARACT( FN , FTILD , UCONV%R , VCONV%R , WCONV%R ,
     &                MESH%X%R,MESH%Y%R,ZSTAR%R,
     &                T1,T2,T3%R,T4%R,T5%R,T6%R,
     &                MESH%Z%R,SHP%R,SHZ%R,
     &                SURDET2%R,DT,IKLE2%I,IFA,
     &                IT1,IT2,IT3,IT4,
     &                IELM,IELMU,NELEM2,NELMAX2,NOMB,NPOIN,NPOIN2,
     &                3,NPLAN,MESH%LV,MSK,MASKEL%R,
     &                MESH,MESH%FAC%R,T7%R,T7,INITLOC)
!
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C