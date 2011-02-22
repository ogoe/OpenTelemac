C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADVECTION STEP.
!>                INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, TOMAWAC_MPI
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, COURAN, ELT, ETA, ETAP1, F, FRE, IKLE2, NELEM2, NF, NPLAN, NPOIN2, NPOIN3, SHF, SHP1, SHP2, SHP3, SHZ, TRA01, TRA02
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> TOMAWAC_MPI :<br>
!> @link TOMAWAC_MPI::IFREQ IFREQ@endlink
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CAR, I, IB, IFF, ISTAT, LU, WW, X
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> INTER4D(), INTERP_TOMAWAC()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!> </td><td> 05/12/95
!> </td><td> F. MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>B
!></td><td>--></td><td>FACTEUR B
!>    </td></tr>
!>          <tr><td>COURAN
!></td><td>--></td><td>LOGIQUE INDIQUANT SI IL Y A DU COURANT
!>    </td></tr>
!>          <tr><td>ELT
!></td><td><-></td><td>NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td><-></td><td>NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>ETAP1
!></td><td><-></td><td>TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
!>                  L'ETAGE SUPERIEUR
!>    </td></tr>
!>          <tr><td>F
!></td><td><-></td><td>FONCTION A CONVECTER
!>    </td></tr>
!>          <tr><td>FRE
!></td><td><-></td><td>NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D ELEMENTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS OU DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>SHF
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT F DES
!>                  NOEUDS DANS LEURS FREQUENCES "FRE" ASSOCIEES.
!>    </td></tr>
!>          <tr><td>SHP1
!></td><td>---</td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHP2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHT
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT TETA DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PROPA
     &   (F,B,SHP1,SHP2,SHP3,SHZ,SHF,ELT,ETA,FRE,IKLE2,ETAP1,
     &    NPOIN3,NPOIN2,NELEM2,NPLAN,NF,COURAN,TRA01,TRA02)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |-->| FACTEUR B
C| COURAN         |-->| LOGIQUE INDIQUANT SI IL Y A DU COURANT
C| ELT            |<->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |<->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
C| ETAP1          |<->| TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
C|                |   | L'ETAGE SUPERIEUR
C| F             |<->| FONCTION A CONVECTER
C| FRE            |<->| NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE DU MAILLAGE 2D.
C| NELEM2         |-->| NOMBRE D ELEMENTS DU MAILLAGE 2D
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE PLANS OU DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| SHF            |<->| COORDONNEES BARYCENTRIQUES SUIVANT F DES
C|                |   | NOEUDS DANS LEURS FREQUENCES "FRE" ASSOCIEES.
C| SHP1           |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHP2           |---| 
C| SHP3           |---| 
C| SHT            |<->| COORDONNEES BARYCENTRIQUES SUIVANT TETA DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| SHZ            |---| 
C| TRA01          |<->| TABLEAU DE TRAVAIL
C| TRA02          |<->| TABLEAU DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_PROPA => PROPA
      USE TOMAWAC_MPI
C
      IMPLICIT NONE
C
      INTEGER NPOIN3,NPOIN2,NELEM2,NPLAN,NF
C
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION SHP1(NPOIN3,NF) , SHP2(NPOIN3,NF)
      DOUBLE PRECISION SHP3(NPOIN3,NF) , SHZ(NPOIN3,NF)
      DOUBLE PRECISION SHF(NPOIN3,NF)
      DOUBLE PRECISION B(NPOIN2,NF)
      DOUBLE PRECISION TRA01(NPOIN3,8),TRA02(NPOIN2,NPLAN,NF)
      INTEGER ELT(NPOIN3,NF),ETA(NPOIN3,NF),FRE(NPOIN3,NF)
      INTEGER IKLE2(NELEM2,3),ETAP1(NPLAN)
      LOGICAL COURAN
      REAL WW(1)
C
      DOUBLE PRECISION X(1)
      INTEGER IFF,I,ISTAT,LU, IB(1)
      CHARACTER*3 CAR
C
C----------------------------------------------------------------------
C
      LU=6
C
        IF (.NOT.COURAN) THEN
C
         DO 300 IFF=1,NF
C
            IFREQ = IFF
            CALL INTERP_TOMAWAC
     &        (F(1,1,IFF),B(1,IFF),SHP1(1,IFF),SHP2(1,IFF),
     &             SHP3(1,IFF),SHZ(1,IFF),ELT(1,IFF),ETA(1,IFF),IKLE2,
     &         ETAP1,NPOIN2,NELEM2,NPLAN,TRA01)
C
300      CONTINUE
C
        ELSE
C
            CALL INTER4D
     &       (F,B,SHP1,SHP2,SHP3,SHZ,SHF,ELT,ETA,
     &        FRE,IKLE2,ETAP1,NPOIN2,NELEM2,NPLAN,NF,TRA02)
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
