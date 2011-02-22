C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE RESULTS WRITTEN TO A RESULTS FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, M_MED
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALIRE, AT, CLAND, DT, FIN, HIST, LISTIN, MAXVAR, NDT, NHIST, NPLAN, NPOIN, NPRE, NUMDEB, NVARCL, STD, TEXTPR, TROUVE, VARCLA, VARSOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DDT, NNDT, NNPLAN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BIEF_SUITE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), SUITE_MED(), SUITE_SERAFIN()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIEF_VALIDA(), CONDIN_ADJ(), HOMERE_ADJ_T2D(), SISYPHE(), TELEMAC2D(), TELEMAC3D()

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
!> </td><td> 09/04/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 09/12/2008
!> </td><td>
!> </td><td> STD IS NOW A STRING OF ANY SIZE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALIRE
!></td><td>--></td><td>VARIABLES QU'IL FAUT LIRE (POUR LES AUTRES ON
!>                  SAUTE L'ENREGISTREMENT CORRESPONDANT)
!>                  LES VARIABLES CLANDESTI-NES SONT LUES
!>                  SYSTEMATIQUEMENT.
!>    </td></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>CLAND
!></td><td><--</td><td>BLOC DES VARIABLES CLANDESTI-NES
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FIN
!></td><td>--></td><td>VOIR LE TROISIEME ARGUMENT NUMDEB
!>    </td></tr>
!>          <tr><td>HIST
!></td><td>--></td><td>TABLEAU DE VALEURS MISES DANS L'ENREGISTREMENT
!>                  DU TEMPS.
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>--></td><td>SI OUI, IMPRESSION D'INFORMATIONS SUR LISTING
!>    </td></tr>
!>          <tr><td>MAXVAR
!></td><td>--></td><td>DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
!>    </td></tr>
!>          <tr><td>NDT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NHIST
!></td><td>--></td><td>NOMBRE DE VALEURS DANS LE TABLEAU HIST.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DANS LE MAILLAGE
!>    </td></tr>
!>          <tr><td>NPRE
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER
!>    </td></tr>
!>          <tr><td>NUMDEB
!></td><td><-></td><td>FIN = .TRUE. NUMERO DU DERNIER ENREGISTREMENT
!>                  FIN = .FALSE. : NUMERO DE L'ENREGISTREMENT
!>                  QUE L'ON VEUT LIRE.
!>    </td></tr>
!>          <tr><td>NVARCL
!></td><td>--></td><td>NOMBRE DE VARIABLES CLANDESTI-NES.
!>    </td></tr>
!>          <tr><td>STD
!></td><td>--></td><td>BINAIRE DU FICHIER : STD, IBM OU I3E
!>    </td></tr>
!>          <tr><td>TEXTPR
!></td><td>--></td><td>NOMS ET UNITES DES VARIABLES.
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td><--</td><td>INDIQUE (TROUVE(K)=1) LES VARIABLES TROUVEES
!>                  DANS LE FICHIER.
!>                  DE K =  1 A 26 VARIABLES NORMALES
!>                  DE K = 27 A 36 VARIABLES CLANDESTI-NES.
!>    </td></tr>
!>          <tr><td>VARCLA
!></td><td>--></td><td>TABLEAU OU L'ON RANGE LES VARIABLES
!>                  CLANDESTIINES.
!>    </td></tr>
!>          <tr><td>VARSOR
!></td><td><--</td><td>BLOC DES TABLEAUX CONTENANT LES VARIABLES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BIEF_SUITE
     &(VARSOR,CLAND,NUMDEB,
     & NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     & TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT,NDT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALIRE          |-->| VARIABLES QU'IL FAUT LIRE (POUR LES AUTRES ON
C|                |   | SAUTE L'ENREGISTREMENT CORRESPONDANT)
C|                |   | LES VARIABLES CLANDESTI-NES SONT LUES
C|                |   | SYSTEMATIQUEMENT.
C| AT             |-->| TEMPS
C| CLAND          |<--| BLOC DES VARIABLES CLANDESTI-NES
C| DT             |---| 
C| FIN            |-->| VOIR LE TROISIEME ARGUMENT NUMDEB
C| HIST           |-->| TABLEAU DE VALEURS MISES DANS L'ENREGISTREMENT
C|                |   | DU TEMPS.
C| LISTIN         |-->| SI OUI, IMPRESSION D'INFORMATIONS SUR LISTING
C| MAXVAR         |-->| DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
C| NDT            |---| 
C| NHIST          |-->| NOMBRE DE VALEURS DANS LE TABLEAU HIST.
C| NPLAN          |---| 
C| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
C| NPRE           |-->| NUMERO DE CANAL DU FICHIER
C| NUMDEB         |<->| FIN = .TRUE. NUMERO DU DERNIER ENREGISTREMENT
C|                |   | FIN = .FALSE. : NUMERO DE L'ENREGISTREMENT
C|                |   | QUE L'ON VEUT LIRE.
C| NVARCL         |-->| NOMBRE DE VARIABLES CLANDESTI-NES.
C| STD            |-->| BINAIRE DU FICHIER : STD, IBM OU I3E
C| TEXTPR         |-->| NOMS ET UNITES DES VARIABLES.
C| TROUVE         |<--| INDIQUE (TROUVE(K)=1) LES VARIABLES TROUVEES
C|                |   | DANS LE FICHIER.
C|                |   | DE K =  1 A 26 VARIABLES NORMALES
C|                |   | DE K = 27 A 36 VARIABLES CLANDESTI-NES.
C| VARCLA         |-->| TABLEAU OU L'ON RANGE LES VARIABLES
C|                |   | CLANDESTIINES.
C| VARSOR         |<--| BLOC DES TABLEAUX CONTENANT LES VARIABLES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_BIEF_SUITE => BIEF_SUITE
      USE M_MED
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARSOR,CLAND
      INTEGER, INTENT(IN), OPTIONAL :: NPLAN
      INTEGER, INTENT(IN)           :: NHIST,NVARCL,MAXVAR
      INTEGER                       :: NUMDEB,NPRE,NPOIN,TROUVE(MAXVAR)
      INTEGER                       :: ALIRE(MAXVAR)
      CHARACTER(LEN=*)              :: STD
      CHARACTER(LEN=32)             :: TEXTPR(MAXVAR),VARCLA(NVARCL)
      DOUBLE PRECISION              :: HIST(*),AT
      LOGICAL                       :: FIN,LISTIN
      DOUBLE PRECISION, INTENT(OUT), OPTIONAL :: DT
      INTEGER, INTENT(OUT), OPTIONAL :: NDT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NNPLAN, NNDT
      DOUBLE PRECISION DDT
C
C-----------------------------------------------------------------------
C
      IF(PRESENT(NPLAN)) THEN
        NNPLAN=NPLAN
      ELSE
        NNPLAN=1
      ENDIF
C
      SELECT CASE(STD)
C
        CASE ('SERAFIN ','SERAFIND')
C
          IF(PRESENT(DT)) THEN
            CALL SUITE_SERAFIN(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,
     &                         NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     &                         TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NNPLAN,
     &                         DT)
          ELSE
            CALL SUITE_SERAFIN(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,
     &                         NHIST,NPOIN,AT,TEXTPR,VARCLA,NVARCL,
     &                         TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NNPLAN)
          ENDIF
C
        CASE ('MED     ')
C
          CALL SUITE_MED(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,NHIST,
     &                   NPOIN,AT,TEXTPR,VARCLA,NVARCL,TROUVE,ALIRE,
     &                   LISTIN,FIN,MAXVAR,NNPLAN,DDT,NNDT)
          IF(PRESENT(DT)) DT=DDT
          IF(PRESENT(NDT)) NDT=NNDT

C
        CASE DEFAULT
C
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'BIEF_SUITE : MAUVAIS FORMAT : ',STD
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'BIEF_SUITE: BAD FILE FORMAT : ',STD
          ENDIF
          CALL PLANTE(1)
          STOP
C
      END SELECT
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C