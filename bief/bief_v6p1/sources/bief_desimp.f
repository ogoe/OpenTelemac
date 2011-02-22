C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES TO RESULT OR LISTING FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, FORMAT_RES, HIST, LEOPRD, LISPRD, LT, MAXVAR, N, NHIST, NRES, PTINIG, PTINIL, SORIMP, SORLEO, STD, TEXTE, VARSOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IMP, K, LEO, LTT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> IMPVEC(), WRITE_DATA()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), HOMERE_ADJ_T2D(), OUTPUT_TELEMAC2D(), SISYPHE(), SIS_ARRET(), TELEMAC2D(), TELEMAC3D(), WAC()

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
!> </td><td> 01/04/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 71 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT , LT
!></td><td>--></td><td>TEMPS , NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>FORMAT_RES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LEOPRD
!></td><td>--></td><td>PERIODE DE SORTIE SUR LE FICHIER DE RESULTAT
!>    </td></tr>
!>          <tr><td>LISPRD
!></td><td>--></td><td>PERIODE DE SORTIE SUR LISTING.
!>    </td></tr>
!>          <tr><td>MAXVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NHIST
!></td><td>--></td><td>NOMBRE DE VALEURS DANS HIST
!>    </td></tr>
!>          <tr><td>NRES
!></td><td>--></td><td>UNITE LOGIQUE DU FICHIER DE RESULTATS.
!>    </td></tr>
!>          <tr><td>PTINIG
!></td><td>--></td><td>1ER PAS DE TEMPS POUR LES SORTIES GRAPHIQUES
!>    </td></tr>
!>          <tr><td>PTINIL
!></td><td>--></td><td>1ER PAS DE TEMPS POUR LES SORTIES LISTING
!>    </td></tr>
!>          <tr><td>SORIMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SORLEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STD
!></td><td>--></td><td>BINAIRE DU FICHIER DE RESULTATS (IBM,I3E,STD)
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td>--></td><td>NOMS ET UNITES DES VARIABLES.
!>    </td></tr>
!>          <tr><td>VARLIS
!></td><td>--></td><td>BLOC CONTENANT LES VARIABLES A IMPRIMER
!>    </td></tr>
!>          <tr><td>VARSOR
!></td><td>--></td><td>BLOC CONTENANT LES VARIABLES A METTRE DANS LES
!>                  RESULTATS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BIEF_DESIMP
     &(FORMAT_RES,VARSOR,HIST,NHIST,N,NRES,STD,AT,LT,LISPRD,LEOPRD,
     & SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT , LT        |-->| TEMPS , NUMERO DU PAS DE TEMPS
C| FORMAT_RES     |---| 
C| HIST           |---| 
C| LEOPRD         |-->| PERIODE DE SORTIE SUR LE FICHIER DE RESULTAT
C| LISPRD         |-->| PERIODE DE SORTIE SUR LISTING.
C| MAXVAR         |---| 
C| N             |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NHIST          |-->| NOMBRE DE VALEURS DANS HIST
C| NRES           |-->| UNITE LOGIQUE DU FICHIER DE RESULTATS.
C| PTINIG         |-->| 1ER PAS DE TEMPS POUR LES SORTIES GRAPHIQUES
C| PTINIL         |-->| 1ER PAS DE TEMPS POUR LES SORTIES LISTING
C| SORIMP         |---| 
C| SORLEO         |---| 
C| STD            |-->| BINAIRE DU FICHIER DE RESULTATS (IBM,I3E,STD)
C| TEXTE          |-->| NOMS ET UNITES DES VARIABLES.
C| VARLIS         |-->| BLOC CONTENANT LES VARIABLES A IMPRIMER
C| VARSOR         |-->| BLOC CONTENANT LES VARIABLES A METTRE DANS LES
C|                |   | RESULTATS
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
      TYPE(BIEF_OBJ)   , INTENT(IN) :: VARSOR
      CHARACTER(LEN=8) , INTENT(IN) :: FORMAT_RES
      INTEGER          , INTENT(IN) :: NRES,LT,LISPRD,LEOPRD
      INTEGER          , INTENT(IN) :: NHIST,PTINIG,PTINIL,N
      INTEGER          , INTENT(IN) :: MAXVAR
      DOUBLE PRECISION , INTENT(IN) :: AT,HIST(NHIST)
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(*)
      CHARACTER(LEN=3) , INTENT(IN) :: STD
      LOGICAL          , INTENT(IN) :: SORLEO(MAXVAR),SORIMP(MAXVAR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER LTT,K
C
      LOGICAL LEO,IMP
C
C-----------------------------------------------------------------------
C
C LOGICAL THAT DEFINE THE OUTPUTS
C
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
C
C-----------------------------------------------------------------------
C
      IF(LEO) THEN
        CALL WRITE_DATA(FORMAT_RES,       ! RESULT FILE FORMAT ID
     &                  NRES,             ! LU RESULT FILE
     &                  MAXVAR,           ! MAX NB OF VARIABLES IN VARSOR
     &                  AT,               ! TIME
     &                  LT,               ! TIMESTEP
     &                  SORLEO(1:MAXVAR), ! OUTPUT OR NOT
     &                  TEXTE(1:MAXVAR),  ! TIMESTEP
     &                  VARSOR,           ! COLLECTION OF VECTORS
     &                  N)                ! NUMBER OF VALUES
      ENDIF
C
C TO LISTING FILE
C
      IF(IMP) THEN
        DO K=1,MAXVAR
          IF(SORIMP(K)) THEN
            IF(ASSOCIATED(VARSOR%ADR(K)%P%R)) THEN
              CALL IMPVEC(VARSOR%ADR(K)%P%R,TEXTE(K),N)
            ELSE
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'DESIMP: VARIABLE NUMERO: ',K
                WRITE(LU,*) '        PAS OU MAL ALLOUEE'
                WRITE(LU,*) '        OU POINTEUR NON ASSOCIE'
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'DESIMP: VARIABLE NUMBER: ',K
                WRITE(LU,*) '        NOT OR NOT WELL ALLOCATED'
                WRITE(LU,*) '        OR POINTER NOT ASSOCIATED '
              ENDIF
C             CALL PLANTE(1)
C             STOP
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C