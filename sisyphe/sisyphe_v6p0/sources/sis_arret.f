C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       STOP TEST IN CASE EVOLUTION BECOMES TOO IMPORTANT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT0, BINRESSIS, EMAX, ESM, FMTRES, HIST, HN, MAXVAR, MN, NPOIN, NRES, RC, SORIMP, SORLEO, T1, T2, TEXTE, VARSOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IMIN, XMIN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SIS_ARRET
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_DESIMP(), MINI(), OS(), PLANTE(), PREDES()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!> </td><td> 05/01/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 11/09/1995
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BINRESSIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ESM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FMTRES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NRES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SORIMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SORLEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARSOR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SIS_ARRET !
     &(ESM,EMAX,HN,VARSOR,NPOIN,MN,NRES,FMTRES,MAXVAR,AT0,RC,HIST,
     & BINRESSIS,TEXTE,SORLEO,SORIMP,T1,T2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT0            |---| 
C| BINRESSIS      |---| 
C| EMAX           |---| 
C| ESM            |---| 
C| FMTRES         |---| 
C| HIST           |---| 
C| HN             |---| 
C| MAXVAR         |---| 
C| MN             |---| 
C| NPOIN          |---| 
C| NRES           |---| 
C| RC             |---| 
C| SORIMP         |---| 
C| SORLEO         |---| 
C| T1             |---| 
C| T2             |---| 
C| TEXTE          |---| 
C| VARSOR         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_SIS_ARRET => SIS_ARRET
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ),    INTENT(IN)    :: ESM, EMAX, HN, VARSOR
      INTEGER,           INTENT(IN)    :: NPOIN, MN, NRES, MAXVAR
      DOUBLE PRECISION,  INTENT(IN)    :: AT0, RC, HIST(1)
      CHARACTER(LEN=3),  INTENT(IN)    :: BINRESSIS
      CHARACTER(LEN=32), INTENT(IN)    :: TEXTE(MAXVAR)
      CHARACTER(LEN=8),  INTENT(IN)    :: FMTRES
      LOGICAL,           INTENT(IN)    :: SORLEO(*), SORIMP(*)
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T1, T2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          :: IMIN
      DOUBLE PRECISION :: XMIN
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! ************************************ !
      ! I - COMPUTES F ABSOLUTE VALUE        !
      ! ************************************ !
      CALL OS('X=ABS(Y)', X=T1, Y=ESM)

      ! ************************************************** !
      ! II - COMPUTES THE DIFFERENCE BETWEEN THE AUTHORISED! (_IMP_)
      !      VALUE FOR DEPOSITION AND F ABSOLUTE VALUE     !
      ! ************************************************** !
      CALL OS('X=Y-Z   ', X=T2, Y=EMAX, Z=T1)

      ! ************************************************************** !
      ! III - COMPUTES THE MINIMUM VALUE OF THIS DIFFERENCE (NPOIN)    !
      ! ************************************************************** !
      CALL MINI(XMIN, IMIN, T2%R, NPOIN)

      ! ************************************************************* !
      ! IV - IF THE MINIMUM VALUE IS NEGATIVE, COMPUTATION IS STOPPED !
      ! ************************************************************* !
      IF (XMIN < 0.D0) THEN

         ! IV.1 - PRINTS THE VALUES
         ! --------------------------
         IF(LNG.EQ.1) THEN
            WRITE(LU,400) MN
            WRITE(LU,*) ' '
            WRITE(LU,402) IMIN
            WRITE(LU,404) HN%R(IMIN)
            WRITE(LU,406) RC
            WRITE(LU,408) EMAX%R(IMIN)
            WRITE(LU,410) ESM%R(IMIN)
            WRITE(LU,412) AT0
            WRITE(LU,*) ' '
            WRITE(LU,*) 'DERNIER RESULTAT SAUVEGARDE'
         ELSE IF(LNG.EQ.2) THEN
            WRITE(LU,401) MN
            WRITE(LU,*) ' '
            WRITE(LU,403) IMIN
            WRITE(LU,405) HN%R(IMIN)
            WRITE(LU,407) RC
            WRITE(LU,409) EMAX%R(IMIN)
            WRITE(LU,411) ESM%R(IMIN)
            WRITE(LU,413) AT0
            WRITE(LU,*) ' '
            WRITE(LU,*) 'LAST RESULT SAVED'
         ENDIF


         ! IV.2 - SAVES THE LAST RESULT
         ! -----------------------------
         CALL PREDES(1,AT0)
         CALL BIEF_DESIMP(FMTRES,VARSOR,HIST,0,NPOIN,NRES,BINRESSIS,AT0,
     &                    1,1,1,SORLEO,SORIMP,MAXVAR,TEXTE,1,1)
         CALL PLANTE(1)
         STOP
      ENDIF


      !----------------------------------------------------------------!
400   FORMAT(1X,/,' EVOLUTION TROP FORTE AU CALCUL  : ',1I6)
402   FORMAT(' NOEUD NUMERO                    : ',1I6)
404   FORMAT(' HAUTEUR D''EAU                  : ',G16.7)
406   FORMAT(' RAPPORT D''EVOLUTION CRITIQUE   : ',G16.7)
408   FORMAT(' EVOLUTION MAXIMALE ADMISSIBLE   : ',G16.7)
410   FORMAT(' EVOLUTION CUMULEE CALCULEE      : ',G16.7)
412   FORMAT(' TEMPS                           : ',G16.7)
      !----------------------------------------------------------------!
401   FORMAT(1X,/,' TOO MUCH EVOLUTION AT COMPUTATION: ',1I6)
403   FORMAT(' NODE NUMBER                     : ',1I6)
405   FORMAT(' WATER DEPTH                     : ',G16.7)
407   FORMAT(' CRITICAL EVOLUTION RATIO        : ',G16.7)
409   FORMAT(' MAXIMAL ALLOWED EVOLUTION       : ',G16.7)
411   FORMAT(' COMPUTED CUMULATED EVOLUTION    : ',G16.7)
413   FORMAT(' TIME                            : ',G16.7)
      !----------------------------------------------------------------!

!======================================================================!
!======================================================================!

      RETURN
      END

C
C#######################################################################
C