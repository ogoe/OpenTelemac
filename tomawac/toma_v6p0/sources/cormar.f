C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES ARRAYS OF PHYSICAL PARAMETERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, LT, NPC, NPM, NVCOU, NVHMA, TC1, TC2, TM1, TM2, TV1, TV2
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::BINCOU BINCOU@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINMAR BINMAR@endlink, 
!> @link DECLARATIONS_TOMAWAC::DDC DDC@endlink, 
!> @link DECLARATIONS_TOMAWAC::IDHMA IDHMA@endlink, 
!> @link DECLARATIONS_TOMAWAC::IELM2 IELM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIC INDIC@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIM INDIM@endlink, 
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink, 
!> @link DECLARATIONS_TOMAWAC::NBOR NBOR@endlink, 
!> @link DECLARATIONS_TOMAWAC::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDEPTH SDEPTH@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDUX SDUX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDUY SDUY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDVX SDVX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDVY SDVY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZHDT SDZHDT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZX SDZX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZY SDZY@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST0 ST0@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST1 ST1@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST2 ST2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST3 ST3@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST4 ST4@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC SUC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC1 SUC1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC2 SUC2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC SVC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC1 SVC1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC2 SVC2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SW1 SW1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXRELC SXRELC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXRELM SXRELM@endlink, 
!> @link DECLARATIONS_TOMAWAC::SYRELC SYRELC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SYRELM SYRELM@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRA01 TRA01@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCOB WACCOB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCOF WACCOF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACMAB WACMAB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACMAF WACMAF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WAC_FILES WAC_FILES@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZM1 ZM1@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZM2 ZM2@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> N1, N2, N3, N4, NPOIN4
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ANAMAR(), NOUDON(), NOUMAR(), OV(), PARCOM(), VECTOR()
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
!>      <td><center> 5.0                                       </center>
!> </td><td> 25/08/2000
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td><--</td><td>DENSITE SPECTRALE D'ENERGIE
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>---</td><td>NPOIN3*NPRIV
!>    </td></tr>
!>          <tr><td>NVCOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVHMA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU POUR L'UTILISATEUR DE DIMENSION
!>    </td></tr>
!>          <tr><td>TC1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TC2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TV1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TV2
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORMAR
     &( AT    , LT    , TC1   , TC2   , TV1   , TV2   , TM1   , TM2   ,
     &  NPC   , NPM   , NVHMA , NVCOU )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |---| 
C| F             |<--| DENSITE SPECTRALE D'ENERGIE
C| LT             |---| 
C| NPC            |---| 
C| NPM            |---| 
C| NPRIV          |---| NPOIN3*NPRIV
C| NVCOU          |---| 
C| NVHMA          |---| 
C| PRIVE          |-->| TABLEAU POUR L'UTILISATEUR DE DIMENSION
C| TC1            |---| 
C| TC2            |---| 
C| TM1            |---| 
C| TM2            |---| 
C| TV1            |---| 
C| TV2            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER          NPC , NPM, NVHMA, NVCOU
      INTEGER          LT
      DOUBLE PRECISION AT, TC1, TC2 , TV1, TV2, TM1 , TM2
C
C     LOCAL VARIABLES
      INTEGER N1,N2,N3,N4
!BD_INCKA IN PARALLEL MODE THE SIZE OF THE ARRAY NEEDS EXTENDING
C         BECAUSE SOME OF THE ARRAYS HAVE BEEN UNDERESTIMATED
      INTEGER NPOIN4
C
C-----------------------------------------------------------------------
C         UPDATES THE TIDAL CURRENT AND WATER LEVEL ARRAYS
C       ==============================================================
C
C            UPDATES THE CURRENT AT TIME 'AT'
C         ---------------------------------------------
C
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      NPOIN4 = MAX(NPOIN3,NPOIN3*NCSIZE*3)
      N1=NPOIN4+1
      N2=2*NPOIN4
      N3=N2+1
      N4=3*NPOIN4
C      N1=NPOIN3+1
C      N2=2*NPOIN3
C      N3=N2+1
C      N4=3*NPOIN3
!BD_INCKA IN THIS SUBROUTINE NPOIN4 REPLACES NPOIN3
C
!BD_INCKA MODIFICATION WITH CORRECT FORMAT
C      IF (NOMCOB(1:1).NE.' ') THEN
      IF (WAC_FILES(WACCOB)%NAME(1:1).NE.' ') THEN
!BD_INCKA END OF MODIFICATION
C        WRITE(*,*)'ON LIT DES COURANT DANS LE FICHIER U'
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
!BD_INCKA MODIFICATION WITH CORRECT FORMAT
C     *   NCOB     , BINCOU    , NBOR       , NPTFR , AT , DDC        ,
     &   WAC_FILES(WACCOB)%LU , BINCOU , NBOR      , NPTFR , AT , DDC ,
CBD_INCA END OF MODIFICATION
     &   TC1      , TC2       , NPC        , SXRELC%R, SYRELC%R,
     &   TRA01(1:NPOIN4),TRA01(N1:N2),TRA01(N3:N4) ,
     &   SUC1%R, SVC1%R , SUC2%R  , SVC2%R  , INDIC  ,
     &   'COURANT', NVCOU     )

!BD_INCKA MODIFICATION WITH CORRECT FILENAME
C      ELSEIF (NOMCOF(1:1).NE.' ') THEN
      ELSEIF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
!BD_INCKA END OF MODIFICATION
C        WRITE(*,*)'ON LIT DES COURANT DANS LE FICHIER F'
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
!BD_INCKA MODIFICATION WITH CORRECT FORMAT
C     *   NCOF     , BINCOU    , NBOR       , NPTFR , AT , DDC        ,
     &   WAC_FILES(WACCOF)%LU  , BINCOU , NBOR   , NPTFR , AT , DDC   ,
!BD_INCKA END OF MODIFICATION
     &   TC1      , TC2       , NPC        , SXRELC%R, SYRELC%R,
     &   TRA01(1:NPOIN4),TRA01(N1:N2),TRA01(N3:N4) ,
     &   SUC1%R, SVC1%R , SUC2%R  , SVC2%R  , INDIC  ,
     &   'COURANT', NVCOU )
      ELSE
        CALL ANAMAR
     & ( SUC%R   , SVC%R   , TRA01(1:NPOIN2), ZM1 , ZM2 ,
     &   SDZHDT%R, MESH%X%R, MESH%Y%R    ,
     &   NPOIN2     , AT   , DDC , LT )
      ENDIF
C
C            UPDATES THE WATER DEPTH AT TIME 'AT'
C         ------------------------------------------------------
C
!BD_INCKA MODIFICATION WITH CORRECT FORMAT
C      IF (NOMMAB(1:1).NE.' ') THEN
      IF (WAC_FILES(WACMAB)%NAME(1:1).NE.' ') THEN
!BD_INCKA END OF MODIFICATION
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
!BD_INCKA MODIFICATION WITH CORRECT FORMAT
C     *  NPOIN2  , NMAB  , BINMAR     , NBOR , NPTFR, AT    , DDC ,
     &  NPOIN2,WAC_FILES(WACMAB)%LU,BINMAR,NBOR,NPTFR,AT    , DDC ,
!BD_INCKA END OF MODIFICATION
     &  TM1     , TM2   , NPM        , SXRELM%R , SYRELM%R ,
     &  TRA01(N1:N2), TRA01(N3:N4), ZM1, ZM2 , INDIM , IDHMA , NVHMA)
!BD_INCKA MODIFICATION WITH CORRECT FILENAME
C      ELSEIF (NOMMAF(1:1).NE.' ') THEN
      ELSEIF (WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
!BD_INCKA END OF MODIFICATION
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
!BD_INCKA MODIFICATION WITH CORRECT FORMAT
C     *  NPOIN2  , NMAF  , BINMAR     , NBOR , NPTFR, AT    , DDC ,
     &  NPOIN2 ,WAC_FILES(WACMAF)%LU,BINMAR,NBOR,NPTFR,AT   , DDC ,
!BD_INCKA END OF MODIFICATION
     &  TM1     , TM2   , NPM        , SXRELM%R , SYRELM%R ,
     &  TRA01(N1:N2), TRA01(N3:N4), ZM1, ZM2 , INDIM , IDHMA , NVHMA)
      ELSE
!BD_INCKA MODIFICATION WITH CORRECT FILENAME
C        IF((NOMCOF(1:1).NE.' ').OR.(NOMCOB(1:1).NE.' ')) THEN
        IF((WAC_FILES(WACCOF)%NAME(1:1).NE.' ').OR.
     &                    (WAC_FILES(WACCOB)%NAME(1:1).NE.' ')) THEN
!BD_INCKA END OF MODIFICATION
C         WRITE(*,*)'ON LIT DE LA MAREE DANS LE FICHIER ANALYTIQUE'
          CALL ANAMAR
     &   ( SUC%R   , SVC%R   , TRA01(1:NPOIN2), ZM1 , ZM2 ,
     &     SDZHDT%R, MESH%X%R, MESH%Y%R    , NPOIN2    ,
     &     AT   , DDC , LT )
       ENDIF
      ENDIF
C
      CALL OV('X=X+Y   ', SDEPTH%R , TRA01(1:NPOIN2) , ST0%R ,
     &         0.D0 , NPOIN2)
C
C
C            UPDATES THE CURRENT AND WATER DEPTH
C                GRADIENTS AT TIME 'AT'
C         ------------------------------------------------------
C
C W1 ( EX MASKEL) SET TO 1 FOR GRADF
C
      CALL OV ( 'X=C     ' , SW1%R , ST0%R , ST1%R ,
     &          1.D0 , NELEM2 )
C
      CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SDEPTH,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST2,'=','GRADF          X',IELM2,1.D0,SUC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST3,'=','GRADF          X',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
          CALL PARCOM(ST4,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
C
      CALL OV('X=Y/Z   ',SDZX%R,ST1%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDUX%R,ST2%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDVX%R,ST3%R,ST4%R,0.D0,NPOIN2)
C
      CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SDEPTH,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST2,'=','GRADF          Y',IELM2,1.D0,SUC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST3,'=','GRADF          Y',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
          CALL PARCOM(ST4,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
C
      CALL OV('X=Y/Z   ',SDZY%R,ST1%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDUY%R,ST2%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDVY%R,ST3%R,ST4%R,0.D0,NPOIN2)
C
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C