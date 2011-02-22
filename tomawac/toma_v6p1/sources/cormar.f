
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES ARRAYS OF PHYSICAL PARAMETERS.

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

C
C#######################################################################
C
                        SUBROUTINE CORMAR
     &( AT    , LT    , TC1   , TC2   , TV1   , TV2   , TM1   , TM2   ,
     &  NPC   , NPM   , NVHMA , NVCOU )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |---| 
C| F              |<--| DENSITE SPECTRALE D'ENERGIE
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
      USE INTERFACE_TOMAWAC, EX_CORMAR => CORMAR
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
C
C-----------------------------------------------------------------------
C         UPDATES THE TIDAL CURRENT AND WATER LEVEL ARRAYS
C       ==============================================================
C
C            UPDATES THE CURRENT AT TIME 'AT'
C         ---------------------------------------------
C
      N1=NPOIN3_G+1
      N2=2*NPOIN3_G
      N3=N2+1
      N4=3*NPOIN3_G
C
      IF (WAC_FILES(WACCOB)%NAME(1:1).NE.' ') THEN
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
     &   WAC_FILES(WACCOB)%LU , BINCOU , NBOR      , NPTFR , AT , DDC ,
     &   TC1      , TC2       , NPC        , SXRELC%R, SYRELC%R,
     &   TRA01(1:NPOIN3_G),TRA01(N1:N2),TRA01(N3:N4) ,
     &   SUC1%R, SVC1%R , SUC2%R  , SVC2%R  , INDIC  ,
     &   'COURANT', NVCOU     )
      ELSEIF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
     &   WAC_FILES(WACCOF)%LU  , BINCOU , NBOR   , NPTFR , AT , DDC   ,
     &   TC1      , TC2       , NPC        , SXRELC%R, SYRELC%R,
     &   TRA01(1:NPOIN3_G),TRA01(N1:N2),TRA01(N3:N4) ,
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
      IF (WAC_FILES(WACMAB)%NAME(1:1).NE.' ') THEN
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
     &  NPOIN2,WAC_FILES(WACMAB)%LU,BINMAR,NBOR,NPTFR,AT    , DDC ,
     &  TM1     , TM2   , NPM        , SXRELM%R , SYRELM%R ,
     &  TRA01(N1:N2), TRA01(N3:N4), ZM1, ZM2 , INDIM , IDHMA , NVHMA)
      ELSEIF (WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
     &  NPOIN2 ,WAC_FILES(WACMAF)%LU,BINMAR,NBOR,NPTFR,AT   , DDC ,
     &  TM1     , TM2   , NPM        , SXRELM%R , SYRELM%R ,
     &  TRA01(N1:N2), TRA01(N3:N4), ZM1, ZM2 , INDIM , IDHMA , NVHMA)
      ELSE
        IF((WAC_FILES(WACCOF)%NAME(1:1).NE.' ').OR.
     &     (WAC_FILES(WACCOB)%NAME(1:1).NE.' ')) THEN
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
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
