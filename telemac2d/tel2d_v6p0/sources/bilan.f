C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALCULATES THE BALANCE OF THE MASS OF WATER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DT, EQUA, FLBOR, FLUX_BOUNDARIES, H, INFO, LT, MASK, MASKEL, MASSES, MESH, MSK, NFRLIQ, NIT, NPTFR, NUMLIQ, OPTBAN, POROS, WORK
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
!>    </th><td> DENOM, ERREUR, FLUX1, I, MASENT, MASSE0, MASSE1, MASSE2, MASSET, PERDUE, RELATI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_SUM(), ENTETE(), OS(), P_DSUM(), VECTOR()
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 27/03/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> PRINTS FLUXES PER BOUNDARY INSTEAD OF FREE AND IMPOSED FLUX
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 14/01/2005
!> </td><td>
!> </td><td> COMPATIBLE COMPUTATION OF FLUXES AT EXITS
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>EQUA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUX_BOUNDARIES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON FAIT LES IMPRESSIONS
!>    </td></tr>
!>          <tr><td>LT,NIT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS, NOMBRE TOTAL DE PAS.
!>    </td></tr>
!>          <tr><td>MASK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASSES
!></td><td>--></td><td>MASSE APPORTEE PAR TERME SOURCE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>POROS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WORK
!></td><td>--></td><td>TABLEAU DE TRAVAIL.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BILAN
     &(MESH,H,WORK,MASK,AT,DT,LT,NIT,INFO,MASSES,MSK,MASKEL,EQUA,POROS,
     & OPTBAN,NPTFR,FLBOR,FLUX_BOUNDARIES,NUMLIQ,NFRLIQ)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| DT             |-->| PAS DE TEMPS
C| EQUA           |---| 
C| FLBOR          |---| 
C| FLUX_BOUNDARIES|---| 
C| H             |---| 
C| INFO           |-->| LOGIQUE INDIQUANT SI ON FAIT LES IMPRESSIONS
C| LT,NIT         |-->| NUMERO DU PAS DE TEMPS, NOMBRE TOTAL DE PAS.
C| MASK           |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASSES         |-->| MASSE APPORTEE PAR TERME SOURCE.
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NFRLIQ         |---| 
C| NPTFR          |---| 
C| NUMLIQ         |---| 
C| OPTBAN         |---| 
C| POROS          |---| 
C| WORK           |-->| TABLEAU DE TRAVAIL.
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
C     SIZE OF NUMLIQ AND FLUX_BOUNDARIES IS NFRLIQ BUT NFRLIQ
C     CAN BE 0.
C
      INTEGER, INTENT(IN)            :: LT,NIT,OPTBAN,NPTFR,NFRLIQ
      INTEGER, INTENT(IN)            :: NUMLIQ(*)
      CHARACTER(LEN=20), INTENT(IN)  :: EQUA
      LOGICAL, INTENT(IN)            :: INFO,MSK
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: WORK,FLBOR
      TYPE(BIEF_OBJ), INTENT(IN)     :: H,MASKEL,POROS,MASK
      DOUBLE PRECISION, INTENT(IN)   :: AT,DT
      DOUBLE PRECISION, INTENT(INOUT):: MASSES,FLUX_BOUNDARIES(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
      DOUBLE PRECISION ERREUR,FLUX1,PERDUE,DENOM
      DOUBLE PRECISION MASSE0,MASSE1,MASSE2,MASENT,RELATI,MASSET
C
      INTRINSIC ABS
C
      SAVE MASSE0,MASSE1,MASSE2,MASENT,MASSET
C
C-----------------------------------------------------------------------
C
C  COMPATIBLE CALCULATION OF THE MASS OF WATER
C
      IF(LT.NE.0) MASSE1 = MASSE2
C
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CALL VECTOR(WORK,'=','MASBAS          ',H%ELM,
     &              1.D0,H,H,H,H,H,H,MESH,MSK,MASKEL)
        CALL OS( 'X=XY    ' , X=WORK , Y=H )
      ELSEIF(OPTBAN.EQ.3) THEN
        CALL VECTOR(WORK,'=','MASVEC          ',H%ELM,
     &              1.D0,H,H,H,H,H,H,MESH,.TRUE.,POROS)
      ELSE
        CALL VECTOR(WORK,'=','MASVEC          ',H%ELM,
     &              1.D0,H,H,H,H,H,H,MESH,MSK,MASKEL)
      ENDIF
      MASSE2 = BIEF_SUM(WORK)
C
      IF(NCSIZE.GT.1) MASSE2 = P_DSUM(MASSE2)
C
      IF(LT.EQ.0) THEN
        MASSE0 = MASSE2
        MASSE1 = MASSE2
        MASENT = 0.D0
        MASSET = 0.D0
C
C       FOR THE FIRST CALL, RETURN HERE
C
        CALL OS('X=0     ',X=FLBOR)
        IF(NFRLIQ.GT.0) THEN
          DO I=1,NFRLIQ
            FLUX_BOUNDARIES(I)=0.D0
          ENDDO
        ENDIF
        RETURN
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   SOURCE TERMS ADDED TO MASS
C
      IF(NCSIZE.GT.1) MASSES = P_DSUM(MASSES)
      MASSET = MASSET + MASSES
C
C=======================================================================
C
C   CALCULATES FLUXES AT THE LIQUID BOUDNARIES
C
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUX_BOUNDARIES(I)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO I=1,NPTFR
C           NOTE: ONE COULD DEFINE FLUX_BOUNDARIES BETWEEN 0 AND NFRLIQ
            IF(NUMLIQ(I).GT.0) THEN
              FLUX_BOUNDARIES(NUMLIQ(I))=
     &        FLUX_BOUNDARIES(NUMLIQ(I))+FLBOR%R(I)
            ENDIF
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO I=1,NFRLIQ
            FLUX_BOUNDARIES(I)=P_DSUM(FLUX_BOUNDARIES(I))
          ENDDO
        ENDIF
      ENDIF
C
C=======================================================================
C
C   TOTAL FLUX AT THE LIQUID BOUNDARY
C
      FLUX1=0.D0
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUX1=FLUX1+FLUX_BOUNDARIES(I)
        ENDDO
      ENDIF
C
C=======================================================================
C
      MASENT = MASENT - FLUX1*DT
C
C=======================================================================
C
C   CALCULATES THE ERROR ON THE MASS FOR THIS TIME STEP
C
      ERREUR = MASSE1 + MASSES - MASSE2 - DT*FLUX1
C
C=======================================================================
C
C   PRINTS:
C
      IF(INFO) THEN
C
C-----------------------------------------------------------------------
C
C     PRINTS THE MASS OF WATER
C
        IF(LT.EQ.0) THEN
C
          CALL ENTETE(7,AT,LT)
          IF(LNG.EQ.1) WRITE(LU,1000) MASSE0
          IF(LNG.EQ.2) WRITE(LU,2000) MASSE0
C
        ELSE
C
          CALL ENTETE(7,AT,LT)
          IF(LNG.EQ.1) THEN
            WRITE(LU,1010) MASSE2
            IF(NFRLIQ.GT.0) THEN
              DO I=1,NFRLIQ
                WRITE(LU,3020) I,-FLUX_BOUNDARIES(I)
              ENDDO
            ENDIF
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,2010) MASSE2
            IF(NFRLIQ.GT.0) THEN
              DO I=1,NFRLIQ
                WRITE(LU,4020) I,-FLUX_BOUNDARIES(I)
              ENDDO
            ENDIF
          ENDIF
          IF(ABS(MASSES).GT.1.D-6) THEN
            IF(LNG.EQ.1) WRITE(LU,1031) MASSES
            IF(LNG.EQ.2) WRITE(LU,2031) MASSES
          ENDIF
C         CALCULATES THE RELATIVE OR ABSOLUTE ERROR
          DENOM = MAX(MASSE2,ABS(FLUX1*DT))
          IF(DENOM.GT.1.D-8) THEN
            ERREUR = ERREUR / DENOM
            IF(LNG.EQ.1) WRITE(LU,1040) AT,ERREUR
            IF(LNG.EQ.2) WRITE(LU,2040) AT,ERREUR
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1050) AT,ERREUR
            IF(LNG.EQ.2) WRITE(LU,2050) AT,ERREUR
          ENDIF
C
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  FINAL MASS BALANCE
C
      IF(LT.EQ.NIT.AND.INFO) THEN
C
        CALL ENTETE(8,AT,LT)
C       PERDUE = MASSE0+MASSET+MASENT+MASAJT-MASSE2
        PERDUE = MASSE0+MASSET+MASENT-MASSE2
        DENOM = MAX( MASSE0 , MASSE2 , ABS(MASENT) )
        IF(DENOM.GT.1.D-8) THEN
          RELATI = PERDUE / DENOM
          IF(LNG.EQ.1) WRITE(LU,1060) RELATI
          IF(LNG.EQ.2) WRITE(LU,2060) RELATI
        ELSE
          RELATI = PERDUE
          IF(LNG.EQ.1) WRITE(LU,1070) RELATI
          IF(LNG.EQ.2) WRITE(LU,2070) RELATI
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE(LU,1080) MASSE0,MASSE2
          IF(ABS(MASENT).GT.1.D-8) WRITE(LU,1081) MASENT
          IF(ABS(MASSET).GT.1.D-8) WRITE(LU,1082) MASSET
C         IF(ABS(MASAJT).GT.1.D-8) WRITE(LU,1083) MASAJT
          WRITE(LU,1084) PERDUE
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,2080) MASSE0,MASSE2
          IF(ABS(MASENT).GT.1.D-8) WRITE(LU,2081) MASENT
          IF(ABS(MASSET).GT.1.D-8) WRITE(LU,2082) MASSET
C         IF(ABS(MASAJT).GT.1.D-8) WRITE(LU,2083) MASAJT
          WRITE(LU,2084) PERDUE
        ENDIF
C
      ENDIF
C
C  END OF PRINTS
C
C=======================================================================
C
C  PRINT FORMATS:
C
1000  FORMAT(5X,'VOLUME D''EAU INITIAL DANS LE DOMAINE: ',G16.7,' M3')
2000  FORMAT(5X,'INITIAL WATER VOLUME IN THE DOMAIN: ',G16.7,' M3')
C
1010  FORMAT(5X,'VOLUME DANS LE DOMAINE :',G16.7,' M3')
2010  FORMAT(5X,'VOLUME IN THE DOMAIN :',G16.7,' M3')
C
1031  FORMAT(5X,'VOLUME AJOUTE PAR TERME SOURCE : ',G16.7,' M3')
2031  FORMAT(5X,'ADDITIONAL VOLUME DUE TO SOURCE TERMS: ',G16.7,' M3')
C
1040  FORMAT(5X,'ERREUR RELATIVE EN VOLUME A T = ',G16.4,' S : ',G16.7)
2040  FORMAT(5X,'RELATIVE ERROR IN VOLUME AT T = ',G16.4,' S : ',G16.7)
C
1050  FORMAT(5X,'ERREUR ABSOLUE EN VOLUME A T = ',G16.4,' S: ',G16.7)
2050  FORMAT(5X,'ABSOLUTE ERROR IN VOLUME AT T = ',G16.4,'S: ',G16.7)
C
1060  FORMAT(/,5X,'ERREUR RELATIVE CUMULEE SUR LE VOLUME : ',G16.7)
2060  FORMAT(/,5X,'RELATIVE ERROR CUMULATED ON VOLUME: ',G16.7)
C
1070  FORMAT(/,5X,'ERREUR ABSOLUE CUMULEE SUR LE VOLUME : ',G16.7)
2070  FORMAT(/,5X,'ABSOLUTE ERROR CUMULATED ON VOLUME: ',G16.7)
C
1080  FORMAT(/,5X,'VOLUME INITIAL              : ',G16.7,' M3',
     &       /,5X,'VOLUME FINAL                : ',G16.7,' M3')
1081  FORMAT(  5X,'VOLUME ENTRE AUX FRONTIERES : ',G16.7,' M3',
     &            '  ( SI <0 VOLUME SORTI )')
1082  FORMAT(  5X,'VOLUME AJOUTE ( SOURCES   ) : ',G16.7,' M3')
C1083  FORMAT(  5X,'VOLUME AJOUTE ( CDT. LIM. ) : ',G16.7,' M3')
1084  FORMAT(  5X,'VOLUME TOTAL PERDU          : ',G16.7,' M3')
2080  FORMAT(/,5X,'INITIAL VOLUME              : ',G16.7,' M3',
     &       /,5X,'FINAL VOLUME                : ',G16.7,' M3')
2081  FORMAT(  5X,'VOLUME THAT ENTERED THE DOMAIN: ',G16.7,' M3',
     &            '  ( IF <0 EXIT )')
2082  FORMAT(  5X,'VOLUME ADDED BY SOURCE TERM   : ',G16.7,' M3')
2084  FORMAT(  5X,'TOTAL VOLUME LOST             : ',G16.7,' M3')
3020  FORMAT(5X,'FLUX FRONTIERE ',I4,' : ', G16.7 ,' M3/S',
     &          '  ( >0 : ENTRANT  <0 : SORTANT )')
4020  FORMAT(5X,'FLUX BOUNDARY ',I4,': ', G16.7 ,' M3/S',
     &          '  ( >0 : ENTERING  <0 : EXITING )')
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C