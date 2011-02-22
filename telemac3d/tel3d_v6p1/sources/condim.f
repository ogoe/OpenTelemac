C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES VELOCITY, DEPTH AND TRACERS.

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
!> </td><td> 05/05/2010
!> </td><td> J-M HERVOUET(LNHE) 01 30 87 80 18
!> </td><td> SUPPRESSED INITIALISATION OF DPWAVE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/03/2010
!> </td><td>
!> </td><td> NEW OPTIONS FOR BUILDING THE MESH IN CONDIM, SEE BELOW
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 23/01/2009
!> </td><td>
!> </td><td> ADDED CHECK OF ZSTAR
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/04/2007
!> </td><td>
!> </td><td> ADDED INITIALISATION OF DPWAVE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONDIM
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_CONDIM => CONDIM
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN, I,J
!
!***********************************************************************
!
C     ORIGIN OF TIME
!
      IF(.NOT.SUIT2) AT  = 0.D0
!
C     INITIALISES H, THE WATER DEPTH
!
      IF(.NOT.SUIT2) THEN
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' ,X=H,C=COTINI)
        CALL OV( 'X=X-Y   ' , H%R , Z , Z , 0.D0 , NPOIN2 )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=0.D0)
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' ,X=H,C=HAUTIN)
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
C     USER INPUT :
C     PROGRAM HERE SPECIAL INITIAL CONDITIONS ON DEPTH
        IF(LNG.EQ.1) WRITE(LU,10)
        IF(LNG.EQ.2) WRITE(LU,11)
10      FORMAT(1X,'CONDIM : AVEC DES CONDITIONS INITIALES PARTICULIERES'
     &      ,/,1X,'         VOUS DEVEZ MODIFIER CONDIM')
11      FORMAT(1X,'CONDIM : WITH SPECIAL INITIAL CONDITIONS'
     &      ,/,1X,'         YOU HAVE TO MODIFY CONDIM')
        CALL PLANTE(1)
        STOP
C     END OF SPECIAL INITIAL CONDITIONS
C     END OF USER INPUT
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIM : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIM: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        STOP
      ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'HAUTEUR LUE DANS LE FICHIER BINAIRE 1'
        IF(LNG.EQ.2) WRITE(LU,*) 'DEPTH IS READ IN THE BINARY FILE 1'
      ENDIF
!
C     CLIPS H
!
      DO I=1,NPOIN2
        H%R(I)=MAX(H%R(I),0.D0)
      ENDDO
!
      CALL OS ('X=Y     ',X=HN,Y=H)
!
!-----------------------------------------------------------------------
!
C     DATA TO BUILD VERTICAL COORDINATES IN CALCOT
!
C     TRANSF IS KEYWORD "MESH TRANSFORMATION"
C     IF TRANSF = 0, SUBROUTINE CALCOT MUST BE IMPLEMENTED BY THE USER
!
C     AN EQUIVALENT OF TRANSF MUST BE GIVEN FOR EVERY PLANE:
!
C     POSSIBLE VALUES OF TRANSF_PLANE :
!
C     1 : SIGMA TRANSFORMATION WITH EVENLY SPACED PLANES
C     2 : SIGMA TRANSFORMATION WITH PROPORTIONS GIVEN IN ZSTAR
C     3 : PRESCRIBED ELEVATION GIVEN IN ZPLANE
!
C     STANDARD BELOW IS: EVENLY SPACED PLANES, NO OTHER DATA REQUIRED
!
      DO IPLAN = 1,NPLAN
        TRANSF_PLANE%I(IPLAN)=1
      ENDDO
C
C     OTHER EXAMPLES:
C
C     EXAMPLE 1: ALL PLANES WITH PRESCRIBED ELEVATION
C
C     DO IPLAN = 1,NPLAN
C       TRANSF_PLANE%I(IPLAN)=3
C     ENDDO
C     ZPLANE%R(2)=-7.D0
C     ZPLANE%R(3)=-4.D0
C     ...
C     ZPLANE%R(NPLAN-1)=-0.05D0
C
C
C     EXAMPLE 2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
C
C     DO IPLAN = 1,NPLAN
C       TRANSF_PLANE%I(IPLAN)=2
C     ENDDO
C     ZSTAR%R(2)=0.02D0
C     ZSTAR%R(3)=0.1D0
C     ...
C     ZSTAR%R(NPLAN-1)=0.95D0
C
C
C     EXAMPLE 3: ONE PLANE (NUMBER 4) WITH PRESCRIBED ELEVATION
C                AND SIGMA ELSEWHERE
C
C     DO IPLAN = 1,NPLAN
C       TRANSF_PLANE%I(IPLAN)=1
C     ENDDO
C     TRANSF_PLANE%I(4)=3
C     ZPLANE%R(4)=-3.D0
C
C
C     EXAMPLE 4: ONE PLANE WITH PRESCRIBED ELEVATION
C                AND 2 SIGMA TRANSFORMATIONS, WITH NPLAN=7
C                SIGMA TRANSFORMATIONS ARE MEANT BETWEEN
C                BOTTOM, FIXED ELEVATION PLANES AND FREE SURFACE
C                THE VALUES OF ZSTAR ARE LOCAL FOR EVERY
C                SIGMA TRANSFORMATION: 0. FOR LOWER FIXED PLANE
C                                      1. FOR UPPER FIXED PLANE
C
C     DO IPLAN = 1,7
C       TRANSF_PLANE%I(IPLAN)=2
C     ENDDO
C     TRANSF_PLANE%I(4)=3
C     ZPLANE%R(4)=3.D0
C     ZSTAR%R(2)=0.2D0
C     ZSTAR%R(3)=0.8D0
C     ZSTAR%R(5)=0.1D0
C     ZSTAR%R(6)=0.9D0
C
!
!***********************************************************************
!
C     COMPUTES ELEVATIONS
C     IF IT IS A CONTINUATION, WILL BE DONE AFTER CALLING 'SUITE'
!
      IF(DEBU) CALL CALCOT(Z,H%R)
!
!***********************************************************************
!
C     INITIALISES VELOCITIES
!
      IF(SUIT2) THEN
        DO I=1,NPLAN
          DO J=1,NPOIN2
           U%R((I-1)*NPOIN2+J)=U2D%R(J)
           V%R((I-1)*NPOIN2+J)=V2D%R(J)
          ENDDO
        ENDDO
      ELSE
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
      ENDIF
!
      CALL OS( 'X=0     ' , X=W )
!
!-----------------------------------------------------------------------
!
C     INITIALISES TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          CALL OS( 'X=C     ', X=TA%ADR(I)%P, C=TRAC0(I))
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
C   INITIALISES THE K-EPSILON MODEL (OPTIONAL)
C   WHEN DONE: AKEP = .FALSE.
!
      AKEP=.TRUE.
!
C     IF(ITURBV.EQ.3) THEN
!
C       HERE INITIALISES K AND EPSILON
!
C       AKEP = .FALSE.
C     ENDIF
!
!-----------------------------------------------------------------------
!
C INITIALISES THE PRESSURE FIELDS TO 0.0
!
      IF(NONHYD) THEN
        CALL OS('X=C     ',X=DP,C=0.D0)
        WRITE (LU,*) 'CONDIM: DYNAMIC PRESSURE INITIALISED TO ZERO'
        CALL OS('X=C     ',X=PH,C=0.D0)
        WRITE (LU,*) '        HYDROSTATIC PRESSURE INITIALISED TO ZERO.'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
