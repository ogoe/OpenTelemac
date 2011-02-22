C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!>                THE RESULTS FILE OR TO THE LISTING.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::AT AT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CF CF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ESTIME ESTIME@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FU FU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FV FV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LEOPRD LEOPRD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LISPRD LISPRD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIST_PTS LIST_PTS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LT LT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXV MAXV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXZ MAXZ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NAME_PTS NAME_PTS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NIT NIT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPTS NPTS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OUTINI OUTINI@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PTINIG PTINIG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PTINIL PTINIL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2 T2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T3 T3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T4 T4@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T5 T5@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T6 T6@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T7 T7@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T9 T9@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TMAXV TMAXV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TMAXZ TMAXZ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC2D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARSOR VARSOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AMP, DEJA1, DEJA2, DEJA3, HHH, I, IMAX, IMP, LEO, LTT, N, NF, PHA, PI, XMAX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CFLPSI(), CPSTVC(), MAXI(), OS(), P_DMAX(), P_DMIN(), SPECTRE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>OUTPUT_TELEMAC2D(), TELEMAC2D()

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
!> </td><td> 24/11/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>LT
!></td><td>--></td><td>ITERATION NUMBER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRERES_TELEMAC2D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| LT             |-->| ITERATION NUMBER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL IMP,LEO,DEJA1,DEJA2,DEJA3
C
      INTEGER LTT,N,IMAX,I
C
      DOUBLE PRECISION HHH,XMAX,NF,PI,AMP,PHA
C
      INTRINSIC MAX,SQRT
C
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
C
C-----------------------------------------------------------------------
C
      DATA DEJA1/.FALSE./
      DATA DEJA2/.FALSE./
      DATA DEJA3/.FALSE./
      SAVE DEJA1,DEJA2,DEJA3,NF
C
C-----------------------------------------------------------------------
C
C     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
C     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
C
C     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
C     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
C     FOR FURTHER COMPUTATIONS)
      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
        LISPRD=1
        LEOPRD=1
      ENDIF
C
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LT/LISPRD)*LISPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
      LTT=(LT/LEOPRD)*LEOPRD
      IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
C
      IF(LT.EQ.0) THEN
        IMP=OUTINI
        LEO=OUTINI
      ENDIF
C
C-----------------------------------------------------------------------
C
C 1)  PART WHICH MUST BE DONE EVEN IF THERE IS NO OUTPUT FOR THIS TIMESTEP
C     BUT ONLY AFTER FIRST TIMESTEP FOR GRAPHIC PRINTOUTS
C
C-----------------------------------------------------------------------
C
      IF(LT.GE.PTINIG) THEN
C
C=======================================================================
C COMPUTES THE MAXIMUM ELEVATION AND ASSOCIATED TIME
C=======================================================================
C
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        IF(.NOT.DEJA1) THEN
          CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
          CALL OS('X=C     ',X=TMAXZ,C=AT)
          DEJA1=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=H%R(N)+ZF%R(N)
C           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXZ%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXZ%R(N)=XMAX
              IF(SORLEO(28).OR.SORIMP(28)) TMAXZ%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
C=======================================================================
C COMPUTES THE MAXIMUM SPEED AND ASSOCIATED TIME
C=======================================================================
C
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        IF(.NOT.DEJA2) THEN
          CALL OS('X=C     ',X=MAXV ,C=0.D0)
          CALL OS('X=C     ',X=TMAXV,C=AT)
          DEJA2=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=SQRT(U%R(N)**2+V%R(N)**2)
C           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXV%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXV%R(N)=XMAX
              IF(SORLEO(30).OR.SORIMP(30)) TMAXV%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
C=======================================================================
C PRINTOUTS FOR THE REMARKABLE POINTS
C=======================================================================
C
      IF(LT.EQ.NIT.AND.NPTS.GT.0) THEN
        DO I=27,30
C         BEWARE : HERE SORLEO IS USED INSTEAD OF SORIMP
          IF(SORLEO(I)) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) TEXTE(I)(1:16)
            WRITE(LU,*) ' '
            DO N=1,NPTS
C             IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
              IF(NCSIZE.GT.0) THEN
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                    P_DMIN(VARSOR%ADR(I)%P%R(LIST_PTS(N)))+
     &                    P_DMAX(VARSOR%ADR(I)%P%R(LIST_PTS(N)))
              ELSE
                WRITE(LU,*) NAME_PTS(N),' : ',
     &                                    VARSOR%ADR(I)%P%R(LIST_PTS(N))
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
C     CASE WHERE OUTINI=.TRUE. : PRIORITY ON PTINIG, VALUES FOR LT=0
C     OTHERWISE THEY WOULD NOT BE INITIALISED
       IF(SORLEO(27).OR.SORIMP(27)) CALL OS('X=Y     ',X=MAXZ ,Y=ZF)
       IF(SORLEO(28).OR.SORIMP(28)) CALL OS('X=C     ',X=TMAXZ,C=AT)
       IF(SORLEO(29).OR.SORIMP(29)) CALL OS('X=C     ',X=MAXV ,C=0.D0)
       IF(SORLEO(30).OR.SORIMP(30)) CALL OS('X=C     ',X=TMAXV,C=AT)
C
C     ENDIF FOR : IF(LT.GE.PTINIG) THEN
      ENDIF
C
C-----------------------------------------------------------------------
C
C 2)  PART WHICH MUST BE DONE ONLY IF THERE IS AN OUTPUT FOR THIS TIMESTEP
C
C-----------------------------------------------------------------------
C
C     NO PRINTOUT REQUIRED (LISTING OR RESULT FILE): EXITS
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
C
C
C=======================================================================
C COMPUTES CELERITY (IN FU, SEE BLOCK: VARSOR)
C=======================================================================
C
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(ZF,FU)
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
C
C=======================================================================
C COMPUTES FREE SURFACE ELEVATION (= H + ZF, IN FV)
C=======================================================================
C
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL CPSTVC(ZF,FV)
        DO N=1,NPOIN
          FV%R(N) = H%R(N)+ZF%R(N)
        ENDDO
      ENDIF
C
C=======================================================================
C COMPUTES FROUDE NUMBER
C=======================================================================
C
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL CPSTVC(ZF,T2)
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
C
C=======================================================================
C COMPUTES FLOWRATE
C=======================================================================
C
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL CPSTVC(ZF,T3)
        DO N=1,NPOIN
         T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
C
C=======================================================================
C COMPUTES FLOWRATE ALONG X
C=======================================================================
C
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL CPSTVC(ZF,T4)
        DO N=1,NPOIN
          T4%R(N)=H%R(N)*U%R(N)
        ENDDO
      ENDIF
C
C=======================================================================
C COMPUTES FLOWRATE ALONG Y
C=======================================================================
C
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL CPSTVC(ZF,T5)
        DO N=1,NPOIN
          T5%R(N)=H%R(N)*V%R(N)
        ENDDO
      ENDIF
C
C=======================================================================
C COMPUTES SPEED
C=======================================================================
C
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , X=T6 , Y=U , Z=V )
      ENDIF
C
C=======================================================================
C COMPUTES COURANT NUMBER
C=======================================================================
C
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
C                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF(NCSIZE.GT.1) THEN
          IF(LNG.EQ.1) WRITE(LU,78) P_DMAX(XMAX)
          IF(LNG.EQ.2) WRITE(LU,79) P_DMAX(XMAX)
        ELSE
          IF(LNG.EQ.1) WRITE(LU,78) XMAX
          IF(LNG.EQ.2) WRITE(LU,79) XMAX
        ENDIF
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
C
C=======================================================================
C COMPUTES FRICTION SPEED
C=======================================================================
C
      IF((LEO.AND.SORLEO(31)).OR.(IMP.AND.SORIMP(31))) THEN
        CALL CPSTVC(CF,T7)
        DO N=1,NPOIN
          T7%R(N) = SQRT(0.5D0*CF%R(N)*(U%R(N)**2+V%R(N)**2))
        ENDDO
      ENDIF
C
C=======================================================================
C
1000  CONTINUE
C
C=======================================================================
C HARMONIC ANALYSIS USING LEAST MEAN ERROR SQUARE METHOD
C=======================================================================
C
      IF(NPERIAF.GT.0) CALL SPECTRE
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C