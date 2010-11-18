C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES USEFUL ARRAYS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IBOR1, IFABOR1, NELEM2_DIM
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::B B@endlink, 
!> @link DECLARATIONS_TOMAWAC::COSF COSF@endlink, 
!> @link DECLARATIONS_TOMAWAC::COSTET COSTET@endlink, 
!> @link DECLARATIONS_TOMAWAC::COURAN COURAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::DFREQ DFREQ@endlink, 
!> @link DECLARATIONS_TOMAWAC::ETAP1 ETAP1@endlink, 
!> @link DECLARATIONS_TOMAWAC::FREQ FREQ@endlink, 
!> @link DECLARATIONS_TOMAWAC::IELM2 IELM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink, 
!> @link DECLARATIONS_TOMAWAC::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NF NF@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TOMAWAC::RAISF RAISF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDEPTH SDEPTH@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDUX SDUX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDUY SDUY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDVX SDVX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDVY SDVY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZX SDZX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZY SDZY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SINTET SINTET@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPHE SPHE@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST0 ST0@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST1 ST1@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST2 ST2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST3 ST3@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST4 ST4@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC SUC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC SVC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SW1 SW1@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA TETA@endlink, 
!> @link DECLARATIONS_TOMAWAC::TGF TGF@endlink, 
!> @link DECLARATIONS_TOMAWAC::Y Y@endlink
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUXI, C, DEGRAD, IELEM2, IFREQ, IPLAN, IPOIN, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PARCOM(), VECTOR()
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
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> DC
!> </td><td> ADDED ARG NPOIN2 TO DIMENSION THE ARRAYS
!> </td></tr>
!>      <tr>
!>      <td><center> 1.2                                       </center>
!> </td><td> 23/05/96
!> </td><td> F.MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IBOR1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2_DIM
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INITAB
     & (IBOR1,IFABOR1,NELEM2_DIM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IBOR1          |---| 
C| IFABOR1        |---| 
C| NELEM2_DIM     |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
      DOUBLE PRECISION DEGRAD,Z,C
C
C$DC$ : NPOIN2 -> NPOIN2_DIM TO DIMENSION ARRAYS
C
C     INTEGER IBOR1(NELEM2,7),IFABOR1(NELEM2,3)
      INTEGER NELEM2_DIM
      INTEGER IBOR1(NELEM2_DIM,7),IFABOR1(NELEM2_DIM,3)

      INTEGER          IPLAN, IPOIN, IELEM2, IFREQ
      DOUBLE PRECISION AUXI
C
C-----------------------------------------------------------------------
C
      DO IPLAN = 1,NPLAN
         COSTET(IPLAN) = COS(TETA(IPLAN))
         SINTET(IPLAN) = SIN(TETA(IPLAN))
         IF (ABS(COSTET(IPLAN)).LT.1.D-10) COSTET(IPLAN)=0.D0
         IF (ABS(SINTET(IPLAN)).LT.1.D-10) SINTET(IPLAN)=0.D0
         IF (IPLAN.LT.NPLAN) THEN
            ETAP1(IPLAN)=IPLAN+1
         ELSE
            ETAP1(IPLAN)=1
         ENDIF
      ENDDO
C
       AUXI=(RAISF-1.D0)/2.D0
       DFREQ(1)=AUXI*FREQ(1)
       DFREQ(NF)=AUXI*FREQ(NF-1)
       DO IFREQ = 2,NF-1
         DFREQ(IFREQ) = AUXI*(FREQ(IFREQ)+FREQ(IFREQ-1))
         DO IPOIN=1,NPOIN2
           B(IPOIN+(IFREQ-1)*NPOIN2)=0.D0
         ENDDO
       ENDDO
C
      IF (SPHE) THEN
         DEGRAD=1.745329252D-2
         DO 30 IPOIN=1,NPOIN2
           COSF(IPOIN)=COS(Y(IPOIN)*DEGRAD)
           TGF(IPOIN)=TAN(Y(IPOIN)*DEGRAD)
30       CONTINUE
      ENDIF
C
      DO 40 IELEM2=1,NELEM2
         IBOR1(IELEM2,1)=IFABOR1(IELEM2,1)
         IBOR1(IELEM2,2)=IFABOR1(IELEM2,2)
         IBOR1(IELEM2,3)=IFABOR1(IELEM2,3)
         IBOR1(IELEM2,4)=1
         IBOR1(IELEM2,5)=1
         IBOR1(IELEM2,6)=1
         IBOR1(IELEM2,7)=1
40    CONTINUE
C
C INITIALISES THE GRADIENTS OF DEPTH, U AND V
C
C W1 ( EX MASKEL) IS SET TO 1 FOR GRADF
C
      CALL OV ( 'X=C     ' , SW1%R, ST1%R, ST2%R,
     &                       1.D0 , NELEM2 )
C
      IF (.NOT.PROINF)
     &CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SDEPTH,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      IF (COURAN) THEN
      CALL VECTOR(ST2,'=','GRADF          X',IELM2,1.D0,SUC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST3,'=','GRADF          X',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
      ENDIF
C
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          IF (.NOT.PROINF) CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST4,2,MESH)
          IF (COURAN) THEN
            CALL PARCOM(ST2,2,MESH)
            CALL PARCOM(ST3,2,MESH)
          ENDIF
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      IF (.NOT.PROINF)
     & CALL OV('X=Y/Z   ',SDZX%R,ST1%R,ST4%R,C,NPOIN2)
      IF (COURAN) THEN
       CALL OV('X=Y/Z   ',SDUX%R,ST2%R,ST4%R,C,NPOIN2)
       CALL OV('X=Y/Z   ',SDVX%R,ST3%R,ST4%R,C,NPOIN2)
      ENDIF
C
      IF (.NOT.PROINF)
     & CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SDEPTH,
     &  ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      IF (COURAN) THEN
       CALL VECTOR(ST2,'=','GRADF          Y',IELM2,1.D0,SUC,
     &  ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
      CALL VECTOR(ST3,'=','GRADF          Y',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
      ENDIF
C
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
C
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          IF (.NOT.PROINF) CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST4,2,MESH)
          IF (COURAN) THEN
            CALL PARCOM(ST2,2,MESH)
            CALL PARCOM(ST3,2,MESH)
          ENDIF
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      IF (.NOT.PROINF)
     & CALL OV('X=Y/Z   ',SDZY%R,ST1%R,ST4%R,C,NPOIN2)
      IF (COURAN) THEN
       CALL OV('X=Y/Z   ',SDUY%R,ST2%R,ST4%R,C,NPOIN2)
       CALL OV('X=Y/Z   ',SDVY%R,ST3%R,ST4%R,C,NPOIN2)
      ENDIF
C
C-----------------------------------------------------------------------
      RETURN
      END
C
C#######################################################################
C