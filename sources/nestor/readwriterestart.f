      SUBROUTINE  ReadWriteRestart               !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( time, whatToDo )

!      USE m_TypeDefs_Nestor
!      USE m_Nestor, ONLY :  F, nFields, ipid, ncsize
!     &                     , ParallelComputing

      IMPLICIT NONE

      REAL (KIND=8) ,INTENT(IN)    :: time          !  time [s]
      CHARACTER(8)  ,INTENT(IN)    :: whatToDo

      !------- local variables ---------------
      CHARACTER (128) :: fileName

      WRITE(6,*)'?>-------  SR ReadWriteRestart -------------'

      IF( whatToDo(1:4) == 'read' ) THEN  ! read one restart file

        !fileName = "_restart_Nestor.dat"
         fileName = "DSCFG1"
        CALL ReadActionToRestart( fileName )
        CALL ReadFieldToRestart(  fileName )
      ENDIF

      IF( whatToDo(1:5) == 'write' ) THEN ! write two restart files

        fileName = "_restart_DigActions.dat"
        CALL WriteActionToRestart( time, fileName )

        fileName = "_restart_FieldData.dat"
         CALL WriteFieldToRestart(  time, fileName )
      ENDIF

      WRITE(6,*)'?>-------  SR ReadWriteRestart End ---------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE ReadWriteRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
!
!
cd!      manual:   FieldDump < 1    ==> kein Verklappen
!      manual:   IF( A%MinVolume >= 0.0_8 ) CALL CalcDigVolumeInRadius(...)
!      manual:         IF( A(m)%GrainClass(1) < -0.0000000_8) CYCLE !> A negative value indicates
!                                                     !  that dummy values are used
!
!      manual: in general all dummy values must be negative ( or zero ? )
!      manual:
!      manual:
!      manual:
!      manual:
!      Annahme : ELAY0    = constant
!      Annahme : Timestep = constant
!      Annahme :
!      Annahme :
!      Annahme :
!      Annahme :
!      Annahme :

!      ?Frage: wird  CALL PARCOM(T13,2,MESH) nach meinen Tests nicht benötigt
!      ?       aber warum wird es in Telemac immer in kombination mit siehe unten
!      ?      aufgerufen
!      ?      CALL VECTOR(T13,'=','MASBAS          '
!      ?     &      ,IELMH_SIS,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)
!      ?!         IF(NCSIZE.GT.1) CALL PARCOM(T13,2,MESH)

!
!      to do:  Berücksichtigung von Ein- , Austrag in action field durch Transport
!
!      to do:  min Baggermenge   Problem ist  die Parallelisierung !! ist erledigt ?
!
!      to do:  testen ob zugabe mit const Dicke noch korret arbeitet
!                ungetestet:  IF( A(m)%tsCount  >  A(m)%nts ) THEN ! The action is over
!                ungetestet:    A(m)%State = 2                     ! 2 = Action for ever inactive
!                ungetestet:  ENDIF
!
!      to do:  RefWSP: aus Dreiecks-Gitter interpolieren
!      to do:  RefWSP: mehrere Ref-horizonte ermöglichen
!      to do:  SR: Dig_by_Criterion  layCL(:), heapCL(:)   bisher nicht deallociert
!      to do:
!      to do:  EXTERNAL  P_SYNC   wann ist die EXTERNAL Anweisung zwingend ?
!      to do:
!      to do:  restart:  beim Einlesen der Restartdatei auf gleiche Prozessorzahl prüfen und ggf. error message
!      to do:
!      to do:  Fehlermeldung auf Prozessor 0 lenken
!      to do:
!      to do:  Fehler/unplausibles Verhalten wenn "DigDepth" kleiner "CritDepth"
!      to do:
!      to do:  "ActionType"   "ActionTypeStr"   eins von beiden ist überflüssig
!      to do:
!      to do:  "DumpPlanar =  TRUE" sollte auch beim "Dig_by_criterion" möglich sein
!      to do:
!      to do:  Bei Einlese-Schleifen IOSTAT = stat ; if(stat.NE.0) testen
!      to do:  bzw. END = 600 einbauen
!      to do:
!      to do:


!













!          !*********************************************************************************************
!          !*********************************************************************************************
!          !***                                              ********************************************
!          !***                                              ********************************************
